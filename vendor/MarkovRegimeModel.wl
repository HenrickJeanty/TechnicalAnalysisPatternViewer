(* ::Package:: *)

(* ================================================================
   Markov Regime Detection Model
   Mathematica implementation of the observable Markov regime
   framework: transition matrix, stationary distribution,
   n-step forecasting, walk-forward backtest, optional HMM.

   Usage:
     Get["MarkovRegimeModel.wl"]
     RunMarkovAnalysis["SPY", 10, 20]
   ================================================================ *)

BeginPackage["MarkovRegimeModel`"]

(* -- Public symbols ------------------------------------------- *)
FetchPrices::usage =
  "FetchPrices[ticker, years] fetches daily closing prices from \
   Yahoo Finance for the past `years` years. Returns an Association \
   <|date -> price, ...|>.";

LabelRegimes::usage =
  "LabelRegimes[prices, window, threshold] labels each day as \
   Bull (2), Sideways (1), or Bear (0) using a rolling return. \
   Returns a list of {date, label} pairs.";

BuildTransitionMatrix::usage =
  "BuildTransitionMatrix[labels] builds a 3x3 MLE transition matrix \
   from a list of integer state labels {0,1,2}.";

MarkovStationaryDist::usage =
  "MarkovStationaryDist[P] solves for the stationary distribution \
   of a row-stochastic matrix P. Returns a length-3 probability vector.";

NStepForecast::usage =
  "NStepForecast[P, n] returns the n-step transition matrix P^n \
   via Chapman-Kolmogorov (matrix power).";

SignalFromMatrix::usage =
  "SignalFromMatrix[P, currentState] returns P[Bull|current] - \
   P[Bear|current] as a signed conviction signal.";

WalkForwardBacktest::usage =
  "WalkForwardBacktest[prices, labels, minTrain] runs a walk-forward \
   backtest, re-estimating the transition matrix at each step using \
   only past data. Returns an Association with Sharpe, MaxDrawdown, \
   NTrades, and EquityCurve.";

FitHMM::usage =
  "FitHMM[returns, nStates] fits a Gaussian Hidden Markov Model to \
   a list of daily returns using Mathematica's built-in \
   EstimatedDistribution. Returns an Association with the fitted \
   model and Viterbi state sequence, or $Failed if unavailable.";

PrintMarkovReport::usage =
  "PrintMarkovReport[ticker, years, window, threshold] is the \
   top-level function. Fetches data, runs all analyses, and prints \
   a formatted report. Equivalent to the CLI entry point.";

RunMarkovAnalysis::usage =
  "RunMarkovAnalysis[ticker, years, window] is a convenience wrapper \
   around PrintMarkovReport with threshold -> 0.02.";


(* ================= Version 2.0 Fix =================================== *)
BuildTransitionMatrixStride::usage =
  "BuildTransitionMatrixStride[labeledDataArg, strideArg] builds a 3x3 \
transition probability matrix from non-overlapping windows of labeledDataArg. \
Unlike BuildTransitionMatrix which counts transitions between every consecutive \
pair (creating overlapping-window autocorrelation bias), this version counts \
transitions only between non-overlapping windows of length strideArg. \
This is the statistically honest version. Rows are FROM states, columns are TO \
states, indexed 1=Bear, 2=Sideways, 3=Bull. Each row sums to 1. Returns a \
uniform 1/3 matrix if no transitions are available.";



Begin["`Private`"]

(* -- Constants ------------------------------------------------ *)
$StateNames = <|0 -> "Bear", 1 -> "Sideways", 2 -> "Bull"|>;
$StateIndex = <|"Bear" -> 0, "Sideways" -> 1, "Bull" -> 2|>;


(* ================================================================
   1. DATA FETCHING
   Primary source: getStock from StockDataManager.wl
   Fallback: FinancialData (Mathematica built-in)
   Final fallback: Yahoo Finance CSV via HTTPS

   getStock returns a Tabular with Date, Open, High, Low, Close,
   Volume fields. We extract {DateObject, Close} pairs to match
   the {{date, price}, ...} format expected by LabelRegimes and
   WalkForwardBacktest.
   ================================================================ *)

FetchPrices[ticker_String, years_Integer: 10] :=
  Module[{endDate, startDate, raw, rows},
    endDate   = Today;
    startDate = DatePlus[endDate, {-years, "Year"}];

    (* Primary: getStock from StockDataManager.wl
       Requires SP500Stocks to be loaded via loadSP500Stocks[] *)
    If[ValueQ[Global`SP500Stocks],
      (* Use fully qualified getStock \[LongDash] try each known context *)
			raw = Quiet[
			  Which[
			    MemberQ[$ContextPath, "StockAnalyzer`StockDataManager`"],
			      StockAnalyzer`StockDataManager`getStock[ticker, startDate, endDate, "daily"],
			    MemberQ[$ContextPath, "StockDataManager`"],
			      StockDataManager`getStock[ticker, startDate, endDate, "daily"],
			    True,
			      $Failed
			  ]
			];
      If[!FailureQ[raw] && Length[Normal[raw]] > 0,
        rows = Normal[raw];
        (* Convert Tabular rows to {{DateObject, closePrice}, ...} *)
        Return[Map[{#["Date"], #["Close"]} &, rows]]
      ]
    ];

    (* First fallback: FinancialData built into Mathematica *)
    raw = Quiet[
      FinancialData[ticker, "Close",
        {startDate, endDate, "Day"}],
      {FinancialData::notent, FinancialData::nodata}
    ];
    If[!FailureQ[raw] && raw =!= $Failed && ListQ[raw] && Length[raw] > 0,
      Return[raw]
    ];

    (* Final fallback: Yahoo Finance CSV via HTTPS *)
    raw = FetchYahooCsv[ticker, startDate, endDate];
    If[!FailureQ[raw] && ListQ[raw],
      Return[raw]
    ];

    Message[FetchPrices::nodata, ticker];
    $Failed
  ]

FetchPrices::nodata =
  "Could not fetch data for `1`. Check the ticker symbol, ensure \
SP500Stocks is loaded via loadSP500Stocks[], or check your internet \
connection for the Yahoo Finance fallback.";

(* Yahoo Finance CSV fallback *)
FetchYahooCsv[ticker_String, startDate_, endDate_] :=
  Module[{t1, t2, url, raw, lines, parsed},
    t1  = UnixTime[startDate];
    t2  = UnixTime[endDate];
    url = StringTemplate[
      "https://query1.finance.yahoo.com/v7/finance/download/\
`ticker`?period1=`t1`&period2=`t2`&interval=1d&events=history"
    ][<|"ticker" -> ticker, "t1" -> t1, "t2" -> t2|>];

    raw = Quiet[Import[url, "CSV"]];

    If[!ListQ[raw] || Length[raw] < 2, Return[$Failed]];

    (* Header: Date, Open, High, Low, Close, Adj Close, Volume *)
    lines = Rest[raw];   (* drop header *)
    parsed = Quiet[
      Select[
        {DateObject[First[#]], N[#[[5]]]}& /@ lines,
        NumericQ[Last[#]]&
      ]
    ];
    If[Length[parsed] === 0, $Failed, parsed]
  ]


(* ================================================================
   2. REGIME LABELLING
   ================================================================ *)

LabelRegimes[
  priceData_?(ListQ),        (* {{DateObject, price}, ...} *)
  window_Integer: 20,
  threshold_Real: 0.02
] :=
  Module[{prices, n, returns, labels, i},
    prices = Last /@ priceData;
    n      = Length[prices];
    If[n < window + 1,
      Message[LabelRegimes::short];
      Return[$Failed]
    ];

    (* rolling return: (p[t] - p[t-window]) / p[t-window] *)
    returns = Table[
      (prices[[i]] - prices[[i - window]]) / prices[[i - window]],
      {i, window + 1, n}
    ];

    labels = Map[
      Which[
        # >  threshold, 2,   (* Bull *)
        # < -threshold, 0,   (* Bear *)
        True,           1    (* Sideways *)
      ]&,
      returns
    ];

    (* Align with the dates that have a valid return *)
    Transpose[{Drop[priceData[[All, 1]], window], labels}]
  ]

LabelRegimes::short =
  "Price series too short for the requested window.";


(* ================================================================
   3. TRANSITION MATRIX
   ================================================================ *)

BuildTransitionMatrix[labeledData_?(ListQ)] :=
  Module[{labels, counts, rowSums, P, i},
    labels = Last /@ labeledData;
    counts = ConstantArray[0., {3, 3}];

    Do[
      counts[[labels[[i]] + 1, labels[[i + 1]] + 1]] += 1.,
      {i, 1, Length[labels] - 1}
    ];

    rowSums = Total[counts, {2}];
    rowSums = Map[If[# == 0., 1., #]&, rowSums];  (* avoid /0 *)

    P = counts / Outer[Times, rowSums, ConstantArray[1., 3]];
    (* Outer product gives a proper row-normalised matrix *)
    P = counts / (rowSums \[TensorProduct] {1., 1., 1.});
    (* cleaner: *)
    P = Table[
      If[rowSums[[i]] == 0., 0., counts[[i, j]] / rowSums[[i]]],
      {i, 3}, {j, 3}
    ];
    P
  ]


(* ================================================================
   4. STATIONARY DISTRIBUTION
   Solve pi P = pi, Sumpi = 1  (left eigenvector with eigenvalue 1)
   ================================================================ *)

MarkovStationaryDist[P_?MatrixQ] :=
  Module[{vals, vecs, idx, vec},
    {vals, vecs} = Eigensystem[Transpose[P]];
    (* Find eigenvector closest to eigenvalue 1 *)
    idx = First[Ordering[Abs[vals - 1.], 1]];
    vec = Abs[Re[vecs[[idx]]]];
    vec / Total[vec]
  ]


(* ================================================================
   5. N-STEP FORECAST  (Chapman-Kolmogorov)
   ================================================================ *)

NStepForecast[P_?MatrixQ, n_Integer] :=
  MatrixPower[P, n]


(* ================================================================
   6. SIGNAL
   ================================================================ *)

SignalFromMatrix[P_?MatrixQ, currentState_Integer] :=
  (* P[Bull|current] - P[Bear|current]  (states 2 and 0) *)
  P[[currentState + 1, 3]] - P[[currentState + 1, 1]]


(* ================================================================
   7. WALK-FORWARD BACKTEST
   Re-estimates the transition matrix at every step t using only
   data up to t-1. No lookahead. No tuning.
   ================================================================ *)

WalkForwardBacktest[
  priceData_?(ListQ),       (* {{date, price}, ...} *)
  labeledData_?(ListQ),     (* {{date, label}, ...} *)
  minTrain_Integer: 252
] :=
  Module[
    {prices, labelDates, labels, priceLookup,
     stratRets, t, Pt, curState, sig, pos, nextRet,
     sr, sharpe, equity, runMax, drawdown, maxDD},

    prices     = Association[Rule @@@ priceData];
    labelDates = First /@ labeledData;
    labels     = Last  /@ labeledData;
    n          = Length[labels];

    If[n < minTrain + 30,
      Return[<|"Sharpe" -> Indeterminate, "MaxDrawdown" -> Indeterminate,
               "NTrades" -> 0, "EquityCurve" -> {}|>]
    ];

    (* Pre-compute daily returns indexed by date *)
    priceLookup = Association[Rule @@@ priceData];
    (* daily return on date d = (price[d] - price[d-1]) / price[d-1] *)
    (* We'll compute them from the labeledData date sequence *)

    stratRets = Table[
      If[t >= n,
        Nothing,
        Module[{trainSlice, curS, signal, position, d1, d2, p1, p2, dayRet},
          trainSlice = Take[labeledData, t - 1];
          Pt         = BuildTransitionMatrix[trainSlice];
          curS       = labels[[t]];
          signal     = SignalFromMatrix[Pt, curS];
          position   = Sign[signal];  (* +1 / 0 / -1 *)

          (* Next-day return between date t and t+1 in the label sequence *)
          d1 = labelDates[[t]];
          d2 = labelDates[[t + 1]];
          p1 = Lookup[priceLookup, d1, Missing[]];
          p2 = Lookup[priceLookup, d2, Missing[]];

          If[MissingQ[p1] || MissingQ[p2] || p1 == 0.,
            Nothing,
            position * (p2 - p1) / p1
          ]
        ]
      ],
      {t, minTrain, n - 1}
    ];

    stratRets = DeleteCases[stratRets, Nothing];

    If[Length[stratRets] == 0,
      Return[<|"Sharpe" -> Indeterminate, "MaxDrawdown" -> Indeterminate,
               "NTrades" -> 0, "EquityCurve" -> {}|>]
    ];

    sr     = N[stratRets];
    sharpe = If[StandardDeviation[sr] == 0.,
      Indeterminate,
      Mean[sr] / StandardDeviation[sr] * Sqrt[252.]
    ];

    equity  = FoldList[Times, 1., 1. + sr];
    runMax  = FoldList[Max, equity];
    drawdown = (equity - runMax) / runMax;
    maxDD   = Min[drawdown];

    <|"Sharpe"      -> sharpe,
      "MaxDrawdown" -> maxDD,
      "NTrades"     -> Length[sr],
      "EquityCurve" -> equity|>
  ]


(* ================================================================
   8. HIDDEN MARKOV MODEL  (optional, uses MixtureDistribution /
      EstimatedDistribution -- built into Mathematica)

   Mathematica's built-in HMM support: HiddenMarkovProcess
   ================================================================ *)

FitHMM[returns_?(ListQ), nStates_Integer: 3] :=
  Module[{proc, fitted, viterbi},
    Quiet[
      Check[
        proc   = HiddenMarkovProcess[nStates, "Gaussian"];
        fitted = EstimatedProcess[returns, proc];
        viterbi = FindHiddenMarkovStates[returns, fitted];
        <|"Model" -> fitted, "HiddenStates" -> viterbi|>,
        $Failed
      ],
      {EstimatedProcess::cvmit, HiddenMarkovProcess::inv}
    ]
  ]


(* ================================================================
   9. REPORTING
   ================================================================ *)

PrintMarkovReport[
  ticker_String,
  years_Integer: 10,
  window_Integer: 20,
  threshold_Real: 0.02
] :=
  Module[
    {priceData, labeledData, P, pi, btResult,
     hmm, hmmStates, hmmMeans, hmmOrder},

    Print[""];
    Print[Style[
      "Markov Regime Detection Model -- " <> ticker,
      Bold, 14]];
    Print[StringRepeat["-", 60]];

    (* -- Fetch -- *)
    Print["Fetching " <> ticker <> " (" <> ToString[years] <>
          " years, daily close)..."];
    priceData = FetchPrices[ticker, years];
    If[FailureQ[priceData] || priceData === $Failed,
      Print["ERROR: Could not fetch data. Aborting."];
      Return[$Failed]
    ];
    Print[StringTemplate[
      "  `` rows  |  `` -> ``"][
      Length[priceData],
      DateString[priceData[[1,  1]], {"Year","-","Month","-","Day"}],
      DateString[priceData[[-1, 1]], {"Year","-","Month","-","Day"}]
    ]];

    (* -- Label -- *)
    Print["\nLabelling regimes (window=" <> ToString[window] <>
          " days, threshold=" <> ToString[threshold*100] <> "%)..."];
    labeledData = LabelRegimes[priceData, window, threshold];
    If[FailureQ[labeledData],
      Print["ERROR: Regime labelling failed."];
      Return[$Failed]
    ];

    Module[{cnts = Counts[Last /@ labeledData], total},
      total = Length[labeledData];
      Print[StringTemplate[
        "  Bear `1`%  |  Sideways `2`%  |  Bull `3`%"][
        Round[100. Lookup[cnts, 0, 0] / total, 0.1],
        Round[100. Lookup[cnts, 1, 0] / total, 0.1],
        Round[100. Lookup[cnts, 2, 0] / total, 0.1]
      ]]
    ];

    (* -- Transition matrix -- *)
    P = BuildTransitionMatrix[labeledData];
    Print[Row[{"\n", Style["Transition Matrix (rows = from, cols = to):", Bold]}]];
    Print[Style["             Bear    Sideways    Bull", Italic]];
    Do[
      Print[StringTemplate[
        "  `from`  `b`%  `s`%  `bull`%"][<|
        "from" -> StringPadLeft[$StateNames[i], 9],
        "b"    -> StringPadLeft[ToString[Round[P[[i+1, 1]]*100, 0.01]], 6],
        "s"    -> StringPadLeft[ToString[Round[P[[i+1, 2]]*100, 0.01]], 9],
        "bull" -> StringPadLeft[ToString[Round[P[[i+1, 3]]*100, 0.01]], 7]
      |>]],
      {i, 0, 2}
    ];

    Print[Row[{"\n", Style["Persistence diagonal:", Bold]}]];
    Do[
      Print["  " <> $StateNames[i] <> " -> " <> $StateNames[i] <> ":  " <>
            ToString[Round[P[[i+1, i+1]] * 100., 0.01]] <> "%"],
      {i, 0, 2}
    ];

    (* -- Stationary distribution -- *)
    pi = MarkovStationaryDist[P];
    Print[Row[{"\n", Style["Stationary Distribution (long-run regime mix):", Bold]}]];
    Do[
      Print["  " <> StringPadLeft[$StateNames[i], 9] <> ":  " <>
            ToString[Round[pi[[i+1]] * 100., 0.01]] <> "%"],
      {i, 0, 2}
    ];

    (* -- 5-step forecast from current state -- *)
    Module[{curState, P5, curName},
      curState = Last[labeledData][[2]];
      curName  = $StateNames[curState];
      P5       = NStepForecast[P, 5];
      Print[Row[{"\n", Style["5-Day Forecast (current regime: " <>
            curName <> "):", Bold]}]];
      Do[
        Print["  P(" <> $StateNames[j] <> " in 5 days) = " <>
              ToString[Round[P5[[curState+1, j+1]] * 100., 0.01]] <> "%"],
        {j, 0, 2}
      ]
    ];

    (* -- Walk-forward backtest -- *)
    Print[Row[{"\n", Style[
      "Walk-Forward Backtest (re-estimating matrix at every step, no lookahead):",
      Bold]}]];
    Print["  Running... (this may take 30-90 seconds for 10y of data)"];
    btResult = WalkForwardBacktest[priceData, labeledData];

    If[NumericQ[btResult["Sharpe"]],
      Print["  Sharpe (annualised):  " <>
            ToString[Round[btResult["Sharpe"], 0.001]]],
      Print["  Sharpe:  Indeterminate (insufficient data)"]
    ];
    If[NumericQ[btResult["MaxDrawdown"]],
      Print["  Max Drawdown:         " <>
            ToString[Round[btResult["MaxDrawdown"] * 100., 0.01]] <> "%"],
      Print["  Max Drawdown:  Indeterminate"]
    ];
    Print["  Trades evaluated:     " <> ToString[btResult["NTrades"]]];

    (* Plot equity curve if we have one *)
    If[Length[btResult["EquityCurve"]] > 0,
      Print[ListLinePlot[
        btResult["EquityCurve"],
        PlotLabel -> ticker <> " Walk-Forward Equity Curve",
        AxesLabel -> {"Trade #", "Equity"},
        PlotStyle -> Directive[Blue, Thick],
        GridLines -> Automatic,
        ImageSize -> 500
      ]]
    ];

    (* -- Optional HMM -- *)
    Print[Row[{"\n", Style["Hidden Markov Model (optional):", Bold]}]];
    Print["  Fitting via HiddenMarkovProcess + EstimatedProcess..."];
    Module[{dailyRets},
      dailyRets = Differences[Log[Last /@ priceData]];
      hmm = FitHMM[dailyRets];
      If[AssociationQ[hmm],
        hmmStates = hmm["HiddenStates"];
        hmmMeans = Table[
          Mean[Pick[dailyRets, hmmStates, s]],
          {s, Union[hmmStates]}
        ];
        hmmOrder = Ordering[hmmMeans];
        Print["  HMM regime mean log-returns (sorted Bear -> Bull):"];
        Do[
          Print["    State " <> ToString[(Union[hmmStates])[[hmmOrder[[k]]]]] <>
                ": " <>
                ToString[Round[hmmMeans[[hmmOrder[[k]]]] * 100., 0.001]] <>
                "% per day"],
          {k, Length[hmmMeans]}
        ];
        Print["  Note: Baum-Welch finds local optima -- fit several \
random seeds for production."],
        Print["  HMM not available or failed (EstimatedProcess unsupported \
on this Mathematica version). Observable model still complete."]
      ]
    ];

    Print[Row[{"\n", StringRepeat["=", 60]}]];
    Print[Style[" Framework: Roan (@RohOnChain)", Italic]];
    Print[Style[
      " Mathematica implementation. Backtests are historical,\n \
not forward-looking. The matrix is the honest measurement.", Italic]];
    Print[StringRepeat["=", 60]];
  ]
  
(* Convenience entry point *)
RunMarkovAnalysis[ticker_String, years_Integer: 10, window_Integer: 20] :=
  PrintMarkovReport[ticker, years, window, 0.02]


(* ================================================================
                          V2.0 Fix
   ================================================================ *)
   
Clear[BuildTransitionMatrixStride];
BuildTransitionMatrixStride[labeledDataArg_List, strideArg_Integer : 16] :=
  Module[{labels, strideIndices, transitionCounts, fromLabel, toLabel, P},
    labels = labeledDataArg[[All, 2]];
    If[Length[labels] < 2 * strideArg,
      (* Not enough data for even one transition *)
      Return[ConstantArray[1./3., {3, 3}]]
    ];
    (* Sample labels at stride intervals: positions strideArg, 2*strideArg, 3*strideArg, ... *)
    strideIndices = Range[strideArg, Length[labels], strideArg];
    transitionCounts = ConstantArray[0, {3, 3}];
    Do[
      fromLabel = labels[[strideIndices[[k]]]];
      toLabel   = labels[[strideIndices[[k + 1]]]];
      (* Labels are 0-based: add 1 for matrix indexing *)
      transitionCounts[[fromLabel + 1, toLabel + 1]] += 1,
      {k, 1, Length[strideIndices] - 1}
    ];
    (* Normalize rows to probabilities *)
    P = Table[
      Module[{rowSum = Total[transitionCounts[[r]]]},
        If[rowSum > 0,
          N[transitionCounts[[r]] / rowSum],
          {1./3., 1./3., 1./3.}
        ]
      ],
      {r, 1, 3}
    ];
    P
  ];


End[]   (* `Private` *)
EndPackage[]


(* ================================================================
   QUICK-START -- uncomment and evaluate to run immediately:

   Get["MarkovRegimeModel.wl"]
   RunMarkovAnalysis["SPY", 10, 20]

   Other examples:
   RunMarkovAnalysis["AAPL", 5, 20]
   RunMarkovAnalysis["BTC-USD", 3, 14]
   PrintMarkovReport["QQQ", 10, 20, 0.015]
   ================================================================ *)
