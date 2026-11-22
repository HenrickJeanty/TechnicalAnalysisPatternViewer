(* ::Package:: *)

(* ::Package:: *)
(**)


(* ============================================================================
   Fibonacci.wl
   StockAnalyzer Package -- Fibonacci Level Analysis

   Provides tools for analyzing how often price touches Fibonacci levels
   derived from MaxU and MaxD edge segments. Supports full historical
   analysis and single-segment prediction assessment.

   Author : Henrick Jeanty, Ph.D.
   Version: 1.0.0
   Requires: Mathematica 14.3+, StockAnalyzer`Analysis`
   ============================================================================ *)

BeginPackage["StockAnalyzer`Fibonacci`"]

(* ============================================================================
   Public symbol declarations
   ============================================================================ *)

analyzeFibonacciLevels::usage =
  "analyzeFibonacciLevels[candlesticksArg, toleranceArg, fibonacciRatiosArg, \
windowSizeArg] calls updateFeatureSet on the full candlestick history, extracts \
MaxU and MaxD edge segments, and computes touch counts per Fibonacci level \
per zone (Before, Inside, After) per price field (O,H,L,C). \
Returns <|SegmentResults->{...}, Aggregated->{...}, AggregatedByDirection->{...}|>.";

analyzeFibonacciSegments::usage =
  "analyzeFibonacciSegments[segmentsArg, allRowsArg, toleranceArg, \
fibonacciRatiosArg, windowSizeArg] takes a pre-extracted Association of segments \
and a List of candlestick rows and computes touch counts per Fibonacci level \
per zone per price field. Returns <|SegmentResults->{...}, Aggregated->{...}, \
AggregatedByDirection->{...}|>.";

analyzeFibonacciSegment::usage =
  "analyzeFibonacciSegment[segmentArg, allRowsArg, toleranceArg, \
fibonacciRatiosArg, windowSizeArg] analyzes a single edge segment, returning \
Before, Inside, After touch counts per Fibonacci ratio and price field. \
Used for prediction confidence assessment.";

aggregateFibonacciResults::usage =
  "aggregateFibonacciResults[segmentResultsArg] aggregates a list of \
per-segment Fibonacci results into a summary keyed by Fibonacci ratio, \
with total Previous (Before+Inside) and After touch counts per price field.";

aggregateFibonacciResultsByDirection::usage =
  "aggregateFibonacciResultsByDirection[segmentResultsArg] aggregates \
per-segment results separately for Up and Down directions, returning \
<|\"Up\" -> {...}, \"Down\" -> {...}|> each keyed by Fibonacci ratio.";

buildFibonacciResultsIndex::usage =
  "buildFibonacciResultsIndex[segmentResultsArg] takes a list of per-segment \
Fibonacci results and organizes them into a nested Association keyed by \
ToIndex -> FromIndex -> Direction. Use as fibIndex[toIndex][fromIndex][direction] \
to retrieve all Fibonacci level data for a specific segment. Useful for \
looking up all segments ending at a given candlestick index.";

fibonacciRatios::usage = 
  "fibonacciRatios is the default list of Fibonacci ratios used throughout \
the StockAnalyzer package: {0., 0.236, 0.382, 0.500, 0.618, 0.786, \
1., 1.272}.";

showSegmentTable::usage =
  "showSegmentTable[resultArg] displays a formatted table of Fibonacci level \
touch counts for a single segment result from analyzeFibonacciSegment. Shows \
Before, Inside, Previous, After counts and percentages for each Fibonacci ratio.";

Begin["`Private`"]

(* Definition of fibonacciRatios *)   
fibonacciRatios = {0., 0.236, 0.382, 0.500, 0.618, 0.786, 1., 1.272};
(* previous list, which also had the 1.618 golden-ratio level: {0., 0.236, 0.382, 0.500, 0.618, 0.786, 1., 1.272, 1.618} *)
(* fibonacciRatios = {0.236, 0.382, 0.500, 0.618, 0.786, 1.}; *) (* 1.`,0.6180339887498948`,0.38196601125010515`,0.2360679774997897`} *)
  
(* ============================================================================
   Core helper: count touches for one candlestick row against one Fibonacci level
   ============================================================================ *)

fibTouchCounts[rowArg_Association, fibPriceArg_Real, toleranceArg_Real] :=
  Module[{lo, hi, fields, counts},
    lo     = fibPriceArg * (1.0 - toleranceArg);
    hi     = fibPriceArg * (1.0 + toleranceArg);
    fields = {"Open", "High", "Low", "Close"};
    Association[Map[
      Function[field,
        field -> If[
          NumberQ[rowArg[field]] && lo <= rowArg[field] <= hi,
          1, 0
        ]
      ],
      fields
    ]]
  ];

(* ============================================================================
   Core helper: sum touch counts across a list of candlestick rows
   ============================================================================ *)

sumTouchCounts[rowsArg_List, fibPriceArg_Real, toleranceArg_Real] :=
  Module[{fields, zero},
    fields = {"Open", "High", "Low", "Close"};
    zero   = Association[Map[# -> 0 &, fields]];
    If[Length[rowsArg] == 0,
      zero,
      Fold[
        Function[{acc, row},
          Association[Map[
            Function[field,
              field -> acc[field] + fibTouchCounts[row, fibPriceArg, toleranceArg][field]
            ],
            fields
          ]]
        ],
        zero,
        rowsArg
      ]
    ]
  ];

(* ============================================================================
   Core helper: add two touch count Associations
   ============================================================================ *)

addTouchCounts[aArg_Association, bArg_Association] :=
  Association[Map[
    Function[field, field -> aArg[field] + bArg[field]],
    {"Open", "High", "Low", "Close"}
  ]];

addTotalField[countsArg_Association] :=
  Append[countsArg, "Total" -> Total[Values[KeyTake[countsArg, {"Open","High","Low","Close"}]]]];

(* ============================================================================
   Core: analyze one segment against all candlestick rows
   ============================================================================ *)

analyzeOneSegment[segmentArg_Association, allRowsArg_List,
                  toleranceArg_Real, fibonacciRatiosArg_List,
                  windowSizeArg_Integer] :=
  Module[{fromIdx, toIdx, fromPrice, toPrice, range, direction,
          maxIdx, beforeRows, insideRows, afterRows, fibLevels},

    fromIdx   = segmentArg["FromIndex"];
    toIdx     = segmentArg["ToIndex"];
    fromPrice = segmentArg["FromPrice"];
    toPrice   = segmentArg["ToPrice"];
    range     = toPrice - fromPrice;
    direction = segmentArg["Direction"];
    maxIdx    = Length[allRowsArg];

    (* Extract three windows *)
    beforeRows = Which[
      fromIdx <= 1,           {},
      fromIdx - windowSizeArg >= 1, allRowsArg[[fromIdx - windowSizeArg ;; fromIdx - 1]],
      True,                   allRowsArg[[1 ;; fromIdx - 1]]
    ];
    insideRows = allRowsArg[[fromIdx ;; Min[toIdx, maxIdx]]];
    afterRows  = Which[
      toIdx >= maxIdx,              {},
      toIdx + windowSizeArg <= maxIdx, allRowsArg[[toIdx + 1 ;; toIdx + windowSizeArg]],
      True,                         allRowsArg[[toIdx + 1 ;; maxIdx]]
    ];

    fibLevels = Map[
      Function[ratio,
        Module[{fibPrice, beforeC, insideC, afterC, previousC},
          fibPrice  = fromPrice + ratio * range;
          beforeC   = sumTouchCounts[beforeRows, fibPrice, toleranceArg];
          insideC   = sumTouchCounts[insideRows, fibPrice, toleranceArg];
          afterC    = sumTouchCounts[afterRows,  fibPrice, toleranceArg];
          previousC = addTouchCounts[beforeC, insideC];
          <|
            "Ratio"    -> ratio,
            "FibPrice" -> fibPrice,
            "Before"   -> addTotalField[beforeC],
            "Inside"   -> addTotalField[insideC],
            "After"    -> addTotalField[afterC],
            "Previous" -> addTotalField[previousC]
          |>
        ]
      ],
      fibonacciRatiosArg
    ];

    <|
      "SegmentKey" -> {fromIdx, toIdx,
                       direction,
                       segmentArg["FromField"],
                       segmentArg["ToField"]},
      "EdgeType"   -> segmentArg["EdgeType"],
      "Direction"  -> direction,
      "FromIndex"  -> fromIdx,
      "ToIndex"    -> toIdx,
      "FromPrice"  -> fromPrice,
      "ToPrice"    -> toPrice,
      "Range"      -> range,
      "FibLevels"  -> fibLevels
    |>
  ];

(* ============================================================================
   Core: aggregate segment results by ratio
   ============================================================================ *)

aggregateFibonacciResults[segmentResultsArg_List] :=
  Module[{fields, ratios},
    fields = {"Open", "High", "Low", "Close"};
    ratios = Union[Flatten[Map[
      Function[seg, Map[#["Ratio"] &, seg["FibLevels"]]],
      segmentResultsArg
    ]]];
    Map[
      Function[ratio,
        Module[{allFL, totBefore, totInside, totPrevious, totAfter},
          allFL      = Flatten[Map[
            Function[seg, Select[seg["FibLevels"], #["Ratio"] == ratio &]],
            segmentResultsArg
          ]];
          totBefore  = Fold[
            Function[{acc, fl}, addTouchCounts[acc, fl["Before"]]],
            Association[Map[# -> 0 &, fields]], allFL
          ];
          totInside  = Fold[
            Function[{acc, fl}, addTouchCounts[acc, fl["Inside"]]],
            Association[Map[# -> 0 &, fields]], allFL
          ];
          totPrevious = Fold[
            Function[{acc, fl}, addTouchCounts[acc, fl["Previous"]]],
            Association[Map[# -> 0 &, fields]], allFL
          ];
          totAfter   = Fold[
            Function[{acc, fl}, addTouchCounts[acc, fl["After"]]],
            Association[Map[# -> 0 &, fields]], allFL
          ];
          <|
            "Ratio"    -> ratio,
            "Before"   -> addTotalField[totBefore],
            "Inside"   -> addTotalField[totInside],
            "Previous" -> addTotalField[totPrevious],
            "After"    -> addTotalField[totAfter]
          |>
        ]
      ],
      ratios
    ]
  ];

(* ============================================================================
   Core: aggregate by direction (Up vs Down)
   ============================================================================ *)

aggregateFibonacciResultsByDirection[segmentResultsArg_List] :=
  Module[{upSegs, downSegs},
    upSegs   = Select[segmentResultsArg, #["Direction"] == "U" &];
    downSegs = Select[segmentResultsArg, #["Direction"] == "D" &];
    <|
      "Up"   -> aggregateFibonacciResults[upSegs],
      "Down" -> aggregateFibonacciResults[downSegs]
    |>
  ];

(* ============================================================================
   Public: analyze pre-extracted segments
   ============================================================================ *)

analyzeFibonacciSegments[segmentsArg_Association,
                          allRowsArg_List,
                          toleranceArg_Real,
                          fibonacciRatiosArg_List : fibonacciRatios,
                          windowSizeArg_Integer : 8] :=
  Module[{segList, segResults, aggregated, aggregatedByDir},
    segList          = Values[segmentsArg];
    segResults       = Map[
      Function[seg,
        analyzeOneSegment[seg, allRowsArg, toleranceArg,
                          fibonacciRatiosArg, windowSizeArg]
      ],
      segList
    ];
    aggregated       = aggregateFibonacciResults[segResults];
    aggregatedByDir  = aggregateFibonacciResultsByDirection[segResults];
    <|
      "SegmentResults"       -> segResults,
      "Aggregated"           -> aggregated,
      "AggregatedByDirection" -> aggregatedByDir
    |>
  ];

(* ============================================================================
   Public: full pipeline - updateFeatureSet + extract MaxU/MaxD + analyze
   ============================================================================ *)

analyzeFibonacciLevels[candlesticksArg_Tabular,
                        toleranceArg_Real,
                        fibonacciRatiosArg_List : fibonacciRatios,
                        windowSizeArg_Integer : 8] :=
  Module[{featureSet, allEdgeSegs, desiredSegments, allRows},

    featureSet = updateFeatureSet[
                   Dataset[<|"Ticker" -> "", "Period" -> "daily", "Sigma" -> 0|>],
                   candlesticksArg,
                   0,
                   StrictRanges -> True
                 ];

    allEdgeSegs     = Association[Normal[featureSet["EdgeSegments"]]];
    desiredSegments = KeySelect[allEdgeSegs,
                        #[[5]] == "MaxU" || #[[5]] == "MaxD" &];
    allRows         = Normal[featureSet["Candlesticks"]];

    Print["Total segments: ",          Length[allEdgeSegs]];
    Print["MaxU/MaxD segments: ",  Length[desiredSegments]];

    analyzeFibonacciSegments[desiredSegments, allRows, toleranceArg,
                              fibonacciRatiosArg, windowSizeArg]
  ];

(* ============================================================================
   Public: single segment analysis for prediction assessment
   ============================================================================ *)

analyzeFibonacciSegment[segmentArg_Association,
                         allRowsArg_List,
                         toleranceArg_Real,
                         fibonacciRatiosArg_List : fibonacciRatios,
                         windowSizeArg_Integer : 8] :=
  analyzeOneSegment[segmentArg, allRowsArg, toleranceArg,
                    fibonacciRatiosArg, windowSizeArg];

buildFibonacciResultsIndex[segmentResultsArg_List] :=
  Association[
    Map[
      Function[toIdx,
        toIdx -> Association[
          Map[
            Function[fromIdx,
              fromIdx -> Association[
                Map[
                  Function[dir,
                    dir -> SelectFirst[
                      segmentResultsArg,
                      #["ToIndex"] == toIdx &&
                      #["FromIndex"] == fromIdx &&
                      #["Direction"] == dir &
                    ]
                  ],
                  Union[Map[#["Direction"] &,
                    Select[segmentResultsArg,
                      #["ToIndex"] == toIdx &&
                      #["FromIndex"] == fromIdx &]
                  ]]
                ]
              ]
            ],
            Union[Map[#["FromIndex"] &,
              Select[segmentResultsArg, #["ToIndex"] == toIdx &]
            ]]
          ]
        ]
      ],
      Union[Map[#["ToIndex"] &, segmentResultsArg]]
    ]
  ];
  
  
Clear[showSegmentTable];
showSegmentTable[resultArg_] :=
  Module[{total},
    Print["Segment: ", resultArg["EdgeType"],
          "  Direction: ", resultArg["Direction"],
          "  FromIndex: ", resultArg["FromIndex"],
          "  ToIndex: ",   resultArg["ToIndex"],
          "  FromPrice: ", resultArg["FromPrice"],
          "  ToPrice: ",   resultArg["ToPrice"],
          "  Range: ",     Round[resultArg["Range"], 0.001]];
    TableForm[
      Map[
        Function[fl,
          total = fl["Before"]["Total"] + fl["Inside"]["Total"] + fl["After"]["Total"];
          {fl["Ratio"],
           Round[fl["FibPrice"], 0.001],
           fl["Before"]["Total"],
           fl["Inside"]["Total"],
           fl["Previous"]["Total"],
           fl["After"]["Total"],
           (* Before% = Before / Total *)
           If[total > 0,
             PercentForm[N[fl["Before"]["Total"] / total, 3]],
             "-"
           ],
           (* Inside% = Inside / Total *)
           If[total > 0,
             PercentForm[N[fl["Inside"]["Total"] / total, 3]],
             "-"
           ],
           (* Previous% = Previous / (Previous + After) *)
           If[fl["Previous"]["Total"] + fl["After"]["Total"] > 0,
             PercentForm[N[fl["Previous"]["Total"] /
               (fl["Previous"]["Total"] + fl["After"]["Total"]), 3]],
             "-"
           ],
           (* After% = After / (Previous + After) *)
           If[fl["Previous"]["Total"] + fl["After"]["Total"] > 0,
             PercentForm[N[fl["After"]["Total"] /
               (fl["Previous"]["Total"] + fl["After"]["Total"]), 3]],
             "-"
           ]
          }
        ],
        resultArg["FibLevels"]
      ],
      TableHeadings -> {
        None,
        {"Ratio", "FibPrice", "Before", "Inside", "Previous(B+I)",
         "After", "Before%", "Inside%", "Previous%", "After%"}
      }
    ]
  ];
   
End[]
EndPackage[]
