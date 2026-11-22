(* ::Package:: *)

(* ====================================================================== *)
(* Engine.wl                                                               *)
(* Technical Analysis Pattern Viewer: finding support, resistance, patterns *)
(* ====================================================================== *)
(*
  WHAT THIS FILE DOES
    It finds, in a table of candlesticks, the peaks and valleys, the segments between them, the SUPPORT and RESISTANCE lines, and the
    PATTERNS that a resistance and a support make together (triangles, wedges, channels, ...). It does so with the analysis code in the
    folder vendor/ (Analysis.wl, TrendlinePatterns.wl, HarmonicPatterns.wl, Fibonacci.wl, MarkovRegimeModel.wl), which is loaded the first time
    it is needed.

  FUNCTIONS (each has a full usage message: ?analyzeCandlesticks, ...)
    loadEngine[]                           loads the analysis code of vendor/ (done automatically by the functions below).
    analyzeCandlesticks[candles, options]  the feature set of the candlesticks (a Dataset): everything that was found.
    supportResistanceTable[featureSet]     the support and resistance lines, as a Dataset.
    patternTable[featureSet]               the patterns, as a Dataset.
    segmentTable[featureSet]               the segments between peaks and valleys, as a Dataset.
    patternNames[]                         the names of the 13 patterns that are looked for.

  THE RULES (from the usage message of the analysis code)
    A resistance (support) goes through two peaks (valleys). It is valid while no candlestick between its first anchor and the last
    candlestick pierces it with its High (Low) by more than the tolerance. The tolerance is a percentage of the High-Low range of the
    candlesticks from the first anchor to the candlestick that is tested. The lines are found incrementally, candlestick by candlestick, so
    a line that ended is kept with its end index and the lines alive at each candlestick are known.

  CONTEXT: TechnicalAnalysisPatternViewer`Engine`
*)

BeginPackage["TechnicalAnalysisPatternViewer`Engine`"];

(* ====================================================================== *)
(* PUBLIC FUNCTIONS AND THEIR USAGE MESSAGES                               *)
(* ====================================================================== *)

loadEngine::usage =
  "loadEngine[] loads the analysis code that finds the support and resistance lines and the patterns (the files of the folder vendor/ of this package) " <>
  "if it is not loaded yet, and gives True when it is loaded. The other functions call it themselves, so you rarely need it; loading takes a few seconds.";

analyzeCandlesticks::usage =
  "analyzeCandlesticks[candles] finds the peaks and valleys, the segments, the support and resistance lines and the patterns of the candlesticks, " <>
  "and gives everything as a feature set: a Dataset whose keys are, among others:\n" <>
  "  \"Candlesticks\" the candlesticks used; \"Peaks\" and \"Valleys\" their numbers; \"Segments\" the segments between peaks and valleys and, with them, the support " <>
  "and resistance lines (rows with Kind \"Res\" or \"Sup\"); \"ValidTrendlines\" every support and resistance line, alive or ended; \"Patterns\" the patterns; " <>
  "\"Graph\" the directed graph of the candlesticks, with the complex edge weight a + b i (a = the number of candlesticks the edge spans, b = the price change along it).\n" <>
  "candles is anything that loadCandlesticks accepts (a Tabular, a Dataset, a list of associations with Date, Open, High, Low and Close). " <>
  "The numbers in the tables are candlestick NUMBERS (1 for the first candlestick), not dates.\n" <>
  "Options: \"Sigma\" -> 0 (peaks and valleys: 0 is a candlestick whose High is above both neighbours' Highs, or whose Low is below both neighbours' Lows; " <>
  "a larger integer asks for more extreme ones), \"StrictRanges\" -> True (True: one MaxU or MaxD edge per segment; False: every pair of fields), " <>
  "\"SupportResistance\" -> True (False switches the lines and patterns off; or a list of rules {\"TolerancePercent\" -> 5, \"MinTouches\" -> 1, \"PatternBand\" -> 0.15}: " <>
  "TolerancePercent is how far, as a percentage of the High-Low range, a High (Low) may pierce a resistance (support) and still count as touching it; " <>
  "MinTouches the number of touches between its two anchors that a line needs; PatternBand the band around 1 of the ratio of the gap between the two lines now to the gap at the start " <>
  "that counts as parallel), \"HarmonicPatterns\" -> False (True, the name of a harmonic pattern or a list of names: also find the harmonic patterns X, A, B, C, D born at each candlestick; " <>
  "or a list of rules such as {\"Patterns\" -> {\"Gartley\", \"Bat\"}, \"Tolerance\" -> 0.05}), \"GraphChart\" -> False (True or a list of options adds the key \"GraphChart\": the candlesticks drawn as a graph, " <>
  "see the usage of the viewer).\n" <>
  "Example: featureSet = analyzeCandlesticks[sampleCandlesticks[\"monthly\"]]; supportResistanceTable[featureSet].";

supportResistanceTable::usage =
  "supportResistanceTable[featureSet] gives the support and resistance lines of a feature set (the result of analyzeCandlesticks) as a Dataset, one row for every line and " <>
  "every candlestick at which it is alive. The columns are: Kind (\"Res\" resistance, \"Sup\" support), Direction (\"ResU\", \"ResD\", \"ResF\", \"SupU\", \"SupD\" or \"SupF\": " <>
  "rising U, falling D or flat F), FromIndex (the first anchor), AnchorIndex (the second anchor), StartIndex, ToIndex (the candlestick at which the row is valid), " <>
  "FromPrice and ToPrice (the prices of the line at FromIndex and ToIndex), Slope (price per candlestick), LogSlope (the same on a log scale), " <>
  "Touches (how many candlesticks touched the line) and Goodness (the number of touches counted with the anchors).";

patternTable::usage =
  "patternTable[featureSet] gives the patterns of a feature set (the result of analyzeCandlesticks) as a Dataset: one row for every pattern and every candlestick at which it is valid. " <>
  "The columns are: PatternName (see patternNames[]), Pattern (the same without spaces), ToIndex (the candlestick at which the pattern is valid), StartIndex (where the pair of lines starts), " <>
  "Width (the number of candlesticks from StartIndex), ResistanceDirection and SupportDirection (U, D or F), Relation (\"converging\", \"diverging\" or \"parallel\"), " <>
  "GapRatio (the gap between the lines now divided by the gap at the start), Apex (the candlestick number where the lines cross, ahead of ToIndex when they converge), " <>
  "ResistanceTouches, SupportTouches and Touches, ResistanceGoodness and SupportGoodness, the indices and prices where the two lines start, ResistanceValue and SupportValue (their prices at ToIndex) " <>
  "and the slopes. The Dataset is empty when no pattern was found.";

segmentTable::usage =
  "segmentTable[featureSet] gives the segments between peaks and valleys of a feature set (the result of analyzeCandlesticks) as a Dataset, without the support and resistance lines. " <>
  "The columns are: Direction (\"U\" up, \"D\" down), FromIndex and ToIndex (the candlestick numbers), FromPrice and ToPrice, Range (ToPrice - FromPrice), DeltaX (the number of candlesticks), " <>
  "LogRatio, Angle and Weight (the angle of the segment).";

patternNames::usage =
  "patternNames[] gives the names of the 13 patterns that are looked for: symmetric, ascending and descending triangles, rising and falling wedges, broadening formations " <>
  "(also right-angled and wedge-shaped), rectangles (trading ranges), and rising and falling channels. A pattern is a resistance and a support that are valid at the same candlestick: " <>
  "its name follows from their directions (up, down or flat) and from whether they converge, diverge or run parallel.";

Begin["`Private`"];

(* ====================================================================== *)
(* MESSAGES                                                                *)
(* ====================================================================== *)

loadEngine::failed = "The analysis code of `1` could not be loaded; run Get on the files of that folder to see the error.";
analyzeCandlesticks::few = "At least 5 candlesticks are needed to find peaks, valleys and lines; there are `1`.";
analyzeCandlesticks::input = "The candlesticks could not be read; see loadCandlesticks for the accepted forms.";
analyzeCandlesticks::engine = "The analysis code could not be loaded.";

(* ====================================================================== *)
(* THE ANALYSIS CODE OF vendor/                                            *)
(* ====================================================================== *)

(* this file is in Kernel/; the analysis code is in vendor/ next to it *)
$vendorDirectory = FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]], "vendor"}];

(* the files in the order in which they depend on each other; StockDataManagerStub.wl declares the data layer that Analysis.wl and Visualization.wl name as a dependency *)
$vendorFiles = {"MarkovRegimeModel", "Fibonacci", "StockDataManagerStub", "TrendlinePatterns", "HarmonicPatterns", "Analysis", "Visualization"};

(* is the analysis code loaded? Analysis.wl gives updateFeatureSet, Visualization.wl the chart *)
engineLoadedQ[] :=
  DownValues[Evaluate[Symbol["StockAnalyzer`Analysis`updateFeatureSet"]]] =!= {} &&
  DownValues[Evaluate[Symbol["StockAnalyzer`Visualization`displayCandlestickGraphWithSegmentPicker"]]] =!= {};

loadEngine[] :=
  (If[! engineLoadedQ[],
     Scan[Quiet[Get[FileNameJoin[{$vendorDirectory, # <> ".wl"}]]] &, $vendorFiles]];
   If[engineLoadedQ[], True, Message[loadEngine::failed, $vendorDirectory]; False]);

(* ====================================================================== *)
(* THE OPTIONS OF THE ANALYSIS CODE FROM THE OPTIONS OF THIS PACKAGE       *)
(* ====================================================================== *)

(* the options of the support and resistance lines: False or None (off), True (the default tolerance, touches and pattern band) or a list of rules *)
supportResistanceRules[srArg_] :=
  With[{a = If[ListQ[srArg], Association[Select[srArg, MatchQ[#, _Rule] &]], <||>]},
    {"SupportResistance" -> (srArg =!= False && srArg =!= None),
     "TolerancePercent" -> Lookup[a, "TolerancePercent", 5],
     "MinTouches" -> Lookup[a, "MinTouches", 1],
     "PatternBand" -> Lookup[a, "PatternBand", 0.15]}];

(* the options of the harmonic patterns: False or None (off), True (every pattern), a pattern name or a list of names, or a list of rules *)
harmonicRules[hArg_] :=
  With[{a = If[ListQ[hArg] && AllTrue[hArg, MatchQ[#, _Rule] &], Association[hArg], <||>]},
    {"HarmonicPatterns" -> Which[hArg === False || hArg === None, False, KeyExistsQ[a, "Patterns"] || a =!= <||>, Lookup[a, "Patterns", True], True, hArg],
     "HarmonicTolerance" -> Lookup[a, "Tolerance", 0.05]}];

(* ====================================================================== *)
(* analyzeCandlesticks                                                     *)
(* ====================================================================== *)

Options[analyzeCandlesticks] = {"Sigma" -> 0, "StrictRanges" -> True, "SupportResistance" -> True, "HarmonicPatterns" -> False, "GraphChart" -> False};

analyzeCandlesticks[candles_, OptionsPattern[]] :=
  Module[{table, rows, stub},
    If[! loadEngine[], Message[analyzeCandlesticks::engine]; Return[$Failed]];
    (* any form of table is accepted: loadCandlesticks makes the standard columns *)
    table = TechnicalAnalysisPatternViewer`Data`loadCandlesticks[candles];
    If[table === $Failed, Message[analyzeCandlesticks::input]; Return[$Failed]];
    rows = Association /@ Normal[table];
    If[Length[rows] < 5, Message[analyzeCandlesticks::few, Length[rows]]; Return[$Failed]];
    stub = Dataset[<|"Ticker" -> First[rows]["Ticker"], "Period" -> First[rows]["Period"], "StartDate" -> First[rows]["Date"], "EndDate" -> Last[rows]["Date"],
      "PeaksAndValleysSigma" -> OptionValue["Sigma"], "StrictRanges" -> TrueQ[OptionValue["StrictRanges"]]|>];
    (* the same call that the analysis code makes itself: an empty feature set, the candlesticks, the sigma and the options *)
    Quiet[Symbol["StockAnalyzer`Analysis`updateFeatureSet"][stub, Tabular[rows], OptionValue["Sigma"],
      "StrictRanges" -> TrueQ[OptionValue["StrictRanges"]], "GraphChart" -> OptionValue["GraphChart"],
      Sequence @@ supportResistanceRules[OptionValue["SupportResistance"]], Sequence @@ harmonicRules[OptionValue["HarmonicPatterns"]]]]];

(* ====================================================================== *)
(* THE TABLES OF A FEATURE SET                                             *)
(* ====================================================================== *)

(* the rows of the key "Segments": the lines (Kind Res or Sup) and the segments between peaks and valleys *)
segmentRows[featureSet_] := Quiet[Check[Normal[featureSet["Segments"]], {}]];
lineQ[row_] := MemberQ[{"Res", "Sup"}, Lookup[row, "Kind", None]];

(* a row of Segments has every key; the keys that do not apply to it (a segment has no Touches) are Missing and are left out *)
supportResistanceTable[featureSet_] := Dataset[DeleteMissing /@ Select[segmentRows[featureSet], lineQ]];
segmentTable[featureSet_] := Dataset[DeleteMissing /@ Select[segmentRows[featureSet], ! lineQ[#] &]];
patternTable[featureSet_] :=
  With[{patterns = Quiet[Check[featureSet["Patterns"], {}]]}, Dataset[If[MissingQ[patterns] || ! MatchQ[Normal[patterns], _List], {}, DeleteMissing /@ Normal[patterns]]]];

patternNames[] :=
  {"Symmetric triangle", "Ascending triangle", "Descending triangle", "Rising wedge", "Falling wedge", "Broadening formation", "Ascending right-angled broadening",
   "Descending right-angled broadening", "Rising broadening wedge", "Falling broadening wedge", "Rectangle (trading range)", "Rising channel", "Falling channel"};

End[];

EndPackage[];
