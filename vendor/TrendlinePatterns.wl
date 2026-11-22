(* ::Package:: *)

(* :Title: TrendlinePatterns *)
(* :Context: StockAnalyzer`TrendlinePatterns` *)
(* :Summary:
   Support and resistance lines found one candlestick at a time, and the 13 patterns that a resistance and a support
   make together. Everything here is a plain function of the candlesticks, the peaks and valleys and the lines found so far:
   no feature set is needed, so the same code serves the incremental pipeline of updateFeatureSet (Analysis.wl) and a stream
   of candlesticks loaded one by one (newTrendlineState / updateTrendlineState).

   A line is an Association with the keys Kind ("Res" / "Sup"), Direction (ResU ResD ResF SupU SupD SupF), FromIndex (first anchor),
   AnchorIndex (second anchor), FromPrice, AnchorPrice, Slope, LogSlope, StartIndex (leftmost candle that touches it), Touches,
   TouchIndices, BackTouchIndices (touches in its backward extension), FirstIndex (first candle at which it is valid), EndIndex (last candle at which it is valid, missing while alive),
   Alive, RangeHigh, RangeLow, TolerancePercent.

   Rules (the lines)
     A resistance goes through two peaks (their Highs), a support through two valleys (their Lows). A line is valid at candle N if no candle
     from its first anchor to N goes through it by more than the tolerance (High for a resistance, Low for a support). The tolerance of a candle
     is TolerancePercent percent of the High-Low range of the candles between the first anchor and that candle. A candle within the tolerance
     of the line touches it. A stub (first anchor an earlier peak, second anchor the peak at N-1) is kept if it has at least MinTouches touches
     between its anchors, and is then extended to N. A line is ended by the first candle that breaks it.
   Rules (the patterns)
     A pattern is one resistance and one support, both valid at N, with the resistance above the support at the start of the pair and at N.
     The directions U (up), D (down) and F (flat) of the two lines give 3 x 3 cells; the cells with two different directions and FF are one
     pattern each (6 + 1); the cells UU and DD split in three by what the gap between the lines does from the start t0 of the pair (the later
     of the two StartIndex) to N: gap(N)/gap(t0) within 1 +- PatternBand is parallel (channel), smaller is converging (wedge), larger is
     diverging (broadening wedge). 6 + 1 + 3 + 3 = 13 patterns: 5 converging, 5 diverging, 3 parallel.
   The tight pair tolerance (2026-11-16, trendlineTightPairs): the tolerance of the finder above (5% of the High-Low range from the first anchor of a line) is wide for a line anchored far back,
   up to half the gap between the lines, so almost every candle near the line is a touch. The tight version takes the tolerance of the Fibonacci touches: a candle touches a line if its High (resistance)
   or Low (support) is within TouchTolerance (1%) of the SIZE OF THE STRUCTURE, here the vertical gap between the two lines at t0, the later of their two first anchors. That band is the same for both
   lines for the life of the pair; a line is broken by the first candle that passes it by more than the band. Single lines (without a pair) keep the old tolerance.
   Last changed 2026-11-16.
*)

BeginPackage["StockAnalyzer`TrendlinePatterns`"];

$trendlinePatterns::usage =
  "$trendlinePatterns is the list of the 13 patterns of a resistance and a support: Associations with Key, Name, ResistanceDirection, " <>
  "SupportDirection and Relation (converging, diverging or parallel). The directions are U, D or F.";

trendlineLineStep::usage =
  "trendlineLineStep[highs, lows, peakIndices, valleyIndices, lines, opts] takes the Highs and Lows of the candlesticks 1..N, the indices of the " <>
  "peaks and valleys known at N (the last candlestick is not one) and the lines found up to N-1 (a list of Associations, {} at first), " <>
  "and returns the lines found up to N: the lines that were alive are extended to N or ended (EndIndex N-1), and the stubs whose second " <>
  "anchor is the peak (valley) N-1 are added. Options: \"TolerancePercent\" (5) and \"MinTouches\" (1).";

trendlineRowsAt::usage =
  "trendlineRowsAt[lines, n] returns the rows of Segments for the lines alive at candlestick n: Direction (ResU, ResD, ResF, SupU, SupD, SupF), " <>
  "DeltaX, FromIndex, ToIndex (n), FromPrice, ToPrice (the line at n), Range, LogRatio, Angle, Weight, Kind, Touches, AnchorIndex, StartIndex, " <>
  "Slope and LogSlope.";

trendlinePatternsAt::usage =
  "trendlinePatternsAt[lines, n, opts] returns the patterns made by one resistance and one support of lines that are both alive at candlestick n, as " <>
  "a list of Associations: Pattern (key), PatternName, ToIndex, ResistanceDirection, SupportDirection, Relation (of the pattern), GapRatio, " <>
  "StartIndex (t0), Width (n - t0), Apex (candle where the two lines meet, missing if parallel), the From and Anchor indices of the two lines and " <>
  "their slopes, ResistanceFromPrice and SupportFromPrice (the price of each line at its FromIndex), ResistanceValue and SupportValue (the price of each line at n; trendlineValueAt gives it at any other index), ResistanceTouches and SupportTouches (the touches of each line from the left start of its extension to n, the anchors not counted: the touches that are drawn as dots; this is the goodness of the line, ResistanceGoodness and SupportGoodness) and their sum Touches. Option \"PatternBand\" (0.15).";

trendlineGoodness::usage =
  "trendlineGoodness[line, n] is the goodness of a support or resistance line at candlestick n: for now the number of confirmations (touches) it has from the left start of its " <>
  "extension (the touches of its backward extension) to n, without its two anchors.";

trendlineValueAt::usage =
  "trendlineValueAt[line, x] gives the price of a straight trendline at candlestick index x (a number, or a list of indices): FromPrice + Slope (x - FromIndex). " <>
  "line is a line of ValidTrendlines or of the stream, or a row of Segments of a line (they all have FromIndex, FromPrice and Slope); x may be before the line starts or " <>
  "after it ends (the straight line continued). trendlineValueAt[patternRow, \"Resistance\" or \"Support\", x] does the same for one of the two lines of a row of the key Patterns.";

trendlineApex::usage =
  "trendlineApex[resistance, support] gives the (real) candle index where the two lines meet, or Missing[\"Parallel\"].";

trendlinePatternEpisodes::usage =
  "trendlinePatternEpisodes[patternRows] groups the rows of trendlinePatternsAt of many candlesticks into episodes: the same pattern made of the same two lines " <>
  "on consecutive candlesticks. Each episode has FirstIndex, LastIndex, Candles and the fields of its first row.";

trendlinePairPatternAt::usage =
  "trendlinePairPatternAt[resistance, support, n, opts] gives <|\"Pattern\" -> key, \"GapRatio\" -> ratio|> for the pattern that these two lines (records with Kind, Direction, FromIndex, FromPrice, " <>
  "Slope and StartIndex; both assumed alive at n) make at candlestick n, or Missing[\"NoPattern\"]. The same rule as trendlinePatternsAt for one pair. Option: \"PatternBand\" (0.15).";

trendlinePairEpisodes::usage =
  "trendlinePairEpisodes[lines, nBars, opts] gives the pattern episodes of a finished set of lines (the final \"Lines\" of the stream, alive or ended) without going through the " <>
  "candlesticks one at a time: for every pair of one resistance and one support it takes the candlesticks at which both are alive (FirstIndex to EndIndex, or nBars) and splits them " <>
  "into runs of the same pattern. The result is the same list as trendlinePatternEpisodes gives for the pattern rows of every candlestick (same pattern, same two lines, same FirstIndex, LastIndex and " <>
  "first row), and much faster than the stream with patterns. Option: \"PatternBand\" (0.15).";

newTrendlineState::usage =
  "newTrendlineState[opts] is the empty state of a stream of candlesticks. Options: \"TolerancePercent\" (5), \"MinTouches\" (1), \"PatternBand\" (0.15), " <>
  "\"Patterns\" (True; False skips the pairs of lines, which are most of the time, and gives the lines only), " <>
  "\"FirstCandlestickIsPeakQ\" and \"FirstCandlestickIsValleyQ\" (False: the first candlestick is a peak or a valley only if the caller knows it from earlier data).";

updateTrendlineState::usage =
  "updateTrendlineState[state, candlestick] adds one candlestick (an Association or row with High and Low) to the state and returns the new state: " <>
  "Lines (all lines, alive or ended), ValidLines (alive at the new candlestick), ValidPatterns (the patterns valid at it), PatternRows (the patterns of every " <>
  "candlestick so far) and N. The peak (valley) at N-1 is found with the strict rule High(N-1) > High(N-2) and High(N-1) > High(N) (sigma 0).";

trendlineTightPairs::usage =
  "trendlineTightPairs[lines, highs, lows, opts] takes the lines of the finder (a list of Associations, alive or ended; they are only candidate anchors: their own tolerance is not used) and the Highs and Lows of the candlesticks 1..N " <>
  "and returns the pairs of one resistance and one support that are valid with the tight pair tolerance: Associations with Resistance and Support (lines with the tight touches: TouchIndices without the two anchors, BackTouchIndices {}, StartIndex = FromIndex), " <>
  "StartIndex (t0: the later of the two first anchors), Gap0 (the gap between the lines at t0), Band (TouchTolerance times Gap0), FirstIndex (the candlestick after the later second anchor), LastIndex (the last candlestick at which neither line is broken and the " <>
  "resistance is above the support), ResistanceDirection and SupportDirection (U, D or F). Options: \"TouchTolerance\" (0.01), \"FlatFraction\" (0.1: a line is flat if its two anchors differ by less than this fraction of Gap0).";

trendlineTightPatternAt::usage =
  "trendlineTightPatternAt[pair, n, opts] gives the row of the pattern that a pair of trendlineTightPairs makes at candlestick n (FirstIndex <= n <= LastIndex), with the keys of the rows of trendlinePatternsAt and the extra keys Band and Gap0; " <>
  "Missing[\"NoPattern\"] if the directions and the gap make none of the 13. Option \"PatternBand\" (0.15).";

Begin["`Private`"];

(* ---------------------------------------------------------------------------------------------------- the 13 patterns *)

$trendlinePatterns = {
  <|"Key" -> "SymmetricTriangle",      "Name" -> "Symmetric triangle",                 "ResistanceDirection" -> "D", "SupportDirection" -> "U", "Relation" -> "converging"|>,
  <|"Key" -> "AscendingTriangle",      "Name" -> "Ascending triangle",                 "ResistanceDirection" -> "F", "SupportDirection" -> "U", "Relation" -> "converging"|>,
  <|"Key" -> "DescendingTriangle",     "Name" -> "Descending triangle",                "ResistanceDirection" -> "D", "SupportDirection" -> "F", "Relation" -> "converging"|>,
  <|"Key" -> "RisingWedge",            "Name" -> "Rising wedge",                       "ResistanceDirection" -> "U", "SupportDirection" -> "U", "Relation" -> "converging"|>,
  <|"Key" -> "FallingWedge",           "Name" -> "Falling wedge",                      "ResistanceDirection" -> "D", "SupportDirection" -> "D", "Relation" -> "converging"|>,
  <|"Key" -> "BroadeningFormation",    "Name" -> "Broadening formation",               "ResistanceDirection" -> "U", "SupportDirection" -> "D", "Relation" -> "diverging"|>,
  <|"Key" -> "AscendingBroadening",    "Name" -> "Ascending right-angled broadening",  "ResistanceDirection" -> "U", "SupportDirection" -> "F", "Relation" -> "diverging"|>,
  <|"Key" -> "DescendingBroadening",   "Name" -> "Descending right-angled broadening", "ResistanceDirection" -> "F", "SupportDirection" -> "D", "Relation" -> "diverging"|>,
  <|"Key" -> "RisingBroadeningWedge",  "Name" -> "Rising broadening wedge",            "ResistanceDirection" -> "U", "SupportDirection" -> "U", "Relation" -> "diverging"|>,
  <|"Key" -> "FallingBroadeningWedge", "Name" -> "Falling broadening wedge",           "ResistanceDirection" -> "D", "SupportDirection" -> "D", "Relation" -> "diverging"|>,
  <|"Key" -> "Rectangle",              "Name" -> "Rectangle (trading range)",          "ResistanceDirection" -> "F", "SupportDirection" -> "F", "Relation" -> "parallel"|>,
  <|"Key" -> "RisingChannel",          "Name" -> "Rising channel",                     "ResistanceDirection" -> "U", "SupportDirection" -> "U", "Relation" -> "parallel"|>,
  <|"Key" -> "FallingChannel",         "Name" -> "Falling channel",                    "ResistanceDirection" -> "D", "SupportDirection" -> "D", "Relation" -> "parallel"|>};

patternNames = Association[#["Key"] -> #["Name"] & /@ $trendlinePatterns];

(* the pattern of the directions of the resistance and the support; UU and DD also need the behaviour of the gap *)
patternKey[rd_String, sd_String, relation_String] :=
  Which[
    rd === "U" && sd === "U", Switch[relation, "parallel", "RisingChannel", "converging", "RisingWedge", _, "RisingBroadeningWedge"],
    rd === "D" && sd === "D", Switch[relation, "parallel", "FallingChannel", "converging", "FallingWedge", _, "FallingBroadeningWedge"],
    True, Lookup[<|{"D", "U"} -> "SymmetricTriangle", {"F", "U"} -> "AscendingTriangle", {"D", "F"} -> "DescendingTriangle",
                   {"U", "D"} -> "BroadeningFormation", {"U", "F"} -> "AscendingBroadening", {"F", "D"} -> "DescendingBroadening",
                   {"F", "F"} -> "Rectangle"|>, Key[{rd, sd}], Missing["NoPattern"]]];

(* ---------------------------------------------------------------------------------------------------- the lines *)

tlLog[x_] := If[x > 0, Log[x], 0];

(* the goodness of a line at n: its touches from the left start of its extension (BackTouchIndices) to n (TouchIndices up to n), without its two anchors.
   This is the one place to change if the goodness gets another definition. *)
trendlineGoodness[line_Association, n_Integer] :=
  Length[Lookup[line, "BackTouchIndices", {}]] + Count[Lookup[line, "TouchIndices", {}], k_ /; k <= n];

(* tolerance of the candlesticks a1 .. a2: pctArg percent of the High-Low range of the candlesticks from a1 to each of them *)
toleranceVector[highs_List, lows_List, a1_Integer, a2_Integer, pct_] :=
  (pct / 100) * (FoldList[Max, highs[[a1 ;; a2]]] - FoldList[Min, lows[[a1 ;; a2]]]);

(* the backward extension of a line: to the left of its first anchor, up to the first candlestick that breaks it. Returns the leftmost candlestick
   that touches the line (StartIndex: the first anchor, or a touch in the extension) and the candlesticks of the extension that touch it (BackTouchIndices, increasing) *)
backExtensionOf[kind_String, highs_List, lows_List, a1_Integer, p1_, slope_, pct_] :=
  Module[{series, ks, lineVals, tols, off, touches = {}},
    If[a1 <= 1, Return[<|"StartIndex" -> a1, "BackTouchIndices" -> {}|>]];
    series   = If[kind === "Res", highs, lows];
    ks       = Range[a1 - 1, 1, -1];
    lineVals = p1 + slope * (ks - a1);
    tols     = (pct / 100) * (FoldList[Max, Reverse[highs[[1 ;; a1]]]] - FoldList[Min, Reverse[lows[[1 ;; a1]]]])[[2 ;;]];
    off      = If[kind === "Res", series[[ks]] - lineVals, lineVals - series[[ks]]] - tols;
    Do[
      If[off[[j]] > 0, Break[]];
      If[Abs[series[[ks[[j]]]] - lineVals[[j]]] <= tols[[j]], AppendTo[touches, ks[[j]]]],
      {j, Length[ks]}];
    <|"StartIndex" -> If[touches === {}, a1, Min[touches]], "BackTouchIndices" -> Reverse[touches]|>];

(* the stub from anchor a1 to anchor a2: a line, or Missing if a candlestick between them breaks it or it has too few touches *)
scanStub[kind_String, highs_List, lows_List, a1_Integer, a2_Integer, pct_, minTouches_] :=
  Module[{series, p1, p2, slope, tols, inside, lineVals, tolInside, excess, touchIndices, touches, direction, back},
    series = If[kind === "Res", highs, lows];
    p1 = series[[a1]]; p2 = series[[a2]];
    slope = (p2 - p1) / (a2 - a1);
    tols  = toleranceVector[highs, lows, a1, a2, pct];
    inside    = series[[a1 + 1 ;; a2 - 1]];
    lineVals  = p1 + slope * Range[1, a2 - a1 - 1];
    tolInside = tols[[2 ;; -2]];
    excess = If[kind === "Res", inside - lineVals, lineVals - inside] - tolInside;
    If[Length[excess] > 0 && Max[excess] > 0, Return[Missing["Pierced"]]];
    touchIndices = a1 + Flatten[Position[Abs[inside - lineVals] - tolInside, _?(# <= 0 &)]];
    touches      = Length[touchIndices];
    If[touches < minTouches, Return[Missing["TooFewTouches"]]];
    (* flat: the two anchors differ by less than the tolerance at the second anchor *)
    direction = Which[Abs[p2 - p1] < tols[[-1]], "F", slope > 0, "U", True, "D"];
    back = backExtensionOf[kind, highs, lows, a1, p1, slope, pct];
    <|"Kind" -> kind, "Direction" -> kind <> direction, "FromIndex" -> a1, "AnchorIndex" -> a2, "FromPrice" -> p1, "AnchorPrice" -> p2,
      "Slope" -> slope, "LogSlope" -> (tlLog[p2] - tlLog[p1]) / (a2 - a1),
      "RangeHigh" -> Max[highs[[a1 ;; a2]]], "RangeLow" -> Min[lows[[a1 ;; a2]]],
      "Touches" -> touches, "TouchIndices" -> touchIndices,
      "StartIndex" -> back["StartIndex"], "BackTouchIndices" -> back["BackTouchIndices"],
      "FirstIndex" -> Missing[], "EndIndex" -> Missing[], "Alive" -> True|>];

(* extends a line valid at n - 1 to n; if candlestick n breaks it the line is ended (Alive False, EndIndex n - 1) *)
extendLine[line_Association, highs_List, lows_List, n_Integer, pct_] :=
  Module[{kind = line["Kind"], rangeHigh, rangeLow, tol, lineVal, price, pierced},
    rangeHigh = Max[line["RangeHigh"], highs[[n]]];
    rangeLow  = Min[line["RangeLow"],  lows[[n]]];
    tol       = (pct / 100) * (rangeHigh - rangeLow);
    lineVal   = line["FromPrice"] + line["Slope"] * (n - line["FromIndex"]);
    price     = If[kind === "Res", highs[[n]], lows[[n]]];
    pierced   = If[kind === "Res", price - lineVal > tol, lineVal - price > tol];
    If[pierced,
      Join[line, <|"Alive" -> False, "EndIndex" -> n - 1|>],
      Join[line, <|"RangeHigh" -> rangeHigh, "RangeLow" -> rangeLow,
                   "Touches" -> line["Touches"] + If[Abs[price - lineVal] <= tol, 1, 0],
                   "TouchIndices" -> If[Abs[price - lineVal] <= tol, Append[line["TouchIndices"], n], line["TouchIndices"]]|>]]];

Options[trendlineLineStep] = {"TolerancePercent" -> 5, "MinTouches" -> 1};

trendlineLineStep[highs_List, lows_List, peakIndices_List, valleyIndices_List, linesArg_List, OptionsPattern[]] :=
  Module[{pct = OptionValue["TolerancePercent"], minTouches = OptionValue["MinTouches"], n = Length[highs], lines, stubs, anchorIndices, stub, anchor},
    If[n < 3, Return[linesArg]];
    (* 1. the lines that were alive at n - 1: extended to n or ended *)
    lines = Map[If[TrueQ[#["Alive"]], extendLine[#, highs, lows, n, pct], #] &, linesArg];
    (* 2. the stubs whose second anchor is candlestick n - 1, extended to n *)
    anchor = n - 1;
    stubs  = {};
    Do[
      anchorIndices = If[kind === "Res", peakIndices, valleyIndices];
      If[MemberQ[anchorIndices, anchor],
        Do[
          stub = scanStub[kind, highs, lows, a1, anchor, pct, minTouches];
          If[AssociationQ[stub],
            stub = extendLine[stub, highs, lows, n, pct];
            If[TrueQ[stub["Alive"]], AppendTo[stubs, Join[stub, <|"FirstIndex" -> n, "TolerancePercent" -> pct|>]]]],
          {a1, Select[anchorIndices, IntegerQ[#] && 1 <= # < anchor &]}]],
      {kind, {"Res", "Sup"}}];
    Join[lines, stubs]];

(* the rows of Segments of the lines alive at n *)
trendlineRowsAt[linesArg_List, n_Integer] :=
  Map[
    Function[line,
      With[{p1 = line["FromPrice"], a1 = line["FromIndex"], pN = line["FromPrice"] + line["Slope"] * (n - line["FromIndex"])},
        <|"Direction" -> line["Direction"], "DeltaX" -> n - a1, "FromIndex" -> a1, "ToIndex" -> n, "FromPrice" -> p1, "ToPrice" -> pN,
          "Range" -> pN - p1, "LogRatio" -> tlLog[pN] - tlLog[p1],
          "Angle" -> Round[ArcTan[n - a1, pN - p1], 0.001], "Weight" -> Round[ArcTan[n - a1, pN - p1], 0.001],
          "Kind" -> line["Kind"], "Touches" -> line["Touches"], "AnchorIndex" -> line["AnchorIndex"], "StartIndex" -> line["StartIndex"],
          "Slope" -> line["Slope"], "LogSlope" -> line["LogSlope"], "Goodness" -> trendlineGoodness[line, n]|>]],
    Select[linesArg, TrueQ[#["Alive"]] &]];

(* ---------------------------------------------------------------------------------------------------- the patterns *)

(* the price of the straight line at x: one definition for the lines, the rows of Segments and the pattern rows *)
trendlineValueAt[line_Association, x_] := line["FromPrice"] + line["Slope"] * (x - line["FromIndex"]);
trendlineValueAt[row_Association, kind : ("Resistance" | "Support"), x_] :=
  row[kind <> "FromPrice"] + row[kind <> "Slope"] * (x - row[kind <> "FromIndex"]);

linePrice[line_Association, x_] := trendlineValueAt[line, x];

trendlineApex[res_Association, sup_Association] :=
  Module[{sR = res["Slope"], sS = sup["Slope"]},
    If[sR == sS, Missing["Parallel"],
      (sup["FromPrice"] - res["FromPrice"] + sR res["FromIndex"] - sS sup["FromIndex"]) / (sR - sS)]];

Options[trendlinePatternsAt] = {"PatternBand" -> 0.15};

trendlinePatternsAt[linesArg_List, n_Integer, OptionsPattern[]] :=
  Module[{band = OptionValue["PatternBand"], alive, resistances, supports, rows = {}, t0, gap0, gapN, ratio, relation, key, rd, sd, apex, rTouches, sTouches},
    alive = Select[linesArg, TrueQ[#["Alive"]] &];
    resistances = Select[alive, #["Kind"] === "Res" &];
    supports    = Select[alive, #["Kind"] === "Sup" &];
    Do[
      (* the start of the pair: the later of the two start candlesticks *)
      t0   = Max[res["StartIndex"], sup["StartIndex"]];
      gap0 = linePrice[res, t0] - linePrice[sup, t0];
      gapN = linePrice[res, n]  - linePrice[sup, n];
      If[gap0 > 0 && gapN > 0 && t0 < n,
        ratio    = gapN / gap0;
        relation = Which[ratio < 1 - band, "converging", ratio <= 1 + band, "parallel", True, "diverging"];
        rd  = StringTake[res["Direction"], -1];
        sd  = StringTake[sup["Direction"], -1];
        key = patternKey[rd, sd, relation];
        If[StringQ[key],
          apex = trendlineApex[res, sup];
          (* the touches of each line, from the left start of its extension to N, without its two anchors (the yellow dots of the display): its goodness *)
          rTouches = trendlineGoodness[res, n];
          sTouches = trendlineGoodness[sup, n];
          AppendTo[rows, <|
            "Pattern" -> key, "PatternName" -> patternNames[key], "ToIndex" -> n,
            "ResistanceDirection" -> rd, "SupportDirection" -> sd,
            "Relation" -> Which[rd === "U" && sd === "U" || rd === "D" && sd === "D", relation,
                                True, SelectFirst[$trendlinePatterns, #["Key"] === key &]["Relation"]],
            "GapRatio" -> ratio, "StartIndex" -> t0, "Width" -> n - t0, "Apex" -> apex,
            "ResistanceTouches" -> rTouches, "SupportTouches" -> sTouches, "Touches" -> rTouches + sTouches,
            "ResistanceGoodness" -> rTouches, "SupportGoodness" -> sTouches,
            "ResistanceFromIndex" -> res["FromIndex"], "ResistanceAnchorIndex" -> res["AnchorIndex"],
            "SupportFromIndex" -> sup["FromIndex"], "SupportAnchorIndex" -> sup["AnchorIndex"],
            "ResistanceFromPrice" -> res["FromPrice"], "SupportFromPrice" -> sup["FromPrice"],
            "ResistanceValue" -> linePrice[res, n], "SupportValue" -> linePrice[sup, n],
            "ResistanceSlope" -> res["Slope"], "SupportSlope" -> sup["Slope"],
            "ResistanceLogSlope" -> res["LogSlope"], "SupportLogSlope" -> sup["LogSlope"]|>]]],
      {res, resistances}, {sup, supports}];
    rows];

(* the episodes: the same pattern of the same two lines on consecutive candlesticks *)
trendlinePatternEpisodes[rowsArg_List] :=
  Module[{groups, episodes},
    groups = GroupBy[rowsArg, {#["Pattern"], #["ResistanceFromIndex"], #["ResistanceAnchorIndex"], #["SupportFromIndex"], #["SupportAnchorIndex"]} &];
    episodes = Flatten[Map[
      Function[g,
        Map[Function[run, Join[First[run], <|"FirstIndex" -> First[run]["ToIndex"], "LastIndex" -> Last[run]["ToIndex"], "Candles" -> Length[run]|>]],
          Split[SortBy[g, #["ToIndex"] &], #2["ToIndex"] - #1["ToIndex"] == 1 &]]],
      Values[groups]], 1];
    SortBy[episodes, {#["FirstIndex"] &, #["Pattern"] &}]];

(* the pattern of one pair of lines at n (the rule of trendlinePatternsAt) *)
Options[trendlinePairPatternAt] = {"PatternBand" -> 0.15};

trendlinePairPatternAt[res_Association, sup_Association, n_Integer, OptionsPattern[]] :=
  Module[{band = OptionValue["PatternBand"], t0, gap0, gapN, ratio, relation, key},
    t0   = Max[res["StartIndex"], sup["StartIndex"]];
    gap0 = linePrice[res, t0] - linePrice[sup, t0];
    gapN = linePrice[res, n]  - linePrice[sup, n];
    If[gap0 > 0 && gapN > 0 && t0 < n,
      ratio    = gapN / gap0;
      relation = Which[ratio < 1 - band, "converging", ratio <= 1 + band, "parallel", True, "diverging"];
      key = patternKey[StringTake[res["Direction"], -1], StringTake[sup["Direction"], -1], relation];
      If[StringQ[key], <|"Pattern" -> key, "GapRatio" -> ratio|>, Missing["NoPattern"]],
      Missing["NoPattern"]]];

(* the episodes of a finished set of lines, pair by pair (see the usage); the pairs of a long series are many (tens of thousands of episodes for a stock of 300 candlesticks) *)
Options[trendlinePairEpisodes] = {"PatternBand" -> 0.15};

trendlinePairEpisodes[linesArg_List, nBars_Integer, OptionsPattern[]] :=
  Module[{band = OptionValue["PatternBand"], res, sup, lastOf, episodes = {}, alive},
    lastOf[l_] := If[IntegerQ[l["EndIndex"]], l["EndIndex"], nBars];
    alive = Map[Join[#, <|"Alive" -> True|>] &, linesArg];
    res = Select[alive, #["Kind"] === "Res" &];
    sup = Select[alive, #["Kind"] === "Sup" &];
    Do[
      Module[{lo, hi, t0, gap0, ns, gaps, ok, keys, runs, rd = StringTake[r["Direction"], -1], sd = StringTake[s["Direction"], -1]},
        t0 = Max[r["StartIndex"], s["StartIndex"]];
        lo = Max[r["FirstIndex"], s["FirstIndex"], t0 + 1];
        hi = Min[lastOf[r], lastOf[s]];
        If[lo <= hi,
          gap0 = linePrice[r, t0] - linePrice[s, t0];
          If[gap0 > 0,
            ns   = Range[lo, hi];
            gaps = linePrice[r, ns] - linePrice[s, ns];
            keys = MapThread[
              If[#2 > 0, patternKey[rd, sd, Which[#2/gap0 < 1 - band, "converging", #2/gap0 <= 1 + band, "parallel", True, "diverging"]], Missing["NoPattern"]] &,
              {ns, gaps}];
            runs = Split[Select[Transpose[{ns, keys}], StringQ[#[[2]]] &], #2[[1]] - #1[[1]] == 1 && #2[[2]] === #1[[2]] &];
            Do[
              With[{row = First[trendlinePatternsAt[{r, s}, run[[1, 1]], "PatternBand" -> band]]},
                AppendTo[episodes, Join[row, <|"FirstIndex" -> run[[1, 1]], "LastIndex" -> run[[-1, 1]], "Candles" -> Length[run]|>]]],
              {run, runs}]]]],
      {r, res}, {s, sup}];
    SortBy[episodes, {#["FirstIndex"] &, #["Pattern"] &}]];

(* ---------------------------------------------------------------------------------------------------- the tight pair tolerance *)

Options[trendlineTightPairs] = {"TouchTolerance" -> 0.01, "FlatFraction" -> 0.1};

trendlineTightPairs[linesArg_List, highs_List, lows_List, OptionsPattern[]] :=
  Module[{nBars = Length[highs], tol = OptionValue["TouchTolerance"], flatF = OptionValue["FlatFraction"], ks, prof, res, sup, pairs = {}, big = 10.^300},
    ks = N[Range[nBars]];
    (* per line: its price at every candlestick, the running maximum (from its first anchor) of how far the candlesticks pass it (High above a resistance, Low below a support)
       and the distance of the High (Low) from it *)
    prof[line_Association] :=
      Module[{a = line["FromIndex"], vals, ex},
        vals = line["FromPrice"] + line["Slope"] (ks - a);
        ex   = If[line["Kind"] === "Res", highs - vals, vals - lows];
        <|"Line" -> line, "Vals" -> vals, "Cum" -> Join[ConstantArray[-big, a - 1], FoldList[Max, ex[[a ;;]]]],
          "Dev" -> Abs[If[line["Kind"] === "Res", highs, lows] - vals]|>];
    res = prof /@ Select[linesArg, #["Kind"] === "Res" &];
    sup = prof /@ Select[linesArg, #["Kind"] === "Sup" &];
    Do[
      Module[{r = pr["Line"], s = ps["Line"], nFirst, t0, gap0, band, breakR, breakS, gapEnd, nLast, dR, dS, sR, sS, touchR, touchS},
        nFirst = Max[r["AnchorIndex"], s["AnchorIndex"]] + 1;
        If[nFirst <= nBars,
          t0   = Max[r["FromIndex"], s["FromIndex"]];
          gap0 = pr["Vals"][[t0]] - ps["Vals"][[t0]];
          If[gap0 > 0,
            band   = tol gap0;
            (* the first candlestick that passes a line by more than the band *)
            breakR = Total[UnitStep[band - pr["Cum"]]] + 1;
            breakS = Total[UnitStep[band - ps["Cum"]]] + 1;
            sR = r["Slope"]; sS = s["Slope"];
            (* the gap is linear: the resistance stays above the support up to the candlestick before the apex *)
            gapEnd = If[sR >= sS, nBars + 1, Ceiling[t0 + gap0/(sS - sR)]];
            nLast  = Min[breakR, breakS, gapEnd, nBars + 1] - 1;
            If[nLast >= nFirst && Min[breakR, breakS] > t0,
              touchR = Complement[Pick[Range[r["FromIndex"], nLast], UnitStep[band - pr["Dev"][[r["FromIndex"] ;; nLast]]], 1], {r["FromIndex"], r["AnchorIndex"]}];
              touchS = Complement[Pick[Range[s["FromIndex"], nLast], UnitStep[band - ps["Dev"][[s["FromIndex"] ;; nLast]]], 1], {s["FromIndex"], s["AnchorIndex"]}];
              dR = Which[Abs[r["AnchorPrice"] - r["FromPrice"]] < flatF gap0, "F", sR > 0, "U", True, "D"];
              dS = Which[Abs[s["AnchorPrice"] - s["FromPrice"]] < flatF gap0, "F", sS > 0, "U", True, "D"];
              AppendTo[pairs, <|
                "Resistance" -> Join[r, <|"TouchIndices" -> touchR, "BackTouchIndices" -> {}, "StartIndex" -> r["FromIndex"], "Touches" -> Length[touchR]|>],
                "Support"    -> Join[s, <|"TouchIndices" -> touchS, "BackTouchIndices" -> {}, "StartIndex" -> s["FromIndex"], "Touches" -> Length[touchS]|>],
                "StartIndex" -> t0, "Gap0" -> gap0, "Band" -> band, "FirstIndex" -> nFirst, "LastIndex" -> nLast,
                "ResistanceDirection" -> dR, "SupportDirection" -> dS|>]]]]],
      {pr, res}, {ps, sup}];
    pairs];

Options[trendlineTightPatternAt] = {"PatternBand" -> 0.15};

trendlineTightPatternAt[pair_Association, n_Integer, OptionsPattern[]] :=
  Module[{r = pair["Resistance"], s = pair["Support"], t0 = pair["StartIndex"], gap0 = pair["Gap0"], band = OptionValue["PatternBand"], rd, sd, gapN, ratio, relation, key, rT, sT},
    If[n < pair["FirstIndex"] || n > pair["LastIndex"], Return[Missing["NoPattern"]]];
    rd = pair["ResistanceDirection"]; sd = pair["SupportDirection"];
    gapN = trendlineValueAt[r, n] - trendlineValueAt[s, n];
    ratio = gapN/gap0;
    relation = Which[ratio < 1 - band, "converging", ratio <= 1 + band, "parallel", True, "diverging"];
    key = patternKey[rd, sd, relation];
    If[! StringQ[key], Return[Missing["NoPattern"]]];
    rT = Count[r["TouchIndices"], k_ /; k <= n]; sT = Count[s["TouchIndices"], k_ /; k <= n];
    <|"Pattern" -> key, "PatternName" -> patternNames[key], "ToIndex" -> n,
      "ResistanceDirection" -> rd, "SupportDirection" -> sd,
      "Relation" -> Which[rd === "U" && sd === "U" || rd === "D" && sd === "D", relation, True, SelectFirst[$trendlinePatterns, #["Key"] === key &]["Relation"]],
      "GapRatio" -> ratio, "StartIndex" -> t0, "Width" -> n - t0, "Apex" -> trendlineApex[r, s],
      "ResistanceTouches" -> rT, "SupportTouches" -> sT, "Touches" -> rT + sT, "ResistanceGoodness" -> rT, "SupportGoodness" -> sT,
      "ResistanceFromIndex" -> r["FromIndex"], "ResistanceAnchorIndex" -> r["AnchorIndex"], "SupportFromIndex" -> s["FromIndex"], "SupportAnchorIndex" -> s["AnchorIndex"],
      "ResistanceFromPrice" -> r["FromPrice"], "SupportFromPrice" -> s["FromPrice"],
      "ResistanceValue" -> trendlineValueAt[r, n], "SupportValue" -> trendlineValueAt[s, n],
      "ResistanceSlope" -> r["Slope"], "SupportSlope" -> s["Slope"], "ResistanceLogSlope" -> r["LogSlope"], "SupportLogSlope" -> s["LogSlope"],
      "Band" -> pair["Band"], "Gap0" -> gap0|>];

(* ---------------------------------------------------------------------------------------------------- a stream of candlesticks *)

Options[newTrendlineState] = {"TolerancePercent" -> 5, "MinTouches" -> 1, "PatternBand" -> 0.15, "Patterns" -> True,
  "FirstCandlestickIsPeakQ" -> False, "FirstCandlestickIsValleyQ" -> False};

newTrendlineState[opts : OptionsPattern[]] :=
  <|"N" -> 0, "Highs" -> {}, "Lows" -> {}, "PeakIndices" -> {}, "ValleyIndices" -> {}, "Lines" -> {}, "ValidLines" -> {},
    "ValidPatterns" -> {}, "PatternRows" -> {},
    "Options" -> Join[Association[Options[newTrendlineState]], Association[FilterRules[{opts}, Options[newTrendlineState]]]]|>;

updateTrendlineState[state_Association, candle_] :=
  Module[{o = state["Options"], highs, lows, peaks, valleys, n, lines, valid, patterns},
    highs = Append[state["Highs"], candle["High"]];
    lows  = Append[state["Lows"],  candle["Low"]];
    n = Length[highs];
    peaks   = state["PeakIndices"];
    valleys = state["ValleyIndices"];
    (* the first candlestick is a peak / a valley only if told so (from earlier data) *)
    If[n == 2,
      If[TrueQ[o["FirstCandlestickIsPeakQ"]],   AppendTo[peaks, 1]];
      If[TrueQ[o["FirstCandlestickIsValleyQ"]], AppendTo[valleys, 1]]];
    (* candlestick n - 1 is a peak (valley) if it is strictly above (below) both neighbours: known only now, at n *)
    If[n >= 3,
      If[highs[[n - 1]] > highs[[n - 2]] && highs[[n - 1]] > highs[[n]], AppendTo[peaks, n - 1]];
      If[lows[[n - 1]]  < lows[[n - 2]]  && lows[[n - 1]]  < lows[[n]],  AppendTo[valleys, n - 1]]];
    lines    = trendlineLineStep[highs, lows, peaks, valleys, state["Lines"], "TolerancePercent" -> o["TolerancePercent"], "MinTouches" -> o["MinTouches"]];
    valid    = Select[lines, TrueQ[#["Alive"]] &];
    (* the pairs of lines are the expensive part (all pairs of alive lines at every candlestick): "Patterns" -> False gives the lines only *)
    patterns = If[TrueQ[o["Patterns"]], trendlinePatternsAt[lines, n, "PatternBand" -> o["PatternBand"]], {}];
    <|"N" -> n, "Highs" -> highs, "Lows" -> lows, "PeakIndices" -> peaks, "ValleyIndices" -> valleys, "Lines" -> lines, "ValidLines" -> valid,
      "ValidPatterns" -> patterns, "PatternRows" -> Join[state["PatternRows"], patterns], "Options" -> o|>];

End[];

EndPackage[];
