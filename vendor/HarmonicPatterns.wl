(* ::Package:: *)

(* ===========================================================================
   HarmonicPatterns.wl  --  harmonic patterns as small directed graphs, found in the stock graph

   Idea (2026-11-08): a stock chart is a directed graph (vertices = candles at a peak or valley, edges = the strict MaxU / MaxD segments,
   featureSet["Graph"]). A pattern that is made of consecutive segments is a small path graph:
       retracement / ABC   X->A->B                  (2 segments)
       AB=CD               A->B->C->D               (3 segments)
       Gartley, Bat, Butterfly, Crab   X->A->B->C->D   (4 segments)
   FindIsomorphicSubgraph[stockGraph, patternGraph, All] finds every copy of the path. The graph shape cannot tell a retracement from an
   extension (|AB| < |XA| or > |XA|), so each copy is then filtered by the ratios of the leg lengths (ranges, |ToPrice - FromPrice|).

   Leg ratios (names used in the rules)
     BofXA  = |AB| / |XA|      B retracement of XA
     CofAB  = |BC| / |AB|      C retracement of AB
     DofXA  = |AD| / |XA|      D as a retracement (< 1) or extension (> 1) of XA, measured from A
     CDofBC = |CD| / |BC|      CD extension of BC
     CDofAB = |CD| / |AB|      CD against AB (AB=CD means 1)
   Rules (ratio ranges) are from the table the user supplied on 2026-11-08 (thinkmarkets / forexbee / dhan / forextraininggroup, as quoted by
   the user). A rule {name, lo, hi} holds when lo - tol <= ratio <= hi + tol (tol absolute, default 0.05). The Violation of a match is the
   largest distance of a filtered ratio outside its range (0 = inside every range), so one scan at a large tolerance gives every smaller one.
   FILTER rules are B (of XA), C (of AB) and D (of XA); CDofBC and CDofAB (for AB=CD it is a filter) are measured for the others, not filtered.
   Cypher (C beyond A: CofXA = |XC|/|XA|; D retraces XC: DofXC = |CD|/|XC|) and DeepCrab are in (ebc.com table). Shark and 3-Drives are not defined
   yet (the reference legs of the Shark are ambiguous in the sources). The sources disagree whether CD is measured against AB or BC; CDofBC and
   CDofAB are both recorded for every match.

   WARNING (look-ahead): peaks and valleys of the strict graph are only known in hindsight. A match is a DESCRIPTION of the chart. To test
   prediction, use only X, A, B, C and project D from them (the reversal zone), never the located D.

   Two finders give the same matches (checked by compareHarmonicMethods): "PathSearch" (default, walks the segments, prunes by the ratios) and
   "FindIsomorphicSubgraph" (uses featureSet["Graph"]).
   =========================================================================== *)

BeginPackage["StockAnalyzer`HarmonicPatterns`"];

$harmonicPatterns::usage =
  "$harmonicPatterns is an Association: pattern name -> <|\"Points\", \"Rules\" (filters, {ratio, lo, hi}), \"Info\" (measured only)|>. " <>
  "Defined: Gartley, Bat, Butterfly, Crab, DeepCrab, Cypher, ABCD. Edit or add a row to define another pattern.";

harmonicPatternGraph::usage =
  "harmonicPatternGraph[name] gives the directed path graph of a pattern (vertices labelled X, A, B, C, D).";

harmonicMeasures::usage =
  "harmonicMeasures[prices] gives the leg ratios (BofXA, CofAB, DofXA, CDofBC, CDofAB) that can be computed from the prices of the points " <>
  "{X, A, B, C, D} (or {A, B, C, D} for 4 points, taken as A..D).";

findHarmonicPatterns::usage =
  "findHarmonicPatterns[featureSet, name, opts] gives the matches (a list of Associations) of a pattern (a name, a list of names or All) " <>
  "in the strict graph of a featureSet. Each match has Pattern, Direction (Bullish when D is a low), Points, Indices, Prices, Edges, " <>
  "CompletionIndex, Measures and Violation. Options: \"Tolerance\" (0.05), \"Method\" (\"PathSearch\" or \"FindIsomorphicSubgraph\"), " <>
  "\"RatioScale\" (1; every ratio range multiplied by this factor, for the placebo scan).";

highlightHarmonicPattern::usage =
  "highlightHarmonicPattern[featureSet, matches, opts] draws the stock graph with the edges of the matches highlighted (HighlightGraph). " <>
  "Options: \"ImageSize\" (800), \"VertexLabels\" (None).";

drawHarmonicPattern::usage =
  "drawHarmonicPattern[featureSet, match, opts] draws one match as candlestick vertices with the pattern's segments as arrows " <>
  "(candlestickVertexGraph) over the candles of the match plus \"Margin\" candles on each side, with the points labelled.";

harmonicMatchTable::usage =
  "harmonicMatchTable[featureSet, matches] gives the matches as a Dataset, one row per match: Dates (year-month of the candle of each point, " <>
  "joined by \" > \") in the first column, Indices, Direction, Violation and every measured ratio. Option \"DateFormat\" ({\"Year\", \"-\", \"Month\"}).";

compareHarmonicMethods::usage =
  "compareHarmonicMethods[featureSet, name, tol] gives the number of matches found by each method and the matches only one of them finds.";

findHarmonicSetups::usage =
  "findHarmonicSetups[featureSet, name, opts] gives the SETUPS of a pattern (a name, a list of names or All): the paths X, A, B, C (A, B, C for ABCD) " <>
  "that obey the B and C rules, with the projected D zone computed from them only (no look-ahead). Each setup has Pattern, Direction (Bullish when " <>
  "C is a peak, so D is a low), Indices, Prices, KnownIndex (the candle that confirms C), ZoneLow, ZoneHigh and Length (the leg the D rule is " <>
  "measured against), and Scale. Options: \"Tolerance\" (0.02, on the B and C rules), \"TolZone\" (0.02, widens the D ratio range), \"DScale\" (1, or a " <>
  "list: the D ratio range is multiplied by each factor, for the placebo zones; one setup per scale).";

harmonicMatchesBornAt::usage =
  "harmonicMatchesBornAt[segments, newSegments, name, opts] gives the matches of a pattern (a name, a list of names or All) that are BORN by newSegments: " <>
  "the matches whose last leg (C to D) is one of newSegments, found by walking backward from it through segments, a list of Associations with " <>
  "FromIndex, ToIndex, FromPrice and ToPrice (or the Association ToIndex -> list of them). Call it with the segments that end at the new candlestick, " <>
  "once per candlestick, and every match is found exactly once, as soon as it is complete. Options: \"Tolerance\" (0.05), \"RatioScale\" (1).";

harmonicMatchRow::usage =
  "harmonicMatchRow[match] turns a match into one row of the HarmonicPatterns table of a feature set: Pattern, Direction, ToIndex (the candlestick that " <>
  "completes it, where it is born), the indices and prices of its points, its Edges, Violation, every ratio (BofXA, CofAB, DofXA, CDofBC, CDofAB, CofXA, " <>
  "DofXC; Missing when the pattern does not have it) and Subgraph, the directed path graph of the match (vertices are candlestick indices labelled X, A, B, C, D).";

harmonicSubgraph::usage =
  "harmonicSubgraph[featureSet, row] gives the match of a HarmonicPatterns row as a subgraph of the graph of the feature set: the graph of the feature set with " <>
  "the vertices and edges of the match highlighted (Options \"Induced\" False: only the edges of the match are drawn; True: the induced subgraph, all edges " <>
  "between its vertices).";

Begin["`Private`"];

(* ---- leg ratios ------------------------------------------------------------------------------------------------------------------------- *)

(* name -> {points needed, function of the Association of prices} *)
$measureDefs = <|
  "BofXA"  -> {{"X", "A", "B"},      Abs[#B - #A]/Abs[#A - #X] &},
  "CofAB"  -> {{"A", "B", "C"},      Abs[#C - #B]/Abs[#B - #A] &},
  "DofXA"  -> {{"X", "A", "D"},      Abs[#D - #A]/Abs[#A - #X] &},
  "CDofBC" -> {{"B", "C", "D"},      Abs[#D - #C]/Abs[#C - #B] &},
  "CDofAB" -> {{"A", "B", "C", "D"}, Abs[#D - #C]/Abs[#B - #A] &},
  (* Cypher: C beyond A, |XC| against |XA|; D retraces XC: |CD| against |XC| *)
  "CofXA"  -> {{"X", "A", "C"},      Abs[#C - #X]/Abs[#A - #X] &},
  "DofXC"  -> {{"X", "C", "D"},      Abs[#D - #C]/Abs[#C - #X] &}
|>;

measureValue[name_String, pts_Association] :=
  Module[{v = Quiet[Check[$measureDefs[name][[2]][pts], Infinity]]},
    If[NumericQ[v], N[v], Infinity]];

(* every measure whose points are all present *)
allMeasures[pts_Association] :=
  Association @ KeyValueMap[
    If[SubsetQ[Keys[pts], #2[[1]]], #1 -> measureValue[#1, pts], Nothing] &, $measureDefs];

harmonicMeasures[prices_List] :=
  allMeasures[AssociationThread[Take[If[Length[prices] == 4, {"A", "B", "C", "D"}, {"X", "A", "B", "C", "D"}], Length[prices]], prices]];

(* ---- pattern table ---------------------------------------------------------------------------------------------------------------------- *)

$harmonicPatterns = <|
  "Gartley" -> <|"Points" -> {"X", "A", "B", "C", "D"},
    "Rules" -> {{"BofXA", 0.618, 0.618}, {"CofAB", 0.382, 0.886}, {"DofXA", 0.786, 0.786}},
    "Info"  -> {{"CDofBC", 1.272, 1.618}}|>,
  "Bat" -> <|"Points" -> {"X", "A", "B", "C", "D"},
    "Rules" -> {{"BofXA", 0.382, 0.5}, {"CofAB", 0.382, 0.886}, {"DofXA", 0.886, 0.886}},
    "Info"  -> {{"CDofBC", 1.618, 2.618}}|>,
  "Butterfly" -> <|"Points" -> {"X", "A", "B", "C", "D"},
    "Rules" -> {{"BofXA", 0.786, 0.786}, {"CofAB", 0.382, 0.886}, {"DofXA", 1.27, 1.618}},
    "Info"  -> {{"CDofBC", 1.618, 2.24}}|>,
  "Crab" -> <|"Points" -> {"X", "A", "B", "C", "D"},
    "Rules" -> {{"BofXA", 0.382, 0.618}, {"CofAB", 0.382, 0.886}, {"DofXA", 1.618, 1.618}},
    "Info"  -> {{"CDofBC", 2.24, 3.618}}|>,
  "DeepCrab" -> <|"Points" -> {"X", "A", "B", "C", "D"},
    "Rules" -> {{"BofXA", 0.886, 0.886}, {"CofAB", 0.382, 0.886}, {"DofXA", 1.618, 1.618}},
    "Info"  -> {{"CDofBC", 2.24, 3.618}}|>,
  "Cypher" -> <|"Points" -> {"X", "A", "B", "C", "D"},
    "Rules" -> {{"BofXA", 0.382, 0.618}, {"CofXA", 1.272, 1.414}, {"DofXC", 0.786, 0.786}},
    "Info"  -> {}|>,
  (* C of AB: 0.382 to 0.886 (ebc.com; 0.618 and 0.786 are the cleanest, the ratio is recorded, so the narrower range is a filter on the result) *)
  "ABCD" -> <|"Points" -> {"A", "B", "C", "D"},
    "Rules" -> {{"CofAB", 0.382, 0.886}, {"CDofAB", 1., 1.}},
    "Info"  -> {{"CDofBC", 1.27, 1.618}}|>
|>;

pathGraphN[n_Integer] := Graph[Range[n], DirectedEdge @@@ Partition[Range[n], 2, 1]];

harmonicPatternGraph[name_String] :=
  With[{pts = $harmonicPatterns[name]["Points"]},
    Graph[Range[Length[pts]], DirectedEdge @@@ Partition[Range[Length[pts]], 2, 1],
      VertexLabels -> Thread[Range[Length[pts]] -> pts]]];

(* ---- rule checks ------------------------------------------------------------------------------------------------------------------------ *)

(* distance of a ratio outside its range, 0 when inside *)
ruleViolation[{m_String, lo_, hi_}, pts_Association] :=
  With[{v = measureValue[m, pts]}, If[v === Infinity, Infinity, Max[lo - v, v - hi, 0.]]];

(* largest violation among the rules whose points are all present *)
violation[rules_List, pts_Association] :=
  Max[Append[
    ruleViolation[#, pts] & /@ Select[rules, SubsetQ[Keys[pts], $measureDefs[#[[1]]][[1]]] &],
    0.]];

(* consecutive legs must alternate in direction *)
alternatesQ[prices_List] :=
  And @@ Table[(prices[[i + 1]] - prices[[i]]) (prices[[i + 2]] - prices[[i + 1]]) < 0, {i, Length[prices] - 2}];

(* structural validity, independent of the ratio ranges (which imply it only for the published ratios): B lies between X and A, and C lies
   between B and A (a bullish C cannot go below X, a bearish C cannot go above X). Cypher is exempt from the C rule (C is beyond A). *)
geometryOK[name_String, pts_Association] :=
  And[
    If[SubsetQ[Keys[pts], {"X", "A", "B"}], (pts["B"] - pts["X"]) (pts["A"] - pts["B"]) > 0, True],
    If[name =!= "Cypher" && SubsetQ[Keys[pts], {"A", "B", "C"}], (pts["C"] - pts["B"]) (pts["A"] - pts["C"]) > 0, True]];

(* placebo: every ratio range of the rules multiplied by one factor (1 = the published ratios) *)
scaledRules[rules_List, scale_] := {#[[1]], #[[2]] scale, #[[3]] scale} & /@ rules;

(* ---- the match record ------------------------------------------------------------------------------------------------------------------- *)

makeMatch[name_String, idx_List, pr_List, viol_] :=
  Module[{spec = $harmonicPatterns[name], labels, pts},
    labels = spec["Points"]; pts = AssociationThread[labels, pr];
    <|"Pattern" -> name,
      "Direction" -> If[pr[[-1]] < pr[[-2]], "Bullish", "Bearish"],
      "Points" -> labels, "Indices" -> idx, "Prices" -> pr,
      "Edges" -> Partition[idx, 2, 1],
      "CompletionIndex" -> Last[idx],
      "Measures" -> allMeasures[pts],
      "Violation" -> viol|>];

(* ---- finder 1: walk the segments -------------------------------------------------------------------------------------------------------- *)

pathSearch[segs_List, name_String, tol_, scale_ : 1] :=
  Module[{spec = $harmonicPatterns[name], labels, n, rules, out, results = {}, extend},
    labels = spec["Points"]; n = Length[labels]; rules = scaledRules[spec["Rules"], scale];
    out = GroupBy[segs, #["FromIndex"] &];
    extend[idx_List, pr_List] :=
      Module[{pts = AssociationThread[Take[labels, Length[idx]], pr], viol},
        viol = violation[rules, pts];
        If[viol <= tol && geometryOK[name, pts],
          If[Length[idx] == n,
            AppendTo[results, makeMatch[name, idx, pr, viol]],
            Do[
              (* the next segment must start at the price where the last one ended: a candle that is both a peak and a valley has two
                 prices (its High and its Low), and a leg may not arrive at one and leave from the other *)
              If[s["FromPrice"] == Last[pr] && (s["ToPrice"] - Last[pr]) (Last[pr] - pr[[-2]]) < 0,
                extend[Append[idx, s["ToIndex"]], Append[pr, s["ToPrice"]]]],
              {s, Lookup[out, Last[idx], {}]}]]]];
    Do[
      extend[{s["FromIndex"], s["ToIndex"]}, {s["FromPrice"], s["ToPrice"]}],
      {s, segs}];
    results];

(* ---- finder 2: FindIsomorphicSubgraph on the stock graph -------------------------------------------------------------------------------- *)

isoSearch[fs_, segs_List, name_String, tol_, scale_ : 1] :=
  Module[{spec = $harmonicPatterns[name], labels, n, rules, g, subs, lookup, results = {}},
    labels = spec["Points"]; n = Length[labels]; rules = scaledRules[spec["Rules"], scale];
    g = fs["Graph"];
    If[Head[g] === Dataset, g = Normal[g]];
    lookup = Association[(({#["FromIndex"], #["ToIndex"]} -> #) & /@ segs)];
    subs = FindIsomorphicSubgraph[g, pathGraphN[n], All];
    Do[
      Module[{idx = Sort[VertexList[sub]], edges, segsOf, pr},
        edges = Partition[idx, 2, 1];
        segsOf = Lookup[lookup, edges, Missing[]];
        If[FreeQ[segsOf, _Missing],
          pr = Prepend[#["ToPrice"] & /@ segsOf, First[segsOf]["FromPrice"]];
          If[alternatesQ[pr] &&
              (* consecutive legs share their price at the joint (see pathSearch) *)
              And @@ Table[segsOf[[i]]["ToPrice"] == segsOf[[i + 1]]["FromPrice"], {i, Length[segsOf] - 1}] &&
              geometryOK[name, AssociationThread[labels, pr]],
            With[{viol = violation[rules, AssociationThread[labels, pr]]},
              If[viol <= tol, AppendTo[results, makeMatch[name, idx, pr, viol]]]]]]],
      {sub, subs}];
    results];

(* ---- public finder ---------------------------------------------------------------------------------------------------------------------- *)

Options[findHarmonicPatterns] = {"Tolerance" -> 0.05, "Method" -> "PathSearch", "RatioScale" -> 1};

findHarmonicPatterns[fs_, namesArg_, OptionsPattern[]] :=
  Module[{names, segs, tol = OptionValue["Tolerance"], method = OptionValue["Method"], res},
    names = Which[namesArg === All, Keys[$harmonicPatterns], StringQ[namesArg], {namesArg}, True, namesArg];
    (* one segment per {From, To}: the table can hold the same segment once per edge type *)
    segs = fs["Segments"];
    If[! ListQ[segs], segs = Normal[segs]];   (* a Tabular / Dataset -> list of Associations *)
    segs = DeleteDuplicatesBy[Select[segs, AssociationQ], {#["FromIndex"], #["ToIndex"]} &];
    res = Flatten[
      Map[
        Function[name,
          If[method === "FindIsomorphicSubgraph",
            isoSearch[fs, segs, name, tol, OptionValue["RatioScale"]],
            pathSearch[segs, name, tol, OptionValue["RatioScale"]]]],
        names], 1];
    SortBy[res, {#["CompletionIndex"], #["Pattern"]} &]
  ];

(* ---- setups: X, A, B, C known, D projected (no look-ahead) ------------------------------------------------------------------------------ *)

(* the D rule of a pattern: the one rule that needs the point D *)
dRuleOf[name_String] := First[Select[$harmonicPatterns[name]["Rules"], MemberQ[$measureDefs[#[[1]]][[1]], "D"] &]];

(* paths over all points but D, obeying the rules that do not need D (with the geometry check) *)
setupSearch[segs_List, name_String, tol_] :=
  Module[{spec = $harmonicPatterns[name], labels, m, rules, out, results = {}, extend},
    labels = Most[spec["Points"]]; m = Length[labels];
    rules = Select[spec["Rules"], FreeQ[$measureDefs[#[[1]]][[1]], "D"] &];
    out = GroupBy[segs, #["FromIndex"] &];
    extend[idx_List, pr_List] :=
      Module[{pts = AssociationThread[Take[labels, Length[idx]], pr], viol},
        viol = violation[rules, pts];
        If[viol <= tol && geometryOK[name, pts],
          If[Length[idx] == m,
            AppendTo[results, <|"Indices" -> idx, "Prices" -> pr|>],
            Do[
              If[s["FromPrice"] == Last[pr] && (s["ToPrice"] - Last[pr]) (Last[pr] - pr[[-2]]) < 0,
                extend[Append[idx, s["ToIndex"]], Append[pr, s["ToPrice"]]]],
              {s, Lookup[out, Last[idx], {}]}]]]];
    Do[
      extend[{s["FromIndex"], s["ToIndex"]}, {s["FromPrice"], s["ToPrice"]}],
      {s, segs}];
    results];

(* the projected D zone: anchor + sign * r * length for r in the D ratio range (scaled, widened by tz) *)
projectZone[name_String, pr_List, dScale_, tz_] :=
  Module[{labels = Most[$harmonicPatterns[name]["Points"]], pts, rule, m, lo, hi, anchor, len, sgn, prices},
    pts = AssociationThread[labels, pr];
    {m, lo, hi} = dRuleOf[name];
    {anchor, len, sgn} = Switch[m,
      "DofXA",  {pts["A"], Abs[pts["A"] - pts["X"]], Sign[pts["X"] - pts["A"]]},
      "DofXC",  {pts["C"], Abs[pts["C"] - pts["X"]], Sign[pts["X"] - pts["C"]]},
      "CDofAB", {pts["C"], Abs[pts["B"] - pts["A"]], Sign[pts["B"] - pts["C"]]}];
    prices = anchor + sgn # len & /@ {Max[lo dScale - tz, 0.], hi dScale + tz};
    <|"ZoneLow" -> Min[prices], "ZoneHigh" -> Max[prices], "Length" -> len|>];

Options[findHarmonicSetups] = {"Tolerance" -> 0.02, "TolZone" -> 0.02, "DScale" -> 1};

findHarmonicSetups[fs_, namesArg_, OptionsPattern[]] :=
  Module[{names, segs, tol = OptionValue["Tolerance"], tz = OptionValue["TolZone"], dss = Flatten[{OptionValue["DScale"]}]},
    names = Which[namesArg === All, Keys[$harmonicPatterns], StringQ[namesArg], {namesArg}, True, namesArg];
    segs = fs["Segments"];
    If[! ListQ[segs], segs = Normal[segs]];
    segs = DeleteDuplicatesBy[Select[segs, AssociationQ], {#["FromIndex"], #["ToIndex"]} &];
    SortBy[
      Flatten[
        Map[
          Function[name,
            Flatten[Map[
              Function[st,
                Table[
                  Join[<|"Pattern" -> name,
                      "Direction" -> If[st["Prices"][[-1]] > st["Prices"][[-2]], "Bullish", "Bearish"],
                      "KnownIndex" -> Ceiling[Last[st["Indices"]]] + 1, "Scale" -> ds|>,
                    st, projectZone[name, st["Prices"], ds, tz]],
                  {ds, dss}]],
              setupSearch[segs, name, tol]], 1]],
          names], 1],
      {#["KnownIndex"], #["Pattern"]} &]];

(* ---- incremental finder: the matches born at one candlestick ----------------------------------------------------------------------------- *)

(* A match is complete when its last leg (C to D) exists, and a segment ends at the candlestick that was just added. So the matches born at a candlestick are
   the paths whose last segment is one of the segments that end at it: walk backward from each of those segments through the segments that end where the
   path begins (same price at the joint, opposite direction), checking the ratio rules as soon as the points that a rule needs are in the path. The same
   rules and checks as pathSearch, so that the matches found over all candlesticks are exactly those of findHarmonicPatterns on the final feature set. *)
segmentsByTo[segs_Association] := segs;
segmentsByTo[segs_List] := GroupBy[Select[segs, AssociationQ], #["ToIndex"] &];

(* One walk for all the patterns: a path of k points (the last k points of X, A, B, C, D, in time order) is extended backward while at least one pattern still
   accepts it (its rules that need only those points hold within the tolerance, and the geometry is right). A path that is complete for a pattern is a match.
   The rules that a pattern can check with k points are worked out once (rulesAt), so that a step of the walk is a few arithmetic operations. *)
rulesAtDepth[rules_List, labels_List, k_Integer] :=
  Select[rules, SubsetQ[Take[labels, -k], $measureDefs[#[[1]]][[1]]] &];

(* violation of the rules that can be checked with the points pts (an Association label -> price); Infinity if a ratio is not a number *)
fastViolation[rulesHere_List, pts_Association] :=
  Max[Append[
    Function[r, With[{v = $measureDefs[r[[1]]][[2]][pts]}, If[NumericQ[v], Max[r[[2]] - v, v - r[[3]], 0.], Infinity]]] /@ rulesHere, 0.]];

pathSearchBornAt[byTo_Association, newSegs_List, names_List, tol_, scale_ : 1] :=
  Module[{specs, results = {}, extend},
    specs = Association[Function[name,
      With[{labels = $harmonicPatterns[name]["Points"], rules = scaledRules[$harmonicPatterns[name]["Rules"], scale]},
        name -> <|"Labels" -> labels, "Rules" -> rules,
          "RulesAt" -> Association[Table[k -> rulesAtDepth[rules, labels, k], {k, Length[labels]}]]|>]] /@ names];
    (* idx, pr: the last k points of the path in time order; alive: the patterns that still accept it *)
    extend[idx_List, pr_List, alive_List] :=
      Module[{k = Length[idx], keep, deeper, ptsOf},
        ptsOf = Association[];  (* label -> price, per pattern labels (the last k labels are the same for every pattern that is long enough) *)
        keep = Select[alive,
          Function[name,
            With[{labels = specs[name]["Labels"]},
              k <= Length[labels] &&
              With[{pts = AssociationThread[Take[labels, -k], pr]},
                fastViolation[specs[name]["RulesAt"][k], pts] <= tol && geometryOK[name, pts]]]]];
        If[keep =!= {},
          Scan[
            Function[name,
              If[Length[specs[name]["Labels"]] == k,
                With[{pts = AssociationThread[specs[name]["Labels"], pr]},
                  AppendTo[results, makeMatch[name, idx, pr, fastViolation[specs[name]["Rules"], pts]]]]]],
            keep];
          deeper = Select[keep, Length[specs[#]["Labels"]] > k &];
          If[deeper =!= {},
            Do[
              (* the leg before must arrive at the price where the path begins, and have the opposite direction to the leg after *)
              If[s["ToPrice"] == First[pr] && (First[pr] - s["FromPrice"]) (pr[[2]] - First[pr]) < 0,
                extend[Prepend[idx, s["FromIndex"]], Prepend[pr, s["FromPrice"]], deeper]],
              {s, Lookup[byTo, First[idx], {}]}]]]];
    Quiet[Do[extend[{s["FromIndex"], s["ToIndex"]}, {s["FromPrice"], s["ToPrice"]}, names], {s, newSegs}]];
    results];

Options[harmonicMatchesBornAt] = {"Tolerance" -> 0.05, "RatioScale" -> 1};

harmonicMatchesBornAt[segs_, newSegsArg_List, namesArg_, OptionsPattern[]] :=
  Module[{names, byTo = segmentsByTo[segs], newSegs},
    names   = Which[namesArg === All, Keys[$harmonicPatterns], StringQ[namesArg], {namesArg}, True, namesArg];
    newSegs = DeleteDuplicatesBy[Select[newSegsArg, AssociationQ], {#["FromIndex"], #["ToIndex"]} &];
    If[newSegs === {}, Return[{}]];
    SortBy[pathSearchBornAt[byTo, newSegs, names, OptionValue["Tolerance"], OptionValue["RatioScale"]], {#["CompletionIndex"], #["Pattern"]} &]
  ];

(* a match as a row of the HarmonicPatterns table; Missing for a ratio the pattern does not have *)
harmonicMatchRow[m_Association] :=
  Module[{labels = m["Points"], idx = m["Indices"]},
    Join[
      <|"Pattern" -> m["Pattern"], "Direction" -> m["Direction"], "ToIndex" -> m["CompletionIndex"],
        "Points" -> labels, "Indices" -> idx, "Prices" -> m["Prices"], "Edges" -> m["Edges"], "Violation" -> m["Violation"]|>,
      Association[# -> Lookup[m["Measures"], #, Missing["NotApplicable"]] & /@ Keys[$measureDefs]],
      <|"Subgraph" -> Graph[idx, DirectedEdge @@@ m["Edges"], VertexLabels -> Thread[idx -> labels],
          VertexCoordinates -> Thread[idx -> Transpose[{idx, m["Prices"]}]], VertexSize -> Medium,
          AspectRatio -> 1/2, ImageSize -> 450]|>]];

Options[harmonicSubgraph] = {"Induced" -> False, "ImageSize" -> 700};

harmonicSubgraph[fs_, row_Association, OptionsPattern[]] :=
  Module[{g = fs["Graph"], idx = row["Indices"], edges = DirectedEdge @@@ row["Edges"]},
    If[Head[g] === Dataset, g = Normal[g]];
    If[TrueQ[OptionValue["Induced"]],
      Subgraph[g, idx, VertexLabels -> Thread[idx -> row["Points"]], ImageSize -> OptionValue["ImageSize"]],
      Graph[idx, edges, VertexLabels -> Thread[idx -> row["Points"]], ImageSize -> OptionValue["ImageSize"]]]];

compareHarmonicMethods[fs_, name_String, tol_ : 0.05] :=
  Module[{a, b, ka, kb},
    a = findHarmonicPatterns[fs, name, "Tolerance" -> tol, "Method" -> "PathSearch"];
    b = findHarmonicPatterns[fs, name, "Tolerance" -> tol, "Method" -> "FindIsomorphicSubgraph"];
    ka = #["Indices"] & /@ a; kb = #["Indices"] & /@ b;
    <|"PathSearch" -> Length[a], "FindIsomorphicSubgraph" -> Length[b],
      "OnlyPathSearch" -> Complement[ka, kb], "OnlyIsomorphic" -> Complement[kb, ka]|>];

(* ---- table ------------------------------------------------------------------------------------------------------------------------------ *)

Options[harmonicMatchTable] = {"DateFormat" -> {"Year", "-", "Month"}};

harmonicMatchTable[fs_, matches_List, OptionsPattern[]] :=
  Module[{dates = Normal[fs["Candlesticks"]][[All, "Date"]], fmt = OptionValue["DateFormat"], dateOf},
    (* an index that is not a candle (a plateau, like 199/2) has no date *)
    dateOf[i_] := If[IntegerQ[i] && 1 <= i <= Length[dates], DateString[dates[[i]], fmt], "?"];
    Dataset[Map[
      Join[
        <|"Dates" -> StringRiffle[dateOf /@ #["Indices"], " > "],
          "Indices" -> StringRiffle[ToString[#, InputForm] & /@ #["Indices"], " > "],
          "Pattern" -> #["Pattern"], "Direction" -> #["Direction"], "Violation" -> Round[#["Violation"], 0.0001]|>,
        Round[#["Measures"], 0.001]] &,
      matches]]];

(* ---- display ---------------------------------------------------------------------------------------------------------------------------- *)

Options[highlightHarmonicPattern] = {"ImageSize" -> 800, "VertexLabels" -> None};

highlightHarmonicPattern[fs_, matchesArg_, OptionsPattern[]] :=
  Module[{matches = If[AssociationQ[matchesArg], {matchesArg}, matchesArg], g, colors},
    g = fs["Graph"];
    If[Head[g] === Dataset, g = Normal[g]];
    colors = ColorData[97] /@ Range[Max[1, Length[matches]]];
    HighlightGraph[g,
      MapIndexed[
        Style[Graph[DirectedEdge @@@ #1["Edges"]], colors[[#2[[1]]]], Thick] &, matches],
      ImageSize -> OptionValue["ImageSize"], VertexLabels -> OptionValue["VertexLabels"]]
  ];

Options[drawHarmonicPattern] = {"Margin" -> 2, "Ticker" -> "", "Period" -> Automatic, "ShowDates" -> True,
  "VertexShapeFunction" -> "Glyph", "ShapeSize" -> 0.2, "GlyphWidth" -> 0.35,
  "ArrowSize" -> 0.005, "EdgeThickness" -> 0.001, "ImageSize" -> 800, "AspectRatio" -> 0.5};

(* the period of a feature set, from the median gap between its candles *)
inferPeriod[dates_List] :=
  Module[{gap = Median[Differences[AbsoluteTime /@ Take[dates, UpTo[200]]]]/86400.},
    Which[gap >= 300, "yearly", gap >= 25, "monthly", gap >= 6, "weekly", True, "daily"]];

periodDateFormat = <|"yearly" -> {"Year"}, "monthly" -> {"Year", "-", "Month"},
  "weekly" -> {"Year", "-", "Month", "-", "Day"}, "daily" -> {"Year", "-", "Month", "-", "Day"}|>;

drawHarmonicPattern[fs_, match_Association, OptionsPattern[]] :=
  Module[{idx = match["Indices"], pr = match["Prices"], m = OptionValue["Margin"], labels, dates, period, fmt, dateOf, ticker = OptionValue["Ticker"], title},
    dates  = Normal[fs["Candlesticks"]][[All, "Date"]];
    period = Replace[OptionValue["Period"], Automatic :> inferPeriod[dates]];
    fmt    = Lookup[periodDateFormat, period, {"Year", "-", "Month", "-", "Day"}];
    (* an index that is not a candle (a plateau, like 199/2) has no date *)
    dateOf[i_] := If[IntegerQ[i] && 1 <= i <= Length[dates], DateString[dates[[i]], fmt], "?"];
    (* label above a peak, below a valley: a point is a peak when it lies above the point before it (the first: above the second);
       the label is the point name and, under it, the date of its candle *)
    labels = Table[
      {idx[[i]], pr[[i]],
       If[TrueQ[OptionValue["ShowDates"]], match["Points"][[i]] <> "
" <> dateOf[idx[[i]]], match["Points"][[i]]],
       If[If[i == 1, pr[[1]] > pr[[2]], pr[[i]] > pr[[i - 1]]], {0, -1.5}, {0, 1.5}]},
      {i, Length[idx]}];
    title = StringRiffle[
      DeleteCases[{ticker, "(" <> period <> ")", match["Pattern"], match["Direction"],
        "violation " <> ToString[Round[match["Violation"], 0.001]]}, ""], "  "];
    StockAnalyzer`Visualization`candlestickVertexGraph[fs, match["Edges"],
      "FromIndex" -> Max[1, Floor[Min[idx]] - m],
      "ToIndex" -> Ceiling[Max[idx]] + m,
      "VertexShapeFunction" -> OptionValue["VertexShapeFunction"],
      "ShapeSize" -> OptionValue["ShapeSize"],
      "GlyphWidth" -> OptionValue["GlyphWidth"],
      "ArrowSize" -> OptionValue["ArrowSize"],
      "EdgeThickness" -> OptionValue["EdgeThickness"],
      "PointLabels" -> labels,
      "ImageSize" -> OptionValue["ImageSize"],
      "AspectRatio" -> OptionValue["AspectRatio"],
      PlotLabel -> Style[title, Bold, 16]]
  ];

End[];
EndPackage[];
