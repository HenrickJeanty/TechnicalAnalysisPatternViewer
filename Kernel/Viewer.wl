(* ::Package:: *)

(* ====================================================================== *)
(* Viewer.wl                                                               *)
(* Technical Analysis Pattern Viewer: the interactive viewer               *)
(* ====================================================================== *)
(*
  WHAT THIS FILE DOES
    It shows the candlesticks of a stock with the tables of the support lines, the resistance lines and the patterns that were found, and lets
    you choose which lines and patterns to draw on the chart. It is made of PANELS, and every panel is independent of the others.

  FUNCTIONS (each has a full usage message: ?showPatternViewer, ...)
    showPatternViewer[candles, title, options]     one panel for a table of candlesticks.
    showPatternViewers[levels, options]            one panel for each level of mergeCandlestickLevels (the same stock at several scales).
    patternViewerFeatureSet[tag]                   the feature set that a panel computed (see analyzeCandlesticks).

  HOW A PANEL IS MADE (so that you can change it)
    1. analyzeCandlesticks (Engine.wl) finds the lines and the patterns: the feature set.
    2. The chart and its tables are drawn by displayCandlestickGraphWithSegmentPicker of vendor/Visualization.wl.
    3. That code keeps its click state (which segment was clicked, which lines are shown, ...) in symbols of its own package, so two panels would share
       it. To keep the panels independent every panel loads its OWN COPY of vendor/Visualization.wl, read as text with the name of its context changed
       (StockAnalyzer`Visualization` becomes TAPVPanel<tag>`Visualization`).
    4. The copy is also changed in a few places (the PATCHES below): narrower tables, a chart that starts without lines, info lines as wide as the
       chart, no regime bars. Each group of patches is applied only if every piece of code it looks for is found exactly once; otherwise the group
       is skipped with a message, so a changed vendor file can never give a half-changed table.
    5. The display is rearranged (topTablesLayout): the resistance and support windows on one line above the chart, the segments table at the right.

  CONTEXT: TechnicalAnalysisPatternViewer`Viewer`
*)

BeginPackage["TechnicalAnalysisPatternViewer`Viewer`"];

(* ====================================================================== *)
(* PUBLIC FUNCTIONS AND THEIR USAGE MESSAGES                               *)
(* ====================================================================== *)

showPatternViewer::usage =
  "showPatternViewer[candles] or showPatternViewer[candles, title] gives an interactive panel for the candlesticks candles (anything that loadCandlesticks accepts): " <>
  "the support lines, the resistance lines and the patterns that were found, in tables, and the chart on which you choose what to draw.\n" <>
  "THE PANEL has, from the top: the title; the Sigma field and the Load button (change the sigma and press Load to find peaks and valleys again) and a status line; " <>
  "then the support / resistance part with its buttons and the field \"Alive at candle\" (the tables list the lines and patterns that are valid at that candlestick; the default is the last one; " <>
  "\"Last\" and \"All\" are shortcuts); then ONE LINE of scrollable windows (Patterns, Resistances, Supports) above the chart; then the chart, with the Segments table at its right " <>
  "(and, with \"TableColumns\" -> \"All\", the Patterns window under it).\n" <>
  "USING IT: the chart starts without lines. Press Show in a row of a table to draw that line (or pattern) on the chart, with its dotted extension and the candlesticks that touch it; " <>
  "press Hide to remove it. \"Select all\" draws every listed line, \"Clear Support/Resistance\" removes them all. Move the mouse over the chart to read the date, " <>
  "the prices and the volume of a candlestick in the line above it; Show in the Segments table draws a segment.\n" <>
  "Options (every option is given with its default):\n" <>
  "  \"Sigma\" -> 0 (peaks and valleys, see analyzeCandlesticks), \"StrictRanges\" -> True, \"HarmonicPatterns\" -> False (True, a name, a list of names or rules: adds the harmonic patterns panel),\n" <>
  "  \"SupportResistance\" -> True (False, or a list of rules {\"TolerancePercent\" -> 5, \"MinTouches\" -> 1, \"PatternBand\" -> 0.15}, see analyzeCandlesticks),\n" <>
  "  \"AutoLoad\" -> Automatic (False: show the panel without computing, press Load to compute),\n" <>
  "  \"TopTables\" -> True (the resistances and supports windows on a line above the chart; False: all the windows at the right, under the segments),\n" <>
  "  \"TableColumns\" -> \"Compact\" (narrow tables, and the Patterns window on the same line as the Resistances and Supports windows; \"All\": every column, and the Patterns window under the segments table),\n" <>
  "  \"HarmonicInRow\" -> False (True: the harmonic patterns panel is a window of the line above the chart; False: at the right, under the patterns),\n" <>
  "  \"LinesAtStart\" -> False (False: the chart starts without support and resistance lines; True: the lines valid at the last candlestick are drawn at the start),\n" <>
  "  \"RegimeBars\" -> False (the line above the chart can show three regime bars read from a global table $regimeHistory; they are left out because they would belong to another series),\n" <>
  "  \"EdgeThickness\" -> 0.002 and \"ArrowSize\" -> 0.005 (the thickness of the segments drawn on the chart and the size of their arrowheads), \"GraphChart\" -> False (True or a list of options: also build the candlesticks-as-graph picture),\n" <>
  "  \"Tag\" -> Automatic (the name of this panel for patternViewerFeatureSet).\n" <>
  "Example: showPatternViewer[sampleCandlesticks[\"weekly\"], \"sample, weekly\"].";

showPatternViewers::usage =
  "showPatternViewers[levels] gives a column of independent panels, one for each level of levels (the result of mergeCandlestickLevels, see mergeCandlesticks), " <>
  "each made by showPatternViewer: the same stock seen at several scales. Each panel is titled with its level, its grouping size (how many original candlesticks make one candlestick of the level) " <>
  "and its number of candlesticks. The panels do not share anything: pressing Show in one does not change another.\n" <>
  "Options: \"Levels\" -> All (or a list of level numbers, for example {2, 3}), \"MergeSize\" -> Automatic (the n of mergeCandlestickLevels, inferred from the levels, for the grouping size in the titles), " <>
  "and every option of showPatternViewer.\n" <>
  "Example: levels = mergeCandlestickLevels[sampleCandlesticks[\"weekly\"], 2]; showPatternViewers[levels, \"Levels\" -> {2, 3}].";

patternViewerFeatureSet::usage =
  "patternViewerFeatureSet[tag] gives the feature set that the panel with this tag computed (see analyzeCandlesticks), so you can read its tables: supportResistanceTable[patternViewerFeatureSet[tag]]. " <>
  "The tag is the option \"Tag\" of showPatternViewer; showPatternViewers gives the panels the tags \"level<k>-<number>\". patternViewerFeatureSet[] gives an association of all the tags and their feature sets.";

Begin["`Private`"];

(* ====================================================================== *)
(* MESSAGES                                                                *)
(* ====================================================================== *)

showPatternViewer::patch = "The code in vendor/Visualization.wl does not contain the expected piece `1`...; `2` is not applied.";
showPatternViewer::few = "A panel needs at least 5 candlesticks to find lines and patterns; `1` has `2`.";
showPatternViewer::engine = "The analysis code could not be loaded (see loadEngine).";
showPatternViewer::input = "The candlesticks could not be read; see loadCandlesticks for the accepted forms.";

(* ====================================================================== *)
(* STATE KEPT BY THE VIEWER                                                *)
(* ====================================================================== *)

(* the vendor file that every panel copies; this file is in Kernel/ *)
$visualizationFile = FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]], "vendor", "Visualization.wl"}];
$temporaryFolder = FileNameJoin[{$TemporaryDirectory, "TechnicalAnalysisPatternViewer"}];

$visualizationContexts = <||>;   (* panel tag -> the context of its copy of Visualization.wl *)
$featureSets = <||>;             (* panel tag -> its feature set *)
$panelCounter = 0;               (* makes the tags of automatically named panels *)

patternViewerFeatureSet[tag_String] := Lookup[$featureSets, tag, Missing["NoSuchPanel", tag]];
patternViewerFeatureSet[] := $featureSets;

(* ====================================================================== *)
(* A PRIVATE COPY OF vendor/Visualization.wl FOR EVERY PANEL               *)
(* ====================================================================== *)

(* The copy is the same file with its context renamed, so every symbol of it (lastSegmentClicked, shownTrendlines, the hover state, ...) is its own.
   It is loaded once for each tag; the other vendor packages (Analysis, Fibonacci, ...) are shared: they keep no click state.
   The arguments say which groups of patches to apply (see the patches below). *)
visualizationContext[tag_String, tableColumns_String, linesAtStart_, regimeBars_] :=
  (* a context name has letters and digits only: "level4-1" becomes "level41" *)
  Module[{safeTag = StringReplace[tag, Except[LetterCharacter | DigitCharacter] -> ""], context, text, file},
    context = "TAPVPanel" <> safeTag <> "`Visualization`";
    If[! KeyExistsQ[$visualizationContexts, tag],
      text = Import[$visualizationFile, "Text", CharacterEncoding -> "UTF-8"];
      text = StringReplace[text, "StockAnalyzer`Visualization`" -> context];
      If[tableColumns === "Compact", text = compactColumns[text]];
      If[! TrueQ[linesAtStart], text = noStartingLines[text]];
      text = fitInfoBars[text];
      If[! TrueQ[regimeBars], text = noRegimeBars[text]];
      If[! DirectoryQ[$temporaryFolder], CreateDirectory[$temporaryFolder]];
      file = FileNameJoin[{$temporaryFolder, "Visualization_" <> safeTag <> ".wl"}];
      Export[file, text, "Text", CharacterEncoding -> "UTF-8"];
      Quiet[Get[file]];
      $visualizationContexts[tag] = context];
    $visualizationContexts[tag]];

(* ====================================================================== *)
(* THE PATCHES OF THE COPY                                                 *)
(* ====================================================================== *)

(* a regular expression for a piece of code: every special character literal, and any white space (line breaks, tabs, none) allowed between any two characters,
   so that the piece is found however the code is laid out *)
looseExpression[literal_String] :=
  RegularExpression[StringJoin[
    (* MemberQ, not StringMatchQ: in a string pattern * is a wildcard *)
    Map[If[MemberQ[Characters["[]{}().*+?|^$\\"], #], "\\" <> #, #] <> "\\s*" &,
      Characters[StringReplace[literal, Whitespace -> ""]]]]];

(* applies the patches {piece of code, replacement} to the code: every piece must be found exactly once, otherwise the code is not the one the patches were
   written for, and then none is applied (a header patched without its rows would shift the columns); what names the group in the message *)
applyPatches[text_String, patches_List, what_String] :=
  Module[{failed = Select[patches, StringCount[text, looseExpression[#[[1]]]] =!= 1 &], result = text},
    If[failed =!= {},
      Message[showPatternViewer::patch, StringTake[failed[[1, 1]], UpTo[40]], what];
      Return[text]];
    Do[result = StringReplace[result, looseExpression[patch[[1]]] -> patch[[2]]], {patch, patches}];
    result];

(* The tables of the resistances, the supports and the patterns have 12, 12 and 10 columns: three of them side by side are about 2100 pixels wide. With "Compact"
   the copy lists fewer columns: the resistances and supports without Start, Anchor, FromPrice, Slope and LogSlope, the patterns without GapRatio and Apex,
   the harmonic patterns without the ratios BofXA, CofAB and DofXA. *)
compactColumns[text_String] :=
  applyPatches[text, {
    (* the resistance and support tables: 7 columns, so that the windows side by side fit *)
    {"{\"Pick\", \"Dir\", \"Start\", \"From\", \"Anchor\", \"To\", \"FromPrice\", \"ToPrice\", \"Slope\", \"LogSlope\", \"Touches\", \"Goodness\"}",
     "{\"Pick\", \"Dir\", \"From\", \"To\", \"ToPrice\", \"Touches\", \"Goodness\"}"},
    {"Style[NumberForm[seg[\"FromPrice\"], {7, 2}], White], Style[NumberForm[seg[\"ToPrice\"], {7, 2}], White], Style[NumberForm[Lookup[seg, \"Slope\", 0], {6, 3}], White],",
     "Style[NumberForm[seg[\"ToPrice\"], {7, 2}], White],"},
    {"Style[Lookup[seg, \"StartIndex\", \"-\"], White],", ""},
    {"Style[seg[\"AnchorIndex\"], White],", ""},
    {"Style[NumberForm[Lookup[seg, \"LogSlope\", 0], {6, 4}], White],", ""},
    (* the pattern table: 8 columns *)
    {"{\"Pick\", \"Pattern\", \"R/S\", \"Start\", \"To\", \"Width\", \"GapRatio\", \"Apex\", \"R touches\", \"S touches\"}",
     "{\"Pick\", \"Pattern\", \"R/S\", \"Start\", \"To\", \"Width\", \"R touches\", \"S touches\"}"},
    {"Style[NumberForm[row[\"GapRatio\"], {4, 2}], White],", ""},
    {"Style[If[NumericQ[row[\"Apex\"]], NumberForm[row[\"Apex\"], {6, 1}], \"-\"], White],", ""},
    (* the harmonic patterns table: without the ratios BofXA, CofAB and DofXA *)
    {"{\"Pick\", \"Pattern\", \"Dir\", \"Born\", \"X\", \"A\", \"B\", \"C\", \"D\", \"Viol\", \"BofXA\", \"CofAB\", \"DofXA\"}",
     "{\"Pick\", \"Pattern\", \"Dir\", \"Born\", \"X\", \"A\", \"B\", \"C\", \"D\", \"Viol\"}"},
    {"Style[NumberForm[row[\"Violation\"], {4, 3}], White], Style[fmt[Lookup[row, \"BofXA\", Missing[]]], White], Style[fmt[Lookup[row, \"CofAB\", Missing[]]], White], Style[fmt[Lookup[row, \"DofXA\", Missing[]]], White]",
     "Style[NumberForm[row[\"Violation\"], {4, 3}], White]"}}, "the compact columns"];

(* The two lines above the chart (the date, open, high, low, close and volume; the regime) are as wide as "Full" in the original, which is wider than the chart image,
   with blank space at the right. Here they are as wide as the chart image. *)
fitInfoBars[text_String] :=
  applyPatches[text, {
    {"ImageSize -> {Full, 35}", "ImageSize -> {If[ListQ[imageSize], First[imageSize], imageSize], 35}"},
    {"ImageSize -> {Full, 22}", "ImageSize -> {If[ListQ[imageSize], First[imageSize], imageSize], 22}"}}, "the width of the two lines above the chart"];

(* The first line also draws three regime bars (Hist, Now, 5d), read from Global`$regimeHistory by the number of the candlestick: that is the history of another series
   (and makes the line more than 1000 pixels wide), not of your candlesticks. They are left out. *)
noRegimeBars[text_String] :=
  applyPatches[text, {
    {"regimeData = If[AssociationQ[Global`$regimeHistory] && KeyExistsQ[Global`$regimeHistory, row[\"Index\"]], Global`$regimeHistory[row[\"Index\"]], None];", "regimeData = None;"}}, "the removal of the regime bars"];

(* When the chart is built, every support and resistance line that is valid at the last candlestick is put on the chart (shownTrendlines). Here the chart starts
   without lines: you choose the ones to show with the Show buttons of the tables. *)
noStartingLines[text_String] :=
  applyPatches[text, {
    {"shownTrendlines = Association[shownTrendlineEntry[#, storeKey] & /@ Select[srRows, #[\"ToIndex\"] == lastIndex &]]", "shownTrendlines = <||>"}}, "the chart without lines at the start"];

(* ====================================================================== *)
(* THE LAYOUT OF THE DISPLAY                                               *)
(* ====================================================================== *)

(* The display of the original code is a grid: the chart on the left and, in a column on the right, the segments table, the support / resistance panel (with the patterns,
   the resistances and the supports one under the other) and the harmonic patterns. Here the display is rebuilt inside the support / resistance panel (one DynamicModule, so
   that the field "Alive at candle" and the buttons work for all its tables):
     the title and the buttons, then one line of windows (the patterns when patternsInRow, the resistances and the supports, the harmonic patterns when harmonicInRow),
     then the chart with, at its right, the segments table (and under it the patterns when they are not in the line, and the harmonic patterns). *)
(* dm stands for DynamicModule while the pieces are matched and rebuilt: it holds all its arguments, so that the variable specifications such as {at = 25} are not evaluated
   (evaluating the assignment would leave the specification {25} and the module would lose its variable) *)
SetAttributes[dm, HoldAll];

topTablesLayout[display_, harmonicInRow_, patternsInRow_] :=
  Replace[display /. DynamicModule -> dm,
    Grid[{{chart_, Column[{segmentsTitle_, segmentsTable_, rest___}, columnOptions___]}}, gridOptions___] :>
      Module[{srPanel, harmonic, others},
        srPanel = FirstCase[{rest}, dm[_, Column[{Style[s_String /; StringStartsQ[s, "Support / Resistance"], ___], __}, ___], ___], None];
        harmonic = FirstCase[{rest}, dm[_, Column[{Style[s_String /; StringStartsQ[s, "Harmonic patterns"], ___], __}, ___], ___], None];
        If[srPanel === None,
          (* no support and resistance lines: the display is not changed *)
          display,
          (* the other items of the right column; SameQ, not DeleteCases: the panels contain patterns (t_, r_ in their Cases), which DeleteCases would use as patterns *)
          others = Select[{rest}, ! SameQ[#, srPanel] && ! (TrueQ[harmonicInRow] && SameQ[#, harmonic]) &];
          srPanel /. dm[vars_, Column[{title_, controls_, items__}, columnOpts___], dmOpts___] :>
            Module[{pairs = Partition[{items}, 2], patternsPairs, windowPairs, column, windows, topRow, rightColumn},
              column = Column[{#1, #2}, Alignment -> Left] &;
              patternsPairs = Select[pairs, MatchQ[#[[1]], Style["Patterns", ___]] &];
              windowPairs = Select[pairs, ! MatchQ[#[[1]], Style["Patterns", ___]] &];
              (* the line of windows: the patterns first when they fit on the line (patternsInRow), then the resistances and the supports, then the harmonic patterns if asked *)
              windows = Join[If[TrueQ[patternsInRow], column @@@ patternsPairs, {}], column @@@ windowPairs, If[TrueQ[harmonicInRow] && harmonic =!= None, {harmonic}, {}]];
              topRow = Row[Riffle[windows, Spacer[12]], Alignment -> Top];
              (* the right column: the segments, then the patterns when they are not in the line, then the harmonic patterns *)
              rightColumn = Column[Join[{segmentsTitle, segmentsTable}, If[TrueQ[patternsInRow], {}, column @@@ patternsPairs], others], columnOptions];
              (* the new body is built (evaluated) first, outside dm, then put into the module with its own variables *)
              With[{newBody = Column[{title, controls, topRow, Grid[{{chart, rightColumn}}, gridOptions]}, columnOpts]},
                dm[vars, newBody, dmOpts]]]]]] /. dm -> DynamicModule;

(* ====================================================================== *)
(* THE DISPLAY OF A FEATURE SET                                            *)
(* ====================================================================== *)

(* the chart with the segment picker and the tables, from the private copy of Visualization.wl, rearranged by topTablesLayout *)
buildDisplay[featureSet_, context_String, edgeThickness_, arrowSize_, topTables_, harmonicInRow_, patternsInRow_] :=
  (If[TrueQ[topTables], topTablesLayout[#, harmonicInRow, patternsInRow], #] &)[Symbol[context <> "displayCandlestickGraphWithSegmentPicker"][
    featureSet, "default",
    GlyphWidth -> 0.35,
    XAxis -> "Both",
    EdgeThickness -> edgeThickness,
    Lines -> {},
    Background -> Black,
    UpColor -> RGBColor[0.0, 0.85, 0.0],
    DownColor -> RGBColor[0.85, 0.0, 0.0],
    ArrowSize -> arrowSize,
    ShowPriceSlider -> False,
    ShowGraphEx -> False,
    ShowScrollbar -> False,
    ScalingFunctions -> {"Linear", "Linear"},
    ImageSize -> {700, 350},
    "TouchTolerance" -> 0.01,
    "TouchMarkerSize" -> 4,
    "ExtraOnLeft" -> 1,
    "ExtraOnRight" -> 8]];

(* ====================================================================== *)
(* showPatternViewer                                                       *)
(* ====================================================================== *)

Options[showPatternViewer] = {"Sigma" -> 0, "AutoLoad" -> Automatic, "StrictRanges" -> True, "GraphChart" -> False, "SupportResistance" -> True,
  "EdgeThickness" -> 0.002, "ArrowSize" -> 0.005, "HarmonicPatterns" -> False, "TopTables" -> True, "HarmonicInRow" -> False, "TableColumns" -> "Compact",
  "LinesAtStart" -> False, "RegimeBars" -> False, "Tag" -> Automatic};

showPatternViewer[candles_, title_String : "", OptionsPattern[]] :=
  Module[{table, rows, tag, context, displayName, displaySymbol},
    If[! TechnicalAnalysisPatternViewer`Engine`loadEngine[], Message[showPatternViewer::engine]; Return[$Failed]];
    table = TechnicalAnalysisPatternViewer`Data`loadCandlesticks[candles];
    If[table === $Failed, Message[showPatternViewer::input]; Return[$Failed]];
    rows = Association /@ Normal[table];
    tag = If[OptionValue["Tag"] === Automatic, "P" <> ToString[++$panelCounter], ToString[OptionValue["Tag"]]];
    (* the private copy of the Visualization code of this panel *)
    context = visualizationContext[tag, OptionValue["TableColumns"], OptionValue["LinesAtStart"], OptionValue["RegimeBars"]];
    (* the display (several MB) is kept in a symbol of the kernel, one for each panel, not in the notebook: the cell stays small and the notebook file does not grow *)
    displayName = "TechnicalAnalysisPatternViewer`Viewer`Private`display" <> StringReplace[tag, Except[LetterCharacter | DigitCharacter] -> ""];
    (* a new panel starts with no display: without the Clear, a symbol that still holds an old display would evaluate to that value, not to itself *)
    Clear[Evaluate[displayName]];
    displaySymbol = Symbol[displayName];
    With[{rows = rows, tag = tag, context = context, displaySymbol = displaySymbol, panelTitle = title, sigma0 = OptionValue["Sigma"], strict = TrueQ[OptionValue["StrictRanges"]],
        chart = OptionValue["GraphChart"], sr = OptionValue["SupportResistance"], harmonic = OptionValue["HarmonicPatterns"],
        edgeThickness = OptionValue["EdgeThickness"], arrowSize = OptionValue["ArrowSize"], autoLoad = OptionValue["AutoLoad"],
        topTables = TrueQ[OptionValue["TopTables"]], harmonicInRow = TrueQ[OptionValue["HarmonicInRow"]], patternsInRow = (OptionValue["TableColumns"] === "Compact")},
      DynamicModule[{uiSigma = sigma0, status = Style["Ready: adjust the sigma, then press Load.", FontFamily -> "Helvetica", FontSize -> 11], loadPanel},
        (* finds the lines and patterns with the sigma of the panel and draws the display; the state is the module's own *)
        loadPanel[] := Module[{dt, featureSet, nSegments},
          If[Length[rows] < 5,
            status = Style[Row[{"A panel needs at least 5 candlesticks; ", panelTitle, " has ", Length[rows], "."}], Red, FontFamily -> "Helvetica", FontSize -> 11],
            status = Style[Row[{"Loading ", panelTitle, "\[Ellipsis]"}], Blue, FontFamily -> "Helvetica", FontSize -> 11];
            dt = First[AbsoluteTiming[
              featureSet = TechnicalAnalysisPatternViewer`Engine`analyzeCandlesticks[rows, "Sigma" -> uiSigma, "StrictRanges" -> strict, "GraphChart" -> chart,
                "SupportResistance" -> sr, "HarmonicPatterns" -> harmonic];
              $featureSets[tag] = featureSet;
              displaySymbol = buildDisplay[featureSet, context, edgeThickness, arrowSize, topTables, harmonicInRow, patternsInRow]]];
            nSegments = Quiet[Check[Count[Normal[featureSet["EdgeSegments"]], seg_ /; MemberQ[{"MaxU", "MaxD"}, seg["EdgeType"]]], 0]];
            status = Style[Row[{"Loaded ", panelTitle, " \[Bullet] ", Length[rows], " candles", " \[Bullet] ", nSegments, " MaxU/MaxD segs", " \[Bullet] ", Round[dt, 0.01], " s"}],
              Darker[Green], FontFamily -> "Helvetica", FontSize -> 11]]];
        If[autoLoad =!= False, loadPanel[]];
        Panel[
          Column[{
            Row[{
              Column[{Style["Sigma", Bold, 10], InputField[Dynamic[uiSigma], Number, FieldSize -> 5, BaseStyle -> {FontSize -> 10}]}, Alignment -> Left, Spacings -> 0.2],
              Spacer[20],
              Column[{Spacer[{Automatic, 12}], Button[Style["Load", Bold, 12], loadPanel[], ImageSize -> {100, 26}, Method -> "Queued"]}, Alignment -> Left, Spacings -> 0.2]
              }, " "],
            Dynamic[status, TrackedSymbols :> {status}],
            Dynamic[If[ValueQ[displaySymbol], displaySymbol, Style["(Press Load to see the chart)", Italic, Gray]], TrackedSymbols :> {displaySymbol}]
            }, Spacings -> 0.6],
          Style[panelTitle, Bold, 11]]]]];

(* ====================================================================== *)
(* showPatternViewers                                                      *)
(* ====================================================================== *)

(* the title of a level: its number, the grouping size (how many original candlesticks make one candlestick of the level) and the number of candlesticks *)
levelTitle[k_Integer, group_, count_Integer] :=
  "level " <> ToString[k] <> ", " <> If[IntegerQ[group], "grouping " <> ToString[group] <> ", ", ""] <> ToString[count] <> " candlesticks";

Options[showPatternViewers] = Join[{"Levels" -> All, "MergeSize" -> Automatic}, Options[showPatternViewer]];

showPatternViewers[levels_List, opts : OptionsPattern[]] :=
  Module[{numbers = If[OptionValue["Levels"] === All, Range[Length[levels]], OptionValue["Levels"]], panelOptions = FilterRules[{opts}, Options[showPatternViewer]],
      groupSizes = TechnicalAnalysisPatternViewer`Candlesticks`mergeGroupSizes[levels, OptionValue["MergeSize"]]},
    Column[
      Table[
        showPatternViewer[levels[[k]], levelTitle[k, groupSizes[[k]], Length[levels[[k]]]],
          Sequence @@ panelOptions, "Tag" -> "level" <> ToString[k] <> "-" <> ToString[++$panelCounter]],
        {k, numbers}],
      Spacings -> 2]];

End[];

EndPackage[];
