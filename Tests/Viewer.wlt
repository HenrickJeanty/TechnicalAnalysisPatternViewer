(* Tests of Kernel/Viewer.wl: the panels. The front end draws a panel; these tests look at the expressions the kernel builds, which is where the layout is decided.
   Run: TestReport["<folder of the package>/Tests/Viewer.wlt"]. The file loads the package itself if it is not loaded. *)
If[Names["TechnicalAnalysisPatternViewer`Viewer`showPatternViewer"] === {},
  Get[FileNameJoin[{ParentDirectory[DirectoryName[$TestFileName]], "Kernel", "init.m"}]]];

monthly = TechnicalAnalysisPatternViewer`Data`sampleCandlesticks["monthly"];
viewerPrivate = "TechnicalAnalysisPatternViewer`Viewer`Private`";

(* a panel with every option spelled out; columns and linesAtStart vary *)
makePanel[tag_String, columns_String, linesAtStart_] :=
  TechnicalAnalysisPatternViewer`Viewer`showPatternViewer[monthly, "test " <> tag,
    "Sigma" -> 0, "AutoLoad" -> Automatic, "StrictRanges" -> True, "GraphChart" -> False, "SupportResistance" -> True, "EdgeThickness" -> 0.002, "ArrowSize" -> 0.008,
    "HarmonicPatterns" -> False, "TopTables" -> True, "HarmonicInRow" -> False, "TableColumns" -> columns, "LinesAtStart" -> linesAtStart, "RegimeBars" -> False, "Tag" -> tag];

(* the display of a panel (kept in a symbol of the kernel) *)
displayOf[tag_String] := ToExpression[viewerPrivate <> "display" <> StringReplace[tag, Except[LetterCharacter | DigitCharacter] -> ""]];
(* the titles of the windows found in an expression *)
titlesIn[expr_] := Cases[expr, Style[t_String, ___] /; MemberQ[{"Patterns", "Resistances", "Supports"}, t] || StringStartsQ[t, "Segments"] :> StringTake[t, UpTo[8]], Infinity];
(* the state of the chart of a panel: the lines it shows *)
shownLines[tag_String] := ToExpression["TAPVPanel" <> StringReplace[tag, Except[LetterCharacter | DigitCharacter] -> ""] <> "`Visualization`shownTrendlines"];

panelCompact = makePanel["tcompact", "Compact", False];
panelAll = makePanel["tall", "All", False];
panelLines = makePanel["tlines", "Compact", True];
panelSecond = makePanel["tsecond", "Compact", False];

VerificationTest[
  Head[panelCompact], DynamicModule, TestID -> "a panel is a DynamicModule"]

(* the layout: one line of windows above the chart, the segments table at the right *)
VerificationTest[
  With[{d = displayOf["tcompact"]}, {Head[d], titlesIn[d[[2, 1, 3]]], titlesIn[d[[2, 1, 4, 1, 1, 2]]]}],
  {DynamicModule, {"Patterns", "Resistan", "Supports"}, {"Segments"}}, TestID -> "Compact: Patterns, Resistances and Supports on one line, Segments at the right of the chart"]

VerificationTest[
  With[{d = displayOf["tall"]}, {titlesIn[d[[2, 1, 3]]], titlesIn[d[[2, 1, 4, 1, 1, 2]]]}],
  {{"Resistan", "Supports"}, {"Segments", "Patterns"}}, TestID -> "All: Resistances and Supports on the line, Patterns under the Segments"]

(* the patches *)
VerificationTest[
  {Count[Cases[displayOf["tcompact"], Style[h_String, Bold, 10, White] :> h, Infinity], "Touches"], Count[Cases[displayOf["tall"], Style[h_String, Bold, 10, White] :> h, Infinity], "LogSlope"]},
  {2, 2}, TestID -> "Compact: tables of 7 columns (no LogSlope); All: every column"]

VerificationTest[
  {Length[shownLines["tcompact"]], Length[shownLines["tlines"]] > 0}, {0, True}, TestID -> "LinesAtStart: False starts without lines, True with them"]

VerificationTest[
  Module[{text = Import[FileNameJoin[{$TemporaryDirectory, "TechnicalAnalysisPatternViewer", "Visualization_tcompact.wl"}], "Text", CharacterEncoding -> "UTF-8"]},
    {StringContainsQ[text, "{Full, 35}"], StringContainsQ[text, "regimeData = None;"], StringContainsQ[text, "TAPVPaneltcompact`Visualization`"], StringContainsQ[text, "StockAnalyzer`Visualization`"]}],
  {False, True, True, False}, TestID -> "the copy of the chart code: info lines fitted, regime bars removed, its own context"]

(* independence: two panels do not share their click state *)
VerificationTest[
  (ToExpression["TAPVPaneltsecond`Visualization`lastSegmentClicked = <|\"FromIndex\" -> 1, \"ToIndex\" -> 3|>"];
   {ToExpression["TAPVPaneltsecond`Visualization`lastSegmentClicked"] =!= None, ToExpression["TAPVPaneltcompact`Visualization`lastSegmentClicked"] === None}),
  {True, True}, TestID -> "two panels have their own click state"]

(* the feature sets of the panels *)
VerificationTest[
  {Head[TechnicalAnalysisPatternViewer`Viewer`patternViewerFeatureSet["tcompact"]], Head[TechnicalAnalysisPatternViewer`Viewer`patternViewerFeatureSet["nosuchpanel"]]},
  {Dataset, Missing}, TestID -> "patternViewerFeatureSet"]

(* panels for the levels of mergeCandlestickLevels *)
VerificationTest[
  With[{levels = TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlestickLevels[TechnicalAnalysisPatternViewer`Data`sampleCandlesticks["weekly"], 2]},
    Head[TechnicalAnalysisPatternViewer`Viewer`showPatternViewers[levels, "Levels" -> {5}]]],
  Column, TestID -> "showPatternViewers gives a Column of panels"]

(* a changed vendor file: the patches are applied all or none *)
VerificationTest[
  Module[{text = "no code here"}, TechnicalAnalysisPatternViewer`Viewer`Private`compactColumns[text]], "no code here",
  {TechnicalAnalysisPatternViewer`Viewer`showPatternViewer::patch}, TestID -> "patches are skipped, with a message, when the code is not the expected one"]

(* too few candlesticks: the panel says so instead of failing *)
VerificationTest[
  Head[TechnicalAnalysisPatternViewer`Viewer`showPatternViewer[Take[monthly, 3], "too short", "Tag" -> "ttoo"]], DynamicModule, TestID -> "a panel for 3 candlesticks is made, with a status message"]
