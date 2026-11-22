(* Tests of Kernel/Engine.wl: loadEngine, analyzeCandlesticks and the tables of a feature set.
   Run: TestReport["<folder of the package>/Tests/Engine.wlt"]. The file loads the package itself if it is not loaded. *)
If[Names["TechnicalAnalysisPatternViewer`Engine`analyzeCandlesticks"] === {},
  Get[FileNameJoin[{ParentDirectory[DirectoryName[$TestFileName]], "Kernel", "init.m"}]]];

monthly = TechnicalAnalysisPatternViewer`Data`sampleCandlesticks["monthly"];
featureSet = TechnicalAnalysisPatternViewer`Engine`analyzeCandlesticks[monthly];

VerificationTest[
  TechnicalAnalysisPatternViewer`Engine`loadEngine[], True, TestID -> "the analysis code loads"]

VerificationTest[
  Head[featureSet], Dataset, TestID -> "analyzeCandlesticks gives a Dataset"]

VerificationTest[
  SubsetQ[Normal[Keys[featureSet]], {"Candlesticks", "Peaks", "Valleys", "Segments", "ValidTrendlines", "Patterns", "Graph"}], True, TestID -> "the keys of the feature set"]

VerificationTest[
  Length[Normal[featureSet["Candlesticks"]]], 58, TestID -> "the feature set holds the 58 monthly candlesticks"]

(* the tables *)
VerificationTest[
  With[{lines = Normal[TechnicalAnalysisPatternViewer`Engine`supportResistanceTable[featureSet]]},
    {Length[lines] > 0, Union[lines[[All, "Kind"]]], SubsetQ[Union[lines[[All, "Direction"]]], {"ResU"}] || SubsetQ[Union[lines[[All, "Direction"]]], {"SupU"}]}],
  {True, {"Res", "Sup"}, True}, TestID -> "support and resistance lines were found, of both kinds"]

VerificationTest[
  With[{segments = Normal[TechnicalAnalysisPatternViewer`Engine`segmentTable[featureSet]]},
    {Length[segments] > 0, Union[segments[[All, "Direction"]]], FreeQ[Union[Lookup[segments, "Kind", None]], "Res" | "Sup"]}],
  {True, {"D", "U"}, True}, TestID -> "segments are up or down; none is a support or resistance line"]

VerificationTest[
  With[{patterns = Normal[TechnicalAnalysisPatternViewer`Engine`patternTable[featureSet]]},
    {Length[patterns] > 0, SubsetQ[TechnicalAnalysisPatternViewer`Engine`patternNames[], Union[patterns[[All, "PatternName"]]]], MemberQ[Keys[First[patterns]], "ToIndex"]}],
  {True, True, True}, TestID -> "patterns were found and their names are among the 13"]

VerificationTest[
  Length[TechnicalAnalysisPatternViewer`Engine`patternNames[]], 13, TestID -> "13 pattern names"]

(* a pattern is valid at the candlestick it is reported for: both of its lines exist in the line table at that candlestick *)
VerificationTest[
  With[{lines = Normal[TechnicalAnalysisPatternViewer`Engine`supportResistanceTable[featureSet]], patterns = Normal[TechnicalAnalysisPatternViewer`Engine`patternTable[featureSet]]},
    AllTrue[patterns, Function[p, AnyTrue[lines, #["Kind"] === "Res" && #["ToIndex"] == p["ToIndex"] &] && AnyTrue[lines, #["Kind"] === "Sup" && #["ToIndex"] == p["ToIndex"] &]]]],
  True, TestID -> "every pattern has a resistance and a support at its candlestick"]

(* the options *)
VerificationTest[
  Length[Normal[TechnicalAnalysisPatternViewer`Engine`supportResistanceTable[TechnicalAnalysisPatternViewer`Engine`analyzeCandlesticks[monthly, "SupportResistance" -> False]]]], 0, TestID -> "SupportResistance -> False finds no lines"]

VerificationTest[
  Length[Normal[TechnicalAnalysisPatternViewer`Engine`supportResistanceTable[
    TechnicalAnalysisPatternViewer`Engine`analyzeCandlesticks[monthly, "SupportResistance" -> {"TolerancePercent" -> 5, "MinTouches" -> 3, "PatternBand" -> 0.15}]]]] <=
    Length[Normal[TechnicalAnalysisPatternViewer`Engine`supportResistanceTable[featureSet]]], True, TestID -> "a larger MinTouches finds no more lines"]

(* errors *)
VerificationTest[
  TechnicalAnalysisPatternViewer`Engine`analyzeCandlesticks[Take[monthly, 3]], $Failed, {TechnicalAnalysisPatternViewer`Engine`analyzeCandlesticks::few}, TestID -> "fewer than 5 candlesticks"]

VerificationTest[
  TechnicalAnalysisPatternViewer`Engine`analyzeCandlesticks[42], $Failed,
  {TechnicalAnalysisPatternViewer`Data`loadCandlesticks::source, TechnicalAnalysisPatternViewer`Engine`analyzeCandlesticks::input}, TestID -> "something that is not candlesticks"]
