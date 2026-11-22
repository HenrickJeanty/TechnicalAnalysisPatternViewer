(* Tests of Kernel/Data.wl: loadCandlesticks, resampleCandlesticks, sampleCandlesticks.
   Run: TestReport["<folder of the package>/Tests/Data.wlt"]. The file loads the package itself if it is not loaded.
   The test of FinancialData needs an internet connection and is the last one. *)
If[Names["TechnicalAnalysisPatternViewer`Data`loadCandlesticks"] === {},
  Get[FileNameJoin[{ParentDirectory[DirectoryName[$TestFileName]], "Kernel", "init.m"}]]];

(* five daily candlesticks of two weeks and two months, with the keys in two different spellings *)
rowsA = {
  <|"Date" -> "2024-01-30", "Open" -> 10, "High" -> 12, "Low" -> 9, "Close" -> 11, "Volume" -> 100|>,
  <|"Date" -> "2024-01-31", "Open" -> 11, "High" -> 13, "Low" -> 10, "Close" -> 12, "Volume" -> 200|>,
  <|"Date" -> "2024-02-01", "Open" -> 12, "High" -> 15, "Low" -> 11, "Close" -> 14, "Volume" -> 300|>,
  <|"Date" -> "2024-02-02", "Open" -> 14, "High" -> 16, "Low" -> 13, "Close" -> 13, "Volume" -> 400|>,
  <|"Date" -> "2024-02-05", "Open" -> 13, "High" -> 14, "Low" -> 12, "Close" -> 12.5, "Volume" -> 500|>};
rowsLower = KeyMap[ToLowerCase, #] & /@ rowsA;

VerificationTest[
  Head[TechnicalAnalysisPatternViewer`Data`loadCandlesticks[rowsA]], Tabular, TestID -> "a list of associations gives a Tabular"]

VerificationTest[
  Keys[First[Normal[TechnicalAnalysisPatternViewer`Data`loadCandlesticks[rowsA]]]],
  {"Date", "Open", "High", "Low", "Close", "Volume", "Ticker", "Period", "logReturn"}, TestID -> "the standard columns"]

VerificationTest[
  Normal[TechnicalAnalysisPatternViewer`Data`loadCandlesticks[rowsA]] === Normal[TechnicalAnalysisPatternViewer`Data`loadCandlesticks[rowsLower]], True, TestID -> "key names are matched without regard to case"]

VerificationTest[
  Normal[TechnicalAnalysisPatternViewer`Data`loadCandlesticks[Reverse[rowsA]]][[All, "Close"]], {11., 12., 14., 13., 12.5}, TestID -> "the result is in date order"]

VerificationTest[
  Normal[TechnicalAnalysisPatternViewer`Data`loadCandlesticks[rowsA]][[All, "logReturn"]],
  {0., Log[12./11.], Log[14./12.], Log[13./14.], Log[12.5/13.]}, SameTest -> (Max[Abs[#1 - #2]] < 10^-12 &), TestID -> "log returns"]

VerificationTest[
  Head[TechnicalAnalysisPatternViewer`Data`loadCandlesticks[Dataset[rowsA]]], Tabular, TestID -> "a Dataset is accepted"]

(* bad rows are left out with a message that counts them *)
VerificationTest[
  Length[TechnicalAnalysisPatternViewer`Data`loadCandlesticks[Append[rowsA, <|"Date" -> "2024-02-06", "Open" -> "x", "High" -> 1, "Low" -> 1, "Close" -> 1|>]]],
  5, {TechnicalAnalysisPatternViewer`Data`loadCandlesticks::dropped}, TestID -> "a row with a bad price is left out"]

VerificationTest[
  TechnicalAnalysisPatternViewer`Data`loadCandlesticks[{<|"Date" -> "2024-01-30", "Open" -> 1|>}], $Failed,
  {TechnicalAnalysisPatternViewer`Data`loadCandlesticks::columns}, TestID -> "missing columns"]

VerificationTest[
  Length[TechnicalAnalysisPatternViewer`Data`loadCandlesticks[rowsA, "StartDate" -> {2024, 2, 1}, "EndDate" -> {2024, 2, 2}]], 2, TestID -> "the window of dates"]

(* resampling *)
VerificationTest[
  Normal[TechnicalAnalysisPatternViewer`Data`loadCandlesticks[rowsA, "Resample" -> "monthly"]][[All, {"Open", "High", "Low", "Close", "Volume"}]],
  {<|"Open" -> 10., "High" -> 13., "Low" -> 9., "Close" -> 12., "Volume" -> 300.|>, <|"Open" -> 12., "High" -> 16., "Low" -> 11., "Close" -> 12.5, "Volume" -> 1200.|>},
  TestID -> "monthly candlesticks from daily ones"]

VerificationTest[
  Normal[TechnicalAnalysisPatternViewer`Data`resampleCandlesticks[rowsA, "weekly"]][[All, {"Date", "Open", "Close", "Volume"}]],
  {<|"Date" -> DateObject[{2024, 1, 30}, "Day"], "Open" -> 10., "Close" -> 13., "Volume" -> 1000.|>, <|"Date" -> DateObject[{2024, 2, 5}, "Day"], "Open" -> 13., "Close" -> 12.5, "Volume" -> 500.|>},
  TestID -> "weekly candlesticks start on Mondays: Tuesday to Friday, then Monday"]

VerificationTest[
  Normal[TechnicalAnalysisPatternViewer`Data`loadCandlesticks[rowsA, "Resample" -> "monthly"]][[All, "Period"]], {"monthly", "monthly"}, TestID -> "the Period column after resampling"]

VerificationTest[
  TechnicalAnalysisPatternViewer`Data`resampleCandlesticks[rowsA, "hourly"], $Failed, {TechnicalAnalysisPatternViewer`Data`resampleCandlesticks::period}, TestID -> "a bad period"]

(* the sample data *)
VerificationTest[
  {Length[TechnicalAnalysisPatternViewer`Data`sampleCandlesticks[]], Length[TechnicalAnalysisPatternViewer`Data`sampleCandlesticks["yearly"]]}, {1250, 5}, TestID -> "the sample has 1250 daily and 5 yearly candlesticks"]

VerificationTest[
  With[{daily = Normal[TechnicalAnalysisPatternViewer`Data`sampleCandlesticks[]], weekly = Normal[TechnicalAnalysisPatternViewer`Data`sampleCandlesticks["weekly"]]},
    {Total[daily[[All, "Volume"]]] == Total[weekly[[All, "Volume"]]], Max[daily[[All, "High"]]] == Max[weekly[[All, "High"]]], Min[daily[[All, "Low"]]] == Min[weekly[[All, "Low"]]]}],
  {True, True, True}, TestID -> "resampling keeps the volume total and the extremes"]

(* a CSV file *)
VerificationTest[
  Module[{file = FileNameJoin[{$TemporaryDirectory, "tapv_test.csv"}], result},
    Export[file, "Date,Open,High,Low,Close,Volume\n2024-01-02,1,2,0.5,1.5,10\n2024-01-03,1.5,2.5,1,2,20\n", "Text"];
    result = Normal[TechnicalAnalysisPatternViewer`Data`loadCandlesticks[file, "Ticker" -> "TST"]];
    DeleteFile[file];
    {Length[result], result[[1, "Ticker"]], result[[2, "Close"]]}],
  {2, "TST", 2.}, TestID -> "a CSV file"]

(* FinancialData: needs an internet connection *)
VerificationTest[
  With[{t = TechnicalAnalysisPatternViewer`Data`loadCandlesticks["PCG", "StartDate" -> {2024, 12, 1}, "EndDate" -> {2024, 12, 13}]},
    {Head[t], Length[t], Normal[t][[1, "Ticker"]]}],
  {Tabular, 10, "PCG"}, TestID -> "a ticker from FinancialData (internet)"]
