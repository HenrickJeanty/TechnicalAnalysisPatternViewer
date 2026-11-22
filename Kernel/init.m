(* ::Package:: *)

(* ====================================================================== *)
(* init.m: loads the Technical Analysis Pattern Viewer                     *)
(* ====================================================================== *)
(*
  USE
      Get["<folder of the package>/Kernel/init.m"]

  It loads, in this order, the four packages of Kernel/:
      Candlesticks.wl   mergeCandlesticks, mergeCandlestickLevels, mergeGroupSizes, showCandlesticks
      Data.wl           loadCandlesticks, resampleCandlesticks, sampleCandlesticks
      Engine.wl         loadEngine, analyzeCandlesticks, supportResistanceTable, patternTable, segmentTable, patternNames
      Viewer.wl         showPatternViewer, showPatternViewers, patternViewerFeatureSet
  The analysis code of the folder vendor/ is loaded the first time it is needed (by analyzeCandlesticks or by a viewer), not here.

  WHY THE LAST PART OF THIS FILE
    A notebook cell is parsed as a whole before it runs. In a cell that loads this file and then calls its functions, each name is already a symbol of Global`
    when the packages are loaded, and an old notebook-local helper may have definitions under the same name. A Global` symbol comes before the package in the
    context path, so the call would stay unevaluated. After the packages are loaded, every such symbol gets its old definitions cleared and then stands for
    the function of the package.
*)

$tapvKernelDirectory = DirectoryName[$InputFileName];

Get[FileNameJoin[{$tapvKernelDirectory, "Candlesticks.wl"}]];
Get[FileNameJoin[{$tapvKernelDirectory, "Data.wl"}]];
Get[FileNameJoin[{$tapvKernelDirectory, "Engine.wl"}]];
Get[FileNameJoin[{$tapvKernelDirectory, "Viewer.wl"}]];

Scan[
  Function[context,
    Scan[
      Function[listedName,
        (* Names gives the short name when the context is on the context path, the full name otherwise *)
        With[{short = If[StringStartsQ[listedName, context], StringDrop[listedName, StringLength[context]], listedName]},
          If[Names["Global`" <> short] =!= {},
            If[DownValues[Evaluate["Global`" <> short]] =!= {},
              Print["init.m: cleared the old definition Global`", short, " (the function of the package is used)."]];
            ClearAll[Evaluate["Global`" <> short]];
            With[{g = Symbol["Global`" <> short], p = Symbol[context <> short]}, g = p]]]],
      Names[context <> "*"]]],
  {"TechnicalAnalysisPatternViewer`Candlesticks`", "TechnicalAnalysisPatternViewer`Data`", "TechnicalAnalysisPatternViewer`Engine`", "TechnicalAnalysisPatternViewer`Viewer`"}];

Remove[$tapvKernelDirectory];
