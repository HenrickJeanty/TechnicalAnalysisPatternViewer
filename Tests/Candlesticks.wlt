(* Tests of Kernel/Candlesticks.wl: mergeCandlesticks, mergeCandlestickLevels, mergeGroupSizes, showCandlesticks.
   Run: TestReport["<folder of the package>/Tests/Candlesticks.wlt"]. The file loads the package itself if it is not loaded.
   Note: tests that expect a message may be reported as failing by tools that do not capture messages; TestReport in a notebook or a script captures them. *)
If[Names["TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlestickLevels"] === {},
  Get[FileNameJoin[{ParentDirectory[DirectoryName[$TestFileName]], "Kernel", "init.m"}]]];

(* 16 synthetic candlesticks; the numbers are easy to check by hand *)
candle[i_] := <|"Date" -> DateObject[{2024, 1, i}, "Day"], "Open" -> 100. + i, "High" -> 110. + i + Mod[i, 3], "Low" -> 90. + i - Mod[i, 4],
  "Close" -> 101. + i, "Volume" -> 1000 i, "Ticker" -> "TST", "Period" -> "daily", "logReturn" -> 0.01 i|>;
list16 = Table[candle[i], {i, 16}];
tab16 = Tabular[list16];

VerificationTest[
  Length[TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlesticks[list16, 4]], 4, TestID -> "four groups of four"]

(* the first merged candlestick is the merge of the first n original ones *)
VerificationTest[
  With[{m = First[TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlesticks[list16, 3]], g = list16[[1 ;; 3]]},
    {m["Open"], m["Close"], m["High"], m["Low"], m["Volume"], m["Date"], m["Ticker"], m["Period"]} ===
      {g[[1]]["Open"], g[[3]]["Close"], Max[g[[All, "High"]]], Min[g[[All, "Low"]]], Total[g[[All, "Volume"]]], g[[1]]["Date"], "TST", "daily"}],
  True, TestID -> "the first candlestick is the merge of the first n"]

VerificationTest[
  With[{m = Last[TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlesticks[list16, 4]], g = list16[[13 ;; 16]]},
    {m["Open"], m["Close"], m["High"], m["Low"]} === {g[[1]]["Open"], g[[4]]["Close"], Max[g[[All, "High"]]], Min[g[[All, "Low"]]]}],
  True, TestID -> "the last candlestick is the merge of the last n"]

VerificationTest[
  TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlesticks[list16, 1], list16, TestID -> "n = 1 gives the original back"]

(* more levels: merging 2 and then 2 again is merging 4 at once, and 2, 2, 2 is 8 *)
VerificationTest[
  TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlesticks[TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlesticks[list16, 2], 2],
  TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlesticks[list16, 4],
  (* every key equal; the log returns are sums added in another order, so they agree to rounding only *)
  SameTest -> (And @@ MapThread[Function[{a, b}, KeyDrop[a, "logReturn"] === KeyDrop[b, "logReturn"] && Abs[a["logReturn"] - b["logReturn"]] < 10^-12], {#1, #2}] &),
  TestID -> "two levels of 2 are one level of 4"]

VerificationTest[
  Length[Nest[TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlesticks[#, 2] &, list16, 4]], 1, TestID -> "four levels of 2 turn 16 candlesticks into 1"]

(* the volume and the log return of the whole series are kept *)
VerificationTest[
  {Total[TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlesticks[list16, 4][[All, "Volume"]]], Total[TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlesticks[list16, 4][[All, "logReturn"]]]} ==
    {Total[list16[[All, "Volume"]]], Total[list16[[All, "logReturn"]]]}, True, TestID -> "volume and log return are conserved"]

(* an incomplete last group *)
VerificationTest[
  Length[TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlesticks[Take[list16, 14], 4]], 3, TestID -> "14 candlesticks, n = 4: the incomplete group is left out"]

VerificationTest[
  With[{r = TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlesticks[Take[list16, 14], 4, "Remainder" -> "Keep"]},
    {Length[r], Last[r]["Open"], Last[r]["Close"], Last[r]["Volume"]} === {4, list16[[13]]["Open"], list16[[14]]["Close"], list16[[13]]["Volume"] + list16[[14]]["Volume"]}],
  True, TestID -> "Remainder Keep merges the incomplete group"]

(* the form of the input is the form of the output *)
VerificationTest[
  {Head[TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlesticks[tab16, 4]], Normal[Keys[First[Normal[TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlesticks[tab16, 4]]]]] === Keys[First[list16]]},
  {Tabular, True}, TestID -> "a Tabular gives a Tabular with the same columns"]

VerificationTest[
  Head[TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlesticks[Dataset[list16], 4]], Dataset, TestID -> "a Dataset gives a Dataset"]

VerificationTest[
  Normal[TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlesticks[tab16, 4]] === TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlesticks[list16, 4], True, TestID -> "the same candlesticks for a Tabular and a list"]

(* shorter than n, and empty *)
VerificationTest[
  TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlesticks[Take[list16, 3], 4], {}, TestID -> "fewer than n candlesticks give none"]

VerificationTest[
  TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlesticks[{}, 3], {}, TestID -> "an empty list gives an empty list"]

(* errors *)
VerificationTest[
  TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlesticks[list16, 0], $Failed, {TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlesticks::n}, TestID -> "n = 0 fails"]

VerificationTest[
  TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlesticks[{<|"Open" -> 1|>}, 2], $Failed, {TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlesticks::input}, TestID -> "candlesticks without High, Low, Close fail"]

(* the sample data of the package: the weekly sample merged 3 at a time *)
VerificationTest[
  Module[{sample = TechnicalAnalysisPatternViewer`Data`sampleCandlesticks["weekly"], rows, m, ok},
    rows = Association /@ Normal[sample];
    m = Association /@ Normal[TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlesticks[Tabular[rows], 3]];
    (* the close of group k is the close of candlestick 3 k; the log return of group k >= 2 is the log of the closes *)
    ok = And @@ Table[
      m[[k]]["Close"] === rows[[3 k]]["Close"] && m[[k]]["Open"] === rows[[3 k - 2]]["Open"] &&
        (k == 1 || Abs[m[[k]]["logReturn"] - Log[rows[[3 k]]["Close"]/rows[[3 (k - 1)]]["Close"]]] < 10^-5),
      {k, Length[m]}];
    {Length[m] === Floor[Length[rows]/3], ok}],
  {True, True}, TestID -> "sample weekly merged 3 at a time"]

(* ---------------------------------------------------------------------------------------------------- mergeCandlestickLevels *)

VerificationTest[
  Length /@ TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlestickLevels[list16, 2], {16, 8, 4, 2, 1}, TestID -> "levels of 2 for 16 candlesticks"]

VerificationTest[
  Length /@ TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlestickLevels[list16, 4], {16, 4, 1}, TestID -> "levels of 4 for 16 candlesticks"]

(* 16 -> 5 (one left over, dropped) -> 1 (two left over, dropped) *)
VerificationTest[
  Length /@ TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlestickLevels[list16, 3], {16, 5, 1}, TestID -> "levels of 3 for 16 candlesticks"]

VerificationTest[
  Length /@ TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlestickLevels[Take[list16, 14], 4, "Remainder" -> "Keep"], {14, 4, 1}, TestID -> "levels with Remainder Keep"]

(* level 1 is the original, and every level is the merge of the one before *)
VerificationTest[
  With[{lv = TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlestickLevels[tab16, 2]},
    First[lv] === tab16 && And @@ Table[lv[[k + 1]] === TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlesticks[lv[[k]], 2], {k, Length[lv] - 1}]],
  True, TestID -> "level 1 is the original and each level merges the one before"]

VerificationTest[
  Length[TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlestickLevels[list16, 2, "MaxLevels" -> 2]], 3, TestID -> "MaxLevels 2: the original and two merges"]

VerificationTest[
  Length[TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlestickLevels[list16, 2, "MaxLevels" -> 0]], 1, TestID -> "MaxLevels 0: only the original"]

VerificationTest[
  Length[TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlestickLevels[Take[list16, 3], 4]], 1, TestID -> "fewer than n candlesticks: only the original"]

VerificationTest[
  Head /@ TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlestickLevels[tab16, 2], ConstantArray[Tabular, 5], TestID -> "every level is a Tabular for a Tabular"]

VerificationTest[
  TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlestickLevels[list16, 1], $Failed, {TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlestickLevels::n}, TestID -> "n = 1 gives no levels"]

VerificationTest[
  TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlestickLevels[list16, 2, "MaxLevels" -> -1], $Failed, {TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlestickLevels::max}, TestID -> "bad MaxLevels fails"]

(* ---------------------------------------------------------------------------------------------------- showCandlesticks *)

VerificationTest[
  Head[TechnicalAnalysisPatternViewer`Candlesticks`showCandlesticks[list16, "list"]], Graphics, TestID -> "showCandlesticks draws a list"]

VerificationTest[
  {Head[TechnicalAnalysisPatternViewer`Candlesticks`showCandlesticks[tab16]], Head[TechnicalAnalysisPatternViewer`Candlesticks`showCandlesticks[Dataset[list16], "dataset"]]}, {Graphics, Graphics},
  TestID -> "showCandlesticks draws a Tabular and a Dataset"]

(* one wick and one body for each candlestick, and the up and down colours follow the Close against the Open *)
VerificationTest[
  With[{g = TechnicalAnalysisPatternViewer`Candlesticks`showCandlesticks[list16, "", "UpColor" -> Blue, "DownColor" -> Orange]},
    {Count[g, _Line, Infinity], Count[g, _Rectangle, Infinity], Count[g, Blue, Infinity] >= 16, Count[g, Orange, Infinity] == 0}],
  {16, 16, True, True}, TestID -> "16 wicks and 16 bodies; all up in the synthetic data"]

VerificationTest[
  Head[TechnicalAnalysisPatternViewer`Candlesticks`showCandlesticks[Last[TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlestickLevels[tab16, 2, "MaxLevels" -> 2]]]], Graphics, TestID -> "showCandlesticks draws a merged level"]

VerificationTest[
  Lookup[Options[TechnicalAnalysisPatternViewer`Candlesticks`showCandlesticks[list16, "", "ImageSize" -> 500, "AspectRatio" -> 0.3]], ImageSize], 500, TestID -> "ImageSize option"]

VerificationTest[
  TechnicalAnalysisPatternViewer`Candlesticks`showCandlesticks[{}], $Failed, {TechnicalAnalysisPatternViewer`Candlesticks`showCandlesticks::input}, TestID -> "no candlesticks fail"]

VerificationTest[
  TechnicalAnalysisPatternViewer`Candlesticks`showCandlesticks[{<|"Open" -> 1|>}], $Failed, {TechnicalAnalysisPatternViewer`Candlesticks`showCandlesticks::input}, TestID -> "candlesticks without High, Low, Close fail in showCandlesticks"]

(* the top axis shows the number of the candlesticks, at the positions of the date ticks of the bottom axis *)
VerificationTest[
  With[{top = FrameTicks /. Options[TechnicalAnalysisPatternViewer`Candlesticks`showCandlesticks[list16, "", "TopIndices" -> True]],
        none = FrameTicks /. Options[TechnicalAnalysisPatternViewer`Candlesticks`showCandlesticks[list16, "", "TopIndices" -> False]]},
    {Length[top[[2, 2]]] > 0, top[[2, 2, All, 1]] === top[[2, 1, All, 1]], top[[2, 2, All, 2]] === top[[2, 2, All, 1]], none[[2, 2]] === None}],
  {True, True, True, True}, TestID -> "TopIndices: the candlestick numbers on the top axis"]

(* ---------------------------------------------------------------------------------------------------- mergeGroupSizes *)

VerificationTest[
  TechnicalAnalysisPatternViewer`Candlesticks`mergeGroupSizes[TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlestickLevels[list16, 2]], {1, 2, 4, 8, 16}, TestID -> "group sizes of the levels of 2"]

VerificationTest[
  TechnicalAnalysisPatternViewer`Candlesticks`mergeGroupSizes[TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlestickLevels[list16, 4]], {1, 4, 16}, TestID -> "group sizes of the levels of 4"]

VerificationTest[
  TechnicalAnalysisPatternViewer`Candlesticks`mergeGroupSizes[TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlestickLevels[list16, 3]], {1, 3, 9}, TestID -> "group sizes of the levels of 3 (16, 5, 1: remainders dropped)"]

(* with Remainder Keep the lengths {14, 4, 1} also fit n = 3 with Drop, so the merge size is given *)
VerificationTest[
  TechnicalAnalysisPatternViewer`Candlesticks`mergeGroupSizes[TechnicalAnalysisPatternViewer`Candlesticks`mergeCandlestickLevels[Take[list16, 14], 4, "Remainder" -> "Keep"], 4], {1, 4, 16}, TestID -> "group sizes with Remainder Keep and the merge size given"]

VerificationTest[
  TechnicalAnalysisPatternViewer`Candlesticks`mergeGroupSizes[{list16, list16}, 5], {1, 5}, TestID -> "the merge size given"]

VerificationTest[
  TechnicalAnalysisPatternViewer`Candlesticks`mergeGroupSizes[{list16}], {Missing["NotInferred"]}, TestID -> "a single level: not inferred"]
