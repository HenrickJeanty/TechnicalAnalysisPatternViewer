(* ::Package:: *)

(* ====================================================================== *)
(* Candlesticks.wl                                                         *)
(* Technical Analysis Pattern Viewer: tools for lists of candlesticks      *)
(* ====================================================================== *)
(*
  WHAT THIS FILE DOES
    It builds new lists of candlesticks from a list of candlesticks and draws them. It needs nothing but the Wolfram Language.

  FUNCTIONS (each has a full usage message: ?mergeCandlesticks, ?showCandlesticks, ...)
    mergeCandlesticks[original, n]        merges n candlesticks at a time (the candlesticks of a weekly chart from a daily one when n = 5,
                                          for example) and gives the result in the same form as the input.
    mergeCandlestickLevels[original, n]   the list of all the levels of merging: {original, merged n at a time, merged again, ...}.
    mergeGroupSizes[levels]               how many original candlesticks make one candlestick of each level.
    showCandlesticks[candles, title]      draws the candlesticks, with the dates below and their numbers above.

  A "list of candlesticks" is a Tabular, a Dataset or a list of associations (or lists of rules) whose rows have the keys
  "Open", "High", "Low" and "Close"; "Date", "Volume" and any other key are optional. loadCandlesticks (Kernel/Data.wl) makes such a table
  from a ticker, a CSV file or your own data.

  CONTEXT: TechnicalAnalysisPatternViewer`Candlesticks`
*)

BeginPackage["TechnicalAnalysisPatternViewer`Candlesticks`"];

(* ====================================================================== *)
(* PUBLIC FUNCTIONS AND THEIR USAGE MESSAGES                               *)
(* ====================================================================== *)

mergeCandlesticks::usage =
  "mergeCandlesticks[original, n] builds a new list of candlesticks from original by merging n candlesticks at a time: " <>
  "candlestick k of the result is the merge of original[[(k - 1) n + 1 ;; k n]].\n" <>
  "The merged candlestick has the Open of the first candlestick of the group, the Close of the last, the highest High and the lowest Low of the group, " <>
  "the Date of the first one, the sum of the Volumes and the sum of the logReturns (the log return of the whole group); " <>
  "every other column (Ticker, Period, ...) is taken from the first candlestick of the group.\n" <>
  "original is a Tabular, a Dataset or a list of associations with the keys Open, High, Low and Close, and the result has the same form " <>
  "(a Tabular gives a Tabular, a Dataset a Dataset, a list a list of associations).\n" <>
  "The result can be merged again: mergeCandlesticks[mergeCandlesticks[original, 2], 2] has the candlesticks of original merged 4 at a time.\n" <>
  "Option: \"Remainder\" -> \"Drop\" (the default: an incomplete last group is left out, so every result candlestick is a merge of exactly n candlesticks) " <>
  "or \"Keep\" (the incomplete last group is merged too).\n" <>
  "Example: mergeCandlesticks[daily, 5] gives 5 daily candlesticks in one.";

mergeCandlestickLevels::usage =
  "mergeCandlestickLevels[original, n] gives the list of all the levels of merging n candlesticks at a time: " <>
  "{original, mergeCandlesticks[original, n], mergeCandlesticks[mergeCandlesticks[original, n], n], ...}. " <>
  "levels[[1]] is the original, levels[[2]] the original merged n at a time, levels[[3]] that merged again, and so on up to the last level, " <>
  "which still has at least one candlestick (the merging stops when fewer than n candlesticks are left to merge). n is an integer of at least 2.\n" <>
  "Options: \"Remainder\" -> \"Drop\" (as in mergeCandlesticks) and \"MaxLevels\" -> All (or a number: at most that many merges after the original).\n" <>
  "Example: levels = mergeCandlestickLevels[weekly, 2]; Length /@ levels gives {102, 51, 25, 12, 6, 3, 1} for 102 weekly candlesticks.";

mergeGroupSizes::usage =
  "mergeGroupSizes[levels] gives the grouping size of each level of levels (the result of mergeCandlestickLevels): " <>
  "the number of original candlesticks in one candlestick of the level, {1, n, n^2, ...}.\n" <>
  "n is inferred from the numbers of candlesticks of the levels, assuming Remainder \"Drop\" (with Remainder \"Keep\" the numbers can fit another n too: give n). " <>
  "mergeGroupSizes[levels, n] uses the given n. The sizes are Missing[\"NotInferred\"] when no n fits the levels (or there is a single level).";

showCandlesticks::usage =
  "showCandlesticks[candles] or showCandlesticks[candles, title] draws the candlesticks: for each one a wick from its Low to its High and " <>
  "a body from its Open to its Close, at x = its number, in the up colour when the Close is at or above the Open and in the down colour when it is below. " <>
  "candles is a Tabular, a Dataset or a list of associations with the keys Open, High, Low and Close (and Date, for the labels of the bottom axis), " <>
  "so it works for the original candlesticks and for merged ones.\n" <>
  "Options: \"ImageSize\" -> 900, \"AspectRatio\" -> 0.5, \"UpColor\" -> RGBColor[0.1, 0.6, 0.2], \"DownColor\" -> RGBColor[0.85, 0.15, 0.15], " <>
  "\"TopIndices\" -> True (the number of each candlestick on the top axis, above the dates).";

Begin["`Private`"];

(* ====================================================================== *)
(* MESSAGES                                                                *)
(* ====================================================================== *)

showCandlesticks::input = "The candlesticks must be a non-empty Tabular, Dataset or list of associations with the keys Open, High, Low and Close.";
mergeCandlestickLevels::n = "The number of candlesticks to merge, `1`, must be an integer of at least 2 to make levels.";
mergeCandlestickLevels::max = "The option \"MaxLevels\" must be All or a non-negative integer, not `1`.";

mergeCandlesticks::n = "The number of candlesticks to merge, `1`, must be a positive integer.";
mergeCandlesticks::input = "The candlesticks must be a Tabular, a Dataset or a list of associations with the keys Open, High, Low and Close.";
mergeCandlesticks::remainder = "The option \"Remainder\" must be \"Drop\" or \"Keep\", not `1`.";

Options[mergeCandlesticks] = {"Remainder" -> "Drop"};

(* ====================================================================== *)
(* THE ROWS OF THE INPUT AS A LIST OF ASSOCIATIONS                         *)
(* ====================================================================== *)

candlestickRows[t_Tabular] := Association /@ Normal[t];
candlestickRows[d_Dataset] := Association /@ Normal[d];
candlestickRows[l_List] := If[AllTrue[l, MatchQ[#, _Association | {___Rule}] &], Association /@ l, $Failed];
candlestickRows[_] := $Failed;

(* ====================================================================== *)
(* ONE MERGED CANDLESTICK FROM A GROUP OF CONSECUTIVE CANDLESTICKS         *)
(* ====================================================================== *)

(* the keys and their order are those of the first candlestick; the keys below are replaced by the values of the group *)
mergeGroup[group_List] :=
  With[{first = First[group], last = Last[group]},
    Join[first,
      <|"Open" -> first["Open"], "High" -> Max[group[[All, "High"]]], "Low" -> Min[group[[All, "Low"]]], "Close" -> last["Close"]|>,
      If[KeyExistsQ[first, "Volume"], <|"Volume" -> Total[group[[All, "Volume"]]]|>, <||>],
      (* log returns add up: the sum over the group is log(Close of the last / Close before the first) *)
      If[KeyExistsQ[first, "logReturn"], <|"logReturn" -> Total[group[[All, "logReturn"]]]|>, <||>]]];

(* ====================================================================== *)
(* THE RESULT IN THE FORM OF THE INPUT                                     *)
(* ====================================================================== *)

sameForm[_Tabular, merged_List] := If[merged === {}, {}, Tabular[merged]];
sameForm[_Dataset, merged_List] := Dataset[merged];
sameForm[_List, merged_List] := merged;

(* ====================================================================== *)
(* mergeCandlesticks                                                       *)
(* ====================================================================== *)

mergeCandlesticks[original_, n_Integer?Positive, OptionsPattern[]] :=
  Module[{rows, remainder = OptionValue["Remainder"], groups, left},
    If[! MemberQ[{"Drop", "Keep"}, remainder], Message[mergeCandlesticks::remainder, remainder]; Return[$Failed]];
    rows = candlestickRows[original];
    If[rows === $Failed || (rows =!= {} && ! AllTrue[{"Open", "High", "Low", "Close"}, KeyExistsQ[First[rows], #] &]),
      Message[mergeCandlesticks::input]; Return[$Failed]];
    (* Partition leaves out an incomplete last group *)
    groups = Partition[rows, n];
    left = Mod[Length[rows], n];
    If[remainder === "Keep" && left > 0, groups = Append[groups, Take[rows, -left]]];
    sameForm[original, mergeGroup /@ groups]];

mergeCandlesticks[_, n_, OptionsPattern[]] := (Message[mergeCandlesticks::n, n]; $Failed);

(* ====================================================================== *)
(* mergeCandlestickLevels                                                  *)
(* ====================================================================== *)

Options[mergeCandlestickLevels] = {"Remainder" -> "Drop", "MaxLevels" -> All};

mergeCandlestickLevels[original_, n_Integer?(# >= 2 &), OptionsPattern[]] :=
  Module[{remainder = OptionValue["Remainder"], max = OptionValue["MaxLevels"], levels},
    If[! (max === All || (IntegerQ[max] && max >= 0)), Message[mergeCandlestickLevels::max, max]; Return[$Failed]];
    (* merge again while the current level has at least n candlesticks to merge, at most max times *)
    levels = NestWhileList[mergeCandlesticks[#, n, "Remainder" -> remainder] &, original, Length[#] >= n &, 1, If[max === All, Infinity, max]];
    If[MemberQ[levels, $Failed], $Failed, levels]];

mergeCandlestickLevels[_, n_, OptionsPattern[]] := (Message[mergeCandlestickLevels::n, n]; $Failed);

(* ====================================================================== *)
(* mergeGroupSizes                                                         *)
(* ====================================================================== *)

(* the n of mergeCandlestickLevels, from the numbers of candlesticks of its levels: the smallest n >= 2 for which the lengths are
   L, floor(L/n), floor(floor(L/n)/n), ... (Remainder "Drop") or the same with the ceiling (Remainder "Keep") *)
mergeSizeOfLevels[lengths_List] :=
  Module[{fits},
    If[Length[lengths] < 2 || First[lengths] < 2, Return[Missing["NotInferred"]]];
    (* the smallest n >= 2 for which the lengths follow a rounding function: floor first (Drop), then ceiling (Keep) *)
    fits[round_] := Select[Range[2, First[lengths]], Function[n, NestList[round[#/n] &, First[lengths], Length[lengths] - 1] === lengths]];
    Which[fits[Floor] =!= {}, First[fits[Floor]], fits[Ceiling] =!= {}, First[fits[Ceiling]], True, Missing["NotInferred"]]];

mergeGroupSizes[levels_List, n : (_Integer?(# >= 2 &) | Automatic) : Automatic] :=
  Module[{size = If[n === Automatic, mergeSizeOfLevels[Length /@ levels], n]},
    If[MissingQ[size], ConstantArray[Missing["NotInferred"], Length[levels]], size^Range[0, Length[levels] - 1]]];

(* ====================================================================== *)
(* showCandlesticks                                                        *)
(* ====================================================================== *)

Options[showCandlesticks] = {"ImageSize" -> 900, "AspectRatio" -> 0.5, "UpColor" -> RGBColor[0.1, 0.6, 0.2], "DownColor" -> RGBColor[0.85, 0.15, 0.15], "TopIndices" -> True};

showCandlesticks[candles_, title_String : "", OptionsPattern[]] :=
  Module[{rows = candlestickRows[candles], n, up = OptionValue["UpColor"], down = OptionValue["DownColor"], ticks},
    If[rows === $Failed || rows === {} || ! AllTrue[{"Open", "High", "Low", "Close"}, KeyExistsQ[First[rows], #] &],
      Message[showCandlesticks::input]; Return[$Failed]];
    n = Length[rows];
    (* a tick label every few candlesticks: the month and year of the Date if the candlesticks have one, their number otherwise *)
    ticks = Table[
      {i, If[DateObjectQ[Lookup[rows[[i]], "Date"]], Rotate[DateString[rows[[i]]["Date"], {"MonthNameShort", " ", "YearShort"}], 90 Degree], i]},
      {i, 1, n, Max[1, Round[n/20]]}];
    Graphics[
      MapIndexed[
        Function[{b, pos}, With[{i = First[pos], col = If[b["Close"] >= b["Open"], up, down]},
          {col, Line[{{i, b["Low"]}, {i, b["High"]}}], Rectangle[{i - 0.32, Min[b["Open"], b["Close"]]}, {i + 0.32, Max[b["Open"], b["Close"]] + 0.001}]}]],
        rows],
      Frame -> True,
      (* the top axis shows the number of the candlesticks (the same positions as the dates at the bottom) *)
      FrameTicks -> {{Automatic, None}, {ticks, If[TrueQ[OptionValue["TopIndices"]], {#[[1]], #[[1]]} & /@ ticks, None]}},
      FrameLabel -> If[TrueQ[OptionValue["TopIndices"]], {{None, None}, {None, "candlestick number"}}, None],
      GridLines -> {None, Automatic}, GridLinesStyle -> Directive[LightGray, Dashed],
      PlotRange -> {{0, n + 1}, All}, PlotRangePadding -> {0, Scaled[0.05]},
      AspectRatio -> OptionValue["AspectRatio"], ImageSize -> OptionValue["ImageSize"], PlotLabel -> title]];

End[];

EndPackage[];
