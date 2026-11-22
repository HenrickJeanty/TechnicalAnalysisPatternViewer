(* ::Package:: *)

(* ====================================================================== *)
(* Data.wl                                                                 *)
(* Technical Analysis Pattern Viewer: getting candlesticks                 *)
(* ====================================================================== *)
(*
  WHAT THIS FILE DOES
    It turns your data into the table of candlesticks that the rest of the viewer works with, and it can make coarser candlesticks
    (weekly, monthly, yearly) from daily ones. The viewer never reads a database: you give it the candlesticks.

  FUNCTIONS (each has a full usage message: ?loadCandlesticks, ...)
    loadCandlesticks[source, options]     source is a table of your own, the path of a CSV file, or a ticker (the candlesticks then come from
                                          FinancialData). The result is a Tabular with the columns
                                          Date, Open, High, Low, Close, Volume, Ticker, Period, logReturn.
    resampleCandlesticks[candles, period] weekly, monthly or yearly candlesticks from finer ones (daily, for example).
    sampleCandlesticks[period]            a synthetic sample (a made-up price series, not real market data) to try the viewer with.

  THE TABLE OF CANDLESTICKS (what loadCandlesticks gives and the other functions accept)
    Date       a DateObject (day), the first day the candlestick covers
    Open, High, Low, Close   the prices (numbers)
    Volume     a number (0 when your data have none)
    Ticker     a string (the ticker, or "" when the data come from a table)
    Period     a string: "daily", "weekly", "monthly", "yearly" (or "custom")
    logReturn  Log[Close / previous Close] (0 for the first candlestick)

  CONTEXT: TechnicalAnalysisPatternViewer`Data`
*)

BeginPackage["TechnicalAnalysisPatternViewer`Data`"];

(* ====================================================================== *)
(* PUBLIC FUNCTIONS AND THEIR USAGE MESSAGES                               *)
(* ====================================================================== *)

loadCandlesticks::usage =
  "loadCandlesticks[source] gives the table of candlesticks (a Tabular with the columns Date, Open, High, Low, Close, Volume, Ticker, Period and logReturn) " <>
  "made from source, in date order. source can be:\n" <>
  "  \[Bullet] a Tabular, a Dataset, or a list of associations (or of lists of rules) with the keys Date, Open, High, Low and Close (Volume is optional; the names are matched without regard to case);\n" <>
  "  \[Bullet] the path of a CSV file whose first line names the columns (Date, Open, High, Low, Close and optionally Volume), or File[\"path\"];\n" <>
  "  \[Bullet] a ticker such as \"PCG\", whose daily prices come from FinancialData (an internet connection is needed).\n" <>
  "Rows with a missing or non-numeric Open, High, Low or Close are left out, with a message that counts them.\n" <>
  "Options: \"Source\" -> Automatic (\"Table\", \"CSV\" or \"FinancialData\" to say which kind source is), " <>
  "\"StartDate\" -> All and \"EndDate\" -> All (a DateObject or {y, m, d}: only the candlesticks in this window), " <>
  "\"Resample\" -> None (\"weekly\", \"monthly\" or \"yearly\": make coarser candlesticks from the loaded ones, see resampleCandlesticks), " <>
  "\"Period\" -> Automatic (the text of the Period column: \"daily\", \"weekly\", \"monthly\" or \"yearly\"; Automatic takes it from the Resample option or from your table, and is \"daily\" otherwise), " <>
  "\"Ticker\" -> Automatic (the text of the Ticker column; for a ticker source, that ticker).\n" <>
  "Examples: loadCandlesticks[\"PCG\", \"StartDate\" -> {2024, 12, 1}, \"Resample\" -> \"weekly\"];  loadCandlesticks[\"prices.csv\"];  loadCandlesticks[myDataset].";

resampleCandlesticks::usage =
  "resampleCandlesticks[candles, period] makes coarser candlesticks from finer ones. period is \"weekly\" (ISO weeks, Monday to Sunday), \"monthly\" or \"yearly\". " <>
  "Each result candlestick has the Date of the first candlestick of its week, month or year, the Open of the first, the Close of the last, the highest High, " <>
  "the lowest Low and the sum of the Volumes; the Period column says the new period. candles is any table that loadCandlesticks accepts; the result is a Tabular.\n" <>
  "Use mergeCandlesticks[candles, n] instead to merge a fixed number n of candlesticks at a time, whatever their dates.";

sampleCandlesticks::usage =
  "sampleCandlesticks[] gives about five years of daily candlesticks of a SYNTHETIC price series (made up with a random-number generator, not real market data), " <>
  "to try the viewer without any data of your own. sampleCandlesticks[period] gives them resampled: \"daily\" (the default), \"weekly\", \"monthly\" or \"yearly\". " <>
  "The result is the table described in loadCandlesticks. The series is read from Data/SampleDaily.csv of this package.";

Begin["`Private`"];

(* ====================================================================== *)
(* MESSAGES                                                                *)
(* ====================================================================== *)

loadCandlesticks::source = "The source `1` is not a table of candlesticks, the path of an existing CSV file or a ticker.";
loadCandlesticks::columns = "The table has no column for `1`; the columns found are `2`.";
loadCandlesticks::dropped = "`1` of `2` rows were left out because their Date, Open, High, Low or Close is missing or not valid.";
loadCandlesticks::empty = "There are no candlesticks (after the window of dates was applied).";
loadCandlesticks::financialdata = "FinancialData gave no prices for the ticker `1`.";
loadCandlesticks::option = "The option `1` must be `2`, not `3`.";
resampleCandlesticks::period = "The period `1` must be \"weekly\", \"monthly\" or \"yearly\".";
sampleCandlesticks::file = "The sample file `1` was not found.";

Options[loadCandlesticks] = {"Source" -> Automatic, "StartDate" -> All, "EndDate" -> All, "Resample" -> None, "Period" -> Automatic, "Ticker" -> Automatic};

(* the folder of this package: this file is in Kernel/ *)
$packageDirectory = ParentDirectory[DirectoryName[$InputFileName]];

(* ====================================================================== *)
(* READING THE SOURCE: A LIST OF ASSOCIATIONS WITH LOWER-CASE KEYS         *)
(* ====================================================================== *)

(* the column names that are recognised, without regard to case *)
$columnNames = {"date", "open", "high", "low", "close", "volume"};

(* a list of associations whose keys are made lower-case strings, so that "Date", "date" and "DATE" are the same column *)
lowerCaseRows[rows_List] :=
  Map[Association[KeyMap[ToLowerCase[ToString[#]] &, Association[#]]] &, rows];

(* the rows of a table, a Dataset or a list; $Failed for anything else *)
tableRows[t_Tabular] := lowerCaseRows[Normal[t]];
tableRows[d_Dataset] := lowerCaseRows[Normal[d]];
tableRows[l_List] := If[l =!= {} && AllTrue[l, MatchQ[#, _Association | {___Rule}] &], lowerCaseRows[l], $Failed];
tableRows[_] := $Failed;

(* the rows of a CSV file: the first line is the header *)
csvRows[path_String] :=
  Module[{data = Import[path, "CSV"], header},
    If[! ListQ[data] || Length[data] < 2, Return[$Failed]];
    header = ToLowerCase[StringTrim[ToString[#]]] & /@ First[data];
    Map[AssociationThread[header, #] &, Rest[data]]];

(* ====================================================================== *)
(* CHECKING AND NORMALISING THE ROWS                                       *)
(* ====================================================================== *)

(* the value of the Date column as a DateObject (day); $Failed if it is not a date *)
toDate[d_DateObject] := DateObject[DateValue[d, {"Year", "Month", "Day"}]];
toDate[s_String] := Quiet[Check[With[{x = DateObject[StringTrim[s]]}, If[DateObjectQ[x], DateObject[DateValue[x, {"Year", "Month", "Day"}]], $Failed]], $Failed]];
toDate[l : {_Integer, _Integer, _Integer}] := Quiet[Check[DateObject[l], $Failed]];
toDate[_] := $Failed;

(* a price as a number; $Failed if it is not one *)
toPrice[x_?NumericQ] := N[x];
toPrice[s_String] := With[{v = Quiet[ToExpression[StringReplace[StringTrim[s], "," -> ""]]]}, If[NumericQ[v], N[v], $Failed]];
toPrice[_] := $Failed;

(* the table of candlesticks from rows with lower-case keys: valid rows only, in date order, with the columns Date ... logReturn *)
normaliseRows[rows_List, ticker_String, period_String] :=
  Module[{missing, valid, sorted, closes},
    missing = Complement[{"date", "open", "high", "low", "close"}, Union[Flatten[Keys /@ rows]]];
    If[missing =!= {},
      Message[loadCandlesticks::columns, StringRiffle[missing, ", "], StringRiffle[Union[Flatten[Keys /@ rows]], ", "]];
      Return[$Failed]];
    valid = Select[
      Map[
        Function[r, <|"Date" -> toDate[Lookup[r, "date", Missing[]]], "Open" -> toPrice[Lookup[r, "open", Missing[]]], "High" -> toPrice[Lookup[r, "high", Missing[]]],
          "Low" -> toPrice[Lookup[r, "low", Missing[]]], "Close" -> toPrice[Lookup[r, "close", Missing[]]],
          "Volume" -> With[{v = toPrice[Lookup[r, "volume", 0]]}, If[v === $Failed, 0, v]]|>],
        rows],
      FreeQ[Values[#], $Failed] &];
    If[Length[valid] < Length[rows], Message[loadCandlesticks::dropped, Length[rows] - Length[valid], Length[rows]]];
    If[valid === {}, Message[loadCandlesticks::empty]; Return[$Failed]];
    sorted = SortBy[valid, AbsoluteTime[#["Date"]] &];
    closes = sorted[[All, "Close"]];
    MapIndexed[
      Function[{r, pos}, Join[r, <|"Ticker" -> ticker, "Period" -> period,
        "logReturn" -> If[First[pos] == 1 || closes[[First[pos] - 1]] <= 0 || r["Close"] <= 0, 0., Log[r["Close"]/closes[[First[pos] - 1]]]]|>]],
      sorted]];

(* ====================================================================== *)
(* FinancialData                                                           *)
(* ====================================================================== *)

(* the daily candlesticks of a ticker from FinancialData: rows with lower-case keys, or $Failed *)
financialDataRows[ticker_String, start_, end_] :=
  Module[{from = If[start === All, DateObject[{1970, 1, 1}], toDate[start]], to = If[end === All, Today, toDate[end]], series, pairs, magnitude},
    series = Quiet[FinancialData[ticker, "OHLCV", {from, to}]];
    If[! MatchQ[series, _TimeSeries], Message[loadCandlesticks::financialdata, ticker]; Return[$Failed]];
    (* Normal[series] is a list of {date, values}; the values are an association (newer versions) or a list {Open, High, Low, Close, Volume} (older ones), with Quantity prices *)
    pairs = Normal[series];
    If[! ListQ[pairs] || pairs === {}, Message[loadCandlesticks::financialdata, ticker]; Return[$Failed]];
    magnitude[x_Quantity] := QuantityMagnitude[x];
    magnitude[x_] := x;
    Map[
      Function[pair,
        With[{v = If[AssociationQ[pair[[2]]], pair[[2]], AssociationThread[{"Open", "High", "Low", "Close", "Volume"}, pair[[2]]]]},
          <|"date" -> pair[[1]], "open" -> magnitude[v["Open"]], "high" -> magnitude[v["High"]], "low" -> magnitude[v["Low"]], "close" -> magnitude[v["Close"]],
            "volume" -> magnitude[v["Volume"]]|>]],
      pairs]];

(* ====================================================================== *)
(* loadCandlesticks                                                        *)
(* ====================================================================== *)

loadCandlesticks[source_, OptionsPattern[]] :=
  Module[{kind = OptionValue["Source"], start = OptionValue["StartDate"], end = OptionValue["EndDate"], resample = OptionValue["Resample"],
      tickerOption = OptionValue["Ticker"], periodOption = OptionValue["Period"], rows, ticker, periodLabel, table},
    (* which kind of source is it? *)
    If[kind === Automatic,
      kind = Which[
        MatchQ[source, _Tabular | _Dataset | _List], "Table",
        MatchQ[source, _File], "CSV",
        StringQ[source] && StringEndsQ[source, ".csv", IgnoreCase -> True], "CSV",
        StringQ[source] && FileExistsQ[source], "CSV",
        StringQ[source], "FinancialData",
        True, $Failed]];
    rows = Switch[kind,
      "Table", tableRows[source],
      "CSV", With[{path = If[MatchQ[source, _File], First[source], source]}, If[StringQ[path] && FileExistsQ[path], csvRows[path], $Failed]],
      "FinancialData", If[StringQ[source], financialDataRows[source, start, end], $Failed],
      _, $Failed];
    If[rows === $Failed || ! ListQ[rows], If[rows === $Failed && kind =!= "FinancialData", Message[loadCandlesticks::source, ToString[Short[source, 2]]]]; Return[$Failed]];
    (* the Ticker and Period columns: the option, else what the source says, else "" and "daily" *)
    ticker = Which[
      tickerOption =!= Automatic, ToString[tickerOption],
      kind === "FinancialData", ToUpperCase[source],
      KeyExistsQ[First[rows], "ticker"], ToString[First[rows]["ticker"]],
      True, ""];
    periodLabel = Which[
      periodOption =!= Automatic, ToString[periodOption],
      resample =!= None, resample,
      KeyExistsQ[First[rows], "period"], ToString[First[rows]["period"]],
      True, "daily"];
    table = normaliseRows[rows, ticker, periodLabel];
    If[table === $Failed, Return[$Failed]];
    (* the window of dates *)
    If[start =!= All || end =!= All,
      table = Select[table, (start === All || #["Date"] >= toDate[start]) && (end === All || #["Date"] <= toDate[end]) &];
      If[table === {}, Message[loadCandlesticks::empty]; Return[$Failed]]];
    (* coarser candlesticks *)
    Which[
      resample === None, Null,
      MemberQ[{"weekly", "monthly", "yearly"}, resample], table = resampleRows[table, resample],
      True, Message[loadCandlesticks::option, "\"Resample\"", "None, \"weekly\", \"monthly\" or \"yearly\"", resample]; Return[$Failed]];
    Tabular[table]];

(* ====================================================================== *)
(* resampleCandlesticks                                                    *)
(* ====================================================================== *)

(* the key of the week, month or year a date is in *)
(* weeks run from Monday to Sunday: the absolute time 0 (1 January 1900) was a Monday, so the number of whole weeks since then names the week *)
periodKey["weekly", d_DateObject] := Floor[AbsoluteTime[d, TimeZone -> 0]/(7*86400)];
periodKey["monthly", d_DateObject] := DateValue[d, {"Year", "Month"}];
periodKey["yearly", d_DateObject] := DateValue[d, "Year"];

(* rows (already normalised and in date order) as coarser rows *)
resampleRows[rows_List, period_String] :=
  Module[{groups = GatherBy[rows, periodKey[period, #["Date"]] &], previousClose = None, result},
    result = Map[
      Function[group,
        Module[{first = First[group], last = Last[group], row},
          row = <|"Date" -> first["Date"], "Open" -> first["Open"], "High" -> Max[group[[All, "High"]]], "Low" -> Min[group[[All, "Low"]]], "Close" -> last["Close"],
            "Volume" -> Total[group[[All, "Volume"]]], "Ticker" -> first["Ticker"], "Period" -> period,
            "logReturn" -> Total[group[[All, "logReturn"]]]|>;
          row]],
      groups];
    result];

resampleCandlesticks[candles_, period_String] :=
  Module[{rows, table},
    If[! MemberQ[{"weekly", "monthly", "yearly"}, period], Message[resampleCandlesticks::period, period]; Return[$Failed]];
    rows = tableRows[candles];
    If[rows === $Failed, Message[loadCandlesticks::source, ToString[Short[candles, 2]]]; Return[$Failed]];
    table = normaliseRows[rows, ToString[Lookup[First[rows], "ticker", ""]], "daily"];
    If[table === $Failed, Return[$Failed]];
    Tabular[resampleRows[table, period]]];

(* ====================================================================== *)
(* sampleCandlesticks                                                      *)
(* ====================================================================== *)

sampleCandlesticks[period_String : "daily"] :=
  Module[{file = FileNameJoin[{$packageDirectory, "Data", "SampleDaily.csv"}], daily},
    If[! FileExistsQ[file], Message[sampleCandlesticks::file, file]; Return[$Failed]];
    daily = loadCandlesticks[file, "Ticker" -> "SAMPLE"];
    If[period === "daily", daily, resampleCandlesticks[daily, period]]];

End[];

EndPackage[];
