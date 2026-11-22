(* ::Package:: *)

(* ====================================================================== *)
(* StockDataManagerStub.wl                                                 *)
(* ====================================================================== *)
(*
  Analysis.wl and Visualization.wl (in this folder) were written for a project that also had a data layer, the context
  StockAnalyzer`StockDataManager`. They declare it as a dependency, and two functions that the viewer does not use
  (the convenience overload of updateFeatureSet that reads a ticker from disk, and MarkovRegimeModel`FetchPrices) call its getStock.

  The viewer takes its candlesticks from the user (see loadCandlesticks in Kernel/Data.wl), so this stub declares the context and
  a getStock that only says so. It makes no file or network access.
*)

BeginPackage["StockAnalyzer`StockDataManager`"];

getStock::usage =
  "getStock[...] is a placeholder: the Technical Analysis Pattern Viewer takes its candlesticks from loadCandlesticks and does not read a stock database.";

Begin["`Private`"];

getStock::notavailable = "getStock is not available in the Technical Analysis Pattern Viewer; use loadCandlesticks to get the candlesticks of a ticker, a CSV file or a table.";

getStock[___] := (Message[getStock::notavailable]; $Failed);

End[];

EndPackage[];
