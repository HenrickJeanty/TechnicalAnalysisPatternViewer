(* ::Package:: *)

(* ============================================================================
   Analysis.wl
   StockAnalyzer Package -- Elliott Wave Analysis Layer

   Provides the incrementalfin candlestick processing pipeline, peak and valley
   detection, wave segment computation, and the top-level updateFeatureSet
   orchestrator used by GraphBuilder_Demo.nb.

   Author : Henry Henrick Jeanty, Ph.D.
   Version: 1.0.0
   Requires: Mathematica 14.3+, StockAnalyzer`StockDataManager`
   ============================================================================ *)

BeginPackage["StockAnalyzer`Analysis`",
  {"StockAnalyzer`StockDataManager`",
   "StockAnalyzer`Fibonacci`",
   "MarkovRegimeModel`"
  }
]
scanFibonacciTouches::badRange =
  "Starting extension (`1`) must be strictly less than ending extension (`2`).";

(* ============================================================================
   Public symbol declarations with usage messages
   ============================================================================ *)

(* -- Feature Set Initialization --------------------------------------------- *)
initializeFeatureSetGNN::usage =
  "initializeFeatureSetGNN[] returns an empty Dataset with all required keys \
for the GNN-based analysis pipeline: Candlesticks, Graph, PeaksQ, ValleysQ, \
Peaks, Valleys, ActiveUptrends, ActiveDowntrends, PeakStack, ValleyStack.";

addFieldsToCandlestick::usage =
  "addFieldsToCandlestick[candlestickArg] adds analysis fields with default \
values to a TabularRow Association: IsPeakQ, IsValleyQ, EdgeIndices, \
VertexIndexCoords, EdgeIndexCoords, Graph, Ranges, LogRatios, Angles, \
FromPrices, ToPrices, Weights, Direction, EdgeColor.";

stockGraphDisplayOptions::usage =
  "stockGraphDisplayOptions is the list of Graph options that make featureSet[\"Graph\"] read as a chart: laid out left to right (earlier candlesticks on the left), each vertex labeled with the index of its candlestick.";

upEdgeColor::usage = "upEdgeColor is the color of the edges (arrows) of an up segment in the graph displays: lime.";
downEdgeColor::usage = "downEdgeColor is the color of the edges (arrows) of a down segment in the graph displays: red.";
flatEdgeColor::usage = "flatEdgeColor is the color of the edges (arrows) of a flat segment (equal prices at both ends) in the graph displays: yellow.";
edgeDirectionColor::usage =
  "edgeDirectionColor[fromPrice, toPrice] or edgeDirectionColor[segment] gives upEdgeColor (lime) when the price goes up, downEdgeColor (red) when it goes down and flatEdgeColor (yellow) when it is the same.";

horizontalStockGraph::usage =
  "horizontalStockGraph[graphOrFeatureSet, opts] returns the graph with the options of stockGraphDisplayOptions (horizontal layout, vertices labeled with the candlestick index). Accepts a Graph, or a feature set (its \"Graph\" is used). Extra opts are passed to Graph and override the defaults, e.g. horizontalStockGraph[fs, ImageSize -> 1200].";

(* -- Core Incremental Pipeline ---------------------------------------------- *)
addNewCandlestickToFeatureSet::usage =
  "addNewCandlestickToFeatureSet[featureSetArg, newCandlestickArg] adds a new \
TabularRow candlestick to the Candlesticks Tabular in featureSetArg, prepending \
a sequential Index field. Returns the updated featureSet Dataset.";

findPreviousHigherHighAndLowerLow::usage =
  "findPreviousHigherHighAndLowerLow[featureSetArg] computes the LastHigher and \
LastLower index fields for the most recently added candlestick using a \
pointer-chain scan. Returns the updated featureSet Dataset.";

computeIsPeakQAndIsValleyQ::usage =
  "computeIsPeakQAndIsValleyQ[featureSetArg] determines whether the penultimate \
candlestick is a peak or valley based on its neighbors, updates the Peaks, \
Valleys, PeaksAndValleys, and VertexToPeak/VertexToValley Tabulars, and \
refreshes the IsPeakQ and IsValleyQ list fields. Returns the updated featureSet.";

updateFeatureSet::usage =
  "updateFeatureSet[featureSetArg, candlesticksArg, sigma, opts] iterates over \
all rows of candlesticksArg, running the full incremental analysis pipeline on \
each one. Returns the completed featureSet Dataset with all analysis fields \
populated. Option \"StrictRanges\" (default True) controls segment validity \
checking - passed through to candlesticksRange via getValidWavesToLastCandlestick - \
and the edge types generated per segment (True: only MaxU and MaxD edges; False: \
every field pair, see updateGraphFromSegments). \
updateFeatureSet[ticker, period, startDate, endDate,sigma, opts] is a convenience overload \
that fetches the stock data and builds the featureSet in one call. Option \"GraphChart\" (default False) \
True adds the key \"GraphChart\", the glyph picture of the last 250 candles; a list of rules, for example \
{\"EdgeThickness\" -> 0.002, \"Window\" -> 120}, passes them to addGraphChart. Option \"HarmonicPatterns\" (default False; True, a pattern name or a list of names) also finds, at every candlestick, the harmonic patterns that are born at it (see getHarmonicPatternsBornAtLastCandlestick) and adds them to the key HarmonicPatterns, with the option \"HarmonicTolerance\" (default 0.05). Option \"SupportResistance\" (default False) True also runs getValidSupportAndResistanceToLastCandlestick for every candlestick, with the options \"TolerancePercent\" (default 5) and \"MinTouches\" (default 1), and the key Patterns (options \"Patterns\" True, \"PatternBand\" 0.15). Peaks and valleys: the last candlestick is never one, and the first one only if it is one in the whole history of the stock: options \"FirstCandlestickIsPeakQ\" and \"FirstCandlestickIsValleyQ\" (Automatic: the ticker overload finds them in SP500Stocks, False if it is not loaded; the featureSet overload uses False; True or False sets them).";

(* -- Peak & Valley Helpers -------------------------------------------------- *)
getPreviousHigherPeak::usage =
  "getPreviousHigherPeak[featureSetArg] returns the index of the most recent \
peak whose High is greater than the last candlestick's Low and whose index is \
at or after LastLower. Returns 0 if none found.";

getPreviousLowerValley::usage =
  "getPreviousLowerValley[featureSetArg] returns the index of the most recent \
valley whose Low is less than the last candlestick's High and whose index is \
at or after LastHigher. Returns 0 if none found.";

getAllPreviousPeaks::usage =
  "getAllPreviousPeaks[featureSetArg] returns all peaks whose High exceeds the \
last candlestick's Low and whose index is at or after Max[LastLower, LastHigher], \
sorted by High ascending.";

getAllPreviousValleys::usage =
  "getAllPreviousValleys[featureSetArg] returns all valleys whose Low is below \
the last candlestick's High and whose index is at or after Max[LastLower, \
LastHigher], sorted by Low ascending.";

getValidPreviousPeaks::usage =
  "getValidPreviousPeaks[featureSetArg] returns a Tabular of valid prior peaks \
scanning backward from the most recent, keeping only those that set a new \
running high. Used for Elliott Wave uptrend validation.";

getValidPreviousValleys::usage =
  "getValidPreviousValleys[featureSetArg] returns a Tabular of valid prior \
valleys scanning backward from the most recent, keeping only those that set a \
new running low. Used for Elliott Wave downtrend validation.";

computeLastCandlestickIsPeakQAndIsValleyQ::usage =
  "computeLastCandlestickIsPeakQAndIsValleyQ[featureSetArg, tabularCandlesticksArg] \
evaluates peak and valley status for the penultimate candlestick in \
tabularCandlesticksArg and updates all associated featureSet fields. Returns \
the updated featureSet Dataset.";

updateLastCandlestickIsPeakQAndIsValleyQ::usage =
  "updateLastCandlestickIsPeakQAndIsValleyQ[featureSetArg] is the legacy \
single-pass wrapper that calls computeLastCandlestickIsPeakQAndIsValleyQ. \
Used by the old algorithm path in updateFeatureSet.";

computeFeatureSetPeaksAndValleys::usage =
  "computeFeatureSetPeaksAndValleys[featureSetArg] is a batch peak and valley \
computation pass over all candlesticks. Returns the updated featureSet Dataset.";

findPeaksAndValleysNew::usage =
  "findPeaksAndValleysNew[featureSetArg, sigma] uses Mathematica's built-in \
FindPeaks on the High and Low series to detect peaks and valleys, returning a \
Dataset with keys PeaksAndValleys, Peaks, Valleys, VertexToPeak, VertexToValley.";

appendPeaksAndValleysToNew::usage =
  "appendPeaksAndValleysToNew[featureSetArg, sigma] calls findPeaksAndValleysNew \
and appends the results to featureSetArg, also adding IsPeakQ and IsValleyQ \
boolean columns to the Candlesticks Tabular. Returns the updated featureSet.";

(* -- Wave Detection --------------------------------------------------------- *)
getUpWavesTo::usage =
  "getUpWavesTo[featureSetArg, peakIndexArg, valleysQArg, percentLeewayArg] \
computes all valid upward wave DirectedEdges leading to the candlestick at \
peakIndexArg, scanning backward for qualifying valley origins. Returns a List \
of DirectedEdge expressions with log-ratio weights.";

getDownWavesTo::usage =
  "getDownWavesTo[featureSetArg, valleyIndexArg, peaksQArg, percentLeewayArg] \
computes all valid downward wave DirectedEdges leading to the candlestick at \
valleyIndexArg, scanning backward for qualifying peak origins. Returns a List \
of DirectedEdge expressions with log-ratio weights.";

getValidWavesToLastCandlestick::usage =
  "getValidWavesToLastCandlestick[featureSetArg, opts] computes all valid uptrend \
and downtrend wave segments to the last candlestick using candlesticksRange, then \
appends them to the Segments Tabular in featureSetArg. Option \"StrictRanges\" \
(default False) is passed through to candlesticksRange. Returns the updated \
featureSet Dataset.";

getHarmonicPatternsBornAtLastCandlestick::usage =
  "getHarmonicPatternsBornAtLastCandlestick[featureSetArg, opts] finds the harmonic patterns that are born at the last candlestick, and adds them to the key HarmonicPatterns of the featureSet " <>
  "(a Tabular, one row per pattern, like Segments). A pattern is a path X, A, B, C, D (A, B, C, D for ABCD) of consecutive segments of the stock graph whose leg ratios are inside the " <>
  "published ranges; it is born at the candlestick where its last segment (C to D) ends, so it is found as soon as it is complete, from the segments that end at the last candlestick " <>
  "(LatestSegments) and the segments before them (the key HarmonicSegments), never from later candlesticks. The rules and the code are in Kernel/HarmonicPatterns.wl. " <>
  "Each row has Pattern, Direction, ToIndex (the candlestick where it is born), Points, Indices, Prices, Edges, Violation, the ratios and Subgraph (the directed path graph of the match, " <>
  "a subgraph of the graph of the featureSet). Options: \"Patterns\" (All, a name or a list of names of $harmonicPatterns) and \"Tolerance\" (0.05, absolute, on every ratio range). Returns the updated featureSet Dataset.";

getValidSupportAndResistanceToLastCandlestick::usage =
  "getValidSupportAndResistanceToLastCandlestick[featureSetArg, opts] finds the support and resistance trendlines that are valid at the last candlestick, incrementally, and the patterns that one resistance and one support make at it. The rules and the code are in Kernel/TrendlinePatterns.wl. A resistance (support) goes through two peaks (valleys) and is valid while no candlestick between its first anchor and the last candlestick pierces it with its High (Low) by more than the tolerance. Every line alive at the last candlestick adds a row to Segments with ToIndex equal to the last index and Direction ResU, ResD, ResF, SupU, SupD or SupF (U up, D down, F flat: anchors closer than the tolerance), plus the columns Kind, Touches, AnchorIndex, StartIndex, Slope and LogSlope. The key ValidTrendlines keeps every line, alive or ended, with its EndIndex (missing while the line is alive). The key Patterns gets one row for every pattern valid at the last candlestick (13 patterns, see $trendlinePatterns). Options: \"TolerancePercent\" (default 5) is the tolerance as a percentage of the High-Low range of the candlesticks from the first anchor to the candlestick being tested; \"MinTouches\" (default 1) is the number of touches a stub needs between its two anchors; \"Patterns\" (True) switches the patterns on or off; \"PatternBand\" (0.15) is the band around 1 of the ratio of the gap between the two lines at the last candlestick to the gap at the start of the pair that counts as parallel. Returns the updated featureSet Dataset.";

candlesticksRange::usage =
  "candlesticksRange[candlesticksArg, spanArg, directionArg, opts] computes a \
segment Association for the given Span of candlesticks in the given direction \
(\"U\" or \"D\"). Returns Nothing if the span does not form a valid trend. \
Option \"StrictRanges\" (default False): when False uses a simple validity test; \
when True applies strict positional checks. \
candlesticksRange[candlesticksArg, spans, directionArg, opts] maps over a List \
of Span values and returns a Tabular of all valid segment Associations.";

make2LevelAssociationToTabular::usage =
  "make2LevelAssociationToTabular[assoc] converts a two-level nested Association \
of OHLC field pairs into a Tabular with row and column headers for display.";

findClosestAngles::usage =
  "findClosestAngles[featureSetArg, referenceSegmentArg] takes a featureSet \
Dataset and a reference segment Association (from EdgeSegments). Filters \
EdgeSegments to those with the same FromIndex, Direction, and ToIndex <= \
reference ToIndex, then ranks them by absolute difference from the reference \
segment flat Angle field. Returns a Dataset sorted by AbsDiff ascending.";

getLinesFromTo::usage =
  "getLinesFromTo[featureSetArg, spanArg] extracts line functions from the \
EdgeSegments of featureSetArg (a Dataset) whose ToIndex falls within spanArg \
(a Span of the form fromIndex ;; toIndex). Returns an Association keyed by \
{FromIndex, ToIndex, Direction, FromField, ToField} where each value is a pure \
function f such that f[x] = slope * x + intercept. \
Example: lines = getLinesFromTo[updatedFeatureSet, 2 ;; 8] returns all lines \
whose ToIndex is between 2 and 8 inclusive. Use lines[{4, 5, \"D\", \"High\", \
\"High\"}] to look up a specific line and lines[{4, 5, \"D\", \"High\", \
\"High\"}][6] to evaluate it at index 6.";

getFibonacciLinesFromTo::usage =
  "getFibonacciLinesFromTo[featureSetArg, spanArg] generates horizontal Fibonacci \
line functions from all MaxU and MaxD edge segments whose ToIndex falls within \
spanArg (a Span of the form fromIndex ;; toIndex). For each qualifying segment, \
8 horizontal lines are created using fibonacciRatios = {0., 0.236, 0.382, 0.500, \
0.618, 0.786, 1., 1.272} applied to the segment Range. Returns an \
Association keyed by {FromIndex, ToIndex, \"F\", FromField, ToField, FibonacciRatio} \
where each value is a constant pure function f[x] = FromPrice + Ratio * Range. \
Example: fibLines = getFibonacciLinesFromTo[updatedFeatureSet, 2 ;; 8] then \
fibLines[{4, 5, \"F\", \"High\", \"High\", 0.618}][9] returns the Fibonacci price.";


scoreLineGoodness::usage =
  "scoreLineGoodness[lineFArg, keyArg, candlesticksArg, fromIndexArg, finalIndexArg, \
toleranceArg] counts how many OHLC prices in the candlestick range \
[fromIndexArg, finalIndexArg] are touched by the line function lineFArg \
within a percentage tolerance, excluding the guaranteed touches at the segment \
endpoints (FromIndex/FromField and ToIndex/ToField). Touch counts are weighted \
by a recency factor Exp[-lambda * (finalIndexArg - ToIndex)] where lambda \
controls the decay rate (default 0.3). Lines whose ToIndex is close to \
finalIndexArg are preferred over older lines with the same raw touch count. \
toleranceArg is a decimal fraction (e.g. 0.01 for 1%). Returns an Association \
<| \"Open\" -> n, \"High\" -> n, \"Low\" -> n, \"Close\" -> n, \"Total\" -> n |> \
with weighted touch counts per field and their sum.";

defaultLambdaByField::usage =
  "defaultLambdaByField is an Association of field-specific lambda parameters \
derived from grid search on ABBV daily data: \
<|\"Open\"->0.80, \"High\"->0.20, \"Low\"->0.50, \"Close\"->0.80|>.";

defaultToleranceByField::usage =
  "defaultToleranceByField is an Association of field-specific tolerance \
parameters derived from grid search on ABBV daily data: \
<|\"Open\"->0.025, \"High\"->0.010, \"Low\"->0.006, \"Close\"->0.018|>.";


(* ============================================================================
   Public constant declarations
   ============================================================================ *)

(* myLog is used throughout the wave detection functions *)
myLog::usage =
  "myLog[x] returns Log[x] with protection against zero or negative inputs, \
returning 0 in those cases.";

updateGraphFromSegments::usage =
  "updateGraphFromSegments[featureSetArg, opts] adds one new vertex for the latest \
candlestick and new directed edges for the segments ending at that candlestick to \
the Graph stored in the featureSet. Option \"StrictRanges\" (default True): with True \
each segment gets exactly one edge, MaxU from Low[fromIndex] to High[toIndex] for an \
uptrend segment or MaxD from High[fromIndex] to Low[toIndex] for a downtrend segment \
(never the wrong-direction pairs such as OuterUp or OuterDown); with False each segment \
gets an edge for every field pair of looseEdgeSpecs, classified by EdgeType. \
The full segment Association is stored \
in EdgeSegments keyed by DirectedEdge[fromIndex, toIndex]. LatestSegments holds \
only the segments added in this iteration as a Tabular. Returns the updated \
featureSet Dataset.";

getLinesFromTo::usage =
  "getLinesFromTo[featureSetArg, spanArg] returns a List of parametric line \
Associations (the \"Line\" field) from all EdgeSegments where \
FromIndex < spanArg[[1]] and ToIndex <= spanArg[[2]].";

predictNextCandlestick::usage =
  "predictNextCandlestick[featureSetArg, candlestickIndexArg, toleranceArg, topNArg, \
lambdaArg, windowSizeArg] finds all regular and Fibonacci edge segment lines whose \
ToIndex falls in the window ending at candlestickIndexArg, scores each line against \
candlesticks in that window using field-specific lambda and tolerance parameters, \
then for each OHLC field returns the top topNArg lines by weighted touch count along \
with their predicted values at candlestickIndexArg + 1 and the mean predicted value. \
toleranceArg and lambdaArg can each be either a Real (applied uniformly to all fields) \
or an Association keyed by field name e.g. <|\"Open\"->0.025, \"High\"->0.010, \
\"Low\"->0.006, \"Close\"->0.018|>. windowSizeArg limits how far back segments are \
scanned \[LongDash] Automatic resolves to Ceiling[3/lambda] per field. \
Fields where no line has touches > 0 are omitted. Returns an Association keyed by \
OHLC field plus a \"Range\" key giving the predicted High-Low range.";

backtestPredictions::usage =
  "backtestPredictions[featureSetArg, fromIndexArg, toIndexArg, toleranceArg, \
topNArg, lambdaArg, tickerArg, regimeWindowArg] runs predictNextCandlestick across \
all indices from fromIndexArg to toIndexArg inclusive, comparing each prediction \
against the actual OHLC values at the next candlestick index. toleranceArg and \
lambdaArg use field-specific optimal parameters when set to Automatic. \
tickerArg is an optional stock ticker string \[LongDash] when provided, the Markov regime \
is computed and updated every regimeWindowArg candlesticks (default 1) using \
getCurrentRegime, giving regime-adjusted predictions with no lookahead. \
Pass tickerArg as empty string \"\" to disable regime adjustment. \
Returns a Dataset with one row per predicted index containing: PredictIndex, \
ActualO/H/L/C, PredictedO/H/L/C, ErrorO/H/L/C, EdgeTypeO/H/L/C, \
and PredictedRange. Fields where no predictor scored > 0 appear as Missing[].";


getCurrentRegime::usage =
  "getCurrentRegime[tickerArg, yearsArg, windowArg, thresholdArg] fetches closing \
prices for tickerArg using FetchPrices from MarkovRegimeModel, builds a transition \
matrix, and returns an Association describing the current market regime. Returns \
<|\"Regime\" -> \"Bull\"|\"Bear\"|\"Sideways\", \"RegimeIndex\" -> 0|1|2, \
\"Signal\" -> s, \"Persistence\" -> p, \
\"P5\" -> <|\"Bear\"->b, \"Sideways\"->s, \"Bull\"->bull|>, \
\"Matrix\" -> P|>. Requires MarkovRegimeModel.wl to be loaded.";

computeRegimeHistory::usage =
  "computeRegimeHistory[featureSetArg, tickerArg, yearsArg, windowArg, thresholdArg] \
pre-computes Markov regime probabilities for every candlestick in featureSetArg \
and returns an Association keyed by candlestick Index. Each value contains \
HistoricalBear/Sideways/Bull (walk-forward regime at that point in time), \
TodayBear/Sideways/Bull (current regime as of today), and \
Forecast5Bear/Sideways/Bull (5-day forecast from current regime). \
Store the result in Global`$regimeHistory for use by infoBarText.";

getSegments::usage = 
  "getSegments[segmentsArg, startIndexArg, endIndexArg] returns the segments \
from segmentsArg whose FromIndex matches startIndexArg and ToIndex matches \
endIndexArg.";

scanFibonacciTouches::usage =
  "scanFibonacciTouches[featureSetArg, segmentArg, fibRatiosArg, " <>
  "toleranceArg, startingExtensionArg, endingExtensionArg, opts] " <>
  "scans Fibonacci touches across a grid of extension ratios from " <>
  "startingExtensionArg (default 1.0) to endingExtensionArg " <>
  "(default: next multiple of toleranceArg at or above " <>
  "If[ValueQ[Global`defaultMaxExtension], Global`defaultMaxExtension, 1.31]), " <>
  "stepping by toleranceArg. The candlestick table is derived internally " <>
  "from featureSetArg[\"Candlesticks\"]. Returns an Association with " <>
  "ScanResults keyed by NormalizedRatio. Extensions are built directly " <>
  "as a clean grid (not derived from scan prices) to guarantee exact key " <>
  "values like 1.0.";
      	
fibonacciTouchesTabular::usage =
  "fibonacciTouchesTabular[scanResultArg] returns a Tabular of all touch \
details from scanResultArg (the result of scanFibonacciTouches), flattened \
across all scan prices and all Fibonacci ratios. Each row carries a \
Segment field {FromIndex, ToIndex} as the first column, followed by the \
standard touch detail fields (FibonacciRatio, CandlestickIndex, PriceType, \
CandlestickPrice, FibPrice, SignedDistance, Delta).";

fibonacciTouchesTabularAll::usage =
  "fibonacciTouchesTabularAll[bySegmentArg] returns a single Tabular of all \
touch details across every segment in bySegmentArg (the Association \
returned by mapping scanFibonacciTouches over a range of segments). \
Each row carries a Segment field identifying which {fromIdx, toIdx} \
segment the touch came from, in addition to all the standard touch \
detail fields.";

scanTrendlineTouches::usage =
  "scanTrendlineTouches[segmentArg, candlesticksArg, fibRatiosArg, toleranceArg, \
nStepsArg, nextPricesCountArg] scans a diagonal trendline that pivots around \
{FromIndex, FromPrice} and sweeps its endpoint from startingScanPrice = \
FromPrice + 0.236*Range to endingScanPrice = FromPrice + 1.272*Range at ToIndex. \
At each scan price, the diagonal line price at each inside candlestick index i \
is computed as FromPrice + ((i - FromIndex) / (ToIndex - FromIndex)) * scanRange. \
All four price fields (O,H,L,C) of each inside candlestick are checked against \
the diagonal line price within toleranceArg% tolerance proportional to scanRange. \
Multiple touches per candlestick are recorded independently. \
Returns an Association with MaxTouches (peak touch count across all scan prices \
for the segment) and ScanResults (Association keyed by NormalizedRatio). \
Each ScanResults entry contains NormalizedRatio, ScanPrice, ScanRange, \
SegmentLine (function returning price on the diagonal line at any index), \
NextPrices (Association mapping indices ToIndex+1..ToIndex+nextPricesCountArg \
to extrapolated diagonal line prices), Touches (touch count for this scan price) \
and TouchDetails (list of touch detail Associations with CandlestickIndex, \
PriceType, Price, SegmentPrice, SignedDistance and Delta). \
fibRatiosArg defaults to fibonacciRatios, toleranceArg defaults to 0.01, \
nStepsArg defaults to 32, nextPricesCountArg defaults to 8.";

countTouchesByRegion::usage =
  "countTouchesByRegion[fibTouchesArg, fromIndexArg, toIndexArg] partitions " <>
  "the touches in a scan result into three regions based on CandlestickIndex: " <>
  "PreTouches (index < fromIndexArg), InTouches (fromIndexArg <= index <= " <>
  "toIndexArg, with the tautological FromIndex-extreme touch subtracted), and " <>
  "PostTouches (index > toIndexArg). Also returns InTouchesRaw (interior count " <>
  "before subtraction). Expects fibTouchesArg as the Association form returned " <>
  "by scanFibonacciTouches under the \"FibTouches\" key.\n\n" <>
  "The subtraction removes exactly ONE touch: the FromIndex-extreme (Low for an " <>
  "Up segment, High for a Down segment) at fib ratio 0, which is tautological at " <>
  "every extension ratio because fib ratio 0 is defined as fromPrice. The " <>
  "ToIndex-extreme touch at fib ratio 1.0 is NOT subtracted; although it is " <>
  "guaranteed to land on the \"1.0\" fib line when extRatio == 1.0, at any other " <>
  "extension that line corresponds to a hypothetical extended endpoint, not the " <>
  "actual toPrice, so the touch is a real coincidence worth counting. Keeping " <>
  "the ToIndex touch preserves cross-extension symmetry in the lift comparison. " <>
  "Supersedes an earlier convention that subtracted 2 at extRatio == 1.0.";

scanTrendlineTouchesForSegment::usage =
  "scanTrendlineTouchesForSegment[desiredSegmentsArg, candlesticksArg, \
fromIndexArg, toIndexArg, toleranceArg, nStepsArg] calls scanTrendlineTouches \
on each segment from fromIndexArg+1 through toIndexArg (all starting at \
fromIndexArg). Segments for which no matching edge exists in desiredSegmentsArg \
are filtered out (scanTrendlineTouches returns <||> for those, so the result \
has no \"ScanResults\" field and is excluded). The returned Association is keyed \
by {fromIndexArg, toIdx} and pre-sorted by MaxTouches descending so segments \
with the highest peak touch count appear first.";

getFibonacciTouchesForSegmentAtExtension::usage =
  "getFibonacciTouchesForSegmentAtExtension[featureSetArg, segmentArg, extensionRatioArg, opts] " <>
  "computes and returns the FibTouches Association for the given segment at the given single " <>
  "extension ratio. Does NOT scan multiple extensions. Returns an Association keyed by Fibonacci " <>
  "ratios (only those with TouchCount > 0), where each value is an Association with keys " <>
  "TouchCount and TouchDetails. " <>
  "Options: ExtraOnLeft (default 0), ExtraOnRight (default 0), Tolerance (default 0.01), " <>
  "FibRatios (default Automatic -> fibonacciRatios).";
  
liftFunctionInTouchesOnly::usage =
  "liftFunctionInTouchesOnly[liftArgs_Association] computes lift using only InTouches. " <>
  "Expected hits = InCandleCount * PriceTypesCount * hitProbability, where " <>
  "hitProbability = (number of Fib lines with ratio in [0,1]) * 2 * tolerance. " <>
  "At extRatio == 1.0, subtracts 2 from ExpectedHits to match InTouches extreme-touch subtraction. " <>
    "Returns {observedHits, expectedHits, liftValue}.";
    
liftFunctionPrePlusIn::usage =
  "liftFunctionPrePlusIn[liftArgsArg] computes a lift value using the combined " <>
  "count of PreTouches and InTouches as the observed signal, against the " <>
  "corresponding expected count from the pre-segment and in-segment candle " <>
  "ranges. Rewards segments whose setup region (candles before FromIndex) also " <>
  "shows Fibonacci touch activity. Follows the standard liftFunction contract: " <>
  "takes liftArgs_Association, returns {observedHits, expectedHits, liftValue}. " <>
  "InTouches is already adjusted (-1 for the FromIndex tautological touch); the " <>
  "expected side applies the same -1 to stay apples-to-apples.";    
  
  liftFunctionWeightedByPriceType::usage =
  "liftFunctionWeightedByPriceType[liftArgs_Association] computes lift " <>
  "(weighted observed / weighted expected) with each touch weighted by " <>
  "its price type, using the global $priceTypeWeights Association " <>
  "(default weight 1.0 for unlisted types). Reads these standard liftArgs " <>
  "keys: \"InCandleCount\", \"Tolerance\", \"FibRatios\", \"PriceTypes\", " <>
  "\"AllTouchDetails\", \"FromIdx\", \"ToIdx\", and \"Segment\" (for its " <>
  "\"Direction\" field). Observed = Total[weight[t.PriceType]] across " <>
  "in-segment touches (filtered from AllTouchDetails by " <>
  "FromIdx <= CandlestickIndex <= ToIdx). Expected = InCandleCount * " <>
  "sum(weight[pt] for pt in PriceTypes) * numFibLines * 2 * Tolerance, " <>
  "where numFibLines counts fibRatios in [0, 1]. One tautological " <>
  "FromIndex-extreme hit (Low for \"U\" segments, High for \"D\") is " <>
  "subtracted from expected (floored at 0) to match the InTouches -1 " <>
  "convention. Returns {observedHits, expectedHits, liftValue} per the " <>
  "standard lift-function contract; liftValue is observedHits/expectedHits " <>
  "(Missing[\"ZeroExpected\"] when expected is 0). Plug into " <>
  "scanFibonacciTouches via `\"LiftFunction\" -> liftFunctionWeightedByPriceType`.";

$defaultLiftFunction::usage =
  "$defaultLiftFunction is the global default lift function used by " <>
  "computeLiftAtRatio and other functions when no explicit LiftFunction " <>
  "option is specified. Can be rebound via " <>
  "Block[{$defaultLiftFunction = myFn}, ...] for scoped experimentation. " <>
  "Defaults to liftFunctionInTouchesOnly.";
  
scanFibonacciTouches::usage =
  "scanFibonacciTouches[featureSetArg, segmentArg, fibRatiosArg, " <>
  "toleranceArg, startingExtensionArg, endingExtensionArg, opts] " <>
  "scans Fibonacci touches across a grid of extension ratios from " <>
  "startingExtensionArg (default 1.0) to endingExtensionArg " <>
  "(default: next multiple of toleranceArg at or above " <>
  "If[ValueQ[Global`defaultMaxExtension], Global`defaultMaxExtension, 1.31]), " <>
  "stepping by toleranceArg. The candlestick table is derived internally " <>
  "from featureSetArg[\"Candlesticks\"]. Returns an Association with " <>
  "ScanResults keyed by NormalizedRatio. Extensions are built directly " <>
  "as a clean grid (not derived from scan prices) to guarantee exact key " <>
  "values like 1.0.";
  
candlestickAsRatiosToSegment::usage =
  "candlestickAsRatiosToSegment[segmentArg, candlestickArg, fibRatiosArg] expresses the O,H,L,C prices \
of candlestickArg as ratios relative to segmentArg, where ratio = \
(price - FromPrice) / Range. Returns an Association with keys Open, High, Low, Close \
containing the normalized ratio values. A ratio of 0 corresponds to FromPrice, 1 to ToPrice, \
values above 1 are extensions beyond ToPrice.";

ratioToSegmentAsPrice::usage =
  "ratioToSegmentAsPrice[segmentArg, ratioArg] converts a normalized ratio back to an \
absolute price relative to segmentArg. Returns FromPrice + ratioArg * Range. \
Inverse of candlestickAsRatiosToSegment.";

predictPrice::usage =
  "predictPrice[segmentArg, candlestickArg, priceTypeArg, fibRatioArg, \
toleranceArg] predicts the ending price of a virtual segment starting at \
segmentArg's FromPrice such that candlestickArg's priceTypeArg value \
would sit at the fibRatioArg Fibonacci level of that segment. \
priceTypeArg must be one of \"Open\", \"High\", \"Low\", or \"Close\". \
toleranceArg (default 0.01) is applied as a fraction of the delta \
between the candlestick's chosen price and FromPrice; the returned \
prediction is a CenteredInterval centered on the predicted price with \
that fractional half-width. The underlying relationship is \
predictedPrice = FromPrice + (candlestickPrice - FromPrice) / fibRatioArg, \
which solves the Fibonacci-level equation for the segment endpoint \
that would place the candlestick price exactly on the given ratio.";

predictionsForSegment::usage =
  "predictionsForSegment[targetSegmentArg, candlesticksArg, priceTypesArg, \
fibonacciRatiosArg, toleranceArg] returns a three-level nested Association \
of price predictions for targetSegmentArg. candlesticksArg must be a \
Tabular. Iterates over each candlestick index from FromIndex to ToIndex \
(inclusive), each price type in priceTypesArg (default {\"Open\", \
\"High\", \"Low\", \"Close\"}), and each Fibonacci ratio in \
fibonacciRatiosArg (default fibonacciRatios). toleranceArg (default \
0.01) is forwarded to predictPrice. Keys: outer = candlestick index, \
middle = price type, inner = Fibonacci ratio. Each leaf value is the \
result of predictPrice[targetSegmentArg, candlesticksArg[[idx]], \
priceType, fibRatio, toleranceArg].";

collectBands::usage =
  "collectBands[expr] returns a flat list of all CenteredInterval objects \
contained in expr at any depth. Typically used to extract prediction bands \
from the nested Association returned by predictionsForSegment.";

intervalBounds::usage =
  "intervalBounds[band] returns {min, max} for a CenteredInterval. For a \
plain numeric value, returns {n, n}. Used internally by unionOfBands and \
bandsToPDF to extract band endpoints regardless of CenteredInterval's \
opaque internal representation.";

unionOfBands::usage =
  "unionOfBands[bandsList, opts] computes the coverage step function for a \
list of CenteredInterval bands. Returns a list of {x1, x2, count} triples, \
where each triple represents a price interval [x1, x2] over which the \
number of bands containing every point is constant. Consecutive triples \
partition the entire range from the lowest band minimum to the highest \
band maximum, including zero-count gaps between disjoint band clusters. \
Option \"ClearDegenerateBands\" (default True) removes bands of width \
less than 0.01 before computing the step function.";

plotUnionOfBands::usage =
  "plotUnionOfBands[bandsList, opts] (or plotUnionOfBands[bandsList, \
candlesticksArg, targetSegmentArg, opts] to overlay next-candle OHLC \
lines) returns a ListLinePlot displaying the band coverage step \
function. The x-axis is price, the y-axis is the number of bands \
covering that price. Adjacent constant-count intervals are connected \
with vertical drop lines. With candlesticksArg and targetSegmentArg \
supplied and \"ShowNextCandle\" -> True (default), the Open, High, Low, \
and Close of the candlestick at targetSegmentArg[\"ToIndex\"]+1 are \
drawn as colored vertical reference lines. Option \
\"ClearDegenerateBands\" (default True) removes bands of width less \
than 0.01 before plotting.";

filterDegenerateBands::usage =
  "filterDegenerateBands[bandsList, minWidthArg] returns the subset of \
bandsList whose total width is at least minWidthArg (default 0.01, one \
penny). Used to remove degenerate predictions where the candlestick's \
chosen price already sits at the segment's FromPrice and the resulting \
band has effectively zero radius.";

sigmaFactorForConfidence::usage =
  "sigmaFactorForConfidence[confidenceArg] returns the multiplier that, when \
applied to a Gaussian's radius (half-width of the credible interval), produces \
the standard deviation needed to represent confidenceArg of the probability \
mass within that radius. For example, sigmaFactorForConfidence[0.95] returns \
approximately 0.51, meaning sigma = 0.51 * radius gives a Gaussian where 95% \
of the mass lies within +/- radius of the center. Default confidenceArg is \
0.95.";

summarizeBandsPDF::usage =
  "summarizeBandsPDF[bandsList, desiredLevelOfConfidenceArg, opts] returns \
an Association summarizing the Gaussian-mixture PDF derived from bandsList. \
desiredLevelOfConfidenceArg (default 0.95) determines how each band's \
radius maps to a Gaussian standard deviation (see bandsToPDF). Fields: \
GlobalMaximum (the {densityValue, price} of the highest peak), LocalMaxima \
(list of {densityValue, price} pairs for every local peak, sorted by \
density descending), ExpectedValue (the probability-weighted mean price), \
and CredibleInterval (the bounding prices that contain CredibleMass of \
the total probability). Options: \"ClearDegenerateBands\" (default True), \
\"CredibleMass\" (default 0.9, the mass for the reported CredibleInterval), \
\"SamplePoints\" (default 2000).";

bandsToPDF::usage =
  "bandsToPDF[bandsList, desiredLevelOfConfidenceArg, opts] returns a pure \
function pdf such that pdf[x] is the value of the Gaussian-mixture \
probability density at price x. Each band contributes one Gaussian \
component centered at the band's center. The Gaussian's standard deviation \
is set so that desiredLevelOfConfidenceArg of its probability mass falls \
within the band's stated radius. For example, 0.95 (default) means each \
band represents a 95% credible interval; 0.99 means each band represents a \
99% credible interval (tighter Gaussian). Option \"ClearDegenerateBands\" \
(default True) removes bands of width less than 0.01 before building the \
mixture. The returned pdf integrates to 1.";

plotBandsPDF::usage =
  "plotBandsPDF[bandsList, desiredLevelOfConfidenceArg, opts] (or \
plotBandsPDF[bandsList, desiredLevelOfConfidenceArg, candlesticksArg, \
targetSegmentArg, opts] to overlay next-candle OHLC lines) plots the \
Gaussian-mixture probability density function derived from bandsList. The \
x-axis is price, the y-axis is probability density. \
desiredLevelOfConfidenceArg (default 0.95) sets the Gaussian width per \
band so the radius corresponds to that confidence level. With \
candlesticksArg and targetSegmentArg supplied and \"ShowNextCandle\" -> \
True (default), the Open, High, Low, and Close of the candlestick at \
targetSegmentArg[\"ToIndex\"]+1 are drawn as colored vertical reference \
lines with O/H/L/C labels at the top. Option \"ClearDegenerateBands\" \
(default True) removes bands of width less than 0.01 before plotting.";



summarizeBandsPDF::usage =
  "summarizeBandsPDF[bandsList, sigmaFactorArg, opts] returns an Association \
summarizing the Gaussian-mixture PDF derived from bandsList. Fields: \
GlobalMaximum (the {densityValue, price} of the highest peak), LocalMaxima \
(list of {densityValue, price} for every local peak), ExpectedValue (the \
probability-weighted mean price), CredibleInterval (the 90% credible price \
range computed from the cumulative distribution). Option \
\"ClearDegenerateBands\" (default True) and \"CredibleMass\" (default 0.9) \
control filtering and the credible interval width.";

plotBandsPDFAnnotated::usage =
  "plotBandsPDFAnnotated[bandsList, desiredLevelOfConfidenceArg, opts] \
plots the Gaussian-mixture PDF together with markers for the global \
maximum (yellow vertical line), local maxima (orange dashed), expected \
value (magenta dot-dashed), and credible interval bounds (white dashed). \
desiredLevelOfConfidenceArg (default 0.95) sets the Gaussian width per \
band so the radius corresponds to that confidence level. Same options \
as summarizeBandsPDF, plus \"ShowSummaryMarkers\".";

plotBandsPDFAnnotatedEx::usage =
  "plotBandsPDFAnnotatedEx[segmentArg, touchPointsArg, candlesticksArg, \
bandsList, desiredLevelOfConfidenceArg, imageSizeArg, opts] plots the \
Gaussian-mixture PDF for bandsList together with segment annotations. \
Includes dashed vertical lines at segmentArg's FromPrice and ToPrice \
(green for uptrend, red for downtrend), an arrow connecting them, a \
yellow line at the Open price of the candlestick at \
segmentArg[\"ToIndex\"]+1, a horizontal candlestick glyph of that next \
bar, and an interpolated curve through the local PDF peaks. Inherits all \
summary markers from summarizeBandsPDF (global max yellow, local maxima \
orange dashed, expected value magenta dot-dashed, credible interval \
white dashed). desiredLevelOfConfidenceArg defaults to 0.95, \
imageSizeArg defaults to 800. If segmentArg is the empty Association, \
returns Null without plotting. Same options as summarizeBandsPDF.";

plotCandlesticksWithPDF::usage =
  "plotCandlesticksWithPDF[candlesticksArg, fromIdxArg, toIdxArg, bandsList, \
sigmaFactorArg, opts] renders a candlestick display alongside a rotated PDF \
panel. The candlesticks occupy the left ~80%; the PDF curve runs vertically \
on the right, using the same price scale on its y-axis, with density values \
extending horizontally to the right. Use to visualize where prediction mass \
concentrates relative to actual price action.";

horizontalCandlestick::usage =
  "horizontalCandlestick[candleArg, yPosArg, halfWidthArg, wickThicknessArg] draws a \
single candlestick horizontally at vertical position yPosArg. The x-axis represents \
price with Low on the left and High on the right. The body is a hollow rectangle from \
Min[Open,Close] to Max[Open,Close], with wicks extending to Low and High. Green for \
bullish (Close >= Open), red for bearish. halfWidthArg controls the body height \
(default 0.3), wickThicknessArg controls wick thickness (default 0.003). \
horizontalCandlestick[candlesArg, yMinArg, yMaxArg, halfWidthArg, wickThicknessArg] \
draws a list of candlesticks evenly spaced from yMaxArg (first/oldest) to yMinArg \
(last/newest), representing a candlestick chart rotated 90 degrees clockwise.";

appendRow::usage =
  "appendRow[tab_Tabular, row_Association] returns a new Tabular with row \
appended to tab. row must be an Association whose keys match tab's column \
names. Internally wraps row in Tabular[{row}] and Joins it to tab, since \
Append does not work directly on Tabular objects.";

appendRows::usage =
  "appendRows[tab_Tabular, rows_List] returns a new Tabular with all rows \
appended to tab. rows must be a list of Associations whose keys match tab's \
column names. Internally wraps rows in Tabular[rows] and Joins them to tab, \
since Append does not work directly on Tabular objects.";

theEmptyFeatureSet::usage =
  "theEmptyFeatureSet is a constant Dataset to be filled by the updateFeatureSet pipeline.";
  
candlestickFibonacciGoodness::usage =
  "candlestickFibonacciGoodness[candlesticksArg, fibonacciPricesArg, \
rangeArg, opts] returns a goodness score in [0, 1] measuring how well the \
Open/High/Low/Close prices of the given candlesticks align with the given \
Fibonacci prices. For each candlestick price, computes a similarity score \
via a distance-based kernel (best-match Fibonacci is used). The scores \
are averaged across all considered prices. 1.0 means all prices exactly \
on Fibonacci levels; 0.0 means all prices far from any Fibonacci level. \
Options: \
\"Kernel\" -> \"Gaussian\" (default) | \"Cauchy\" | \"Bump\" \[LongDash] kernel \
family. Gaussian has smooth infinite tails; Cauchy has heavier tails; \
Bump has compact support (exactly 0 beyond width). \
\"KernelWidth\" -> 0.005 (default, matching 0.5% tolerance) \[LongDash] the \
distance (as fraction of rangeArg) where the kernel falls to a \
characteristic value (Gaussian sigma, Cauchy gamma, Bump support width). \
\"PriceTypes\" -> {\"Open\", \"High\", \"Low\", \"Close\"} \[LongDash] subset of \
price types to include in scoring.";

candlestickFibonacciGoodnessDetail::usage =
  "candlestickFibonacciGoodnessDetail[candlesticksArg, fibonacciPricesArg, \
rangeArg, opts] returns an Association with GoodnessScore (same value as \
candlestickFibonacciGoodness), PriceScores (list of per-price kernel \
values), and BestMatches (list of the best-matching Fibonacci price for \
each candlestick price). Useful for diagnostics and understanding why a \
particular segment scored high or low.";

bestFibonacciSegmentByGoodness::usage =
  "bestFibonacciSegmentByGoodness[desiredSegmentsArg, candlesticksArg, \
fromIndexArg, toIndexArg, fibonacciRatiosArg, opts] iterates over each \
segment from fromIndexArg+1 through toIndexArg (all starting at \
fromIndexArg), computes the Fibonacci goodness score for the candlesticks \
inside each segment, and returns the results sorted by goodness \
descending. Fibonacci prices are computed per segment as \
FromPrice + ratio * (ToPrice - FromPrice) for each ratio in \
fibonacciRatiosArg. Returns an Association keyed by \
{fromIndexArg, toIdx} where each value is <|Goodness, Segment, \
FibonacciPrices|>. Options are forwarded to candlestickFibonacciGoodness \
(\"Kernel\", \"KernelWidth\", \"PriceTypes\"). Segments for which no \
matching edge exists in desiredSegmentsArg are filtered out.";


(* Segment goodness computations *)

segmentGoodness::usage =
  "segmentGoodness[segmentArg, candlesticksArg, fibonacciRatiosArg, opts] \
computes the Fibonacci goodness score for a segment, using ALL candlesticks \
from segmentArg[\"FromIndex\"] through segmentArg[\"ToIndex\"] (inclusive). \
The Fibonacci prices are computed per segment as FromPrice + ratio * Range \
for each ratio in fibonacciRatiosArg. Returns a scalar goodness value in \
[0, 1] where 1.0 means every OHLC of every candlestick lands exactly on a \
Fibonacci level and 0.0 means all prices are far from any Fibonacci level. \
Options are forwarded to candlestickFibonacciGoodness (\"Kernel\", \
\"KernelWidth\", \"PriceTypes\").";

bestFibonacciSegmentByGoodness::usage =
  "bestFibonacciSegmentByGoodness[desiredSegmentsArg, candlesticksArg, \
fromIndexArg, toIndexArg, fibonacciRatiosArg, opts] iterates over each \
segment from fromIndexArg+1 through toIndexArg (all starting at \
fromIndexArg), computes the Fibonacci goodness score for the segment \
using ALL candlesticks from FromIndex through ToIndex, and returns the \
results sorted by goodness descending. Fibonacci prices are computed per \
segment as FromPrice + ratio * (ToPrice - FromPrice) for each ratio in \
fibonacciRatiosArg. Returns an Association keyed by {fromIndexArg, toIdx} \
where each value is <|Goodness, Segment, FibonacciPrices|>. Options are \
forwarded to candlestickFibonacciGoodness (\"Kernel\", \"KernelWidth\", \
\"PriceTypes\"). Segments for which no matching edge exists in \
desiredSegmentsArg are filtered out.";

segmentsGoodnessStatistics::usage =
  "segmentsGoodnessStatistics[segmentsArg, candlesticksArg, \
fibonacciRatiosArg, opts] returns statistics over all segments in \
segmentsArg. For each segment, the goodness is computed as the Fibonacci \
goodness of ALL candlesticks from FromIndex through ToIndex (see \
segmentGoodness). Returns an Association with: Count, Mean, Median, \
StandardDeviation, Min, Max, Quartiles, Best (highest-scoring segment \
with its score), Worst (lowest), Histogram (per-decile bucket counts), \
and Details (a Tabular of per-segment goodness sorted descending). \
segmentsArg can be a List of segment Associations or a Tabular with the \
same fields as scanFibonacciTouches output.";

segmentGoodnessCurve::usage =
  "segmentGoodnessCurve[segmentArg, candlesticksArg, fibonacciRatiosArg, opts] \
scans candidate 'ToPrice' values from FromPrice through FromPrice + range * \
upperMultiplier and returns goodness scores at each. For each scanPrice, the \
effective range used to compute Fibonacci prices is: (scanPrice - FromPrice) \
if scanPrice is beyond the original ToPrice in the trend direction, or the \
original (ToPrice - FromPrice) if scanPrice lies within [FromPrice, ToPrice]. \
Goodness is scored over candlesticks [FromIndex .. ToIndex] via \
candlestickFibonacciGoodness. Returns a list of {ScanPrice, Goodness} pairs \
sorted by ScanPrice. \
Options: \"NumScanPoints\" -> 64 (default), \"UpperMultiplier\" -> 1.31 \
(default), plus \"Kernel\", \"KernelWidth\", \"PriceTypes\" passed to \
candlestickFibonacciGoodness.";

plotSegmentGoodnessCurve::usage =
  "plotSegmentGoodnessCurve[segmentArg, candlesticksArg, fibonacciRatiosArg, opts] \
computes segmentGoodnessCurve for the given segment and returns a \
ListLinePlot with goodness on the y-axis and price on the x-axis. \
Vertical markers indicate the original FromPrice and ToPrice. Options \
are forwarded to segmentGoodnessCurve.";

scanSegmentGoodnessByEndpoint::usage =
  "scanSegmentGoodnessByEndpoint[segmentArg, candlesticksArg, \
fibonacciRatiosArg, numExtraCandlesArg, opts] scans candidate endpoint \
prices for a hypothetical extension of segmentArg by numExtraCandlesArg \
candlesticks, and returns goodness scores for each candidate. The extended \
segment runs from (FromIndex, FromPrice) to (ToIndex+numExtraCandlesArg, \
scanPrice). For each scanPrice, the effective range used to compute \
Fibonacci prices is: (scanPrice - FromPrice) if scanPrice is beyond ToPrice \
in the trend direction, or the original (ToPrice - FromPrice) if scanPrice \
lies within [FromPrice, ToPrice]. Goodness is scored over candlesticks \
[FromIndex .. ToIndex+numExtraCandlesArg] via candlestickFibonacciGoodness. \
For an uptrend (Direction \"U\"), scan runs upward from ToPrice to \
FromPrice + Range * upperMultiplier. For a downtrend, scan runs downward. \
Also compares against the ACTUAL endpoint: for an uptrend, uses the maximum \
High across candlesticks [ToIndex+1 .. ToIndex+numExtraCandlesArg] (or 0 \
goodness if that max is a reversal - lower than the original ToPrice); for a \
downtrend, uses the minimum Low (or 0 if higher than ToPrice). \
Arguments: segmentArg (required), candlesticksArg (required), \
fibonacciRatiosArg (default fibonacciRatios), numExtraCandlesArg (default 1). \
Options: \"UpperMultiplier\" -> 1.618 (default), \"NumScanPoints\" -> 64 \
(default), plus \"Kernel\", \"KernelWidth\", \"PriceTypes\" passed to \
candlestickFibonacciGoodness. Returns an Association with: ScanResults, \
BestScanPrice, BestScanGoodness, ActualPrice, ActualGoodness, IsReversal, \
OriginalToPrice, OriginalRange, NumExtraCandles.";

findShortestOppositeSegmentEndingAt::usage =
  "findShortestOppositeSegmentEndingAt[segmentsArg, toIndexArg, directionArg] \
returns the segment from segmentsArg with the smallest \
(ToIndex - FromIndex) span whose ToIndex equals toIndexArg and whose \
Direction is opposite to directionArg (D if directionArg is U, and vice \
versa). Returns Missing[\"NotFound\"] if no such segment exists. \
segmentsArg can be a Tabular or a list of Associations.";

findLongestOppositeSegmentEndingAt::usage =
  "findLongestOppositeSegmentEndingAt[segmentsArg, toIndexArg, directionArg] " <>
  "returns the LONGEST segment with Direction opposite to directionArg whose " <>
  "ToIndex equals toIndexArg (i.e., ending at the same bar). Returns " <>
  "Missing[\"NotFound\"] if no such segment exists.";

segmentGoodnessWithContext::usage =
  "segmentGoodnessWithContext[featureSetArg, segmentArg, fibonacciRatiosArg, opts] \
computes a normalized Fibonacci goodness score for a segment, using ALL \
candlesticks from the shortest prior opposite-direction segment's FromIndex \
through segmentArg[\"ToIndex\"] (inclusive). The shared candlestick at the \
segments' boundary is counted exactly once. The Fibonacci prices are \
computed based on the CURRENT segment's FromPrice and ToPrice. The raw \
kernel scores are averaged over ALL prices in the extended range, then \
normalized by (number of candlesticks in the current segment) / (number of \
candlesticks in the extended range) so that segments of the same length \
are scored comparably regardless of the length of their prior opposite \
segment. Returns an Association with: Goodness (normalized score), \
RawGoodness (unnormalized average), PriorSegmentFromIndex, \
PriorSegmentToIndex, PriorSegmentFound (bool), CandlesticksScored (count), \
CurrentSegmentCandlesticks (count). Options are forwarded to \
candlestickFibonacciGoodness (\"Kernel\", \"KernelWidth\", \"PriceTypes\").";



binomialUpperTail::usage =
  "binomialUpperTail[m, p, h] returns Pr(H >= h) where H ~ Binomial[m, p]. \
This is the one-tailed p-value for observing h or more successes under the \
null hypothesis of independent probability-p Bernoulli trials.";

fisherCombinedPValue::usage =
  "fisherCombinedPValue[pValuesArg] combines a list of independent p-values \
using Fisher's method: computes chi-squared statistic -2 Sum[Log[p_i]] with \
2k degrees of freedom (k = number of tests). Returns the combined p-value. \
Very small p-values are floored at 1e-300 to avoid underflow.";

computeFibonacciSegmentSignificance::usage =
  "computeFibonacciSegmentSignificance[featureSetArg, currentSegmentArg, \
scanPriceArg, fibonacciRatiosArg, toleranceArg, opts] computes a combined \
statistical significance for a hypothetical extended current segment with \
endpoint scanPriceArg. Two independent Binomial tests are run: one over the \
candlestick prices in the current segment (excluding its two trivial-hit \
endpoints), and one over the candlestick prices in the shortest prior \
opposite-direction segment ending at currentSegmentArg[\"FromIndex\"]. \
Both tests use the SAME Fibonacci prices, derived from the current segment's \
FromPrice and scanPriceArg, and the SAME null-hypothesis price universe \
(union of prev+current segment price extents). Each test yields a one-tailed \
upper-tail p-value; the two are combined via Fisher's method. \
Returns an Association with: Significance (combined p-value; smaller = more \
significant), CurrentPValue, PrevPValue, CurrentHits, CurrentPriceCount, \
PrevHits, PrevPriceCount, HitProbability, FibonacciPrices, \
PriceUniverseLength, BandWidth, PriorSegmentFound. \
Options: \"ExtendedToIndex\" -> Automatic (uses currentSegmentArg[\"ToIndex\"], \
i.e. only endpoint price is hypothesized; pass an integer to also extend in time). \
\"PriceTypes\" -> {\"Open\", \"High\", \"Low\", \"Close\"}.";

scanSegmentSignificance::usage =
  "scanSegmentSignificance[featureSetArg, currentSegmentArg, \
fibonacciRatiosArg, toleranceArg, opts] scans hypothetical scan prices from \
FromPrice through FromPrice + range * upperMultiplier and computes the \
combined significance at each. For scan prices within [FromPrice, ToPrice] \
the original segment range is used; for prices beyond, the extended range \
is used. \
Returns an Association with: ScanResults (Tabular of {ScanPrice, \
Significance, CurrentPValue, PrevPValue, CurrentHits, PrevHits}), \
BestScanPrice (lowest p-value = most significant), BestSignificance, \
OriginalToPrice, OriginalRange, PriorSegmentFromIndex, PriorSegmentToIndex, \
PriorSegmentFound. \
Options: \"NumScanPoints\" -> 64 (default), \"UpperMultiplier\" -> 1.31 \
(default), \"ExtendedToIndex\" -> Automatic, \"PriceTypes\" -> \
{\"Open\", \"High\", \"Low\", \"Close\"}.";

significanceByRangeRatio::usage =
  "significanceByRangeRatio[featureSetArg, currentSegmentArg, \
fibonacciRatiosArg, toleranceArg, opts] scans hypothetical extensions of \
the current segment from range ratio 1.0 (unextended, at the original \
ToPrice) up to MaxRatio (default 1.31 via defaultMaxExtension, 32 points of step 0.01) and returns an \
Association keyed by range ratio, mapping to the combined statistical \
significance at that ratio. \
For each ratio r, the hypothetical scanPrice is FromPrice + r * originalRange, \
Fibonacci prices are recomputed from the new range, and the combined \
significance (Fisher's method over current+prior buckets) is computed via \
computeFibonacciSegmentSignificance. \
The result <| ratio -> significance |> shows how likely the next extreme \
(next candle's High for Up, next candle's Low for Down) would cluster near \
each extension ratio if Fibonacci structure is a true signal. Peaks of \
significance (very small p-values) suggest the most likely target prices. \
Returns an Association with: SignificanceByRatio (Association <| ratio -> \
significance |>), BestRatio (ratio with lowest p-value = most significant), \
BestSignificance, BestScanPrice, OriginalToPrice, OriginalRange, \
PriorSegmentFound, PriorSegmentFromIndex, PriorSegmentToIndex, MaxRatio, \
NumScanPoints, ScanDetails (Tabular of {RangeRatio, ScanPrice, Significance, \
CurrentPValue, PrevPValue, CurrentHits, PrevHits} sorted by ratio). \
Options: \"MaxRatio\" -> 1.31 (defaultMaxExtension), \"MinRatio\" -> 1.0, \
\"NumScanPoints\" -> 32, \"ExtendedToIndex\" -> Automatic, \"PriceTypes\" \
-> {\"Open\", \"High\", \"Low\", \"Close\"}.";

computeFibonacciLift::usage =
  "computeFibonacciLift[featureSetArg, currentSegmentArg, scanPriceArg, \
fibonacciRatiosArg, toleranceArg, opts] computes an observed/expected lift \
score for a segment's Fibonacci hit count. Returns an Association with: \
Lift (observed / expected \[LongDash] 1.0 means exactly random, >1 means more clustering \
than random, <1 means less), CurrentLift, PrevLift, CombinedLift (weighted \
by sample size), ObservedTotal, ExpectedTotal, CurrentHits, CurrentExpected, \
PrevHits, PrevExpected, HitProbability. \
Uses the same hit-counting logic as computeFibonacciSegmentSignificance \
(exclude trivial endpoints, use shortest prior opposite segment). \
Options: same as computeFibonacciSegmentSignificance.";

liftByRangeRatio::usage =
  "liftByRangeRatio[featureSetArg, currentSegmentArg, fibonacciRatiosArg, \
toleranceArg, opts] scans hypothetical extensions from range ratio MinRatio \
to MaxRatio and returns lift (observed/expected) at each. Peaks in the lift \
curve indicate range ratios where the Fibonacci fit is most enhanced beyond \
random expectation. Complement to significanceByRangeRatio, which reports \
p-values. \
Returns Association with: LiftByRatio (Association <| ratio -> lift |>), \
BestRatio, BestLift, BestScanPrice, ScanDetails (Tabular with columns \
RangeRatio, ScanPrice, Lift, ObservedTotal, ExpectedTotal, HitProbability, \
Significance). \
Options: \"MinRatio\" -> 1.0, \"MaxRatio\" -> 1.31 (defaultMaxExtension), \
\"NumScanPoints\" -> 32, plus all options of computeFibonacciLift.";

computeTouchCountsForSegmentAtExtension::usage =
  "computeTouchCountsForSegmentAtExtension[featureSetArg_Dataset, segmentArg_Association, \
fibonacciRatiosArg_List, extensionRatioArg_?NumericQ, touchToleranceArg_?NumericQ, \
touchPriceTypesArg_List, extraOnRightArg_Integer] returns an Association with \
three touch counts for a segment at an extension ratio: PreTouches (touches on \
the prior opposite-direction segment's candles from priorFromIdx to \
priorToIdx-1, exclusive of the shared boundary); InTouches (touches on the \
current segment's candles from FromIndex to ToIndex, minus 2 to exclude the \
two always-present extreme touches at FromPrice and ToPrice); and PostTouches \
(touches on extra candles from ToIndex+1 through ToIndex+extraOnRight, clamped \
to totalCandles). Also returns the candle counts for each range and the \
Fibonacci price levels for reference. Assumes TouchPriceTypes includes \
\"Low\" and \"High\" (always true for segments going High-to-Low or \
Low-to-High).";


findLongestSegmentEndingAt::usage =
  "findLongestSegmentEndingAt[segmentsArg, idxArg] returns the segment " <>
  "in segmentsArg (a Segments Tabular or list) with the largest DeltaX " <>
  "whose ToIndex equals idxArg. Returns Missing[\"NoSegment\"] if no " <>
  "segment ends at idxArg. Direction-agnostic: picks the longest among " <>
  "both U and D segments ending at the clicked candlestick.";
  
  findSegmentsEndingAt::usage =
  "findSegmentsEndingAt[segmentsArg, idxArg] returns all segments in " <>
  "segmentsArg (a Segments Tabular or list of Associations) whose " <>
  "ToIndex equals idxArg, sorted by DeltaX ascending (shortest first). " <>
  "Direction-agnostic. Returns {} if no segments end at idxArg.";


flattenFibTouches::usage =
  "flattenFibTouches[fibTouchesArg] takes the Association form of FibTouches " <>
  "returned by scanFibonacciTouches (keyed by fib ratio, each value an Association " <>
  "with TouchCount and TouchDetails) and returns a flat list of touch records, " <>
  "one per touch, with the fib ratio prepended as a \"FibRatio\" field. Useful " <>
  "for Dataset rendering without inner-list truncation.";


Get[FileNameJoin[{DirectoryName[$InputFileName], "Fibonacci.wl"}]];


Begin["`Private`"]

(* ---- how the graph of a feature set is shown: left to right, each vertex labeled with its candlestick index ----
   The graph is rebuilt on every candlestick (Graph[vertices, edges, ...]), and a rebuild drops the options of the old graph,
   so the options are applied at every rebuild from this one list. *)
stockGraphDisplayOptions = {
  VertexLabels        -> Placed["Name", Center],
  VertexShapeFunction -> "Circle",
  VertexSize          -> 0.45,
  VertexStyle         -> RGBColor[0.55, 0.75, 1.],
  VertexLabelStyle    -> Directive[Black, 9],
  GraphLayout         -> {"LayeredDigraphEmbedding", "Orientation" -> Left}
};

(* the colors of the edges: lime up, red down, yellow flat *)
upEdgeColor   = RGBColor[0.45, 1., 0.1];
downEdgeColor = RGBColor[1., 0.2, 0.2];
flatEdgeColor = RGBColor[1., 0.9, 0.1];
edgeDirectionColor[from_?NumericQ, to_?NumericQ] := Which[to > from, upEdgeColor, to < from, downEdgeColor, True, flatEdgeColor];
edgeDirectionColor[seg_Association] := edgeDirectionColor[seg["FromPrice"], seg["ToPrice"]];
(* the weight of an edge of the graph is the complex number a + i b (edgeComplexWeight): a is the index distance of its segment (ToIndex - FromIndex), b its range (ToPrice - FromPrice),
   so the sign of the imaginary part is the direction of the edge; a real weight (an older graph) is its own direction *)
edgeComplexWeight[seg_Association] := Complex[seg["ToIndex"] - seg["FromIndex"], seg["ToPrice"] - seg["FromPrice"]];
edgeWeightColor[w_?NumericQ] := With[{v = If[Head[w] === Complex, Im[w], w]}, Which[v > 0, upEdgeColor, v < 0, downEdgeColor, True, flatEdgeColor]];
stockGraphEdgeStyle[edges_List, weights_List] :=
  If[Length[edges] === Length[weights] && edges =!= {},
    DeleteDuplicates[MapThread[#1 -> edgeWeightColor[#2] &, {edges, weights}]], {}];

(* the one place where the graph of a feature set is rebuilt: vertices, edges and the weight (angle) of each edge, with the display options and the colors of the edges *)
rebuildStockGraph[vertices_List, edges_List, weights_List] :=
  Graph[vertices, edges, EdgeWeight -> weights, DirectedEdges -> True,
    EdgeStyle -> stockGraphEdgeStyle[edges, weights], Sequence @@ stockGraphDisplayOptions];

horizontalStockGraph[g_Graph, opts___?OptionQ] :=
  Module[{w = Quiet[OptionValue[Graph, AbsoluteOptions[g, EdgeWeight], EdgeWeight]]},
    Graph[g, opts, EdgeStyle -> If[ListQ[w], stockGraphEdgeStyle[EdgeList[g], w], {}], Sequence @@ stockGraphDisplayOptions]];
horizontalStockGraph[fs_Dataset, opts___?OptionQ] := horizontalStockGraph[Normal[fs]["Graph"], opts];
horizontalStockGraph[fs_Association, opts___?OptionQ] := horizontalStockGraph[fs["Graph"], opts];

(* OHLC field names used throughout *)
ohlcFields = {"Open", "High", "Low", "Close"};

(* ============================================================================
   Utility
   ============================================================================ *)
Clear[findSegmentsEndingAt];
findSegmentsEndingAt[segmentsArg_, idxArg_Integer] :=
  Module[{segmentsNormal, matching},
    segmentsNormal = If[Head[segmentsArg] === Tabular,
      Normal[segmentsArg],
      segmentsArg
    ];
    (* the wave segments only: the support / resistance rows (Kind Res / Sup) are not segments of the waves *)
    matching = Select[segmentsNormal, #["ToIndex"] === idxArg && ! MemberQ[{"Res", "Sup"}, Lookup[#, "Kind", None]] &];
    SortBy[matching, #["DeltaX"] &]
  ];
  
Clear[findLongestSegmentEndingAt];
findLongestSegmentEndingAt[segmentsArg_, idxArg_Integer] :=
  Module[{segmentsNormal, matching},
    segmentsNormal = If[Head[segmentsArg] === Tabular,
      Normal[segmentsArg],
      segmentsArg
    ];
    matching = Select[segmentsNormal, #["ToIndex"] === idxArg && ! MemberQ[{"Res", "Sup"}, Lookup[#, "Kind", None]] &];
    If[Length[matching] == 0,
      Missing["NoSegment"],
      First[MaximalBy[matching, #["DeltaX"] &]]
    ]
  ];
  
  appendRow[tab_Tabular, row_Association] := Join[tab, Tabular[{row}]];

  appendRows[tab_Tabular, rows_List] :=   Join[tab, Tabular[rows]];

(* Protected log that returns 0 for non-positive inputs *)
myLog[x_] := If[x > 0, Log[x], 0];

(* ============================================================================
   Group 1: Feature Set Initialization
   ============================================================================ *)
theEmptyFeatureSet := <|  
	      "Candlesticks"     -> Tabular[{<|"Open" -> 0., "High" -> 0., "Low" -> 0., "Close" -> 0., "Volume" -> 0|>}][[{}]],
		  "Peaks"            -> Tabular[{<|"Index" -> 0, "High" -> 0.|>}][[{}]],
		  "Valleys"          -> Tabular[{<|"Index" -> 0, "Low"  -> 0.|>}][[{}]],
		  "PeaksAndValleys"  -> Tabular[{<|"Index" -> 0, "High" -> 0., "Low" -> 0., "Type" -> ""|>}][[{}]],
		  "VertexToPeak"     -> Tabular[{<|"Index" -> 0, "High" -> 0.|>}][[{}]],
		  "VertexToValley"   -> Tabular[{<|"Index" -> 0, "Low"  -> 0.|>}][[{}]],
		  "Graphs"           -> {},
		  "Graph"            -> Graph[{}, {}, DirectedEdges -> True],
		  "GraphEdgeWeights" -> <||>,
		  "EdgeTags"         -> <||>,
		  "Segments"         -> Tabular[{<|
							      	"Direction" -> "", 
							      	"DeltaX"    -> 0,
							      	"FromIndex" -> 0,  
							  	    "ToIndex"   -> 0,
							    	"FromPrice" -> 0.,
							      	"ToPrice"   -> 0.,
							      	"Range"     -> 0., 
							      	"LogRatio"  -> 0.,
							      	"Angle"     -> 0., 
							      	"Weight"    -> 0.,
							      	"Line"      -> {}
								  |>}][[{}]],
		  "GraphEx"          -> Graph[{}, {}, DirectedEdges -> True],
		  "EdgeSegments"     -> <||>,
		  "LatestSegments"   -> Tabular[{<|"FromIndex" -> 0, "ToIndex" -> 0|>}][[{}]]
        |>;


(* This function is a helper function to create the starting empty featureSet for the pipeline *)  
Clear[initializeEmptyFeatureSet];
initializeEmptyFeatureSet[] := <|  
	      "Candlesticks"     -> Tabular[{<|"Open" -> 0., "High" -> 0., "Low" -> 0., "Close" -> 0., "Volume" -> 0|>}][[{}]],
		  "Peaks"            -> Tabular[{<|"Index" -> 0, "High" -> 0.|>}][[{}]],
		  "Valleys"          -> Tabular[{<|"Index" -> 0, "Low"  -> 0.|>}][[{}]],
		  "PeaksAndValleys"  -> Tabular[{<|"Index" -> 0, "High" -> 0., "Low" -> 0., "Type" -> ""|>}][[{}]],
		  "VertexToPeak"     -> Tabular[{<|"Index" -> 0, "High" -> 0.|>}][[{}]],
		  "VertexToValley"   -> Tabular[{<|"Index" -> 0, "Low"  -> 0.|>}][[{}]],
		  "Graphs"           -> {},
		  "Graph"            -> Graph[{}, {}, DirectedEdges -> True],
		  "GraphEdgeWeights" -> <||>,
		  "EdgeTags"         -> <||>,
		  "Segments"         -> Tabular[{<|
							      	"Direction" -> "", 
							      	"DeltaX"    -> 0,
							      	"FromIndex" -> 0,  
							  	    "ToIndex"   -> 0,
							    	"FromPrice" -> 0.,
							      	"ToPrice"   -> 0.,
							      	"Range"     -> 0., 
							      	"LogRatio"  -> 0.,
							      	"Angle"     -> 0., 
							      	"Weight"    -> 0.,
							      	"Line"      -> {}
								  |>}][[{}]],
		  "GraphEx"          -> Graph[{}, {}, DirectedEdges -> True],
		  "EdgeSegments"     -> <||>,
		  "LatestSegments"   -> Tabular[{<|"FromIndex" -> 0, "ToIndex" -> 0|>}][[{}]]
        |>

(* Returns an empty Dataset with all required GNN pipeline keys *)
Clear[initializeFeatureSetGNN];

initializeFeatureSetGNN[] := Dataset@<|
  "Candlesticks"     -> Tabular[{}],
  "Graph"            -> Graph[{}, {},
    EdgeWeight        -> {},
    DirectedEdges     -> True,
    EdgeLabels        -> Automatic,
    Sequence @@ stockGraphDisplayOptions
  ],
  "PeaksQ"           -> {},
  "ValleysQ"         -> {},
  "Peaks"            -> {},
  "Valleys"          -> {},
  "ActiveUptrends"   -> {},
  "ActiveDowntrends" -> {},
  "PeakStack"        -> {},
  "ValleyStack"      -> {}
|>;

(* Adds analysis fields with default values to a candlestick Association *)
Clear[addFieldsToCandlestick];

addFieldsToCandlestick[candlestickArg_TabularRow] :=
  AssociateTo[candlestickArg, <|
    "IsPeakQ"          -> False,
    "IsValleyQ"        -> False,
    "EdgeIndices"      -> {},
    "VertexIndexCoords" -> {},
    "EdgeIndexCoords"  -> {},
    "Graph"            -> Graph[{}],
    "Direction"        -> "",
    "EdgeColor"        -> Black
  |>];

(* Helpter function to show all the levels *) 
Clear[flattenFibTouches];  
flattenFibTouches[fibTouchesArg_] :=
  Flatten[
    KeyValueMap[
      Function[{ratio, entry},
        Prepend[#, "FibRatio" -> ratio] & /@ entry["TouchDetails"]
      ],
      fibTouchesArg
    ],
    1
  ];  
      
(* ============================================================================
   Group 2: Core Incremental Pipeline
   ============================================================================ *)

(* Adds a new candlestick to the featureSet, prepending a sequential Index field *)
Clear[addNewCandlestickToFeatureSet];

addNewCandlestickToFeatureSet[featureSetArg_Dataset, newCandlestickArg_TabularRow] :=
  Module[{
    updatedFeatureSet      = featureSetArg,
    tabularCandlesticks,
    numberOfCandlesticks,
    newCandlestickAssociation,
    newCandlestickAsTabular,
    done
    },
    tabularCandlesticks      = updatedFeatureSet["Candlesticks"];
    numberOfCandlesticks     = Length@tabularCandlesticks;
    newCandlestickAssociation = newCandlestickArg // Normal;
    (* Prepend the sequential Index field *)
    PrependTo[newCandlestickAssociation, "Index" -> (numberOfCandlesticks + 1)];
    Label[done];
    newCandlestickAsTabular = Tabular[{newCandlestickAssociation}];
    (* Append to existing Tabular, or initialize if empty *)
    tabularCandlesticks = If[
      Length@tabularCandlesticks == 0,
      newCandlestickAsTabular,
      Join[tabularCandlesticks[[1 ;; -1]], newCandlestickAsTabular]
    ];
    updatedFeatureSet = ReplacePart[updatedFeatureSet, "Candlesticks" -> tabularCandlesticks];
    Return[updatedFeatureSet]
  ];

(* Computes LastHigher and LastLower pointer fields for the most recent candlestick
   using a skip-pointer chain scan for efficiency *)
Clear[findPreviousHigherHighAndLowerLow];
findPreviousHigherHighAndLowerLow[featureSetArg_Dataset] :=
  Module[{
    candlesticksAsTabular,
    candlesticksAsList,
    currentCandlestick,
    indexOfCurrentCandlestick,
    updatedCandlesticks,
    updatedCandlestick,
    updatedFeatureSet,
    numberOfCandlesticks,
    indexOfTheLastHigherCandlestick,
    indexOfTheLastLowerCandlestick
    },
    updatedFeatureSet           = featureSetArg;
    updatedCandlesticks         = candlesticksAsTabular = featureSetArg["Candlesticks"];
    indexOfCurrentCandlestick   = numberOfCandlesticks  = Length@candlesticksAsTabular;
    candlesticksAsList          = candlesticksAsTabular // Normal;
    updatedCandlestick          = currentCandlestick    = Last@candlesticksAsList;
    indexOfTheLastHigherCandlestick = indexOfTheLastLowerCandlestick = 0;
    (* First candlestick: insert LastHigher and LastLower columns initialized to 0 *)
    If[numberOfCandlesticks == 1,
      updatedCandlesticks = InsertColumns[candlesticksAsTabular, {"LastHigher" -> 0, "LastLower" -> 0}];
      updatedFeatureSet   = ReplacePart[updatedFeatureSet, "Candlesticks" -> updatedCandlesticks];
      Return[updatedFeatureSet]
    ];
    (* Second candlestick: set both pointers to 0 *)
    If[numberOfCandlesticks == 2,
      updatedCandlestick  = ReplacePart[updatedCandlestick, {"LastHigher" -> 0, "LastLower" -> 0}];
      updatedCandlesticks = Join[updatedCandlesticks[[1 ;; -2]], Tabular[{updatedCandlestick}]];
      updatedFeatureSet   = ReplacePart[updatedFeatureSet, "Candlesticks" -> updatedCandlesticks];
      Return[updatedFeatureSet]
    ];
    (* Scan backward via skip-pointer chain to find last higher High *)
    Block[{newHigh, scannedCandlestick, scannedHigh, scannedIndex},
      newHigh      = updatedCandlestick["High"];
      scannedIndex = numberOfCandlesticks - 1;
      While[scannedIndex > 0,
        scannedCandlestick = candlesticksAsList[[scannedIndex]];
        scannedHigh        = scannedCandlestick["High"];
        If[newHigh < scannedHigh,
          indexOfTheLastHigherCandlestick = scannedIndex;
          Break[]
        ];
        scannedIndex = scannedCandlestick["LastHigher"];
      ];
    ];
    (* Scan backward via skip-pointer chain to find last lower Low *)
    Block[{newLow, scannedCandlestick, scannedLow, scannedIndex},
      newLow       = updatedCandlestick["Low"];
      scannedIndex = numberOfCandlesticks - 1;
      While[scannedIndex > 0,
        scannedCandlestick = candlesticksAsList[[scannedIndex]];
        scannedLow         = scannedCandlestick["Low"];
        If[newLow > scannedLow,
          indexOfTheLastLowerCandlestick = scannedIndex;
          Break[]
        ];
        scannedIndex = scannedCandlestick["LastLower"];
      ];
    ];
    updatedCandlestick  = ReplacePart[updatedCandlestick,{"LastHigher" -> indexOfTheLastHigherCandlestick,"LastLower"  -> indexOfTheLastLowerCandlestick}];
    updatedCandlesticks = Join[updatedCandlesticks[[1 ;; -2]], Tabular[{updatedCandlestick}]];
    updatedFeatureSet   = ReplacePart[updatedFeatureSet, {"Candlesticks" -> updatedCandlesticks}];
    Return[updatedFeatureSet]
  ];

(* ============================================================================
   Group 3: Peak & Valley Helpers
   ============================================================================ *)

(* Returns the index of the most recent peak with High > last candlestick's Low,
   at or after LastLower. Returns 0 if none found. *)
getPreviousHigherPeak[featureSetArg_Dataset] :=
  Module[{
    updatedFeatureSet,
    candlesticks,
    lastCandlestick,
    peaks,
    lastLow,
    indexOfLastLower,
    lastHigherPeak,
    indexOfLastHigherPeak
    },
    updatedFeatureSet    = featureSetArg;
    candlesticks         = updatedFeatureSet["Candlesticks"];
    lastCandlestick      = Last[candlesticks];
    peaks                = ReverseSortBy[updatedFeatureSet["Peaks"], "Index"];
    lastLow              = lastCandlestick["Low"];
    indexOfLastLower     = lastCandlestick["LastLower"];
    lastHigherPeak       = SelectFirst[peaks, #["High"] > lastLow &, Null];
    indexOfLastHigherPeak = 0;
    If[(lastHigherPeak =!= Null) && (lastHigherPeak["Index"] >= indexOfLastLower),
      indexOfLastHigherPeak = lastHigherPeak["Index"]
    ];
    Return[indexOfLastHigherPeak]
  ];

(* Returns the index of the most recent valley with Low < last candlestick's High,
   at or after LastHigher. Returns 0 if none found. *)
getPreviousLowerValley[featureSetArg_Dataset] :=
  Module[{
    updatedFeatureSet,
    candlesticks,
    lastCandlestick,
    valleys,
    lastHigh,
    indexOfLastHigher,
    lastLowerValley,
    indexOfLastLowerValley
    },
    updatedFeatureSet     = featureSetArg;
    candlesticks          = updatedFeatureSet["Candlesticks"];
    lastCandlestick       = Last[candlesticks];
    valleys               = ReverseSortBy[updatedFeatureSet["Valleys"], "Index"];
    lastHigh              = lastCandlestick["High"];
    indexOfLastHigher     = lastCandlestick["LastHigher"];
    lastLowerValley       = SelectFirst[valleys, #["Low"] < lastHigh &, Null];
    indexOfLastLowerValley = 0;
    If[(lastLowerValley =!= Null) && (lastLowerValley["Index"] >= indexOfLastHigher),
      indexOfLastLowerValley = lastLowerValley["Index"]
    ];
    Return[indexOfLastLowerValley]
  ];

(* Returns all peaks with High > last Low, at or after Max[LastLower, LastHigher],
   sorted by High ascending *)
getAllPreviousPeaks[featureSetArg_Dataset] :=
  Module[{
    updatedFeatureSet,
    candlesticks,
    lastCandlestick,
    indexOfLastLower,
    indexOfLastHigher,
    startingIndex,
    peaks,
    lastLow,
    lastHigherPeaks = {}
    },
    updatedFeatureSet = featureSetArg;
    candlesticks      = updatedFeatureSet["Candlesticks"];
    lastCandlestick   = Last[candlesticks];
    lastLow           = lastCandlestick["Low"];
    indexOfLastLower  = lastCandlestick["LastLower"];
    indexOfLastHigher = lastCandlestick["LastHigher"];
    startingIndex     = Max[indexOfLastLower, indexOfLastHigher];
    peaks             = ReverseSortBy[updatedFeatureSet["Peaks"], "Index"];
    lastHigherPeaks   = Select[peaks, ((#["High"] > lastLow) && (#["Index"] >= startingIndex)) &];
    lastHigherPeaks   = SortBy[lastHigherPeaks, "High"];
    Return[lastHigherPeaks]
  ];

(* Returns all valleys with Low < last High, at or after Max[LastLower, LastHigher],
   sorted by Low ascending *)
getAllPreviousValleys[featureSetArg_Dataset] :=
  Module[{
    updatedFeatureSet,
    candlesticks,
    lastCandlestick,
    indexOfLastLower,
    indexOfLastHigher,
    startingIndex,
    valleys,
    lastHigh,
    lastLowerValleys = {}
    },
    updatedFeatureSet = featureSetArg;
    candlesticks      = updatedFeatureSet["Candlesticks"];
    lastCandlestick   = Last[candlesticks];
    lastHigh          = lastCandlestick["High"];
    indexOfLastLower  = lastCandlestick["LastLower"];
    indexOfLastHigher = lastCandlestick["LastHigher"];
    startingIndex     = Max[indexOfLastLower, indexOfLastHigher];
    valleys           = ReverseSortBy[updatedFeatureSet["Valleys"], "Index"];
    lastLowerValleys  = Select[valleys, ((#["Low"] < lastHigh) && (#["Index"] >= startingIndex)) &];
    lastLowerValleys  = SortBy[lastLowerValleys, "Low"];
    Return[lastLowerValleys]
  ];

(* Returns valid prior peaks scanning backward, keeping only those that set a
   new running high. Suitable for Elliott Wave uptrend identification. *)
Clear[getValidPreviousPeaks];

getValidPreviousPeaks[featureSetArg_Dataset] :=
  Module[{
    reversedPeaks,
    currentCandlestick,
    currentPeak,
    currentHigh,
    validPeaks = Tabular[{<||>}]
    },
    reversedPeaks = Tabular@Reverse[Normal@featureSetArg["Peaks"]];
    If[Length[reversedPeaks] > 0,
      currentCandlestick = Last@featureSetArg["Candlesticks"];
      currentPeak        = First[reversedPeaks];
      (* If the peak is the previous bar allow breaking the high *)
      currentHigh = If[
        currentCandlestick["Index"] == currentPeak["Index"] + 1,
        currentCandlestick["Low"],
        currentCandlestick["High"]
      ];
      validPeaks = Table[
        If[currentPeak["High"] >= currentHigh,
          currentHigh = currentPeak["High"];
          currentPeak,
          Nothing
        ],
        {currentPeak, Normal@reversedPeaks}
      ];
    ];
    Return[Tabular@validPeaks]
  ];

(* Returns valid prior valleys scanning backward, keeping only those that set a
   new running low. Suitable for Elliott Wave downtrend identification. *)
Clear[getValidPreviousValleys];

getValidPreviousValleys[featureSetArg_Dataset] :=
  Module[{
    reversedValleys,
    currentCandlestick,
    currentValley,
    currentLow,
    validValleys = Tabular[{<||>}]
    },
    reversedValleys = Tabular@Reverse[Normal@featureSetArg["Valleys"]];
    If[Length[reversedValleys] > 0,
      currentCandlestick = Last@featureSetArg["Candlesticks"];
      currentValley      = First[reversedValleys];
      (* If the valley is the previous bar allow breaking the low *)
      currentLow = If[
        currentCandlestick["Index"] == currentValley["Index"] + 1,
        currentCandlestick["High"],
        currentCandlestick["Low"]
      ];
      validValleys = Table[
        If[currentValley["Low"] <= currentLow,
          currentLow = currentValley["Low"];
          currentValley,
          Nothing
        ],
        {currentValley, Normal@reversedValleys}
      ];
    ];
    Return[Tabular@validValleys]
  ];

(* Evaluates peak and valley status for the penultimate candlestick and updates
   all associated featureSet fields *)
computeLastCandlestickIsPeakQAndIsValleyQ[featureSetArg_Dataset, tabularCandlesticksArg_Tabular] :=
  Module[{
    updatedCandlesticks,
    currentCandlestick,
    updatedFeatureSet      = featureSetArg,
    n,
    antePenultimate,
    penultimateCandlestick,
    isPeak   = False,
    isValley = False,
    currentCandlestickAsAssociation,
    done
    },
    n                           = Length[tabularCandlesticksArg];
    currentCandlestick          = tabularCandlesticksArg[[n]];
    currentCandlestickAsAssociation = currentCandlestick // Normal;
    (* Need at least 3 rows to detect a peak or valley *)
    If[n < 3,
      AssociateTo[currentCandlestickAsAssociation, <|"IsPeakQ" -> False, "IsValleyQ" -> False|>];
      If[n == 1,
        updatedCandlesticks = Tabular[{currentCandlestickAsAssociation}];
        Goto[done]
      ];
      If[n == 2,
        updatedCandlesticks = Join[tabularCandlesticksArg[[1 ;; 1]], Tabular[{currentCandlestickAsAssociation}]];
        Goto[done]
      ]
    ];
    antePenultimate        = tabularCandlesticksArg[[n - 2]];
    penultimateCandlestick = tabularCandlesticksArg[[n - 1]];
    (* Peak: penultimate High > both neighbors *)
    isPeak   = antePenultimate["High"] < penultimateCandlestick["High"] > currentCandlestick["High"];
    (* Valley: penultimate Low < both neighbors *)
    isValley = antePenultimate["Low"]  > penultimateCandlestick["Low"]  < currentCandlestick["Low"];
    AssociateTo[currentCandlestickAsAssociation, <|"IsPeakQ" -> isPeak, "IsValleyQ" -> isValley|>];
    updatedCandlesticks = Join[
      tabularCandlesticksArg[[1 ;; n - 1]],
      Tabular[{currentCandlestickAsAssociation}]
    ];
    Label[done];
    updatedFeatureSet = ReplacePart[updatedFeatureSet, "Candlesticks" -> updatedCandlesticks];
    Block[{updatedIsPeaks, updatedIsValleys},
      updatedIsPeaks    = Append[updatedFeatureSet["IsPeakQ"]  // Normal, isPeak];
      updatedIsValleys  = Append[updatedFeatureSet["IsValleyQ"] // Normal, isValley];
      updatedFeatureSet = ReplacePart[updatedFeatureSet,
        {"IsPeakQ" -> updatedIsPeaks, "IsValleyQ" -> updatedIsValleys}
      ]
    ];
    Block[{
      currentPeaks, newPeak, updatedPeaks,
      currentPeakVertices, newPeakVertex, updatedPeakVertices,
      currentValleys, newValley, updatedValleys,
      currentValleyVertices, newValleyVertex, updatedValleyVertices,
      currentPeaksAndValleys, newPeakAndValley, updatedPeaksAndValleys
      },
      If[isPeak,
        currentPeaks      = Normal[updatedFeatureSet]["Peaks"];
        newPeak           = TabularRow[<|"Index" -> n, "High" -> currentCandlestick["High"]|>];
        updatedPeaks      = If[currentPeaks === Null, Tabular[{newPeak}], Join[currentPeaks, Tabular[{newPeak}]]];
        currentPeakVertices = Normal[updatedFeatureSet]["VertexToPeak"];
        newPeakVertex     = <|n -> penultimateCandlestick["High"]|>;
        updatedPeakVertices = Append[currentPeakVertices, newPeakVertex];
        currentPeaksAndValleys = Normal[updatedFeatureSet]["PeaksAndValleys"];
        newPeakAndValley  = TabularRow[<|"Index" -> n, "Low" -> Missing[], "High" -> currentCandlestick["High"], "Type" -> "P"|>];
        updatedPeaksAndValleys = If[currentPeaksAndValleys === Null,
          Tabular[{newPeakAndValley}],
          Join[currentPeaksAndValleys, Tabular[{newPeakAndValley}]]
        ];
        updatedFeatureSet = ReplacePart[updatedFeatureSet,
          {"Peaks" -> updatedPeaks, "PeaksAndValleys" -> updatedPeaksAndValleys, "VertexToPeak" -> updatedPeakVertices}
        ]
      ];
      If[isValley,
        currentValleys    = Normal[updatedFeatureSet]["Valleys"];
        newValley         = TabularRow[<|"Index" -> n, "Low" -> currentCandlestick["Low"]|>];
        updatedValleys    = If[currentValleys === Null, Tabular[{newValley}], Join[currentValleys, Tabular[{newValley}]]];
        currentValleyVertices = Normal[updatedFeatureSet]["VertexToValley"];
        newValleyVertex   = <|n -> penultimateCandlestick["Low"]|>;
        updatedValleyVertices = Append[currentValleyVertices, newValleyVertex];
        currentPeaksAndValleys = Normal[updatedFeatureSet]["PeaksAndValleys"];
        newPeakAndValley  = TabularRow[<|"Index" -> n, "Low" -> currentCandlestick["Low"], "High" -> Missing[], "Type" -> "V"|>];
        updatedPeaksAndValleys = If[currentPeaksAndValleys === Null,
          Tabular[{newPeakAndValley}],
          Join[currentPeaksAndValleys, Tabular[{newPeakAndValley}]]
        ];
        updatedFeatureSet = ReplacePart[updatedFeatureSet,
          {"Valleys" -> updatedValleys, "PeaksAndValleys" -> updatedPeaksAndValleys, "VertexToValley" -> updatedValleyVertices}
        ]
      ]
    ];
    Return[updatedFeatureSet]
  ];

(* Legacy single-pass wrapper over computeLastCandlestickIsPeakQAndIsValleyQ *)
Clear[updateLastCandlestickIsPeakQAndIsValleyQ];

updateLastCandlestickIsPeakQAndIsValleyQ[featureSetArg_Dataset] :=
  Module[{
    updatedFeatureSet  = featureSetArg,
    tabularCandlesticks,
    tabularNewCandlestick,
    numberOfCandlesticks,
    newCandlestickAssociation,
    done
    },
    tabularCandlesticks  = featureSetArg["Candlesticks"];
    numberOfCandlesticks = Length@tabularCandlesticks;
    (* Initialize peak/valley fields if this is the first candlestick *)
    If[!MemberQ[Keys[updatedFeatureSet], "Peaks"],
      AppendTo[updatedFeatureSet, {
        "IsPeakQ"             -> {},
        "IsValleyQ"           -> {},
        "Peaks"               -> Null,
        "Valleys"             -> Null,
        "PeaksAndValleys"     -> Null,
        "VertexToPeak"        -> <||>,
        "VertexToValley"      -> <||>,
        "EdgeIndicesToCoords" -> <||>,
        "WaveSegmentsToCoords" -> <||>
      }]
    ];
    newCandlestickAssociation = tabularCandlesticks // Last // Normal;
    AppendTo[newCandlestickAssociation, {"IsPeakQ" -> False, "IsValleyQ" -> False}];
    tabularNewCandlestick = Tabular[{newCandlestickAssociation}];
    If[numberOfCandlesticks == 1,
      tabularCandlesticks = tabularNewCandlestick;
      Goto[done]
    ];
    If[numberOfCandlesticks == 2,
      tabularCandlesticks = Join[tabularCandlesticks[[1 ;; 1]], tabularNewCandlestick];
      Goto[done]
    ];
    tabularCandlesticks = Join[tabularCandlesticks[[1 ;; -2]], tabularNewCandlestick];
    Label[done];
    updatedFeatureSet = computeLastCandlestickIsPeakQAndIsValleyQ[updatedFeatureSet, tabularCandlesticks];
    Return[updatedFeatureSet]
  ];

(* Batch peak and valley computation pass \[LongDash] placeholder for full batch mode *)
Clear[computeFeatureSetPeaksAndValleys];

computeFeatureSetPeaksAndValleys[featureSetArg_Dataset] :=
  Module[{
    updatedFeatureSet,
    candlesticksAsTabular,
    updatedCandlesticks,
    indexOfCurrentCandlestick,
    candlesticksAsList,
    numberOfCandlesticks
    },
    updatedFeatureSet         = featureSetArg;
    candlesticksAsTabular     = featureSetArg["Candlesticks"];
    updatedCandlesticks       = candlesticksAsTabular;
    indexOfCurrentCandlestick = numberOfCandlesticks = Length@candlesticksAsTabular;
    candlesticksAsList        = candlesticksAsTabular // Normal;
    Return[updatedFeatureSet]
  ];

(* Uses FindPeaks on High and Low series to detect peaks and valleys *)
Attributes[findPeaksAndValleysNew] = {Listable};

(* FindPeaks reports the two ends of a series as peaks (valleys): the first element is a peak if it is above its
   right neighbour, the last one if it is above its left neighbour. A candlestick with only one neighbour is not a
   peak or a valley, so the ends are dropped here:
   - the last candlestick (it has no next candlestick yet) is never a peak or a valley;
   - the first candlestick is one only if firstPeakArg / firstValleyArg say so. The caller knows it from the
     candlesticks before the first one (see firstCandlestickPeakValleyFlags); the default False is right for the
     very first candlestick of the stock data. *)
findPeaksAndValleysNew[featureSetArg_Dataset,  sigmaArg_Integer : 0, firstPeakArg_ : False, firstValleyArg_ : False] :=
  Module[{
    n, keepEnds, firstHigh, firstLow,
    candlesticks,
    peaks, fixedPeaks, vertexToPeak,
    valleys, fixedValleys, vertexToValley,
    peaksAndValleys, peaksAndValleysFeatures
    },
    candlesticks = Dataset@featureSetArg["Candlesticks"];
    n = Length[candlesticks];
    firstHigh = If[n >= 2, featureSetArg["Candlesticks"][[1]]["High"], 0];
    firstLow  = If[n >= 2, featureSetArg["Candlesticks"][[1]]["Low"],  0];
    keepEnds  = Function[{found, firstFlag, firstValue},
      Join[If[TrueQ[firstFlag] && n >= 2, {{1, firstValue}}, {}], Select[found, 1 < #[[1]] < n &]]];
    peaks        = <|"Index" -> #[[1]], "Low" -> Missing[], "High" -> #[[2]], "Type" -> "P"|> & /@
                     keepEnds[FindPeaks[candlesticks[All, #High &], sigmaArg], firstPeakArg, firstHigh];
    vertexToPeak = <|#"Index" -> #"High" & /@ peaks|>;
    fixedPeaks   = peaks[[All, {"Index", "High"}]];
    valleys      = <|"Index" -> #[[1]], "Low" -> #[[2]], "High" -> Missing[], "Type" -> "V"|> & /@
                     keepEnds[Abs@FindPeaks[candlesticks[All, -#Low &], sigmaArg], firstValleyArg, firstLow];
    vertexToValley = <|#"Index" -> #"Low" & /@ valleys|>;
    fixedValleys   = valleys[[All, {"Index", "Low"}]];
    peaksAndValleys = Tabular@SortBy[Join@@{peaks, valleys}, #Index &];
    peaksAndValleysFeatures = Dataset[<|
      "PeaksAndValleys" -> peaksAndValleys,
      "Peaks"           -> Dataset[fixedPeaks],
      "Valleys"         -> Dataset@fixedValleys,
      "VertexToPeak"    -> vertexToPeak,
      "VertexToValley"  -> vertexToValley
    |>];
    Return[peaksAndValleysFeatures]
  ];

(* Calls findPeaksAndValleysNew and appends results to featureSetArg *)
Clear[appendPeaksAndValleysToNew];
appendPeaksAndValleysToNew[featureSetArg_Dataset,  sigmaArg_Integer : 0, firstPeakArg_ : False, firstValleyArg_ : False] :=
  Module[{
    updatedFeatureSet,
    updatedCandlesticks,
    peaksTabular, valleysTabular,
    peaksIndices, peaksList,
    valleysIndices, valleysList,
    numberOfCandlesticks,
    allPeaksAndValleysInfoDataset
    },
    updatedFeatureSet   = featureSetArg;
    updatedCandlesticks = updatedFeatureSet["Candlesticks"];
    allPeaksAndValleysInfoDataset = findPeaksAndValleysNew[updatedFeatureSet, sigmaArg, firstPeakArg, firstValleyArg];
    updatedFeatureSet   = AppendTo[updatedFeatureSet, allPeaksAndValleysInfoDataset];
    numberOfCandlesticks = Length@updatedFeatureSet["Candlesticks"];
    peaksTabular   = updatedFeatureSet["Peaks"];
    peaksIndices   = peaksTabular[All, "Index"];
    peaksList      = Table[If[MemberQ[peaksIndices, i], True, False], {i, 1, numberOfCandlesticks}];
    valleysTabular = updatedFeatureSet["Valleys"];
    valleysIndices = valleysTabular[All, "Index"];
    valleysList    = Table[If[MemberQ[valleysIndices, i], True, False], {i, 1, numberOfCandlesticks}];
    updatedCandlesticks = InsertColumns[updatedCandlesticks, {"Volume", "IsPeakQ" -> peaksList}];
    updatedCandlesticks = InsertColumns[updatedCandlesticks, {"IsPeakQ", "IsValleyQ" -> valleysList}];
    updatedFeatureSet   = ReplacePart[updatedFeatureSet, "Candlesticks" -> updatedCandlesticks];
    updatedFeatureSet   = Append[updatedFeatureSet, {"IsPeakQ" -> peaksList, "IsValleyQ" -> valleysList}];
    Return[updatedFeatureSet]
  ];

(* Determines peak/valley status for the most recent candlestick and updates
   all associated featureSet fields using the new algorithm *)
Clear[computeIsPeakQAndIsValleyQ];

computeIsPeakQAndIsValleyQ[featureSetArg_Dataset] :=
  Module[{
    updatedFeatureSet,
    candlesticksAsTabular,
    updatedCandlesticks,
    indexOfCurrentCandlestick,
    indexOfPreviousCandlestick,
    candlesticksAsList,
    updatedCandlestick,
    currentCandlestick,
    previousCandlestick,
    penultimateCandlestick,
    numberOfCandlesticks,
    isPeak,
    isValley,
    previousHigherPeak,
    previousLowerValley,
    finish
    },
    updatedFeatureSet         = featureSetArg;
    candlesticksAsTabular     = featureSetArg["Candlesticks"];
    candlesticksAsList        = candlesticksAsTabular // Normal;
    updatedCandlestick        = currentCandlestick = Last@candlesticksAsList;
    indexOfCurrentCandlestick = currentCandlestick["Index"];
    numberOfCandlesticks      = Length@candlesticksAsTabular;
    updatedCandlesticks       = candlesticksAsTabular;
    isPeak = isValley = False;
    (* First candlestick: initialize all peak/valley Tabular fields *)
    If[numberOfCandlesticks == 1,
      isPeak = isValley = False;
      (* Plural *) 
      updatedCandlesticks = InsertColumns[candlesticksAsTabular,
        {"LastHigherPeak" -> 0, "LastLowerValley" -> 0, "IsPeakQ" -> isPeak, "IsValleyQ" -> isValley}
      ];
      (* Singular. Last one *) 
      updatedCandlestick = Last@updatedCandlesticks;
      (* Update the featureSet *) 
      updatedFeatureSet = Append[updatedFeatureSet, {
		  "Peaks"           -> Tabular[{<|
		      "Index" -> 0,
		      "High"  -> 0.
		    |>}][[{}]],
		  "Valleys"         -> Tabular[{<|
		      "Index" -> 0,
		      "Low"   -> 0.
		    |>}][[{}]],
		  "PeaksAndValleys" -> Tabular[{<|
		      "Index" -> 0,
		      "High"  -> 0.,
		      "Low"   -> 0.,
		      "Type"  -> ""
		    |>}][[{}]],
		  "VertexToPeak"    -> Tabular[{<|
		      "Index" -> 0,
		      "High"  -> 0.
		    |>}][[{}]],
		  "VertexToValley"  -> Tabular[{<|
		      "Index" -> 0,
		      "Low"   -> 0.
		    |>}][[{}]],
		  "Segments"        -> Tabular[{<|
		      "Direction" -> "",
		      "DeltaX"    -> 0,
		      "FromIndex" -> 0,
		      "ToIndex"   -> 0,
		      "FromPrice" -> 0.,
		      "ToPrice"   -> 0.,
		      "Range"     -> 0.,
		      "LogRatio"  -> 0.,
		      "Angle"     -> 0.,
		      "Weight"    -> 0.,
		      "Line"      -> {}
		    |>}][[{}]],
		  "Graphs"          -> {},
		  "Graph"           -> Graph[{}, {},
		      EdgeWeight          -> {},
		      DirectedEdges       -> True,
		      EdgeLabels          -> Automatic,
		      Sequence @@ stockGraphDisplayOptions
		    ],
		  "EdgeSegments"    -> <||>,
		  "LatestSegments"  -> Tabular[{<|
		      "FromIndex" -> 0,
		      "ToIndex"   -> 0
		    |>}][[{}]],
		  "GraphEx"         -> Graph[{}, {}, DirectedEdges -> True]
		}];
      Goto[finish]
    ];
    (* Second candlestick: set pointers to 0 *)
    If[numberOfCandlesticks == 2,
      isPeak = isValley = False;
      updatedCandlestick  = ReplacePart[updatedCandlestick,
        {"LastHigherPeak" -> 0, "LastLowerValley" -> 0, "IsPeakQ" -> False, "IsValleyQ" -> False}
      ];
      updatedCandlesticks = Join[updatedCandlesticks[[1 ;; -2]], Tabular[{updatedCandlestick}]];
      Goto[finish]
    ];
    AssociateTo[updatedCandlestick, <|"LastHigherPeak" -> 0, "LastLowerValley" -> 0, "IsPeakQ" -> False, "IsValleyQ" -> False|>];
    penultimateCandlestick     = candlesticksAsList[[-3]];
    previousCandlestick        = candlesticksAsList[[-2]];
    indexOfPreviousCandlestick = previousCandlestick["Index"];
    isPeak   = currentCandlestick["High"] < previousCandlestick["High"] > penultimateCandlestick["High"];
    isValley = currentCandlestick["Low"]  > previousCandlestick["Low"]  < penultimateCandlestick["Low"];
    Label[finish];
    updatedCandlesticks = Join[updatedCandlesticks[[1 ;; -2]], Tabular[{updatedCandlestick}]];
    (* Update peak fields *)
    Block[{newPeak, currentPeaks, updatedPeaks, currentPeakVertices, updatedPeakVertices,
           currentPeaksAndValleys, newPeakAndValley, updatedPeaksAndValleys},
      If[isPeak,
        previousCandlestick = ReplacePart[previousCandlestick, "IsPeakQ" -> True];
        updatedCandlesticks = Join[updatedCandlesticks[[1 ;; -3]], Tabular[{previousCandlestick, updatedCandlestick}]];
        currentPeaks        = updatedFeatureSet["Peaks"];
        newPeak             = TabularRow[<|"Index" -> indexOfPreviousCandlestick, "High" -> previousCandlestick["High"]|>];
        updatedPeaks        = Join[currentPeaks, Tabular[{newPeak}]];
        currentPeakVertices = updatedFeatureSet["VertexToPeak"];
        updatedPeakVertices = Join[currentPeakVertices, Tabular[{newPeak}]];
        currentPeaksAndValleys = updatedFeatureSet["PeaksAndValleys"];
        newPeakAndValley    = TabularRow[<|"Index" -> indexOfPreviousCandlestick, "Low" -> Missing[], "High" -> previousCandlestick["High"], "Type" -> "P"|>];
        updatedPeaksAndValleys = Join[currentPeaksAndValleys, Tabular[{newPeakAndValley}]];
        updatedFeatureSet   = ReplacePart[updatedFeatureSet,
          {"Peaks" -> updatedPeaks, "PeaksAndValleys" -> updatedPeaksAndValleys, "VertexToPeak" -> updatedPeakVertices}
        ]
      ]
    ];
    (* Update valley fields *)
    Block[{newValley, currentValleys, updatedValleys, currentValleyVertices, updatedValleyVertices,
           currentPeaksAndValleys, newPeakAndValley, updatedPeaksAndValleys},
      If[isValley,
        previousCandlestick  = ReplacePart[previousCandlestick, "IsValleyQ" -> True];
        updatedCandlesticks  = Join[updatedCandlesticks[[1 ;; -3]], Tabular[{previousCandlestick, updatedCandlestick}]];
        currentValleys       = updatedFeatureSet["Valleys"];
        newValley            = TabularRow[<|"Index" -> indexOfPreviousCandlestick, "Low" -> previousCandlestick["Low"]|>];
        updatedValleys       = Join[currentValleys, Tabular[{newValley}]];
        currentValleyVertices = updatedFeatureSet["VertexToValley"];
        updatedValleyVertices = Join[currentValleyVertices, Tabular[{newValley}]];
        currentPeaksAndValleys = updatedFeatureSet["PeaksAndValleys"];
        newPeakAndValley     = TabularRow[<|"Index" -> indexOfPreviousCandlestick, "Low" -> previousCandlestick["Low"], "High" -> Missing[], "Type" -> "V"|>];
        updatedPeaksAndValleys = Join[currentPeaksAndValleys, Tabular[{newPeakAndValley}]];
        updatedFeatureSet    = ReplacePart[updatedFeatureSet,
          {"Valleys" -> updatedValleys, "PeaksAndValleys" -> updatedPeaksAndValleys, "VertexToValley" -> updatedValleyVertices}
        ]
      ]
    ];
    Block[{},
      previousHigherPeak   = getPreviousHigherPeak[updatedFeatureSet];
      previousLowerValley  = getPreviousLowerValley[updatedFeatureSet];
      updatedCandlestick   = ReplacePart[updatedCandlestick,
        {"LastHigherPeak" -> previousHigherPeak, "LastLowerValley" -> previousLowerValley}
      ];
      updatedCandlesticks  = Join[updatedCandlesticks[[1 ;; -2]], Tabular[{updatedCandlestick}]]
    ];
    updatedFeatureSet = Append[updatedFeatureSet, <|
      "Candlesticks" -> updatedCandlesticks,
      "IsPeakQ"      -> Normal@updatedCandlesticks[All, "IsPeakQ"],
      "IsValleyQ"    -> Normal@updatedCandlesticks[All, "IsValleyQ"]
    |>];
    Return[updatedFeatureSet]
  ];

(* ============================================================================
   Group 4: Wave Detection
   ============================================================================ *)

(* Computes all valid upward DirectedEdge waves leading to peakIndexArg *)
Clear[getUpWavesTo];

getUpWavesTo[featureSetArg_Dataset, peakIndexArg_Integer, valleysQArg_List, percentLeewayArg_Real] :=
  Module[{
    candlesticks,
    reversedCandlesticks,
    numberOfCandlesticks,
    currentHigh,
    logCurrentHigh,
    minPrice,
    open,
    close,
    resultingCandlesticks,
    lastGoodIndex,
    lastGoodIndexInReverse,
    currentCandlestick,
    currentIndex,
    result
    },
    candlesticks         = featureSetArg["Candlesticks"];
    numberOfCandlesticks = Length@candlesticks;
    currentCandlestick   = Last@Normal@candlesticks;
    currentIndex         = currentCandlestick["Index"];
    Block[{lastGoodCandlestick, lower, lowest, good, reverseSweepStartingIndex = 2},
      {lowest, currentHigh, open, close} = currentCandlestick[#] & /@ {"Low", "High", "Open", "Close"};
      reversedCandlesticks    = Reverse@Normal[candlesticks];
      lastGoodCandlestick     = SelectFirst[reversedCandlesticks, #["High"] > currentHigh &];
      lastGoodIndex           = If[MissingQ[lastGoodCandlestick], 1, lastGoodCandlestick["Index"]];
      lastGoodIndexInReverse  = numberOfCandlesticks - lastGoodIndex + 1;
      lowest = currentCandlestick["Low"];
      resultingCandlesticks = Select[
        reversedCandlesticks[[reverseSweepStartingIndex ;; lastGoodIndexInReverse]],
        (
          lower = #Low < lowest;
          If[lower, lowest = #Low];
          good  = valleysQArg[[#["Index"]]] && lower && lowest <= #["Low"];
          good
        ) &
      ]
    ];
    logCurrentHigh = myLog[currentHigh];
    result = DirectedEdge[
      #["Index"],
      peakIndexArg,
      Round[100.0 * ((logCurrentHigh - myLog[#Low]) / (currentIndex - #["Index"])), 0.01]
    ] & /@ resultingCandlesticks;
    Return[result]
  ];

(* Computes all valid downward DirectedEdge waves leading to valleyIndexArg *)
Clear[getDownWavesTo];

getDownWavesTo[featureSetArg_Dataset, valleyIndexArg_Integer, peaksQArg_List, percentLeewayArg_Real] :=
  Module[{
    candlesticks,
    reversedCandlesticks,
    numberOfCandlesticks,
    currentLow,
    logCurrentLow,
    maxPrice,
    open,
    close,
    resultingCandlesticks,
    lastGoodIndex,
    lastGoodIndexInReverse,
    currentCandlestick,
    currentIndex,
    result
    },
    candlesticks         = featureSetArg["Candlesticks"];
    numberOfCandlesticks = Length@candlesticks;
    currentCandlestick   = Last@Normal@candlesticks;
    currentIndex         = currentCandlestick["Index"];
    Block[{lastGoodCandlestick, higher, highest, good, reverseSweepStartingIndex = 2},
      {currentLow, highest, open, close} = currentCandlestick[#] & /@ {"Low", "High", "Open", "Close"};
      reversedCandlesticks   = Reverse@Normal[candlesticks];
      lastGoodCandlestick    = SelectFirst[reversedCandlesticks, #["Low"] < currentLow &];
      lastGoodIndex          = If[MissingQ[lastGoodCandlestick], 1, lastGoodCandlestick["Index"]];
      lastGoodIndexInReverse = numberOfCandlesticks - lastGoodIndex + 1;
      highest = currentCandlestick["High"];
      resultingCandlesticks = Select[
        reversedCandlesticks[[reverseSweepStartingIndex ;; lastGoodIndexInReverse]],
        (
          higher = #High > highest;
          If[higher, highest = #High];
          good   = peaksQArg[[#["Index"]]] && higher && highest >= #["High"];
          good
        ) &
      ]
    ];
    logCurrentLow = myLog[currentLow];
    result = DirectedEdge[
      #["Index"],
      valleyIndexArg,
      Round[100.0 * ((logCurrentLow - myLog[#High]) / (currentIndex - #["Index"])), 0.01]
    ] & /@ resultingCandlesticks;
    Return[result]
  ];

(* Computes all valid uptrend and downtrend segments to the last candlestick
   and appends them to the Segments Tabular in featureSetArg *)
Clear[getValidWavesToLastCandlestick];

Options[getValidWavesToLastCandlestick] = {"StrictRanges" -> True};

getValidWavesToLastCandlestick[featureSetArg_Dataset, opts : OptionsPattern[]] :=
  Module[{
    updatedFeatureSet,
    candlesticks,
    toCandlestick,
    toIndex,
    peaksAndValleys,
    peaks,
    valleys,
    downSegments,
    upSegments,
    segments
    },
    updatedFeatureSet = featureSetArg;
    candlesticks      = updatedFeatureSet["Candlesticks"];
    toCandlestick     = Last@candlesticks;
    toIndex           = toCandlestick["Index"];
    peaksAndValleys   = updatedFeatureSet["PeaksAndValleys"] // Normal;
    valleys           = Reverse@Normal@updatedFeatureSet["Valleys"];
    peaks             = Reverse@Normal@updatedFeatureSet["Peaks"];
    (* Compute uptrends from each valley to current candlestick *)
    Block[{span, ranges},
      upSegments = Table[
        span   = valley["Index"] ;; toIndex;
        ranges = candlesticksRange[candlesticks, span, "U", "StrictRanges" -> OptionValue["StrictRanges"]];
        ranges,
        {valley, valleys}
      ]
    ];
    (* Compute downtrends from each peak to current candlestick *)
    Block[{span, ranges},
      downSegments = Table[
        span   = peak["Index"] ;; toIndex;
        ranges = candlesticksRange[candlesticks, span, "D", "StrictRanges" -> OptionValue["StrictRanges"]];
        ranges,
        {peak, peaks}
      ]
    ];
    segments = Join[
      Cases[upSegments,   _Association],
      Cases[downSegments, _Association]
    ];
    Block[{updatedSegments},
      updatedSegments = updatedFeatureSet["Segments"];
      If[Length@segments > 0,
        If[Length@updatedSegments != 0,
          updatedSegments = Join[updatedSegments, Tabular[segments]],
          updatedSegments = Tabular[segments]
        ]
      ];
      updatedFeatureSet = ReplacePart[updatedFeatureSet, "Segments" -> updatedSegments]
    ];
    Return[updatedFeatureSet]
  ];

(* ============================================================================
   Group 4b: Support and resistance trendlines and the patterns they make
   The rules and the code are in Kernel/TrendlinePatterns.wl (plain functions of the candlesticks, so that they also work on a stream of
   candlesticks without a feature set). This function adds one step to a feature set: it finds the lines valid at the last candlestick, adds their rows
   to Segments (ToIndex = the last index), keeps every line in ValidTrendlines, and adds the patterns that one resistance and one support make at the last
   candlestick to Patterns (one row per pattern and candlestick, like Segments).
   ============================================================================ *)

Clear[getValidSupportAndResistanceToLastCandlestick];

Options[getValidSupportAndResistanceToLastCandlestick] = {"TolerancePercent" -> 5, "MinTouches" -> 1, "Patterns" -> True, "PatternBand" -> 0.15};

getValidSupportAndResistanceToLastCandlestick[featureSetArg_Dataset, opts : OptionsPattern[]] :=
  Module[{
    updatedFeatureSet, candlesticks, n, existing, highs, lows, peakIndices, valleyIndices, lines,
    newSegments, updatedSegments, patternRows, existingPatterns, updatedPatterns
    },
    updatedFeatureSet = featureSetArg;
    candlesticks = Normal[updatedFeatureSet["Candlesticks"]];
    n            = Length[candlesticks];
    existing = If[KeyExistsQ[updatedFeatureSet, "ValidTrendlines"], Normal[updatedFeatureSet["ValidTrendlines"]], {}];
    If[! ListQ[existing], existing = {}];
    If[n < 3, Return[Append[updatedFeatureSet, <|"ValidTrendlines" -> existing|>]]];
    highs         = candlesticks[[All, "High"]];
    lows          = candlesticks[[All, "Low"]];
    peakIndices   = #["Index"] & /@ Normal[updatedFeatureSet["Peaks"]];
    valleyIndices = #["Index"] & /@ Normal[updatedFeatureSet["Valleys"]];
    (* the lines valid at n *)
    lines = StockAnalyzer`TrendlinePatterns`trendlineLineStep[highs, lows, peakIndices, valleyIndices, existing,
      "TolerancePercent" -> OptionValue["TolerancePercent"], "MinTouches" -> OptionValue["MinTouches"]];
    (* one Segments row for every line alive at n *)
    newSegments     = StockAnalyzer`TrendlinePatterns`trendlineRowsAt[lines, n];
    updatedSegments = updatedFeatureSet["Segments"];
    If[Length[newSegments] > 0,
      If[Length[updatedSegments] != 0,
        updatedSegments = Join[updatedSegments, Tabular[newSegments]],
        updatedSegments = Tabular[newSegments]
      ]
    ];
    updatedFeatureSet = Append[updatedFeatureSet, <|"ValidTrendlines" -> lines, "Segments" -> updatedSegments|>];
    (* the patterns that one resistance and one support make at n *)
    If[TrueQ[OptionValue["Patterns"]],
      patternRows      = StockAnalyzer`TrendlinePatterns`trendlinePatternsAt[lines, n, "PatternBand" -> OptionValue["PatternBand"]];
      existingPatterns = If[KeyExistsQ[updatedFeatureSet, "Patterns"], updatedFeatureSet["Patterns"], {}];
      updatedPatterns  = existingPatterns;
      If[Length[patternRows] > 0,
        If[Length[existingPatterns] != 0,
          updatedPatterns = Join[existingPatterns, Tabular[patternRows]],
          updatedPatterns = Tabular[patternRows]
        ]
      ];
      updatedFeatureSet = Append[updatedFeatureSet, <|"Patterns" -> updatedPatterns|>]
    ];
    Return[updatedFeatureSet]
  ];

(* ============================================================================
   Group 4c: Harmonic patterns born at the last candlestick
   The matches of a pattern are paths of consecutive segments of the stock graph (Kernel/HarmonicPatterns.wl). A path is complete, and so born, when its last
   segment ends at the last candlestick; this step walks backward from the segments that end there (LatestSegments), through the segments before them
   (HarmonicSegments: ToIndex -> the segments that end there), and adds the matches to HarmonicPatterns. It must run after updateGraphFromSegments, which sets LatestSegments.
   ============================================================================ *)

Clear[getHarmonicPatternsBornAtLastCandlestick];

Options[getHarmonicPatternsBornAtLastCandlestick] = {"Patterns" -> All, "Tolerance" -> 0.05};

getHarmonicPatternsBornAtLastCandlestick[featureSetArg_Dataset, opts : OptionsPattern[]] :=
  Module[{updatedFeatureSet = featureSetArg, n, latest, store, matches, rows, existing, updated},
    n = Length[updatedFeatureSet["Candlesticks"]];
    (* the segments that end at n: only the fields the finder needs *)
    latest = If[KeyExistsQ[updatedFeatureSet, "LatestSegments"] && Length[updatedFeatureSet["LatestSegments"]] > 0,
      Normal[updatedFeatureSet["LatestSegments"]], {}];
    latest = DeleteDuplicatesBy[
      KeyTake[#, {"Direction", "FromIndex", "ToIndex", "FromPrice", "ToPrice"}] & /@ Select[latest, AssociationQ], {#["FromIndex"], #["ToIndex"]} &];
    (* the segments so far, by the candlestick where they end *)
    store = If[KeyExistsQ[updatedFeatureSet, "HarmonicSegments"], Normal[updatedFeatureSet["HarmonicSegments"]], <||>];
    If[! AssociationQ[store], store = <||>];
    If[latest =!= {}, store = Append[store, n -> latest]];
    matches = StockAnalyzer`HarmonicPatterns`harmonicMatchesBornAt[store, latest, OptionValue["Patterns"], "Tolerance" -> OptionValue["Tolerance"]];
    rows = StockAnalyzer`HarmonicPatterns`harmonicMatchRow /@ matches;
    existing = If[KeyExistsQ[updatedFeatureSet, "HarmonicPatterns"], updatedFeatureSet["HarmonicPatterns"], {}];
    updated  = existing;
    If[Length[rows] > 0,
      updated = If[Length[existing] != 0, Join[existing, Tabular[rows]], Tabular[rows]]];
    Append[updatedFeatureSet, <|"HarmonicSegments" -> store, "HarmonicPatterns" -> updated|>]
  ];

(* ============================================================================
   Group 5: Segment Math
   ============================================================================ *)

Clear[candlesticksRange];

Options[candlesticksRange] = {"StrictRanges" -> True};
candlesticksRange[candlesticksArg_Tabular, spanArg_Span, directionArg_String, opts : OptionsPattern[]] :=
  Module[{
    spanCandlesticks,
    minY, maxY,
    numberOfCandlesticksInSpan,
    lows, highs,
    highest, lowest,
    positionsOfLows, positionsOfHighs,
    lowestCandlestick, highestCandlestick,
    positionOfLowestCandlestick, positionOfHighestCandlestick,
    positionOfSecondLowestCandlestick, positionOfSecondHighestCandlestick,
    fromIndex, toIndex,
    fromCandlestick, toCandlestick,
    fromPrice, toPrice,
    simpleValidityTest,insideFirst,
    range, logRange, dx, dy, angle, edgeWeight, direction,
    newSegment, useStrict
    },

    fromIndex        = spanArg[[1]];
    toIndex          = spanArg[[2]];    
    (* Guard. No single candlestick segment *) 
    If[fromIndex == toIndex,
       newSegment = Nothing;
       Return[newSegment];
    ];
    
    direction        = directionArg;
	           
    spanCandlesticks = Normal[candlesticksArg][[spanArg]];
    fromCandlestick  = First@spanCandlesticks;
    toCandlestick    = Last@spanCandlesticks;
    If[direction == "U",
      fromPrice = fromCandlestick["Low"];
      toPrice   = toCandlestick["High"],
      fromPrice = fromCandlestick["High"];
      toPrice   = toCandlestick["Low"]
    ];
     
    (* Must calculate those first *)  
    highs            = spanCandlesticks[[All, "High"]];
    lows             = spanCandlesticks[[All, "Low"]];
    lowest           = Min[lows];
    highest          = Max[highs]; 
    
    (* Do this here before the strictness test *)
    numberOfCandlesticksInSpan          = Length@spanCandlesticks;
    positionsOfLows                     = Ordering[lows];
    positionsOfHighs                    = Ordering[highs];
    positionOfLowestCandlestick         = First[positionsOfLows];
    positionOfHighestCandlestick        = Last[positionsOfHighs];
    positionOfSecondLowestCandlestick   = positionsOfLows[[2]];
    positionOfSecondHighestCandlestick  = positionsOfHighs[[-2]];
    lowestCandlestick  = spanCandlesticks[[positionOfLowestCandlestick]];
    highestCandlestick = spanCandlesticks[[positionOfHighestCandlestick]];  
    insideFirst = positionOfLowestCandlestick == positionOfHighestCandlestick == 1;
    
    (* DO simple check if we are not asking for strict ranges *) 
    If[OptionValue["StrictRanges"] == False,
	    simpleValidityTest = (direction== "U" && (fromCandlestick["Low"] <= lowest)   && (toCandlestick["Low"]  >= lowest)) ||
	                         (direction== "D" && (fromCandlestick["High"] >= highest) && (toCandlestick["High"] <= highest));
	    If[simpleValidityTest,
	      range      = toPrice - fromPrice;
	      logRange   = myLog[toPrice] - myLog[fromPrice];
	      dx         = toIndex - fromIndex;
	      dy         = range;
	      angle      = Round[ArcTan[dx, dy], 0.001];
	      edgeWeight = angle;
	      newSegment = <|
	          "Direction" -> direction,
	          "DeltaX"    -> dx,
	          "FromIndex" -> fromIndex,
	          "ToIndex"   -> toIndex,
	          "FromPrice" -> fromPrice,
	          "ToPrice"   -> toPrice,
	          "Range"     -> range,
	          "LogRatio"  -> logRange,
	          "Angle"     -> angle,
	          "Weight"    -> edgeWeight
	        |>;
        ,
        newSegment = Nothing
	    ];
	    Return[newSegment];
	  ];
	  
      (* If We are doing StrictRanges *) 
	  If[OptionValue["StrictRanges"] == True,
	    
    (* 
      If[insideFirst,Echo["InsideFirst", spanArg]];
      If[(direction == "D" && insideFirst && positionOfSecondLowestCandlestick == numberOfCandlesticksInSpan),Echo["ID",spanArg]];
      If[(direction == "U" && insideFirst && positionOfSecondHighestCandlestick == numberOfCandlesticksInSpan),Echo["IU",spanArg]];        
      If[(direction == "D" && positionOfHighestCandlestick == 1 && positionOfLowestCandlestick == numberOfCandlesticksInSpan),Echo[2,spanArg]];
      If[(direction == "D" && positionOfHighestCandlestick ==  positionOfLowestCandlestick == numberOfCandlesticksInSpan && positionOfSecondHighestCandlestick == 1),Echo["2A",spanArg]];
      If[(direction == "U" && positionOfLowestCandlestick  == 1 && positionOfHighestCandlestick == numberOfCandlesticksInSpan),Echo[3,spanArg]];
      If[(direction == "U" && positionOfLowestCandlestick  ==  positionOfHighestCandlestick == numberOfCandlesticksInSpan && positionOfSecondLowestCandlestick == 1),Echo["3A",spanArg]];
      If[(direction == "D" && (positionOfHighestCandlestick == numberOfCandlesticksInSpan == 2) && positionOfSecondHighestCandlestick == 1),Echo[4,spanArg]];
      If[(direction == "U" && (positionOfLowestCandlestick  == numberOfCandlesticksInSpan == 2) && positionOfSecondLowestCandlestick  == 1),Echo[5,spanArg]];
      If[(direction == "D" && positionOfSecondHighestCandlestick == positionOfSecondLowestCandlestick == 1 && positionOfHighestCandlestick == positionOfLowestCandlestick == numberOfCandlesticksInSpan),Echo[6,spanArg]];
      If[(direction == "U" && positionOfSecondHighestCandlestick == positionOfSecondLowestCandlestick == 1 && positionOfHighestCandlestick == positionOfLowestCandlestick == numberOfCandlesticksInSpan),Echo[7,spanArg]];
 	*)         
	    
		If[ 
		(
           (direction == "D" && insideFirst && positionOfSecondLowestCandlestick == numberOfCandlesticksInSpan)
        || (direction == "U" && insideFirst && positionOfSecondHighestCandlestick == numberOfCandlesticksInSpan)      
        || (direction == "D" && positionOfHighestCandlestick == 1 && positionOfLowestCandlestick == numberOfCandlesticksInSpan)
        || (direction == "D" && positionOfHighestCandlestick ==  positionOfLowestCandlestick == numberOfCandlesticksInSpan && positionOfSecondHighestCandlestick == 1)
        || (direction == "U" && positionOfLowestCandlestick  == 1 && positionOfHighestCandlestick == numberOfCandlesticksInSpan)
        || (direction == "U" && positionOfLowestCandlestick  ==  positionOfHighestCandlestick == numberOfCandlesticksInSpan && positionOfSecondLowestCandlestick == 1)
        || (direction == "D" && (positionOfHighestCandlestick == numberOfCandlesticksInSpan == 2) && positionOfSecondHighestCandlestick == 1)
        || (direction == "U" && (positionOfLowestCandlestick  == numberOfCandlesticksInSpan == 2) && positionOfSecondLowestCandlestick  == 1)
        (* Extended Engulfing *)
        || (direction == "D" && positionOfSecondHighestCandlestick == positionOfSecondLowestCandlestick == 1 && positionOfHighestCandlestick == positionOfLowestCandlestick == numberOfCandlesticksInSpan)
        || (direction == "U" && positionOfSecondHighestCandlestick == positionOfSecondLowestCandlestick == 1 && positionOfHighestCandlestick == positionOfLowestCandlestick == numberOfCandlesticksInSpan)
        )
        ,
        range      = toPrice - fromPrice;
        logRange   = myLog[toPrice] - myLog[fromPrice];
        dx         = toIndex - fromIndex;
        dy         = range;
        angle      = Round[ArcTan[dx, dy], 0.001];
        edgeWeight = angle;
        newSegment = <|
          "Direction" -> direction,
          "DeltaX"    -> dx,
          "FromIndex" -> fromIndex,
          "ToIndex"   -> toIndex,
          "FromPrice" -> fromPrice,
          "ToPrice"   -> toPrice,
          "Range"     -> range,
          "LogRatio"  -> logRange,
          "Angle"     -> angle,
          "Weight"    -> edgeWeight
        |>
        ,
        newSegment = Nothing
      ];
      Return[newSegment];
	  ];
	];
  

(* Maps candlesticksRange over a List of Span values, passing opts through *)
candlesticksRange[candlesticksArg_Tabular, spans_List, directionArg_String, opts : OptionsPattern[]] :=
  Tabular[Cases[
    Map[candlesticksRange[candlesticksArg, #, directionArg, opts] &, spans],
    _Association
  ]];

make2LevelAssociationToTabular[assoc_Association] := Module[
  {fields, rows},
  fields = {"Open", "High", "Low", "Close"};
  rows = Map[
    Function[fromField,
      Prepend[assoc[fromField], "Field" -> fromField]
    ],
    fields
  ];
  Tabular[rows]
];


(* ============================================================================
   Group 5b: Angle Similarity Search
   ============================================================================ *)

Clear[findClosestAngles];

findClosestAngles[featureSetArg_Dataset, referenceSegmentArg_] :=
  Module[{
    edgeSegs,
    refFromIndex,
    refToIndex,
    refDirection,
    refFromField,
    refToField,
    refAngle,
    filteredKeys,
    results
    },
    (* Extract EdgeSegments as plain Association *)
    edgeSegs = Which[
      AssociationQ[featureSetArg["EdgeSegments"]],
        featureSetArg["EdgeSegments"],
      True,
        Association[Normal[featureSetArg["EdgeSegments"]]]
    ];
    refFromIndex = referenceSegmentArg["FromIndex"];
    refToIndex   = referenceSegmentArg["ToIndex"];
    refDirection = referenceSegmentArg["Direction"];
    refFromField = referenceSegmentArg["FromField"];
    refToField   = referenceSegmentArg["ToField"];
    (* Use flat Angle field directly from the reference segment *)
    refAngle     = referenceSegmentArg["Angle"];
    (* Filter: same FromIndex, same Direction, ToIndex <= reference ToIndex *)
    filteredKeys = Select[
      Keys[edgeSegs],
      Function[k,
        edgeSegs[k]["FromIndex"] == refFromIndex &&
        edgeSegs[k]["Direction"] == refDirection &&
        edgeSegs[k]["ToIndex"]   <= refToIndex
      ]
    ];
    (* Compute AbsDiff for each filtered key using its own flat Angle field *)
    results = Map[
      Function[k,
        Module[{seg, angle},
          seg   = edgeSegs[k];
          angle = seg["Angle"];
          <|
            "AbsDiff"   -> Abs[refAngle - angle],
            "EdgeKey"   -> k,
            "Direction" -> seg["Direction"],
            "FromIndex" -> seg["FromIndex"],
            "ToIndex"   -> seg["ToIndex"],
            "FromField" -> seg["FromField"],
            "ToField"   -> seg["ToField"],
            "EdgeType"  -> seg["EdgeType"],
            "Angle"     -> angle,
            "RefAngle"  -> refAngle
          |>
        ]
      ],
      filteredKeys
    ];
    (* Sort by AbsDiff ascending and return as Dataset *)
    Dataset@SortBy[results, #["AbsDiff"] &]
  ];
  
  
(* ============================================================================
   updateGraphFromSegments
   ============================================================================ *)

(* \[HorizontalLine]\[HorizontalLine] segmentType \[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]
   Computes a 4-character analytical code for an edge segment:
   - char 1: TrendDirection (U or D)
   - char 2: EdgeDirection derived from Range sign (U, D, or F)
   - char 3: FromField initial (O, H, L, C)
   - char 4: ToField initial (O, H, L, C)
   Examples: MaxU Low->High = "UULH", OuterUp High->Low = "UDHL"
   \[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine] *)
segmentType[trendDirArg_String, rangeArg_, fromFieldArg_String, toFieldArg_String] :=
  Module[{edgeDir, fromInitial, toInitial},
    edgeDir     = Which[rangeArg > 0, "U", rangeArg < 0, "D", True, "F"];
    fromInitial = StringTake[fromFieldArg, 1];
    toInitial   = StringTake[toFieldArg,   1];
    trendDirArg <> edgeDir <> fromInitial <> toInitial
  ];

(* ============================================================================
   Edge specifications per segment
   Both functions return a list of {fromField, toField, edgeType} triples; updateGraphFromSegments
   builds one edge for every triple.
   ============================================================================ *)

(* StrictRanges -> True: only the primary edge of the segment, i.e.
     "MaxU"  Low  -> High  on an up segment,
     "MaxD"  High -> Low   on a down segment.
   The edge is returned only if its price move agrees with the segment direction (U: the High of the
   ToCandlestick is not below the Low of the FromCandlestick; D: the Low of the ToCandlestick is not
   above the High of the FromCandlestick). Wrong-direction pairs, for example Low -> High on a down
   segment (OuterDown), are never generated. *)
strictEdgeSpecs[directionArg_, fromCandleArg_, toCandleArg_] :=
  Which[
    directionArg === "U",
      If[toCandleArg["High"] >= fromCandleArg["Low"],  {{"Low",  "High", "MaxU"}}, {}],
    directionArg === "D",
      If[toCandleArg["Low"]  <= fromCandleArg["High"], {{"High", "Low",  "MaxD"}}, {}],
    True,
      {}
  ];

(* StrictRanges -> False: every pair {from, to} of the fields below with from =!= to, classified by
   EdgeType (table in updateGraphFromSegments). Open and Close are commented out of the field list;
   re-enable them there to get the inner edge types as well. *)
looseEdgeSpecs[directionArg_] :=
  Module[{up = (directionArg === "U"), fields = {"Low", "High" (*, "Open", "Close"*)}, primaryFF, primaryTF},
    primaryFF = If[up, "Low",  "High"];
    primaryTF = If[up, "High", "Low"];
    Map[
      Function[pair,
        With[{ff = pair[[1]], tf = pair[[2]]},
          {ff, tf,
           Which[
             ff === primaryFF && tf === primaryTF,
               If[up, "MaxU", "MaxD"],
             ff === primaryFF && (tf === "Open" || tf === "Close"),
               If[up, "UpInner", "DownInner"],
             ff === primaryFF && tf === primaryFF,
               If[up, "Support", "Resistance"],
             ff === "Open" || ff === "Close",
               If[up, "InnerUp", "InnerDown"],
             True,
               If[up, "OuterUp", "OuterDown"]
           ]}
        ]
      ],
      Select[Tuples[fields, 2], #[[1]] =!= #[[2]] &];
      Tuples[fields,2]
    ]
  ];

Clear[updateGraphFromSegments];

Options[updateGraphFromSegments] = {"StrictRanges" -> True};

updateGraphFromSegments[featureSetArg_Dataset, opts : OptionsPattern[]] :=
  Module[{
    updatedFeatureSet,
    candlesticks,
    candlesticksList,
    candlesticksByIndex,
    currentIndex,
    currentCandlestick,
    currentGraph,
    currentEdgeSegments,
    allSegments,
    latestSegments,
    newVertex,
    newEdges,
    newEdgeKeys,
    newWeights,
    newEdgeSegments,
    updatedGraph,
    fromFields,
    toFields,
    fieldCombos,
    primaryFromField,
    primaryToField,
    edgeType,
    fromPrice,
    toPrice,
    augmentedSeg,
    allNewKeys,
    allNewEdges,
    allNewWeights,
    allNewSegments,
    strictEdges
    },
    (* True: one MaxU / MaxD edge per segment; False: every field pair *)
    strictEdges         = TrueQ[OptionValue["StrictRanges"]];
    updatedFeatureSet   = featureSetArg;
    currentGraph        = updatedFeatureSet["GraphEx"];
    currentEdgeSegments = updatedFeatureSet["EdgeSegments"];
    (* Ensure we have a plain Association, not a Dataset wrapper *)
    currentEdgeSegments = Which[
      AssociationQ[currentEdgeSegments],
        currentEdgeSegments,
      Head[currentEdgeSegments] === Dataset,
        Association[Normal[currentEdgeSegments]],
      True,
        <||>
    ];
    candlesticks     = updatedFeatureSet["Candlesticks"];
    candlesticksList = Normal[candlesticks];
    (* Build index lookup: candlesticksByIndex[i] = row Association *)
    candlesticksByIndex = Association[
      Map[#["Index"] -> # &, candlesticksList]
    ];
    currentIndex       = Length[candlesticksList];
    currentCandlestick = Last[candlesticksList];
    newVertex          = currentIndex;
    allSegments        = Normal[updatedFeatureSet["Segments"]];
    (* Only the wave rows (U / D), without the support / resistance columns Kind and Touches: support and resistance
       rows (ResU, SupF, ...) never become graph edges *)
    latestSegments     = KeyDrop[#, {"Kind", "Touches", "AnchorIndex", "StartIndex", "Slope", "LogSlope", "Goodness"}] & /@
      Select[allSegments, #["ToIndex"] == currentIndex && MemberQ[{"U", "D"}, #["Direction"]] &];
    (* Store LatestSegments as a Tabular for downstream use *)
    updatedFeatureSet = ReplacePart[updatedFeatureSet,
      "LatestSegments" -> If[Length[latestSegments] > 0,
        Tabular[latestSegments],
        Tabular[{<|"FromIndex" -> Null, "ToIndex" -> Null|>}][[2 ;;]]
      ]
    ];
    (* If no new segments, just add the new vertex and return *)
    If[Length[latestSegments] == 0,
      updatedGraph = rebuildStockGraph[
        Join[VertexList[currentGraph], {newVertex}],
        EdgeList[currentGraph],
        edgeComplexWeight /@ Values[currentEdgeSegments]
      ];
      updatedFeatureSet = ReplacePart[updatedFeatureSet, "GraphEx"      -> updatedGraph];
	    updatedFeatureSet = ReplacePart[updatedFeatureSet, "Graph"        -> updatedGraph];
	    updatedFeatureSet = ReplacePart[updatedFeatureSet, "EdgeSegments" -> currentEdgeSegments];
      Return[updatedFeatureSet]
    ];
    (* Build all edges per segment - Primary, Secondary and Tertiary *)
    allNewKeys     = {};
    allNewEdges    = {};
    allNewWeights  = {};
    allNewSegments = {};
    Map[
      Function[seg,
        Module[{
          direction, fromIdx, toIdx,
          primaryFF, primaryTF, tertiaryDoubleBackTF,
          toCandle, fromCandle,
          fp, tp, key, augSeg, edgeType,
          deltaX, deltaY, edgeRange, edgeLogRatio, edgeAngle, edgeWeight,
          makeEdge
          },
          direction  = seg["Direction"];
          fromIdx    = seg["FromIndex"];
          toIdx      = seg["ToIndex"];
          deltaX     = toIdx - fromIdx;
          (* Primary fromField/toField by direction *)
          primaryFF           = If[direction === "U", "Low",  "High"];
          primaryTF           = If[direction === "U", "High", "Low"];
          (* Tertiary "doubling back" toField: U starts at Low ends at Low,
             D starts at High ends at High *)
          tertiaryDoubleBackTF = primaryFF;
          fromCandle = candlesticksByIndex[fromIdx];
          toCandle   = candlesticksByIndex[toIdx];
          (* Helper: build one augSeg and append to accumulator lists *)
          makeEdge = Function[{ff, tf, et},
					  fp           = fromCandle[ff];
					  tp           = toCandle[tf];
					  deltaY       = tp - fp;
					  edgeRange    = tp - fp;
					  edgeLogRatio = GoldenRatio * (tp / fp);
					  edgeAngle    = ArcTan[deltaX, deltaY];
					  edgeWeight   = edgeAngle;
					  augSeg       = Join[seg, <|
					    "FromField"   -> ff,
					    "ToField"     -> tf,
					    "EdgeType"    -> et,
					    "SegmentType" -> segmentType[direction, edgeRange, ff, tf],  (* NEW *)
					    "FromPrice"   -> fp,
					    "ToPrice"     -> tp,
					    "Range"       -> edgeRange,
					    "LogRatio"    -> edgeLogRatio,
					    "Angle"       -> edgeAngle,
					    "Weight"      -> edgeWeight,
					    "ComplexWeight" -> Complex[deltaX, edgeRange],   (* a + i b: the index distance of the segment and the range of the edge; the weight of the edge in the graph *)
					    "Line"        -> StockAnalyzer`Visualization`makeParametricLine[
					                       {fromIdx, fp}, {toIdx, tp}]
					  |>];
					  key = {DirectedEdge[fromIdx, toIdx], direction, ff, tf, et,
					         segmentType[direction, edgeRange, ff, tf]};  (* NEW - 6th element *)
					  allNewKeys     = Append[allNewKeys,     key];
					  allNewEdges    = Append[allNewEdges,    DirectedEdge[fromIdx, toIdx]];
					  allNewWeights  = Append[allNewWeights,  Complex[deltaX, edgeRange]];
					  allNewSegments = Append[allNewSegments, augSeg]
					];
          (* ------- EdgeType classification ------------------------------------
   "MaxU"       U  Low        High       Primary up move
   "UpInner"    U  Low        Open/Close Secondary up
   "Support"    U  Low        Low        Low->Low (support level)
   "InnerUp"    U  Open/Close any        Open/Close origin, up seg
   "OuterUp"    U  High       any        High origin on up segment
   "MaxD"       D  High       Low        Primary down move
   "DownInner"  D  High       Open/Close Secondary down
   "Resistance" D  High       High       High->High (resistance level)
   "InnerDown"  D  Open/Close any        Open/Close origin, down seg
   "OuterDown"  D  Low        any        Low origin on down segment
   -------------------------------------------------------------------- *)
          (* One edge per {fromField, toField, edgeType} spec. StrictRanges -> True gives only the
             MaxU / MaxD edge of the segment; StrictRanges -> False gives every field pair, classified
             as in the table above. *)
          Map[
            Function[spec, makeEdge @@ spec],
            If[strictEdges,
              strictEdgeSpecs[direction, fromCandle, toCandle],
              looseEdgeSpecs[direction]
            ]
          ]
        ]
      ],
      latestSegments
    ];
    (* Build new EdgeSegments entries *)
    newEdgeSegments     = AssociationThread[allNewKeys -> allNewSegments];
    currentEdgeSegments = Join[currentEdgeSegments, newEdgeSegments];
    (* Build multigraph with all edges including repeated DirectedEdge pairs *)
    updatedGraph = rebuildStockGraph[
      Join[VertexList[currentGraph], {newVertex}],
      Join[EdgeList[currentGraph], allNewEdges],
      Join[
        edgeComplexWeight /@ Values[currentEdgeSegments][[
          ;; Length[currentEdgeSegments] - Length[allNewKeys]
        ]],
        allNewWeights
      ]
    ];
    updatedFeatureSet = ReplacePart[updatedFeatureSet, "GraphEx"      -> updatedGraph];
    updatedFeatureSet = ReplacePart[updatedFeatureSet, "Graph"        -> updatedGraph];
    updatedFeatureSet = ReplacePart[updatedFeatureSet, "EdgeSegments" -> currentEdgeSegments];
    Return[updatedFeatureSet]
  ];

(* ============================================================================
   Group 6: Top-Level Orchestrator
   ============================================================================ *)
   
   (* Converts a two-level nested Association of OHLC field pairs into a Tabular *)
Clear[updateFeatureSet];
    
(* Is the first candlestick of candlesticksArg a peak / a valley in the whole history of the stock? Returns
   {isPeak, isValley}. The candlesticks before the first one are in SP500Stocks, so the first candlestick gets the value
   FindPeaks (same sigma) gives for it in the whole history. {False, False} if the first candlestick is the first or the
   last one of the history (an end of the series is not a peak or a valley), if its date is not found, or if
   SP500Stocks is not loaded. *)
Clear[firstCandlestickPeakValleyFlags];
firstCandlestickPeakValleyFlags[tickerArg_String, periodArg_String, candlesticksArg_, sigmaArg_Integer : 0] :=
  Quiet @ Check[
    Module[{all, firstDate, dates, pos, highs, lows, peakIndices, valleyIndices},
      If[! ValueQ[Global`SP500Stocks] || ! KeyExistsQ[Global`SP500Stocks[periodArg], tickerArg] || Length[candlesticksArg] == 0,
        Return[{False, False}]];
      all       = Normal[Global`SP500Stocks[periodArg][tickerArg]];
      firstDate = AbsoluteTime[Normal[candlesticksArg[[1]]]["Date"]];
      dates     = AbsoluteTime[#["Date"]] & /@ all;
      pos       = FirstPosition[dates, firstDate, {None}, {1}][[1]];
      If[! IntegerQ[pos] || pos <= 1 || pos >= Length[all], Return[{False, False}]];
      highs = #["High"] & /@ all;
      lows  = #["Low"] & /@ all;
      peakIndices   = FindPeaks[highs, sigmaArg][[All, 1]];
      valleyIndices = FindPeaks[-lows, sigmaArg][[All, 1]];
      {MemberQ[peakIndices, pos], MemberQ[valleyIndices, pos]}],
    {False, False}];

Options[updateFeatureSet] = {"StrictRanges" -> True, "ContinueExisting" -> False, "GraphChart" -> False,
  "SupportResistance" -> False, "TolerancePercent" -> 5, "MinTouches" -> 1, "Patterns" -> True, "PatternBand" -> 0.15,
  "HarmonicPatterns" -> False, "HarmonicTolerance" -> 0.05,
  "FirstCandlestickIsPeakQ" -> Automatic, "FirstCandlestickIsValleyQ" -> Automatic};
updateFeatureSet[featureSetArg_Dataset, candlesticksArg_Tabular, sigmaForPeaksAndValleysArg_Integer : 0, opts : OptionsPattern[]] :=
  Module[{featureSet,
   candlesticks = candlesticksArg,
    useNewAlgorithm = True,
    numberOfCandlesticks,
    candlesticksAsTabular,
    strictOption = OptionValue["StrictRanges"],
    continueOption = OptionValue["ContinueExisting"],
    chartOption = OptionValue["GraphChart"],
    supportResistanceOption = OptionValue["SupportResistance"],
    tolerancePercentOption = OptionValue["TolerancePercent"],
    minTouchesOption = OptionValue["MinTouches"],
    patternsOption = OptionValue["Patterns"],
    patternBandOption = OptionValue["PatternBand"],
    harmonicOption    = OptionValue["HarmonicPatterns"],
    harmonicTolOption = OptionValue["HarmonicTolerance"],
    firstPeakOption   = TrueQ[OptionValue["FirstCandlestickIsPeakQ"]],
    firstValleyOption = TrueQ[OptionValue["FirstCandlestickIsValleyQ"]]
    },

	(* append an empty featureSet to the current one. The empty set will have to be filled by the pipeline.
	   With "ContinueExisting" -> True, featureSetArg is an already-populated featureSet (for example a cached one)
	   and the per-candlestick loop below simply continues from its current state with the new candlesticks. *)
    featureSet = If[TrueQ[continueOption], featureSetArg, Append[featureSetArg, theEmptyFeatureSet]];
    Do[
	   candlesticksAsTabular = featureSet["Candlesticks"];
	   numberOfCandlesticks =  Length @candlesticksAsTabular;
	 	       
   	(* Add the new candlestick with its Index field *)
   	featureSet = addNewCandlestickToFeatureSet[featureSet, candlesticks[[index]]];
     
       (* Compute LastHigher and LastLower pointer fields *) 
	   featureSet = findPreviousHigherHighAndLowerLow[featureSet];
	  
	   (* Detect peaks and valleys and update all associated fields, i.e. isPeakQ, isValleyQ *)
	   featureSet = appendPeaksAndValleysToNew[featureSet,sigmaForPeaksAndValleysArg, firstPeakOption, firstValleyOption];
	 
       (* Compute valid support and resistance to this candlestick (rows added to Segments, lines kept in ValidTrendlines) *)
       If[TrueQ[supportResistanceOption],
         featureSet = getValidSupportAndResistanceToLastCandlestick[featureSet,
           "TolerancePercent" -> tolerancePercentOption, "MinTouches" -> minTouchesOption,
           "Patterns" -> patternsOption, "PatternBand" -> patternBandOption]];

       (* Compute valid wave segments to this candlestick *)
	   featureSet = getValidWavesToLastCandlestick[featureSet, "StrictRanges" -> strictOption];

	   (* Build/update the simplified Graph - new version *)
       featureSet = updateGraphFromSegments[featureSet, "StrictRanges" -> strictOption];

       (* Harmonic patterns born at this candlestick (X, A, B, C, D as a path of segments of the graph): rows added to HarmonicPatterns *)
       If[harmonicOption =!= False && harmonicOption =!= None,
         featureSet = getHarmonicPatternsBornAtLastCandlestick[featureSet,
           "Patterns" -> If[harmonicOption === True, All, harmonicOption], "Tolerance" -> harmonicTolOption]];
	       
       ,{index, Length@candlesticks}
    ];
    (* "GraphChart" -> True: add the key "GraphChart", the glyph picture of the last 250 candles *)
    If[chartOption =!= False && chartOption =!= None,
      featureSet = StockAnalyzer`Visualization`addGraphChart[featureSet, Sequence @@ Replace[chartOption, Except[_List] -> {}]]];
    Return[featureSet]
  ];

(* Convenience overload: fetches stock data and builds featureSet in one call *)
updateFeatureSet[tickerArg_String , testingPeriodArg_String , startDateArg_DateObject, endDateArg_DateObject, sigmaForPeaksAndValleysArg_Integer : 0, opts : OptionsPattern[]] :=
  Module[{featureSet, featureSetStub, candlesticks, strictOption = OptionValue["StrictRanges"], chartOption = OptionValue["GraphChart"],
      supportResistanceOption = OptionValue["SupportResistance"], tolerancePercentOption = OptionValue["TolerancePercent"], minTouchesOption = OptionValue["MinTouches"],
      patternsOption = OptionValue["Patterns"], patternBandOption = OptionValue["PatternBand"],
      harmonicOption = OptionValue["HarmonicPatterns"], harmonicTolOption = OptionValue["HarmonicTolerance"],
      firstPeakOption = OptionValue["FirstCandlestickIsPeakQ"], firstValleyOption = OptionValue["FirstCandlestickIsValleyQ"], firstFlags},
	candlesticks = StockAnalyzer`StockDataManager`getStock[tickerArg, startDateArg, endDateArg, testingPeriodArg];    
    (* Automatic: the first candlestick is a peak / a valley if it is one in the whole history of the stock *)
    If[firstPeakOption === Automatic || firstValleyOption === Automatic,
      firstFlags = firstCandlestickPeakValleyFlags[tickerArg, testingPeriodArg, candlesticks, sigmaForPeaksAndValleysArg];
      If[firstPeakOption === Automatic, firstPeakOption = firstFlags[[1]]];
      If[firstValleyOption === Automatic, firstValleyOption = firstFlags[[2]]]];
	featureSetStub = Dataset[<|"Ticker"->tickerArg,
								"Period"->testingPeriodArg,
								"StartDate"->startDateArg,
								"EndDate"->endDateArg,
								"PeaksAndValleysSigma"->sigmaForPeaksAndValleysArg, 
								"StrictRanges"->strictOption
							 |>];
    featureSet = updateFeatureSet[featureSetStub, candlesticks, sigmaForPeaksAndValleysArg,"StrictRanges"-> strictOption, "GraphChart" -> chartOption,
      "SupportResistance" -> supportResistanceOption, "TolerancePercent" -> tolerancePercentOption, "MinTouches" -> minTouchesOption,
      "Patterns" -> patternsOption, "PatternBand" -> patternBandOption,
      "HarmonicPatterns" -> harmonicOption, "HarmonicTolerance" -> harmonicTolOption,
      "FirstCandlestickIsPeakQ" -> firstPeakOption, "FirstCandlestickIsValleyQ" -> firstValleyOption];
    Return[featureSet];
  ];
  
Clear[getLinesFromTo];
getLinesFromTo[featureSetArg_Dataset, spanArg_Span] :=
  Module[{edgeSegs, fromBound, toBound, filtered},
    fromBound = spanArg[[1]];
    toBound   = spanArg[[2]];
    edgeSegs  = Which[
      AssociationQ[featureSetArg["EdgeSegments"]],
        featureSetArg["EdgeSegments"],
      True,
        Association[Normal[featureSetArg["EdgeSegments"]]]
    ];
    filtered = Select[
      Values[edgeSegs],
      Function[seg,
        fromBound <= seg["ToIndex"] <= toBound
      ]
    ];
    Association[
      Map[
        Function[seg,
          {seg["FromIndex"], seg["ToIndex"], seg["Direction"], seg["FromField"], seg["ToField"]} ->
            seg["Line"]["f"]
        ],
        filtered
      ]
    ]
  ];
  
(* ============================================================================
   getFibonacciLinesFromTo
   For all MaxU and MaxD edge segments whose ToIndex falls within spanArg,
   generates 10 horizontal Fibonacci line functions per segment.
   Returns an Association keyed by 
   {FromIndex, ToIndex, Direction, FromField, ToField, FibonacciRatio}
   where each value is a constant function f[x] = FromPrice + Ratio * Range.
   Only processes segments with EdgeType "MaxU" or "MaxD".

   Parameters:
     featureSetArg - updatedFeatureSet Dataset containing "EdgeSegments" key
     spanArg       - Span of the form fromIndex ;; toIndex

   Example:
     fibLines = getFibonacciLinesFromTo[updatedFeatureSet, 2 ;; 8]
     fibLines[{4, 5, "F", "High", "High", 0.618}]
     fibLines[{4, 5, "F", "High", "High", 0.618}][9]
   ============================================================================ *)

getFibonacciLinesFromTo::usage =
  "getFibonacciLinesFromTo[featureSetArg, spanArg] generates horizontal Fibonacci \
line functions from all MaxU and MaxD edge segments whose ToIndex falls within \
spanArg (a Span of the form fromIndex ;; toIndex). For each qualifying segment, \
8 horizontal lines are created using fibonacciRatios = {0., 0.236, 0.382, 0.500, \
0.618, 0.786, 1., 1.272} applied to the segment Range. Returns an \
Association keyed by {FromIndex, ToIndex, \"F\", FromField, ToField, FibonacciRatio} \
where each value is a constant pure function f[x] = FromPrice + Ratio * Range. \
Example: fibLines = getFibonacciLinesFromTo[updatedFeatureSet, 2 ;; 8] then \
fibLines[{4, 5, \"F\", \"High\", \"High\", 0.618}][9] returns the Fibonacci price.";

Clear[getFibonacciLinesFromTo];
getFibonacciLinesFromTo[featureSetArg_Dataset, spanArg_Span] :=
  Module[{edgeSegs, fromBound, toBound, filtered, result},
    fromBound       = spanArg[[1]];
    toBound         = spanArg[[2]];
    (* the ratios come from the package-wide fibonacciRatios (Fibonacci.wl); no local copy *)
    edgeSegs        = Which[
      AssociationQ[featureSetArg["EdgeSegments"]],
        featureSetArg["EdgeSegments"],
      True,
        Association[Normal[featureSetArg["EdgeSegments"]]]
    ];
    (* Filter to MaxU and MaxD segments within span *)
    filtered = Select[
      Values[edgeSegs],
      Function[seg,
        fromBound <= seg["ToIndex"] <= toBound &&
        MemberQ[{"MaxU", "MaxD"}, seg["EdgeType"]]
      ]
    ];
    (* For each segment, generate one entry per Fibonacci ratio *)
    result = Flatten[
      Map[
        Function[seg,
          Map[
            Function[ratio,
              With[{price = seg["FromPrice"] + ratio * seg["Range"]},
                {seg["FromIndex"], seg["ToIndex"], "F", 
                 seg["FromField"], seg["ToField"], ratio} ->
                  Function[x, price]
              ]
            ],
            fibonacciRatios
          ]
        ],
        filtered
      ],
      1
    ];
    Association[result]
  ];  

Clear[scoreLineGoodness];
scoreLineGoodness[lineFArg_, keyArg_, rowsArg_List,
                  fromIndexArg_Integer, finalIndexArg_Integer,
                  toleranceArg_Real, lambda_Real : 0.3] :=
  Module[{fields, counts, total, fromIndex, toIndex,
          fromField, toField, recencyFactor},
    fromIndex     = keyArg[[1]];
    toIndex       = keyArg[[2]];
    fromField     = keyArg[[4]];
    toField       = keyArg[[5]];
    recencyFactor = Exp[(-lambda) * (finalIndexArg - toIndex)];
    fields        = {"Open", "High", "Low", "Close"};
    counts = Association[Function[field,
      field -> recencyFactor * Count[rowsArg,
        row_ /;
          Abs[lineFArg[row["Index"]] - row[field]] / row[field] <= toleranceArg &&
          !(row["Index"] == fromIndex && field == fromField) &&
          !(row["Index"] == toIndex   && field == toField)
      ]
    ] /@ fields];
    total = Total[Values[counts]];
    Append[counts, "Total" -> total]
  ];
  
(* ============================================================================
   Default field-specific parameters from grid search on ABBV daily data
   May 21, 2026 - 315 combinations, 36 candlesticks
   ============================================================================ *)
defaultLambdaByField = <|
  "Open"  -> 0.80,
  "High"  -> 0.20,
  "Low"   -> 0.50,
  "Close" -> 0.80
|>;

defaultToleranceByField = <|
  "Open"  -> 0.025,
  "High"  -> 0.010,
  "Low"   -> 0.006,
  "Close" -> 0.018
|>;

(* \[HorizontalLine]\[HorizontalLine] Regime lambda adjustment factor \[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]
   Bull regime (signal > 0): increase lambda \[RightArrow] prefer recent lines
   Bear regime (signal < 0): decrease lambda \[RightArrow] trust older lines
   Sideways (signal \[TildeTilde] 0):    no change
   \[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine] *)
regimeLambdaFactor[signal_] := 1.0 + 0.5 * signal;

(* \[HorizontalLine]\[HorizontalLine] Regime touch count bonus \[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]
   Boosts lines whose direction aligns with the current regime:
   Bull:     U lines get bonus proportional to signal strength
   Bear:     D lines get bonus proportional to |signal| strength
   Sideways: F (Fibonacci) lines get a fixed 1.2x bonus
   Neutral lines or misaligned lines get no bonus (factor = 1.0)
   \[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine] *)
regimeBonus[signal_Real, direction_String] :=
  Which[
    signal >  0.3 && direction === "U",  1.0 + signal,
    signal < -0.3 && direction === "D",  1.0 + Abs[signal],
    Abs[signal] <= 0.3,                  1.2,
    True,                                1.0
  ];
  
Clear[predictNextCandlestick];
predictNextCandlestick[featureSetArg_, candlestickIndexArg_Integer,
                        toleranceArg_: Automatic,
                        topNArg_Integer: 1,
                        lambdaArg_: Automatic,
                        windowSizeArg_: Automatic,
                        regimeArg_: None] :=
  Module[{regularLines, fibLines, allLines, candlesticks, nextIndex,
          fields, fieldResults, edgeSegs, segLookup,
          lambdaByField, toleranceByField, windowByField,
          fromIndex},
    fields       = {"Open", "High", "Low", "Close"};
    candlesticks = featureSetArg["Candlesticks"];
    nextIndex    = candlestickIndexArg + 1;

    (* \[HorizontalLine]\[HorizontalLine] Resolve lambda per field \[HorizontalLine]\[HorizontalLine] *)
    lambdaByField = Association[Map[
      Function[field,
        field -> Which[
          lambdaArg === Automatic,
            defaultLambdaByField[field],
          AssociationQ[lambdaArg],
            Lookup[lambdaArg, field, defaultLambdaByField[field]],
          True,
            lambdaArg  (* plain Real \[LongDash] uniform *)
        ]
      ],
      fields
    ]];

    (* \[HorizontalLine]\[HorizontalLine] Resolve tolerance per field \[HorizontalLine]\[HorizontalLine] *)
    toleranceByField = Association[Map[
      Function[field,
        field -> Which[
          toleranceArg === Automatic,
            defaultToleranceByField[field],
          AssociationQ[toleranceArg],
            Lookup[toleranceArg, field, defaultToleranceByField[field]],
          True,
            toleranceArg  (* plain Real \[LongDash] uniform *)
        ]
      ],
      fields
    ]];

    (* \[HorizontalLine]\[HorizontalLine] Resolve window size per field \[HorizontalLine]\[HorizontalLine] *)
    (* Effective window: beyond Ceiling[3/lambda] candles, 
       contribution is < 5% \[LongDash] scanning further wastes computation *)
    windowByField = Association[Map[
      Function[field,
        field -> Which[
          windowSizeArg === Automatic,
            Ceiling[3.0 / lambdaByField[field]],
          True,
            windowSizeArg
        ]
      ],
      fields
    ]];

    (* \[HorizontalLine]\[HorizontalLine] Build segment lookup for full detail retrieval \[HorizontalLine]\[HorizontalLine] *)
    edgeSegs = Which[
      AssociationQ[featureSetArg["EdgeSegments"]],
        featureSetArg["EdgeSegments"],
      True,
        Association[Normal[featureSetArg["EdgeSegments"]]]
    ];
    segLookup = Association[
      Map[
        Function[seg,
          {seg["FromIndex"], seg["ToIndex"], seg["Direction"],
           seg["FromField"], seg["ToField"]} -> seg
        ],
        Values[edgeSegs]
      ]
    ];

    (* \[HorizontalLine]\[HorizontalLine] Score lines independently per field using field-specific params \[HorizontalLine]\[HorizontalLine] *)
    fieldResults = Association[
      Map[
        Function[field,
          Module[{lambda, tolerance, window, fromBound,
                  regularLinesF, fibLinesF, allLinesF,
                  scores, ranked, top, topKeys, topPredictions, topDetails},

            lambda    = lambdaByField[field];
            tolerance = toleranceByField[field];
            window    = windowByField[field];

            (* Limit scan window \[LongDash] no need to look further back than effective window *)
            fromBound = Max[1, candlestickIndexArg - window + 1];

            (* Get lines within effective window *)
            regularLinesF = getLinesFromTo[featureSetArg, fromBound ;; candlestickIndexArg];
            fibLinesF     = getFibonacciLinesFromTo[featureSetArg, fromBound ;; candlestickIndexArg];
            allLinesF     = Join[regularLinesF, fibLinesF];

            (* Score every line using this field's lambda and tolerance, adjusted by regime if provided *)
						scores = KeyValueMap[
						  Function[{key, f},
						    Module[{baseScore, adjustedLambda, direction, bonus},
						      (* Get direction from key \[LongDash] position 3 for both regular and Fibonacci *)
						      direction = key[[3]];
						
						      (* Adjust lambda based on regime signal *)
						      adjustedLambda = If[regimeArg === None,
						        lambda,
						        lambda * regimeLambdaFactor[regimeArg["Signal"]]
						      ];
						
						      (* Compute base score with adjusted lambda *)
						      baseScore = scoreLineGoodness[f, key, candlesticks,
						                                    fromBound, candlestickIndexArg,
						                                    tolerance, adjustedLambda];
						
						      (* Apply regime bonus to touch counts *)
						      bonus = If[regimeArg === None,
						        1.0,
						        regimeBonus[regimeArg["Signal"], direction]
						      ];
						
						      (* Return score with bonus applied to all fields *)
						      key -> Map[# * bonus &, baseScore]
						    ]
						  ],
						  allLinesF
						] // Association;

            (* Rank by this field's touch count descending *)
            ranked = ReverseSortBy[
              KeyValueMap[
                Function[{key, score}, {key, score[field]}],
                scores
              ],
              Last
            ];
            ranked = Select[ranked, Last[#] > 0 &];

            If[Length[ranked] == 0,
              Nothing,
              top     = Take[ranked, Min[topNArg, Length[ranked]]];
              topKeys = First /@ top;

              (* Build uniform output schema for each top line *)
              topDetails = Map[
                Function[entry,
                  Module[{key, touches, seg, prediction},
                    key        = entry[[1]];
                    touches    = entry[[2]];
                    prediction = allLinesF[key][nextIndex];
                    If[Length[key] == 5,
                      (* Regular line *)
                      seg = segLookup[key];
                      <|
                        "FromIndex"      -> seg["FromIndex"],
                        "ToIndex"        -> seg["ToIndex"],
                        "Direction"      -> seg["Direction"],
                        "FromField"      -> seg["FromField"],
                        "ToField"        -> seg["ToField"],
                        "EdgeType"       -> seg["EdgeType"],
                        "FromPrice"      -> seg["FromPrice"],
                        "ToPrice"        -> seg["ToPrice"],
                        "Range"          -> seg["Range"],
                        "Slope"          -> seg["Line"]["slope"],
                        "Intercept"      -> seg["Line"]["intercept"],
                        "FibonacciRatio" -> None,
                        "FibonacciPrice" -> None,
                        "Touches"        -> touches,
                        "Prediction"     -> prediction
                      |>,
                      (* Fibonacci line *)
                      Module[{ratio, parentKey, parentSeg, fibPrice},
                        ratio     = key[[6]];
                        parentKey = SelectFirst[
                          Keys[segLookup],
                          Function[k,
                            k[[1]] == key[[1]] && k[[2]] == key[[2]] &&
                            k[[4]] == key[[4]] && k[[5]] == key[[5]] &&
                            MemberQ[{"MaxU", "MaxD"}, segLookup[k]["EdgeType"]]
                          ],
                          Missing["NotFound"]
                        ];
                        If[MissingQ[parentKey],
                          Nothing,
                          parentSeg = segLookup[parentKey];
                          fibPrice  = parentSeg["FromPrice"] + ratio * parentSeg["Range"];
                          <|
                            "FromIndex"      -> parentSeg["FromIndex"],
                            "ToIndex"        -> parentSeg["ToIndex"],
                            "Direction"      -> "F",
                            "FromField"      -> parentSeg["FromField"],
                            "ToField"        -> parentSeg["ToField"],
                            "EdgeType"       -> "Fibonacci",
                            "FromPrice"      -> parentSeg["FromPrice"],
                            "ToPrice"        -> parentSeg["ToPrice"],
                            "Range"          -> parentSeg["Range"],
                            "Slope"          -> 0,
                            "Intercept"      -> fibPrice,
                            "FibonacciRatio" -> ratio,
                            "FibonacciPrice" -> fibPrice,
                            "Touches"        -> touches,
                            "Prediction"     -> prediction
                          |>
                        ]
                      ]
                    ]
                  ]
                ],
                top
              ];

              field -> <|
                "Lambda"      -> lambda,
                "Tolerance"   -> tolerance,
                "WindowSize"  -> window,
                "TopLines"    -> topDetails,
                "Mean"        -> Mean[Map[#["Prediction"] &, topDetails]]
              |>
            ]
          ]
        ],
        fields
      ]
    ];

    (* \[HorizontalLine]\[HorizontalLine] Add predicted Range = High - Low \[HorizontalLine]\[HorizontalLine] *)
    If[KeyExistsQ[fieldResults, "High"] && KeyExistsQ[fieldResults, "Low"],
      fieldResults = Append[fieldResults,
        "PredictedRange" -> <|
          "High"  -> fieldResults["High"]["Mean"],
          "Low"   -> fieldResults["Low"]["Mean"],
          "Range" -> fieldResults["High"]["Mean"] - fieldResults["Low"]["Mean"]
        |>
      ]
    ];

    fieldResults
  ];
    
Clear[backtestPredictions];
backtestPredictions[featureSetArg_, fromIndexArg_Integer,
                    toIndexArg_Integer, toleranceArg_: Automatic,
                    topNArg_Integer: 1, lambdaArg_: Automatic,
                    tickerArg_String: "", regimeWindowArg_Integer: 1] :=
  Module[{candlesticks, results, total, counter, regime, lastRegimeIndex},
    candlesticks   = featureSetArg["Candlesticks"];
    total          = toIndexArg - fromIndexArg + 1;
    counter        = 0;
    regime         = None;
    lastRegimeIndex = -999;  (* force regime computation on first iteration *)
    
		(* Auto-advance fromIndex to first index with valid predictions *)
		effectiveFromIndex = fromIndexArg;
		While[
		  effectiveFromIndex <= toIndexArg &&
		  Length[predictNextCandlestick[featureSetArg, effectiveFromIndex,
		                                 toleranceArg, topNArg, lambdaArg]] == 0,
		  effectiveFromIndex++
		];
		
		total = toIndexArg - effectiveFromIndex + 1;

    results = Monitor[
      Map[
        Function[i,
          counter++;

          (* Recompute regime every regimeWindowArg steps if ticker provided *)
          If[tickerArg =!= "" &&
             (i - lastRegimeIndex >= regimeWindowArg || regime === None),
            regime         = Quiet[getCurrentRegime[tickerArg]];
            lastRegimeIndex = i;
            If[FailureQ[regime], regime = None]
          ];

          Module[{prediction, actualList, actualRow},
            prediction = predictNextCandlestick[
                           featureSetArg, i, toleranceArg, topNArg,
                           lambdaArg, Automatic, regime];
            actualList = Normal[Select[candlesticks,
                           #["Index"] == i + 1 &]];
            If[Length[actualList] == 0,
              Nothing,
              actualRow = First[actualList];
              <|
                "PredictIndex"   -> i + 1,
                "Regime"         -> If[regime === None, Missing[],
                                       regime["Regime"]],
                "RegimeSignal"   -> If[regime === None, Missing[],
                                       regime["Signal"]],
                "ActualOpen"     -> actualRow["Open"],
                "PredictedOpen"  -> If[KeyExistsQ[prediction, "Open"],
                                       prediction["Open"]["Mean"], Missing[]],
                "ErrorOpen"      -> If[KeyExistsQ[prediction, "Open"],
                                       Abs[prediction["Open"]["Mean"] -
                                           actualRow["Open"]] / actualRow["Open"],
                                       Missing[]],
                "EdgeTypeOpen"   -> If[KeyExistsQ[prediction, "Open"],
                                       prediction["Open"]["TopLines"][[1]]["EdgeType"],
                                       Missing[]],
                "ActualHigh"     -> actualRow["High"],
                "PredictedHigh"  -> If[KeyExistsQ[prediction, "High"],
                                       prediction["High"]["Mean"], Missing[]],
                "ErrorHigh"      -> If[KeyExistsQ[prediction, "High"],
                                       Abs[prediction["High"]["Mean"] -
                                           actualRow["High"]] / actualRow["High"],
                                       Missing[]],
                "EdgeTypeHigh"   -> If[KeyExistsQ[prediction, "High"],
                                       prediction["High"]["TopLines"][[1]]["EdgeType"],
                                       Missing[]],
                "ActualLow"      -> actualRow["Low"],
                "PredictedLow"   -> If[KeyExistsQ[prediction, "Low"],
                                       prediction["Low"]["Mean"], Missing[]],
                "ErrorLow"       -> If[KeyExistsQ[prediction, "Low"],
                                       Abs[prediction["Low"]["Mean"] -
                                           actualRow["Low"]] / actualRow["Low"],
                                       Missing[]],
                "EdgeTypeLow"    -> If[KeyExistsQ[prediction, "Low"],
                                       prediction["Low"]["TopLines"][[1]]["EdgeType"],
                                       Missing[]],
                "ActualClose"    -> actualRow["Close"],
                "PredictedClose" -> If[KeyExistsQ[prediction, "Close"],
                                       prediction["Close"]["Mean"], Missing[]],
                "ErrorClose"     -> If[KeyExistsQ[prediction, "Close"],
                                       Abs[prediction["Close"]["Mean"] -
                                           actualRow["Close"]] / actualRow["Close"],
                                       Missing[]],
                "EdgeTypeClose"  -> If[KeyExistsQ[prediction, "Close"],
                                       prediction["Close"]["TopLines"][[1]]["EdgeType"],
                                       Missing[]],
                "PredictedRange" -> If[KeyExistsQ[prediction, "PredictedRange"],
                                       prediction["PredictedRange"]["Range"],
                                       Missing[]]
              |>
            ]
          ]
        ],
        Range[effectiveFromIndex, toIndexArg]
      ],
      (* Progress display *)
      Row[{
        ProgressIndicator[counter, {0, total}],
        "  ", counter, " / ", total,
        "  (predicting index ", fromIndexArg + counter, ")",
        If[regime =!= None,
          Row[{"  Regime: ", regime["Regime"],
               "  Signal: ", Round[regime["Signal"], 0.01]}],
          ""
        ]
      }]
    ];
    Dataset[DeleteCases[results, Nothing]]
  ]; 
  
  Clear[getCurrentRegime];
	getCurrentRegime[tickerArg_String, yearsArg_Integer: 2,
                  windowArg_Integer: 20, thresholdArg_Real: 0.02] :=
	  Module[{priceData, labeledData, P, curState, signal, persistence, P5},
	
	    (* Fetch prices using MarkovRegimeModel's FetchPrices
	       which now uses getStock as primary source *)
	    priceData = MarkovRegimeModel`FetchPrices[tickerArg, yearsArg];
	    If[FailureQ[priceData] || priceData === $Failed,
	      Return[$Failed]
	    ];
	
	    (* Label regimes *)
	    labeledData = MarkovRegimeModel`LabelRegimes[priceData, windowArg, thresholdArg];
	    If[FailureQ[labeledData],
	      Return[$Failed]
	    ];
	
	    (* Build transition matrix *)
	    P = MarkovRegimeModel`BuildTransitionMatrix[labeledData];
	
	    (* Current state is the last labeled day *)
	    curState    = Last[labeledData][[2]];
	    signal      = MarkovRegimeModel`SignalFromMatrix[P, curState];
	    persistence = P[[curState + 1, curState + 1]];
	
	    (* 5-day forecast *)
	    P5 = MarkovRegimeModel`NStepForecast[P, 5];
	
	    <|
	      "Regime"      -> <|0 -> "Bear", 1 -> "Sideways", 2 -> "Bull"|>[curState],
	      "RegimeIndex" -> curState,
	      "Signal"      -> signal,
	      "Persistence" -> persistence,
	      "P5"          -> <|
	        "Bear"     -> P5[[curState + 1, 1]],
	        "Sideways"  -> P5[[curState + 1, 2]],
	        "Bull"      -> P5[[curState + 1, 3]]
	      |>,
	      "Matrix"      -> P
	    |>
	  ];

	Clear[computeRegimeHistory];
	computeRegimeHistory[featureSetArg_, tickerArg_String,
	                      yearsArg_Integer: 2, windowArg_Integer: 20,
	                      thresholdArg_Real: 0.02] :=
	  Module[{candlesticks, indices, todayRegime, todayP5,
	          priceData, labeledData, result},
	
	    candlesticks = featureSetArg["Candlesticks"];
	    indices      = Normal[candlesticks[All, "Index"]];
	
	    (* Compute today's regime once \[LongDash] same for all candlesticks *)
	    todayRegime = getCurrentRegime[tickerArg, yearsArg, windowArg, thresholdArg];
	    todayP5     = If[AssociationQ[todayRegime], todayRegime["P5"],
	                     <|"Bear"->0.33, "Sideways"->0.34, "Bull"->0.33|>];
	
	    (* Fetch full price history for walk-forward historical regime *)
	    priceData   = MarkovRegimeModel`FetchPrices[tickerArg, yearsArg];
	    labeledData = If[!FailureQ[priceData],
	                    MarkovRegimeModel`LabelRegimes[priceData, windowArg, thresholdArg],
	                    $Failed];
	
	    result = Association[Map[
	      Function[idx,
	        Module[{historicalRegime, hBear, hSideways, hBull},
	
	          (* Walk-forward: use only data up to this candlestick's date *)
	          historicalRegime = If[
	            !FailureQ[labeledData] && Length[labeledData] >= windowArg,
	            Module[{subData, subLabeled, subP, curState, pi},
	              (* Take price data up to this candlestick index *)
	              subData    = Take[priceData, Min[idx + windowArg, Length[priceData]]];
	              subLabeled = MarkovRegimeModel`LabelRegimes[subData, windowArg, thresholdArg];
	              If[!FailureQ[subLabeled] && Length[subLabeled] > 0,
	                subP     = MarkovRegimeModel`BuildTransitionMatrix[subLabeled];
	                curState = Last[subLabeled][[2]];
	                pi       = MarkovRegimeModel`MarkovStationaryDist[subP];
	                <|"Bear" -> pi[[1]], "Sideways" -> pi[[2]], "Bull" -> pi[[3]], "Matrix"->subP|>,
	                <|"Bear" -> 0.33, "Sideways" -> 0.34, "Bull" -> 0.33|>
	              ]
	            ],
	            <|"Bear" -> 0.33, "Sideways" -> 0.34, "Bull" -> 0.33|>
	          ];
	
	          idx -> <|
	            "Matrix"             -> historicalRegime["Matrix"],
	            "HistoricalBear"     -> historicalRegime["Bear"],
	            "HistoricalSideways" -> historicalRegime["Sideways"],
	            "HistoricalBull"     -> historicalRegime["Bull"],
	            "TodayBear"          -> If[AssociationQ[todayRegime],
	                                       todayRegime["P5"]["Bear"], 0.33],
	            "TodaySideways"      -> If[AssociationQ[todayRegime],
	                                       todayRegime["P5"]["Sideways"], 0.34],
	            "TodayBull"          -> If[AssociationQ[todayRegime],
	                                       todayRegime["P5"]["Bull"], 0.33],
	            "Forecast5Bear"      -> todayP5["Bear"],
	            "Forecast5Sideways"  -> todayP5["Sideways"],
	            "Forecast5Bull"      -> todayP5["Bull"]
	          |>
	        ]
	      ],
	      indices
	    ]];
	
	    (* Store in global for infoBarText to access *)
	    Global`$regimeHistory = result;
	    result
	  ];
  
  (* Multi-scale wrapper: runs predictNextCandlestick and enriches the result *)
	Clear[predictNextCandlestickMultiScale];		
	predictNextCandlestickMultiScale[candlesticksArg_Tabular, featureSetArg_Dataset] :=
	  Module[{pred},
	    pred = predictNextCandlestick[candlesticksArg, featureSetArg];
	    (* predictNextCandlestick already returns an Association \[LongDash] just return it *)
	    If[AssociationQ[pred], pred, <||>]
	  ];
	  

(* Get a specific segment given the starting and ending indices *) 
Clear[getSegments];
getSegments[segmentsArg_Association,startIndexArg_Integer,endIndexArg_Integer] := Module[{res},
	res = Select[segmentsArg,(#["FromIndex"] === startIndexArg) && (#["ToIndex"] === endIndexArg)&];
	Return[If[res =!= <||>,First@res,<||>]];
]



Options[getFibonacciTouchesForSegmentAtExtension] = {
  "ExtraOnLeft"  -> 0,
  "ExtraOnRight" -> 0,
  "Tolerance"    -> 0.01,
  "FibRatios"    -> Automatic
};

Clear[getFibonacciTouchesForSegmentAtExtension];
getFibonacciTouchesForSegmentAtExtension[
    featureSetArg_Dataset,
    segmentArg_Association,
    extensionRatioArg_?NumericQ,
    opts : OptionsPattern[]] :=
  Module[{candles, totalCandlesCount, fibRatiosToUse, tolerance,
          extraOnLeft, extraOnRight,
          fromIdx, toIdx, fromPrice, toPrice, segRange, direction,
          segments, priorSeg, priorFromIdx, priorFound, extendedFromIdx,
          scanStartIdx, scanEndIdx, extendedCandlesticks,
          scanRange, rangeAllowedDelta, fibTouches},

    (* Resolve options *)
    fibRatiosToUse = OptionValue["FibRatios"];
    If[fibRatiosToUse === Automatic, fibRatiosToUse = fibonacciRatios];
    tolerance    = OptionValue["Tolerance"];
    extraOnLeft  = OptionValue["ExtraOnLeft"];
    extraOnRight = OptionValue["ExtraOnRight"];

    (* Candles as List *)
    candles           = Normal[featureSetArg["Candlesticks"]];
    totalCandlesCount = Length[candles];

    (* Segment info *)
    fromIdx   = segmentArg["FromIndex"];
    toIdx     = segmentArg["ToIndex"];
    fromPrice = segmentArg["FromPrice"];
    toPrice   = segmentArg["ToPrice"];
    direction = segmentArg["Direction"];
    segRange  = segmentArg["Range"];

    (* Longest prior opposite segment *)
    segments = If[KeyExistsQ[featureSetArg, "Segments"],
      featureSetArg["Segments"],
      Missing["NoSegments"]
    ];

    priorSeg = If[MissingQ[segments],
      Missing["NoSegments"],
      findLongestOppositeSegmentEndingAt[segments, fromIdx, direction]
    ];

    priorFound      = !MissingQ[priorSeg];
    priorFromIdx    = If[priorFound, priorSeg["FromIndex"], fromIdx];
    extendedFromIdx = priorFromIdx;

    (* Scan window: ExtraOnLeft before priorFromIdx, ExtraOnRight after toIdx *)
    scanStartIdx = Max[1, extendedFromIdx - extraOnLeft];
    scanEndIdx   = Min[totalCandlesCount, toIdx + extraOnRight];

    extendedCandlesticks = candles[[scanStartIdx ;; scanEndIdx]];

    (* Compute scan range from the single given extension ratio *)
    scanRange         = extensionRatioArg * segRange;
    rangeAllowedDelta = Abs[tolerance * scanRange];

    (* For each Fib ratio, count touches on extendedCandlesticks *)
    fibTouches = Association[
      Map[
        Function[ratio,
          Module[{fibPrice, touchCount, touchDetails},
            fibPrice = fromPrice + ratio * scanRange;  (* exact line price, as the chart uses; no rounding to 0.01 *)
            {touchCount, touchDetails} = Fold[
              Function[{acc, row},
                Module[{rowTouches},
                  rowTouches = Map[
                    Function[field,
                      Module[{fieldPrice, delta, inBand},
                        fieldPrice = row[field];
                        delta      = fieldPrice - fibPrice;
                        inBand     = -rangeAllowedDelta <= delta <= rangeAllowedDelta;
                        If[inBand,
                          Association[
                            "CandlestickIndex" -> row["Index"],
                            "PriceType"        -> field,
                            "CandlestickPrice" -> fieldPrice,
                            "FibonacciRatio"   -> ratio,
                            "FibPrice"         -> fibPrice,
                            "SignedDistance"   -> If[scanRange != 0,
                              Round[delta / scanRange, 0.001],
                              0.0
                            ],
                            "Delta"            -> Round[delta, 0.01]
                          ],
                          Nothing
                        ]
                      ]
                    ],
                    {"Open", "High", "Low", "Close"}
                  ];
                  {acc[[1]] + Length[rowTouches], Join[acc[[2]], rowTouches]}
                ]
              ],
              {0, {}},
              extendedCandlesticks
            ];
            If[touchCount > 0,
              ratio -> Association[
                "TouchCount"   -> touchCount,
                "TouchDetails" -> touchDetails
              ],
              Nothing
            ]
          ]
        ],
        fibRatiosToUse
      ]
    ];

    fibTouches
  ];	

(* Returns <|"PreTouches" -> n, "InTouches" -> n, "PostTouches" -> n|>
   where InTouches already has the 2 tautological touches subtracted. *)
  
(* Correct from now on *)
countTouchesByRegion[fibTouchesArg_, fromIndexArg_Integer, toIndexArg_Integer] :=
  Module[{allDetails, pre, in, post},
    allDetails = Flatten[Values[#["TouchDetails"] & /@ fibTouchesArg], 1];
    pre  = Count[allDetails, d_ /; d["CandlestickIndex"] < fromIndexArg];
    in   = Count[allDetails, d_ /; fromIndexArg <= d["CandlestickIndex"] <= toIndexArg];
    post = Count[allDetails, d_ /; d["CandlestickIndex"] > toIndexArg];
    <|
      "PreTouches"   -> pre,
      "InTouches"    -> Max[in - 1, 0],
      "PostTouches"  -> post,
      "InTouchesRaw" -> in
    |>
  ]  																																																																
(* --- Pluggable lift computation functions --- *)

Clear[liftFunctionInTouchesOnly];
liftFunctionInTouchesOnly[liftArgs_Association] :=
  Module[{inTouches, inCandleCount, extRatio, toleranceArg, fibRatiosArg,
          priceTypes, numPriceTypes, numFibLines, hitProbability,
          observedHits, expectedHits, liftValue},

    inTouches     = liftArgs["InTouches"];
    inCandleCount = liftArgs["InCandleCount"];
    extRatio      = liftArgs["ExtRatio"];
    toleranceArg  = liftArgs["Tolerance"];
    fibRatiosArg  = liftArgs["FibRatios"];
    priceTypes    = liftArgs["PriceTypes"];
    numPriceTypes = Length[priceTypes];

    numFibLines = Count[fibRatiosArg, r_ /; 0 <= r <= 1];
    hitProbability = numFibLines * 2 * toleranceArg;

    observedHits = inTouches;
    expectedHits = inCandleCount * numPriceTypes * hitProbability;

    expectedHits = Max[expectedHits - 1, 0];  (* new: match observed's -1 *)

    liftValue = If[expectedHits > 0,
      N[observedHits / expectedHits],
      Missing["ZeroExpected"]
    ];

    {observedHits, expectedHits, liftValue}
  ];
  
$defaultLiftFunction = liftFunctionInTouchesOnly;



liftFunctionPrePlusIn[liftArgsArg_Association] :=
  Module[{preTouches, inTouches, preCandleCount, inCandleCount,
          toleranceArg, fibRatiosArg, priceTypes,
          numPriceTypes, numFibLines, hitProbability,
          observedHits, expectedHits, liftValue},

    preTouches     = liftArgsArg["PreTouches"];
    inTouches      = liftArgsArg["InTouches"];
    preCandleCount = liftArgsArg["PreCandleCount"];
    inCandleCount  = liftArgsArg["InCandleCount"];
    toleranceArg   = liftArgsArg["Tolerance"];
    fibRatiosArg   = liftArgsArg["FibRatios"];
    priceTypes     = liftArgsArg["PriceTypes"];
    numPriceTypes  = Length[priceTypes];

    (* Fib lines inside the base scan range [0, 1] *)
    numFibLines = Count[fibRatiosArg, r_ /; 0 <= r <= 1];

    (* Null-model: probability each independent price point lands within tolerance of ANY fib line *)
    hitProbability = numFibLines * 2 * toleranceArg;

    (* Observed: combine pre-segment and in-segment touches.
       InTouches already excludes the FromIndex tautological touch. *)
    observedHits = preTouches + inTouches;

    (* Expected: all price points in both regions * hitProbability,
       then subtract 1 to match the tautological subtraction on the observed side. *)
    expectedHits = Max[
      (preCandleCount + inCandleCount) * numPriceTypes * hitProbability - 1,
      0
    ];

    liftValue = If[expectedHits > 0,
      N[observedHits / expectedHits],
      Missing["ZeroExpected"]
    ];

    {observedHits, expectedHits, liftValue}
  ];


(* $priceTypeWeights can be tuned/overridden; defaults give High and Low 1.5x weight *)
$priceTypeWeights = <|"High" -> 1.5, "Low" -> 1.5, "Open" -> 0.75, "Close" -> 0.75|>;
Clear[liftFunctionWeightedByPriceType];
Clear[liftFunctionWeightedByPriceType];
liftFunctionWeightedByPriceType[liftArgs_Association] :=
  Module[{weightedObserved, inCandleCount, toleranceArg, fibRatiosArg,
          priceTypes, allTouchDetails, fromIdx, toIdx, inTouchDetails,
          segmentDirection, numFibLines, hitProbability, expectedHits,
          liftRatio, totalWeight, tautologicalType, tautologicalWeight},

    (* ---- Pull standard liftArgs keys (as built in scanFibonacciTouches) ---- *)
    inCandleCount    = liftArgs["InCandleCount"];
    toleranceArg     = liftArgs["Tolerance"];
    fibRatiosArg     = liftArgs["FibRatios"];
    priceTypes       = liftArgs["PriceTypes"];
    allTouchDetails  = liftArgs["AllTouchDetails"];
    fromIdx          = liftArgs["FromIdx"];
    toIdx            = liftArgs["ToIdx"];
    segmentDirection = liftArgs["Segment"]["Direction"];

    (* ---- Derive in-segment touches from AllTouchDetails ---- *)
    inTouchDetails = Select[allTouchDetails,
      fromIdx <= #["CandlestickIndex"] <= toIdx &];

    numFibLines    = Count[fibRatiosArg, r_ /; 0 <= r <= 1];
    hitProbability = numFibLines * 2 * toleranceArg;

    (* ---- Weighted observed: sum weight(PriceType) over in-segment touches ---- *)
    weightedObserved = Total[
      Lookup[$priceTypeWeights, #["PriceType"], 1.0] & /@ inTouchDetails
    ];

    (* ---- Weighted expected: for each candle, sum weight(each priceType) * hitProbability ---- *)
    totalWeight  = Total[Lookup[$priceTypeWeights, #, 1.0] & /@ priceTypes];
    expectedHits = inCandleCount * totalWeight * hitProbability;

    (* ---- Subtract the weight of the single tautological FromIndex-extreme touch
            (Low for Up segments, High for Down). Floored at 0. ---- *)
    tautologicalType   = If[segmentDirection === "U", "Low", "High"];
    tautologicalWeight = Lookup[$priceTypeWeights, tautologicalType, 1.0];
    expectedHits       = Max[expectedHits - tautologicalWeight, 0];

    liftRatio = If[expectedHits > 0,
      N[weightedObserved / expectedHits],
      Missing["ZeroExpected"]
    ];

    (* ---- Standard 3-element contract: {observed, expected, lift} ---- *)
    {weightedObserved, N[expectedHits], liftRatio}
  ];
  
Clear[scanFibonacciTouches];
Options[scanFibonacciTouches] = {
  "ExtraOnLeft"  -> 0,
  "ExtraOnRight" -> 0,
  "LiftFunction" -> liftFunctionInTouchesOnly
};

scanFibonacciTouches[featureSetArg_,
                      segmentArg_Association,
                      fibRatiosArg_List : fibonacciRatios,
                      toleranceArg_Real : 0.01,
                      startingExtensionArg : (_?NumericQ) : 1.0,
                      endingExtensionArg : (_?NumericQ | Automatic) : Automatic,
                      opts : OptionsPattern[]] := 
  Module[{fromIdx, toIdx, fromPrice, toPrice, segRange, 
          candlesticksArg,
          direction,
          startingScanPrice, endingScanPrice,
          scanPrices, updatedScanPrices,
          scanExtensionRatios,
          results, sortedResults, scanResultsTabular, maxTouches,
          segments, priorSeg, priorFromIdx, priorFound,
          extendedFromIdx, extraOnLeft, extraOnRight,
          scanStartIdx, scanEndIdx, totalCandlesCount,
          resolvedEndingExtension, rawExtensions,
          liftFn, priceTypes},

    priceTypes        = {"Open", "High", "Low", "Close"};
    candlesticksArg   = Normal[featureSetArg["Candlesticks"]]; (* It is a List *) 
    extraOnLeft       = OptionValue["ExtraOnLeft"];
    extraOnRight      = OptionValue["ExtraOnRight"];
    liftFn            = OptionValue["LiftFunction"];
    totalCandlesCount = Length[candlesticksArg];

    fromIdx   = segmentArg["FromIndex"];
    toIdx     = segmentArg["ToIndex"];
    fromPrice = segmentArg["FromPrice"];
    toPrice   = segmentArg["ToPrice"];
    direction = segmentArg["Direction"];
    segRange  = segmentArg["Range"];

	(* Guard: bail out if getSegments handed us a sentinel sub-segment with
	   non-numeric prices/range. Returning an Association without a ScanResults
	   key lets scanFibonacciTouchesForSegment's parent-level Select drop us. *)
	If[! NumericQ[fromPrice] || ! NumericQ[segRange] || segRange === 0.,
	  Return[Association[
	    FromIndex -> fromIdx,
	    ToIndex   -> toIdx,
	    FromPrice -> fromPrice,
	    ToPrice   -> toPrice,
	    Reason    -> "InvalidOrEmptySegment"
	  ]]
	];
	
    segments = If[KeyExistsQ[featureSetArg, "Segments"],
      featureSetArg["Segments"],
      Missing["NoSegments"]
    ];

    priorSeg = If[MissingQ[segments],
      Missing["NoSegments"],
      findLongestOppositeSegmentEndingAt[segments, fromIdx, direction]
    ];

    priorFound      = !MissingQ[priorSeg];
    priorFromIdx    = If[priorFound, priorSeg["FromIndex"], fromIdx];
    extendedFromIdx = priorFromIdx;

    scanStartIdx = Max[1, extendedFromIdx - extraOnLeft];
    scanEndIdx   = Min[totalCandlesCount, toIdx + extraOnRight];

    (* Resolve endingExtensionArg: Automatic snaps to the next multiple of
       toleranceArg at or above If[ValueQ[Global`defaultMaxExtension], Global`defaultMaxExtension, 1.31], which with tol=0.01 is already on the step grid
       (1.00, 1.01, ..., 1.31: 32 extensions). *)
        resolvedEndingExtension = If[endingExtensionArg === Automatic,
        If[ValueQ[Global`defaultMaxExtension], Global`defaultMaxExtension, 1.31],
      endingExtensionArg
    ];

    (* Guard: start must be strictly less than end. *)
    If[startingExtensionArg >= resolvedEndingExtension,
      Message[scanFibonacciTouches::badRange,
        startingExtensionArg, resolvedEndingExtension];
      Return[$Failed]
    ];

    (* Build extensions directly as a clean grid. Range[start, end, step] starts
       exactly at start; if (end-start) isn't a multiple of step, Range stops short,
       so append end to guarantee the top is covered. Final Round at tolerance/10
       scrubs any accumulated ulp drift from the Range additions. *)
    rawExtensions = Range[startingExtensionArg, resolvedEndingExtension, toleranceArg];
    scanExtensionRatios = Round[
      If[Length[rawExtensions] == 0 ||
         Last[rawExtensions] < resolvedEndingExtension - toleranceArg / 2,
        Append[rawExtensions, resolvedEndingExtension],
        rawExtensions
      ],
      toleranceArg / 10
    ];

    (* Derive scan prices FROM the clean extensions (not the other way around).
       First scan price = fromPrice + startingExtensionArg * segRange = toPrice
       when startingExtensionArg == 1.0. *)
    scanPrices = Round[fromPrice + # * segRange, 0.01] & /@ scanExtensionRatios;
    updatedScanPrices = scanPrices;  (* keep name for compatibility if used below *)
    
    results = Association[
      MapThread[
        Function[{scanPrice, extRatio},
          Module[{fibTouches, totalTouches, scanRange,
                  allTouchDetails, candleIndices,
                  preTouches, inTouches, postTouches,
                  preCandleCount, inCandleCount, postCandleCount,
                  liftArgs, observedHits, expectedHits, liftValue},

            fibTouches = getFibonacciTouchesForSegmentAtExtension[
              featureSetArg, segmentArg, extRatio,
              "ExtraOnLeft"  -> extraOnLeft,
              "ExtraOnRight" -> extraOnRight,
              "Tolerance"    -> toleranceArg,
              "FibRatios"    -> fibRatiosArg
            ];

            totalTouches = Total[Map[#["TouchCount"] &, Values[fibTouches]]];
            scanRange    = scanPrice - fromPrice;

            allTouchDetails = Flatten[Values[Map[#["TouchDetails"] &, fibTouches]]];
            candleIndices   = Map[#["CandlestickIndex"] &, allTouchDetails];

            preTouches  = Count[candleIndices, idx_ /; idx < fromIdx];
            inTouches   = Count[candleIndices, idx_ /; fromIdx <= idx <= toIdx];
            postTouches = Count[candleIndices, idx_ /; idx > toIdx];

            inTouches = Max[inTouches - 1, 0];

            preCandleCount  = fromIdx - scanStartIdx;
            inCandleCount   = toIdx - fromIdx + 1;
            postCandleCount = scanEndIdx - toIdx;

            (* --- Build arguments Association for the lift function --- *)
            liftArgs = Association[
              "PreTouches"      -> preTouches,
              "InTouches"       -> inTouches,
              "PostTouches"     -> postTouches,
              "TotalTouches"    -> totalTouches,
              "PreCandleCount"  -> preCandleCount,
              "InCandleCount"   -> inCandleCount,
              "PostCandleCount" -> postCandleCount,
              "ExtRatio"        -> extRatio,
              "ScanRange"       -> scanRange,
              "SegRange"        -> segRange,
              "FromPrice"       -> fromPrice,
              "FromIdx"         -> fromIdx,
              "ToIdx"           -> toIdx,
              "ScanStartIdx"    -> scanStartIdx,
              "ScanEndIdx"      -> scanEndIdx,
              "Tolerance"       -> toleranceArg,
              "FibRatios"       -> fibRatiosArg,
              "PriceTypes"      -> priceTypes,
              "Candles"         -> candlesticksArg,
              "FibTouches"      -> fibTouches,
              "AllTouchDetails" -> allTouchDetails,
              "FeatureSet"      -> featureSetArg,
              "Segment"         -> segmentArg
            ];

            (* --- Call the pluggable lift function --- *)
            {observedHits, expectedHits, liftValue} = liftFn[liftArgs];

            extRatio -> Association[
              "NormalizedRatio" -> extRatio,
              "Touches"         -> totalTouches,
              "PreTouches"      -> preTouches,
              "InTouches"       -> inTouches,
              "PostTouches"     -> postTouches,
              "ObservedHits"    -> observedHits,
              "ExpectedHits"    -> expectedHits,
              "Lift"            -> liftValue,
              "ScanPrice"       -> scanPrice,
              "ScanRange"       -> scanRange,
              "FibTouches"      -> fibTouches
            ]
          ]
        ],
        {updatedScanPrices, scanExtensionRatios}
      ]
    ];

    maxTouches = Max[Map[#["Touches"] &, Values[results]]];

    sortedResults = Association[
      SortBy[Normal[results],
        {-#[[2]]["Touches"] &, Abs[1.0 - #[[2]]["NormalizedRatio"]] &}
      ]
    ];

    scanResultsTabular = Tabular[
      Map[
        Function[entry,
          Association[
            "NormalizedRatio" -> entry["NormalizedRatio"],
            "Touches"         -> entry["Touches"],
            "PreTouches"      -> entry["PreTouches"],
            "InTouches"       -> entry["InTouches"],
            "PostTouches"     -> entry["PostTouches"],
            "ObservedHits"    -> entry["ObservedHits"],
            "ExpectedHits"    -> entry["ExpectedHits"],
            "Lift"            -> entry["Lift"],
            "ScanPrice"       -> entry["ScanPrice"],
            "ScanRange"       -> entry["ScanRange"]
          ]
        ],
        Values[sortedResults]
      ]
    ];

    Association[
      "FromIndex"             -> fromIdx,
      "ToIndex"               -> toIdx,
      "FromPrice"             -> fromPrice,
      "ToPrice"               -> toPrice,
      "MaxTouches"            -> maxTouches,
      "ScanResultsTable"      -> scanResultsTabular,
      "ScanResults"           -> sortedResults,
      "PriorSegmentFromIndex" -> If[priorFound, priorSeg["FromIndex"], Missing["NotFound"]],
      "PriorSegmentToIndex"   -> If[priorFound, priorSeg["ToIndex"],   Missing["NotFound"]],
      "PriorSegmentFound"     -> priorFound,
      "ScanStartIndex"        -> scanStartIdx,
      "ScanEndIndex"          -> scanEndIdx,
      "ExtraOnLeft"           -> extraOnLeft,
      "ExtraOnRight"          -> extraOnRight,
      "LiftFunction"          -> liftFn
    ]
  ];

Clear[fibonacciTouchesTabular];
fibonacciTouchesTabular[scanResultArg_Association] :=
  Module[{segKey},
    segKey = {scanResultArg["FromIndex"], scanResultArg["ToIndex"]};
    Tabular[Flatten[
      Map[
        Function[scanEntry,
          Map[
            Prepend[#, "Segment" -> segKey] &,
            Flatten[Map[#["TouchDetails"] &, Values[scanEntry["FibTouches"]]]]
          ]
        ],
        Values[scanResultArg["ScanResults"]]
      ]
    ]]
  ];

Clear[fibonacciTouchesTabularAll];
fibonacciTouchesTabularAll[bySegmentArg_Association] :=
  Tabular[Flatten[
    KeyValueMap[
      Function[{segKey, segResult},
        Map[
          Prepend[#, "Segment" -> segKey] &,
          Flatten[
            Map[
              Function[scanEntry,
                Map[#["TouchDetails"] &, Values[scanEntry["FibTouches"]]]
              ],
              Values[segResult["ScanResults"]]
            ]
          ]
        ]
      ],
      bySegmentArg
    ]
  ]
];

  
Clear[scanTrendlineTouches];
scanTrendlineTouches[<||>, ___] := <||>;
scanTrendlineTouches[segmentArg_Association,
                     candlesticksArg_List,
                     fibRatiosArg_List : fibonacciRatios,
                     toleranceArg_Real : 0.01,
                     nStepsArg_Integer : 32, 
                     nextPricesCountArg_Integer : 8
                     ] :=
  Module[{fromIdx, toIdx, fromPrice, toPrice, segRange,
          startingScanPrice, endingScanPrice, scanPrices, insideCandlesticks,
          updatedScanPrices, results, maxTouches, nextPrices,scanResultsTabular,sortedResults},

    fromIdx   = segmentArg["FromIndex"];
    toIdx     = segmentArg["ToIndex"];
    fromPrice = segmentArg["FromPrice"];
    toPrice   = segmentArg["ToPrice"];
    segRange  = segmentArg["Range"];

    startingScanPrice = fromPrice + 1.0 * segRange;
    endingScanPrice   = fromPrice + If[ValueQ[Global`defaultMaxExtension], Global`defaultMaxExtension, 1.31] * segRange;
    scanPrices        = Round[Subdivide[startingScanPrice, endingScanPrice, nStepsArg - 1], 0.01];
    updatedScanPrices = If[First[scanPrices] <= Last[scanPrices],
						   scanPrices,
						   ReverseSort[scanPrices]
						  ];
    updatedScanPrices = Round[updatedScanPrices, 0.01];
    
    insideCandlesticks = candlesticksArg[[fromIdx ;; Min[toIdx, Length[candlesticksArg]]]];

    results = Association[Map[
      Function[scanPrice,
        Module[{scanRange, normalizedRatio, rangeAllowedDelta,
                touchCount, touchDetails, segmentLine},        (* <-- added segmentLine here *)
          scanRange         = scanPrice - fromPrice;
          normalizedRatio   = Round[scanRange / segRange, 0.001];
          rangeAllowedDelta = Abs[toleranceArg * scanRange];

          {touchCount, touchDetails} = Fold[
            Function[{acc, row},
              Module[{candleIdx, segmentPrice, rowTouches},
                candleIdx    = row["Index"];
                segmentPrice = fromPrice +
                               ((candleIdx - fromIdx) / (toIdx - fromIdx)) * scanRange;
                rowTouches   = Map[
                  Function[field,
                    Module[{fieldPrice, delta, inBand},
                      fieldPrice = row[field];
                      delta      = segmentPrice - fieldPrice;
                      inBand     = -rangeAllowedDelta <= delta <= rangeAllowedDelta;
                      If[inBand,
                        <|
                          "CandlestickIndex" -> candleIdx,
                          "PriceType"        -> field,
                          "Price"            -> fieldPrice,
                          "SegmentPrice"     -> segmentPrice,
                          "SignedDistance"   -> Round[delta / scanRange, 0.001],
                          "Delta"            -> Round[delta, 0.01]
                        |>,
                        Nothing
                      ]
                    ]
                  ],
                  {"Open", "High", "Low", "Close"}
                ];
                {acc[[1]] + Length[rowTouches], Join[acc[[2]], rowTouches]}
              ]
            ],
            {0, {}},
            insideCandlesticks
          ];

				(* Line function from {FromIndex,FromPrice} to {ToIndex,scanPrice} *)
				segmentLine = Function[indexArg,
				  fromPrice + ((indexArg - fromIdx) / (toIdx - fromIdx)) * scanRange
				];
				
				(* Next N predicted prices beyond ToIndex *)
				nextPrices = Association[Map[
				  Function[idx, idx -> segmentLine[idx]],
				  Range[toIdx + 1, toIdx + nextPricesCountArg]
				]];			
					
				normalizedRatio -> <|
				  "NormalizedRatio" -> normalizedRatio, 
				  "Touches"         -> touchCount,
				  "ScanPrice"       -> scanPrice,
				  "ScanRange"       -> scanRange,
				  "TouchDetails"    -> touchDetails,
				  "NextPrices"      -> nextPrices,              (* <-- added here *)
				  "SegmentLine"     -> segmentLine
				|>
			 ]
      ],
      updatedScanPrices
    ]];

		(* Sort by best number of touches and then by closest to 1.00 *) 
		results = Association[
		  SortBy[
		    Normal[results],
		    {-#[[2]]["Touches"] &, Abs[1.0 - #[[2]]["NormalizedRatio"]] &}
		  ]
		];
		
		
		(* Compute MaxTouches before sorting *)
		maxTouches = Max[Map[#["Touches"] &, Values[results]]];
		
		(* Sort results in descending order of Touches, ties broken by ratios closest to 1.0 *)
		sortedResults = Association[
		  SortBy[
		    Normal[results],
		    {-#[[2]]["Touches"] &, Abs[1.0 - #[[2]]["NormalizedRatio"]] &}
		  ]
		];
		
		(* Build Tabular from already-sorted results - no need to re-sort *)
		scanResultsTabular = Tabular[
		  Map[
		    Function[entry,
		      <|
		        "NormalizedRatio" -> entry["NormalizedRatio"],
		        "Touches"         -> entry["Touches"],
		        "ScanPrice"       -> entry["ScanPrice"],
		        "ScanRange"       -> entry["ScanRange"]
		      |>
		    ],
		    Values[sortedResults]
		  ]
		];
		
		Return[<|
      "FromIndex"        -> fromIdx,
      "ToIndex"          -> toIdx,
		  "MaxTouches"       -> maxTouches,
		  "ScanResultsTable" -> scanResultsTabular,
		  "ScanResults"      -> sortedResults
		|>];
					
  ];
  
Clear[scanTrendlineTouchesForSegment];
scanTrendlineTouchesForSegment[desiredSegmentsArg_Association,
                              candlesticksArg_List,
                              fromIndexArg_Integer,
                              toIndexArg_Integer,
                              toleranceArg_Real : 0.01,
                              nStepsArg_Integer : 32] :=
  Module[{raw, filtered},
    raw = <|
      {fromIndexArg, #} -> scanTrendlineTouches[
        getSegments[desiredSegmentsArg, fromIndexArg, #],
        candlesticksArg, toleranceArg, nStepsArg]
      & /@ Range[fromIndexArg + 1, toIndexArg]
    |>;
    filtered = Select[raw, KeyExistsQ[#, "ScanResults"] &];
    Association[
      ReverseSortBy[
        Normal[filtered],
        #[[2]]["MaxTouches"] &
      ]
    ]
  ];

Clear[scanFibonacciTouchesForSegment];

Options[scanFibonacciTouchesForSegment] = {
  "ExtraOnLeft"  -> 0,
  "ExtraOnRight" -> 0
};

scanFibonacciTouchesForSegment[featureSetArg_Dataset,
                             desiredSegmentsArg_Association,
                             fromIndexArg_Integer,
                             toIndexArg_Integer,
                             fibRatiosArg_List : fibonacciRatios,
                             toleranceArg_Real : 0.01,
                             startingExtensionArg_Real : 1.0,
                             endingExtensionArg : (_?NumericQ | Automatic) : Automatic,
                             opts : OptionsPattern[]] :=
     Module[{raw, filtered, extraOnLeft, extraOnRight},

    extraOnLeft  = OptionValue["ExtraOnLeft"];
    extraOnRight = OptionValue["ExtraOnRight"];

    raw = <|{fromIndexArg, #} -> scanFibonacciTouches[featureSetArg
                                                      ,getSegments[desiredSegmentsArg, fromIndexArg, #]
                                                      ,fibRatiosArg
                                                      ,toleranceArg
                                                      ,startingExtensionArg
                                                      ,endingExtensionArg
                                                      ,"ExtraOnLeft"  -> extraOnLeft
                                                      ,"ExtraOnRight" -> extraOnRight
                                                     ] & /@ Range[fromIndexArg + 1, toIndexArg]
          |>;
    filtered = Select[raw, KeyExistsQ[#, "ScanResults"] &];

    Association[
      SortBy[
        Normal[filtered],
        #[[2]]["MaxTouches"] &
      ]
    ]
  ];               
     
Clear[candlestickAsRatiosToSegment];                                                                                                 
candlestickAsRatiosToSegment[segmentArg_Association,candlestickArg_Association,fibRatiosArg_List : fibonacciRatios]:= Module[{ratios, ohlc, baseSegmentRange,fibPrices},
 ohlc = KeyTake[candlestickArg,{"Open","High","Low","Close"}];
 baseSegmentRange = segmentArg["Range"];
 fibPrices = AssociationMap[segmentArg["FromPrice"] +(# * baseSegmentRange) &,fibRatiosArg];
 ratios = ((#-segmentArg["FromPrice"]) / baseSegmentRange)& /@ ohlc;
 (* Print[{"baseSegmentRange:",baseSegmentRange," FromPrice: ",segmentArg["FromPrice"]," ToPrice: ",segmentArg["ToPrice"],Dataset@ohlc,Dataset@ratios,Dataset@fibPrices}]; *) 
 
 Return[ratios];
]

Clear[ratioToSegmentAsPrice];	
ratioToSegmentAsPrice[segmentArg_Association,ratioArg_?NumericQ] := segmentArg["FromPrice"]+ ratioArg * segmentArg["Range"]                                      
  
Clear[predictPrice];
predictPrice[segmentArg_Association, candlestickArg_TabularRow, priceTypeArg_String, fibRatioArg_Real, touchTolerancePercentArg_: 0.01] := 
  Module[{fromPrice, typePrice, delta, band, predictedPrice},
    fromPrice = segmentArg["FromPrice"];
    typePrice = candlestickArg[priceTypeArg];
    (* Get the difference between the candlestick price and the segment's fromPrice *) 
    delta = typePrice - fromPrice;
    (* Create the band around the candlestick price. This is even MORE CONSERVATIVE *) 
    band = CenteredInterval[delta,touchTolerancePercentArg*delta];
    (* Now, predict the ending price that would match this with the current fibonacci ratio *)
    predictedPrice = fromPrice + band / fibRatioArg;
    (* Print[delta,"  ",band, "-->",predictedPrice]; *) 			      
	Return[predictedPrice];
]  
  
Clear[predictionsForSegment];
predictionsForSegment[targetSegmentArg_Association,
                      candlesticksArg_Tabular,
                      priceTypesArg_List : {"Open", "High", "Low", "Close"},
                      fibonacciRatiosArg_List : fibonacciRatios,
                      touchTolerancePercentArg_: 0.01] :=
  With[{fromIdx = targetSegmentArg["FromIndex"],
        toIdx   = targetSegmentArg["ToIndex"]},
    AssociationMap[
      Function[idx,
        With[{currentCandlestick = candlesticksArg[[idx]]},
          AssociationMap[
            Function[priceType,
              AssociationMap[
                Function[fibRatio,
                  predictPrice[targetSegmentArg, currentCandlestick, priceType, fibRatio, touchTolerancePercentArg]
                ],
                fibonacciRatiosArg
              ]
            ],
            priceTypesArg
          ]
        ]
      ],
      Range[fromIdx, toIdx]
    ]
  ] 
  
Clear[collectBands];
collectBands[expr_] := Cases[expr, _CenteredInterval, Infinity] // Sort;

Clear[intervalBounds];
intervalBounds[ci_CenteredInterval] :=
  Module[{bounds, center, halfWidth, minHalfWidth},
    bounds       = N[MinMax[Interval[ci]]];
    center       = (bounds[[1]] + bounds[[2]])/2;
    halfWidth    = (bounds[[2]] - bounds[[1]])/2;
    minHalfWidth = 0.001;
    If[halfWidth < minHalfWidth,
      {center - minHalfWidth, center + minHalfWidth},
      bounds
    ]
  ];
intervalBounds[n_?NumericQ] := {n - 0.001, n + 0.001};

Clear[unionOfBands];

Options[unionOfBands] = {"ClearDegenerateBands" -> True};

unionOfBands[bandsList_List, opts : OptionsPattern[]] :=
  Module[{filteredBands, intervals, boundaries},
    filteredBands = If[OptionValue["ClearDegenerateBands"],
      filterDegenerateBands[bandsList],
      bandsList
    ];
    intervals  = Map[intervalBounds, filteredBands];
    boundaries = Sort[DeleteDuplicates[Flatten[intervals]]];
    Table[
      With[{x1 = boundaries[[i]], x2 = boundaries[[i + 1]]},
        Module[{midpoint, count},
          midpoint = (x1 + x2) / 2;
          count    = Count[intervals, {low_, high_} /; low <= midpoint <= high];
          {x1, x2, count}
        ]
      ],
      {i, 1, Length[boundaries] - 1}
    ]
  ];

Clear[nextCandleOHLCGridLines];
nextCandleOHLCGridLines[candlesticksArg_, targetSegmentArg_Association] :=
  Module[{toIdx, nextIdx, nextRow, candleData},
    toIdx   = targetSegmentArg["ToIndex"];
    nextIdx = toIdx + 1;
    candleData = Normal[candlesticksArg];
    If[nextIdx > Length[candleData], Return[{}]];
    nextRow = candleData[[nextIdx]];
    {
      {nextRow["Open"],  Directive[Green,   Thick]},
      {nextRow["High"],  Directive[Cyan,    Thick]},
      {nextRow["Low"],   Directive[Magenta, Thick]},
      {nextRow["Close"], Directive[Yellow,  Thick]}
    }
  ];
nextCandleOHLCGridLines[___] := {};

Clear[plotUnionOfBands];

Options[plotUnionOfBands] = {
  "ClearDegenerateBands" -> True,
  "ShowNextCandle"       -> True
};

plotUnionOfBands[bandsList_List, opts : OptionsPattern[]] :=
  plotUnionOfBands[bandsList, None, None, opts];

plotUnionOfBands[bandsList_List,
                  candlesticksArg_,
                  targetSegmentArg_,
                  opts : OptionsPattern[]] :=
  Module[{segments, contiguousGroups, datasets, ohlcLines},
    segments = unionOfBands[bandsList,
                             "ClearDegenerateBands" -> OptionValue["ClearDegenerateBands"]];
    contiguousGroups = Select[
      SplitBy[segments, #[[3]] > 0 &],
      AllTrue[#, #[[3]] > 0 &] &
    ];
    datasets = Map[
      Function[group,
        Flatten[
          Map[{{#[[1]], #[[3]]}, {#[[2]], #[[3]]}} &, group],
          1
        ]
      ],
      contiguousGroups
    ];
    If[datasets === {},
      Return[Graphics[{}, Axes -> True, AxesStyle -> White,
                          Background -> Black, ImageSize -> 600]]
    ];
    ohlcLines = If[OptionValue["ShowNextCandle"] &&
                    candlesticksArg =!= None &&
                    targetSegmentArg =!= None,
      nextCandleOHLCGridLines[candlesticksArg, targetSegmentArg],
      {}
    ];
    ListLinePlot[datasets,
      PlotStyle        -> Directive[Cyan, Thick],
      Filling          -> Axis,
      FillingStyle     -> Directive[Cyan, Opacity[0.3]],
      PlotHighlighting -> "Dropline",
      Mesh             -> All,
      MeshStyle        -> Directive[Red, PointSize[Small]],
      GridLines        -> {Join[ohlcLines, {}], Automatic},
      GridLinesStyle   -> Directive[Gray, Opacity[0.3]],
      Background       -> Black,
      AxesStyle        -> White,
      LabelStyle       -> Directive[White, FontSize -> 12],
      AxesLabel        -> {"Price", "Number of Overlapping Bands"},
      ImageSize        -> 600,
      PlotRange        -> All
    ]
  ];
        
Clear[filterDegenerateBands];
filterDegenerateBands[bandsList_List, minWidthArg_ : 0.01] :=
  Select[bandsList,
    Function[band,
      Module[{bounds},
        bounds = intervalBounds[band];
        (bounds[[2]] - bounds[[1]]) >= minWidthArg
      ]
    ]
  ];        
        
Clear[bandsToPDF];

Options[bandsToPDF] = {"ClearDegenerateBands" -> True};

bandsToPDF[bandsList_List, desiredLevelOfConfidenceArg_ : 0.95, opts : OptionsPattern[]] :=
  Module[{filteredBands, intervals, centers, sigmas, n, sigmaFactor},
    sigmaFactor   = sigmaFactorForConfidence[desiredLevelOfConfidenceArg];
    filteredBands = If[OptionValue["ClearDegenerateBands"],
      filterDegenerateBands[bandsList],
      bandsList
    ];
    intervals = Map[intervalBounds, filteredBands];
    centers   = Map[(#[[1]] + #[[2]])/2 &, intervals];
    sigmas    = Map[sigmaFactor * (#[[2]] - #[[1]])/2 &, intervals];
    sigmas    = Map[Max[#, $MachineEpsilon] &, sigmas];
    n         = Length[filteredBands];
    Function[x,
      Quiet[
        Total[
          MapThread[
            Function[{c, s},
              (1/(s * Sqrt[2 Pi])) * Exp[-((x - c)^2)/(2 s^2)]
            ],
            {centers, sigmas}
          ]
        ] / n,
        General::munfl
      ]
    ]
  ];
  
Clear[plotBandsPDF];
  
Options[plotBandsPDF] = {
  "ClearDegenerateBands" -> True,
  "ShowNextCandle"       -> True
};

plotBandsPDF[bandsList_List, sigmaFactorArg_ : 0.5, opts : OptionsPattern[]] :=
  plotBandsPDF[bandsList, sigmaFactorArg, None, None, opts];

plotBandsPDF[bandsList_List,
              desiredLevelOfConfidenceArg_ : 0.95,
              candlesticksArg_,
              targetSegmentArg_,
              opts : OptionsPattern[]] :=
  Module[{filteredBands, pdf, intervals, allBounds, xMin, xMax, padding, ohlcLines},
    filteredBands = If[OptionValue["ClearDegenerateBands"],
      filterDegenerateBands[bandsList],
      bandsList
    ];
    pdf       = bandsToPDF[filteredBands, desiredLevelOfConfidenceArg];
    intervals = Map[intervalBounds, filteredBands];
    allBounds = Flatten[intervals];
    xMin      = Min[allBounds];
    xMax      = Max[allBounds];
    padding   = 0.1 * (xMax - xMin);
    ohlcLines = If[OptionValue["ShowNextCandle"] &&
                    candlesticksArg =!= None &&
                    targetSegmentArg =!= None,
      nextCandleOHLCGridLines[candlesticksArg, targetSegmentArg],
      {}
    ];
    Quiet[
      Plot[pdf[x], {x, xMin - padding, xMax + padding},
        PlotStyle        -> Directive[Cyan, Thick],
        Filling          -> Axis,
        FillingStyle     -> Directive[Cyan, Opacity[0.3]],
        PlotHighlighting -> "Dropline",
        GridLines        -> {ohlcLines, Automatic},
        GridLinesStyle   -> Directive[Gray, Opacity[0.3]],
        Background       -> Black,
        AxesStyle        -> White,
        LabelStyle       -> Directive[White, FontSize -> 12],
        AxesLabel        -> {"Price", "Probability Density"},
        ImageSize        -> 800,
        PlotRange        -> All
      ],
      General::munfl
    ]
  ];  


(* Get the number of sigmas for a particular level of confidence *) 
(*
sigmaFactorForConfidence[0.50]    (* 0.6745 \[LongDash] 50% CI is \[PlusMinus]0.67\[Sigma] *)
sigmaFactorForConfidence[0.68]    (* 0.9944 \[LongDash] close to \[PlusMinus]1\[Sigma] *)
sigmaFactorForConfidence[0.90]    (* 1.6449 \[LongDash] 90% CI is \[PlusMinus]1.64\[Sigma] *)
sigmaFactorForConfidence[0.95]    (* 1.9600 \[LongDash] 95% CI is \[PlusMinus]1.96\[Sigma] *)
sigmaFactorForConfidence[0.99]    (* 2.5758 \[LongDash] 99% CI is \[PlusMinus]2.58\[Sigma] *)
*) 
Clear[sigmaFactorForConfidence];
sigmaFactorForConfidence[confidenceArg_ : 0.95] :=
  1 / InverseCDF[NormalDistribution[0, 1], (1 + confidenceArg)/2];  
  
Options[summarizeBandsPDF] = {
  "ClearDegenerateBands" -> True,
  "CredibleMass"         -> 0.9,
  "SamplePoints"         -> 2000
};

Clear[summarizeBandsPDF];
summarizeBandsPDF[bandsList_List, desiredLevelOfConfidenceArg_ : 0.95, opts : OptionsPattern[]] :=
  Module[{filteredBands, pdf, intervals, allBounds, xMin, xMax, padding,
          nSamples, xs, dx, ys, totalMass, cumulative, normalizedCDF,
          peakIndices, localMaxima, globalMax, expectedValue,
          credibleMass, lowerTarget, upperTarget, leftIdx, rightIdx,
          leftPrice, rightPrice, credibleInterval},

    filteredBands = If[OptionValue["ClearDegenerateBands"],
      filterDegenerateBands[bandsList],
      bandsList
    ];

    If[filteredBands === {},
      Return[<|
        "GlobalMaximum"    -> Missing["NoBands"],
        "LocalMaxima"      -> {},
        "ExpectedValue"    -> Missing["NoBands"],
        "CredibleInterval" -> Missing["NoBands"]
      |>]
    ];

    pdf       = bandsToPDF[filteredBands, desiredLevelOfConfidenceArg];   (* <-- updated *)
    intervals = Map[intervalBounds, filteredBands];
    allBounds = Flatten[intervals];
    xMin      = Min[allBounds];
    xMax      = Max[allBounds];
    padding   = 0.1 * (xMax - xMin);

    nSamples = OptionValue["SamplePoints"];
    xs       = Subdivide[xMin - padding, xMax + padding, nSamples - 1];
    dx       = xs[[2]] - xs[[1]];
    ys       = Quiet[pdf /@ xs, General::munfl];
    ys       = Map[If[NumericQ[#] && # >= 0, #, 0.0] &, ys];

    peakIndices = Select[
      Range[2, Length[ys] - 1],
      ys[[# - 1]] < ys[[#]] >= ys[[# + 1]] &
    ];
    localMaxima = Map[{ys[[#]], xs[[#]]} &, peakIndices];
    localMaxima = SortBy[localMaxima, -#[[1]] &];

    globalMax = If[localMaxima === {},
      With[{maxIdx = First[Ordering[ys, -1]]},
        {ys[[maxIdx]], xs[[maxIdx]]}
      ],
      First[localMaxima]
    ];

    totalMass     = Total[ys] * dx;
    expectedValue = If[totalMass > 0,
      Total[xs * ys] * dx / totalMass,
      Mean[xs]
    ];

    cumulative    = Accumulate[ys] * dx;
    normalizedCDF = If[totalMass > 0, cumulative / totalMass, cumulative];
    credibleMass  = OptionValue["CredibleMass"];
    lowerTarget   = (1 - credibleMass) / 2;
    upperTarget   = 1 - lowerTarget;
    leftIdx       = LengthWhile[normalizedCDF, # < lowerTarget &] + 1;
    rightIdx      = LengthWhile[normalizedCDF, # < upperTarget &] + 1;
    leftPrice     = xs[[Min[leftIdx,  Length[xs]]]];
    rightPrice    = xs[[Min[rightIdx, Length[xs]]]];
    credibleInterval = {leftPrice, rightPrice};

    <|
      "GlobalMaximum"    -> globalMax,
      "LocalMaxima"      -> localMaxima,
      "ExpectedValue"    -> expectedValue,
      "CredibleInterval" -> credibleInterval
    |>
  ];

Clear[plotBandsPDFAnnotated];
    
Options[plotBandsPDFAnnotated] = Options[summarizeBandsPDF];
plotBandsPDFAnnotated[segmentArg_Association
  				   ,candlesticksArg_List
		             ,bandsList_List
		             ,desiredLevelOfConfidenceArg_ : 0.95
		             ,imageSizeArg_Integer : 800
		             ,opts : OptionsPattern[]
		            ] :=
  Module[{summary, filteredBands, pdf, intervals, allBounds, xMin, xMax, padding,
          globalMax, localMaxima, expectedValue, credibleInterval, gridSpecs,
          
          fromPrice, toPrice, direction, segColor,
          nextCandlestick, nextOpen, nextOpenLine,
          horizontalNextCandlestick, toIndex
         },

    (* ========================================================== *)
    (* Check the segment that was sent *)     
    If[segmentArg == <||>, Return[]];
    
    fromPrice = segmentArg["FromPrice"];
    toPrice   = segmentArg["ToPrice"];
    toIndex   = segmentArg["ToIndex"];
    direction = segmentArg["Direction"];

    segColor = If[direction == "U",
      RGBColor[0.0, 0.85, 0.0],
      RGBColor[0.85, 0.0, 0.0]
    ];

    (* Next candlestick after segment *)
    If[toIndex >= Length[candlesticksArg],
      nextCandlestick           = candlesticksArg[[toIndex]];
      nextOpen                  = nextCandlestick["Close"];
      nextOpenLine              = {};
      horizontalNextCandlestick = {};
      ,
      nextCandlestick           = candlesticksArg[[toIndex + 1]];
      nextOpen                  = nextCandlestick["Open"];
      nextOpenLine              = Line[{{nextOpen, 0}, {nextOpen, 1000}}];
      
      horizontalNextCandlestick = horizontalCandlestick[nextCandlestick, 0.3, 0.3];
    ];
    (* ========================================================== *)

    filteredBands = If[OptionValue["ClearDegenerateBands"],
      filterDegenerateBands[bandsList],
      bandsList
    ];
    summary = summarizeBandsPDF[bandsList, desiredLevelOfConfidenceArg, opts];

    pdf              = bandsToPDF[filteredBands, desiredLevelOfConfidenceArg];
    intervals        = Map[intervalBounds, filteredBands];
    allBounds        = Flatten[intervals];
    xMin             = Min[allBounds];
    xMax             = Max[allBounds];
    padding          = 0.1 * (xMax - xMin);
    globalMax        = summary["GlobalMaximum"];
    localMaxima      = summary["LocalMaxima"];
    expectedValue    = summary["ExpectedValue"];
    credibleInterval = summary["CredibleInterval"];

    gridSpecs = Join[
      {{globalMax[[2]], Directive[Yellow, Thick]}},
      Map[{#[[2]], Directive[Orange, Dashed, Opacity[0.6]]} &, Rest[localMaxima]],
      {{expectedValue, Directive[Magenta, DotDashed]}},
      {{credibleInterval[[1]], Directive[White, Dashed, Opacity[0.5]]},
       {credibleInterval[[2]], Directive[White, Dashed, Opacity[0.5]]}}
    ];

    Quiet[
      Plot[pdf[x], {x, xMin - padding, xMax + padding},
        PlotStyle        -> Directive[Cyan, Thick],
        Filling          -> Axis,
        FillingStyle     -> Directive[Cyan, Opacity[0.3]],
        GridLines        -> {gridSpecs, Automatic},
        GridLinesStyle   -> Directive[Gray, Opacity[0.3]],
        Background       -> Black,
        AxesStyle        -> White,
        LabelStyle       -> Directive[White, FontSize -> 12],
        AxesLabel        -> {"Price", "Probability Density"},
        PlotLabel        -> Style[StringForm[
								          "Peak: `1`  Expected: `2`  CI: [`3`, `4`]",
								          NumberForm[globalMax[[2]],     {4, 2}],
								          NumberForm[expectedValue,      {4, 2}],
								          NumberForm[credibleInterval[[1]], {4, 2}],
								          NumberForm[credibleInterval[[2]], {4, 2}]
								        ],
							 White
							],
        ImageSize        -> 800,
        PlotRange        -> All,
        PlotHighlighting -> "Dropline",
	      Epilog           -> {
	        horizontalNextCandlestick,
	        {segColor, Dashed, Thickness[0.005],
	         Line[{{fromPrice, 0}, {fromPrice, 1000}}]},
	        {segColor, Dashed, Thickness[0.005],
	         Line[{{toPrice, 0}, {toPrice, 1000}}]},
	        {segColor, Thickness[0.005],
	         Arrow[{{fromPrice, 0.08}, {toPrice, 0.08}}]},
	        {Yellow, Thickness[0.002], nextOpenLine}
	      }

      ],
      General::munfl
    ]
  ]; 
    
Clear[plotCandlesticksWithPDF];

Options[plotCandlesticksWithPDF] = Join[
  Options[summarizeBandsPDF],
  {
    "SigmaFactor"        -> 0.5,
    "PDFPanelWidth"      -> 0.2,
    "ShowSummaryMarkers" -> True
  }
];

plotCandlesticksWithPDF[candlesticksArg_Tabular,
                         fromIdxArg_Integer,
                         toIdxArg_Integer,
                         bandsList_List,
                         opts : OptionsPattern[]] :=
  Module[{slice, candleData, candleGraphic, pdf, filteredBands, intervals,
          allBounds, priceMin, priceMax, padding, sigmaFactor, summary,
          globalMax, expectedValue, credibleInterval, pdfPanelWidth,
          xCandleMin, xCandleMax, xPanelMin, xPanelMax, priceRange,
          pdfMaxValue, pdfCurve, pdfPanelData, summaryMarkers},

    sigmaFactor   = OptionValue["SigmaFactor"];
    pdfPanelWidth = OptionValue["PDFPanelWidth"];

    (* --- Candlestick rendering --- *)
    slice = Normal[candlesticksArg[[fromIdxArg ;; toIdxArg]]];
    candleData = MapIndexed[
      Function[{row, idx},
        With[{xPos = fromIdxArg + idx[[1]] - 1},
          {
            (* High-low wick *)
            {Black,
             Line[{{xPos, row["Low"]}, {xPos, row["High"]}}]},
            (* Body *)
            {
              If[row["Close"] >= row["Open"], Green, Red],
              Rectangle[
                {xPos - 0.35, Min[row["Open"], row["Close"]]},
                {xPos + 0.35, Max[row["Open"], row["Close"]]}
              ]
            }
          }
        ]
      ],
      slice
    ];

    (* --- PDF rendering --- *)
    filteredBands = If[OptionValue["ClearDegenerateBands"],
      filterDegenerateBands[bandsList],
      bandsList
    ];
    pdf = bandsToPDF[filteredBands, sigmaFactor];

    intervals = Map[intervalBounds, filteredBands];
    allBounds = Flatten[intervals];
    priceMin  = Min[Min[allBounds], Min[Map[#["Low"]  &, slice]]];
    priceMax  = Max[Max[allBounds], Max[Map[#["High"] &, slice]]];
    padding   = 0.05 * (priceMax - priceMin);
    priceMin  = priceMin - padding;
    priceMax  = priceMax + padding;

    (* Sample PDF on a fine grid in price space *)
    priceRange    = Subdivide[priceMin, priceMax, 500];
    pdfPanelData  = Quiet[
      Map[{pdf[#], #} &, priceRange],
      General::munfl
    ];
    pdfMaxValue   = Max[Map[First, pdfPanelData]];

    (* Map PDF panel into the right portion of the x-axis *)
    xCandleMin = fromIdxArg - 0.5;
    xCandleMax = toIdxArg + 0.5;
    xPanelMin  = xCandleMax + 1;
    xPanelMax  = xPanelMin + (xCandleMax - xCandleMin) * pdfPanelWidth;

    pdfCurve = Map[
      {
        Rescale[#[[1]], {0, pdfMaxValue}, {xPanelMin, xPanelMax}],
        #[[2]]
      } &,
      pdfPanelData
    ];

    (* Summary markers (horizontal lines at peaks and CI bounds) *)
    summaryMarkers = If[OptionValue["ShowSummaryMarkers"],
      summary = summarizeBandsPDF[bandsList, "SigmaFactor" -> sigmaFactor,
                                   "ClearDegenerateBands" -> OptionValue["ClearDegenerateBands"]];
      globalMax        = summary["GlobalMaximum"];
      expectedValue    = summary["ExpectedValue"];
      credibleInterval = summary["CredibleInterval"];
      {
        (* Global peak: yellow horizontal line across both panels *)
        {Yellow, Thick,
          Line[{{xCandleMin, globalMax[[2]]}, {xPanelMax, globalMax[[2]]}}]},
        (* Expected value: magenta dot-dashed line *)
        {Magenta, Dashed,
          Line[{{xCandleMin, expectedValue}, {xPanelMax, expectedValue}}]},
        (* Credible interval: white dashed bounds *)
        {White, Dashed, Opacity[0.5],
          Line[{{xCandleMin, credibleInterval[[1]]}, {xPanelMax, credibleInterval[[1]]}}],
          Line[{{xCandleMin, credibleInterval[[2]]}, {xPanelMax, credibleInterval[[2]]}}]
        }
      },
      {}
    ];

    Graphics[{
      candleData,
      (* Separator between candlesticks and PDF panel *)
      {Gray, Dashed, Line[{{xPanelMin - 0.5, priceMin}, {xPanelMin - 0.5, priceMax}}]},
      (* PDF curve *)
      {Cyan, Thick, Line[pdfCurve]},
      (* Fill area between curve and panel left edge *)
      {Cyan, Opacity[0.3],
        Polygon[Join[
          {{xPanelMin, priceMin}},
          pdfCurve,
          {{xPanelMin, priceMax}}
        ]]
      },
      summaryMarkers
    },
      Background -> Black,
      Frame      -> True,
      FrameStyle -> White,
      FrameLabel -> {{"Price", None}, {"Candlestick Index", None}},
      LabelStyle -> Directive[White, FontSize -> 12],
      PlotRange  -> {{xCandleMin, xPanelMax}, {priceMin, priceMax}},
      AspectRatio -> 1/(1.2 + pdfPanelWidth),
      ImageSize  -> 900
    ]
  ];  
  
Clear[plotBandsPDFAnnotatedEx];
  
Options[plotBandsPDFAnnotatedEx] = Options[summarizeBandsPDF];

plotBandsPDFAnnotatedEx[segmentArg_Association
					   ,candlesticksArg_List
			           ,bandsList_List
			           ,desiredLevelOfConfidenceInBandArg_ : 95.0
			           ,imageSizeArg_Integer : 800
			           ,opts : OptionsPattern[]
	                   ] :=
  Module[{summary, filteredBands, pdf, intervals, allBounds, xMin, xMax, padding,
          globalMax, localMaxima, expectedValue, credibleInterval,
          
          fromPrice, toPrice, direction, segColor,
          nextCandlestick, nextOpen, nextOpenLine,
          horizontalNextCandlestick, fromIndex, toIndex,
          
          priceSamples, pdfValues, yMin, yMax, verticalLines, delta,
          sigmasForDesiredLevelOfConfidenceInBand, peakPoints, peakLine, peakDots
         },

    (* ========================================================== *)
    (* Check the segment that was sent *)     
    If[segmentArg == <||>, Return[]];
    
    sigmasForDesiredLevelOfConfidenceInBand = sigmasForConfidence[desiredLevelOfConfidenceInBandArg];
    
    fromPrice   = segmentArg["FromPrice"];
    toPrice     = segmentArg["ToPrice"];
    fromIndex   = segmentArg["FromIndex"];
    toIndex     = segmentArg["ToIndex"];
    direction   = segmentArg["Direction"];

    segColor = If[direction == "U",
      RGBColor[0.0, 0.85, 0.0],
      RGBColor[0.85, 0.0, 0.0]
    ];

    filteredBands = If[OptionValue["ClearDegenerateBands"],
      filterDegenerateBands[bandsList],
      bandsList
    ];
    
    summary = summarizeBandsPDF[bandsList, desiredLevelOfConfidenceInBandArg, opts];
	peakPoints = SortBy[Map[{#[[2]], #[[1]]} &, summary["LocalMaxima"]], First];
	peakPoints = Select[peakPoints, #[[2]] >= 0.05 * summary["GlobalMaximum"][[1]] &];
	peakLine = Which[
	  Length[peakPoints] < 2,
	    {},
	  Length[peakPoints] == 2,
	    (* Straight line between two points *)
	    {Orange, Thick, Line[peakPoints]},
	  True,
	    (* Interpolate through the peaks and draw a smooth curve *)
	    Module[{interpolationOrder, peakInterp, xPeakMin, xPeakMax, samplePts, interpPoints},
	      interpolationOrder = Min[1, Length[peakPoints] - 1];
	      peakInterp   = Interpolation[peakPoints, InterpolationOrder -> interpolationOrder];
	      xPeakMin     = First[peakPoints][[1]];
	      xPeakMax     = Last[peakPoints][[1]];
	      samplePts    = Subdivide[xPeakMin, xPeakMax, 200];
	      interpPoints = Map[{#, peakInterp[#]} &, samplePts];
	      {Orange, Thick, Line[interpPoints]}
	    ]
	];
	peakDots = {Yellow, PointSize[0.012], Point /@ peakPoints};	
	
    pdf              = bandsToPDF[filteredBands, desiredLevelOfConfidenceInBandArg];
    intervals        = Map[intervalBounds, filteredBands];
    allBounds        = Flatten[intervals];
    xMin             = Min[allBounds];
    xMax             = Max[allBounds];
    padding          = 0.1 * (xMax - xMin);
    globalMax        = summary["GlobalMaximum"];
    localMaxima      = summary["LocalMaxima"];
    expectedValue    = summary["ExpectedValue"];
    credibleInterval = summary["CredibleInterval"];

	priceSamples = Subdivide[xMin - padding, xMax + padding, 200];
	pdfValues    = Map[pdf, priceSamples];
	yMax         = Max[pdfValues] * 1.05;  (* same as I used earlier *)
	yMax = summary["GlobalMaximum"][[1]];
	yMin         = -0.01;    

    (* Build vertical lines as Epilog primitives *)
		verticalLines = Join[
		  {{Green, Thick, Line[{{globalMax[[2]], yMin}, {globalMax[[2]], yMax}}]}},
		  Map[{Green, Dashed, Opacity[0.6], Line[{{#[[2]], yMin}, {#[[2]], #[[1]]}}]} &, Rest[localMaxima]],
		  {{Magenta, DotDashed, Line[{{expectedValue, -0.01}, {expectedValue, yMax}}]}},
		  {{White, Dashed, Opacity[0.5], Line[{{credibleInterval[[1]], yMin}, {credibleInterval[[1]], yMax}}]}},
		  {{White, Dashed, Opacity[0.5], Line[{{credibleInterval[[2]], yMin}, {credibleInterval[[2]], yMax}}]}}
		];

    (* ========================================================== *)
    
    (* Next candlestick after segment *)
    If[toIndex >= Length[candlesticksArg],
      nextCandlestick           = candlesticksArg[[toIndex]];
      nextOpen                  = nextCandlestick["Close"];
      nextOpenLine              = {};
      horizontalNextCandlestick = {};
      ,
      nextCandlestick           = candlesticksArg[[toIndex + 1]];
      nextOpen                  = nextCandlestick["Open"];
      nextOpenLine              = Line[{{nextOpen, 0}, {nextOpen, yMax}}];
      
      delta = yMax / (toIndex - fromIndex + 1);
      (* Print["yMax ",yMax," NumCan: ",toIndex - fromIndex, " Delta = ", delta]; *) 
      horizontalNextCandlestick = horizontalCandlestick[candlesticksArg[[#]],(toIndex - # + 1) * delta, 0.001,0.003]& /@ Range[fromIndex,toIndex+1];
    ];
    
    (* ========================================================== *)

    Quiet[
      Plot[pdf[x], {x, xMin - padding, xMax + padding},
        PlotStyle        -> Directive[Lighter[segColor,0.9], Thick],
        Filling          -> None (*Axis*),
        FillingStyle     -> Directive[Cyan, Opacity[0.3]],
        GridLines        -> Automatic,
        GridLinesStyle   -> Directive[Gray, Opacity[0.3]],
        Background       -> Black,
        AxesStyle        -> White,
        LabelStyle       -> Directive[White, FontSize -> 12],
        AxesLabel        -> {"Price", "Probability Density"},
        PlotLabel        -> Style[StringForm[
						          "Peak: `1`  Expected: `2`  CI: [`3`, `4`]",
						          NumberForm[globalMax[[2]],     {4, 2}],
						          NumberForm[expectedValue,      {4, 2}],
						          NumberForm[credibleInterval[[1]], {4, 2}],
						          NumberForm[credibleInterval[[2]], {4, 2}]
						        ],
							 White
							],
        ImageSize        -> 800,
        PlotRange        -> All,
        PlotHighlighting -> "Dropline",
        Epilog           -> {
	      verticalLines
	      ,horizontalNextCandlestick
	      ,{White, Thickness[0.002], Line[{{xMin-padding, yMax}, {fromPrice, yMax}}]}
	      ,{segColor, Dashed, Thickness[0.004], Line[{{fromPrice, 0}, {fromPrice, yMax}}]}
	      ,{segColor, Dashed, Thickness[0.004], Line[{{toPrice, 0}, {toPrice, yMax}}]}
	      ,{segColor, Dotted, Thickness[0.004],Arrowheads[0.02],Arrow[{{fromPrice, yMax}, {toPrice, delta}}]}
	      ,{Yellow, Thickness[0.002], nextOpenLine}
	      ,peakLine,peakDots
	    }

      ],
      General::munfl
    ]
  ];    
    
Clear[horizontalCandlestick];
horizontalCandlestick[candleArg_Association, yPosArg_Real,
                       halfWidthArg_Real : 0.001,
                       wickThicknessArg_Real : 0.003] :=
  Module[{open, high, low, close, color, bodyLeft, bodyRight},
    open      = candleArg["Open"];
    high      = candleArg["High"];
    low       = candleArg["Low"];
    close     = candleArg["Close"];
    color     = If[close >= open, RGBColor[0.0, 0.85, 0.0], RGBColor[0.85, 0.0, 0.0]];
    bodyLeft  = Min[open, close];
    bodyRight = Max[open, close];
    {
      (* Left wick: Low to body left edge *)
      {color, Thickness[wickThicknessArg],
       Line[{{low, yPosArg}, {bodyLeft, yPosArg}}]},
      (* Body: hollow rectangle with colored outline *)
      {color, EdgeForm[{color, Thickness[wickThicknessArg]}], FaceForm[None],
       Rectangle[{bodyLeft, yPosArg - halfWidthArg},
                 {bodyRight, yPosArg + halfWidthArg}]},
      (* Right wick: body right edge to High *)
      {color, Thickness[wickThicknessArg],
       Line[{{bodyRight, yPosArg}, {high, yPosArg}}]}
    }
  ];
    
(* =============================================================================================================================== *) 
Clear[gaussianKernel, cauchyKernel, bumpKernel, kernelForName];

gaussianKernel[distanceArg_, widthArg_] :=
  Quiet[Exp[-(distanceArg / widthArg)^2], General::munfl];
  
cauchyKernel[distanceArg_, widthArg_] :=
  Quiet[1 / (1 + (distanceArg / widthArg)^2), General::munfl];
  
bumpKernel[distanceArg_, widthArg_] :=
  If[distanceArg >= widthArg, 0, (1 - (distanceArg / widthArg)^2)^2];

kernelForName["Gaussian"] := gaussianKernel;
kernelForName["Cauchy"]   := cauchyKernel;
kernelForName["Bump"]     := bumpKernel;
kernelForName[_]          := gaussianKernel;

Clear[candlestickFibonacciGoodness];

Options[candlestickFibonacciGoodness] = {
  "Kernel"      -> "Gaussian",
  "KernelWidth" -> 0.005,
  "PriceTypes"  -> {"Open", "High", "Low", "Close"}
};

candlestickFibonacciGoodness[candlesticksArg_,
                              fibonacciPricesArg_List,
                              rangeArg_?NumericQ,
                              opts : OptionsPattern[]] :=
  Module[{candles, priceTypes, kernel, width, allPrices,
          priceScore, scores},

    (* Accept either a List of Associations or a Tabular *)
    candles = If[Head[candlesticksArg] === Tabular,
      Normal[candlesticksArg],
      candlesticksArg
    ];

    priceTypes = OptionValue["PriceTypes"];
    kernel     = kernelForName[OptionValue["Kernel"]];
    width      = OptionValue["KernelWidth"];

    (* Extract every relevant price value from every candlestick *)
    allPrices = Flatten[Map[
      Function[row, Map[row[#] &, priceTypes]],
      candles
    ]];

    (* For a single price, take the best-matching Fibonacci score *)
    priceScore[p_] := Max[Map[
      kernel[Abs[p - #] / rangeArg, width] &,
      fibonacciPricesArg
    ]];

    scores = Map[priceScore, allPrices];

    Total[scores] / Length[allPrices]
  ];  
  
Clear[candlestickFibonacciGoodnessDetail];

Options[candlestickFibonacciGoodnessDetail] = Options[candlestickFibonacciGoodness];

candlestickFibonacciGoodnessDetail[candlesticksArg_,
                                    fibonacciPricesArg_List,
                                    rangeArg_?NumericQ,
                                    opts : OptionsPattern[]] :=
  Module[{candles, priceTypes, kernel, width, allPrices,
          scoreAndMatch, results, scores, matches},
    candles = If[Head[candlesticksArg] === Tabular,
      Normal[candlesticksArg],
      candlesticksArg
    ];
    priceTypes = OptionValue["PriceTypes"];
    kernel     = kernelForName[OptionValue["Kernel"]];
    width      = OptionValue["KernelWidth"];
    allPrices  = Flatten[Map[
      Function[row, Map[row[#] &, priceTypes]],
      candles
    ]];
    scoreAndMatch[p_] :=
      Module[{kernelVals, bestIdx},
        kernelVals = Map[kernel[Abs[p - #] / rangeArg, width] &, fibonacciPricesArg];
        bestIdx    = First[Ordering[kernelVals, -1]];
        {kernelVals[[bestIdx]], fibonacciPricesArg[[bestIdx]]}
      ];
    results = Map[scoreAndMatch, allPrices];
    scores  = Map[First, results];
    matches = Map[Last,  results];
    <|
      "GoodnessScore" -> Total[scores] / Length[scores],
      "PriceScores"   -> scores,
      "BestMatches"   -> matches
    |>
  ];
  
Clear[segmentGoodness];
 
Options[segmentGoodness] = Options[candlestickFibonacciGoodness];

segmentGoodness[segmentArg_Association,
                candlesticksArg_,
                fibonacciRatiosArg_List : fibonacciRatios,
                opts : OptionsPattern[]] :=
  Module[{candles, fromIdx, toIdx, fromPrice, toPrice, range,
          fibPrices, sliceCandles},

    candles = If[Head[candlesticksArg] === Tabular,
      Normal[candlesticksArg],
      candlesticksArg
    ];

    fromIdx   = segmentArg["FromIndex"];
    toIdx     = segmentArg["ToIndex"];
    fromPrice = segmentArg["FromPrice"];
    toPrice   = segmentArg["ToPrice"];
    range     = toPrice - fromPrice;

    If[range == 0, Return[0.0]];
    If[fromIdx > toIdx, Return[0.0]];
    If[toIdx > Length[candles], Return[0.0]];

    fibPrices    = Map[fromPrice + # * range &, fibonacciRatiosArg];
    sliceCandles = candles[[fromIdx ;; toIdx]];

    candlestickFibonacciGoodness[
      sliceCandles, fibPrices, Abs[range],
      Sequence @@ FilterRules[{opts}, Options[candlestickFibonacciGoodness]]
    ]
  ];

Clear[bestFibonacciSegmentByGoodness];

Options[bestFibonacciSegmentByGoodness] = Options[candlestickFibonacciGoodness];

bestFibonacciSegmentByGoodness[desiredSegmentsArg_Association,
                                candlesticksArg_,
                                fromIndexArg_Integer,
                                toIndexArg_Integer,
                                fibonacciRatiosArg_List : fibonacciRatios,
                                opts : OptionsPattern[]] :=
  Module[{candles, evaluate, raw, filtered},

    candles = If[Head[candlesticksArg] === Tabular,
      Normal[candlesticksArg],
      candlesticksArg
    ];

    evaluate[toIdx_] := Module[
      {segment, fromPrice, toPrice, range, fibPrices, goodness},

      segment = getSegments[desiredSegmentsArg, fromIndexArg, toIdx];

      If[segment === <||>,
        Return[Missing["NoSegment"], Module]
      ];

      fromPrice = segment["FromPrice"];
      toPrice   = segment["ToPrice"];
      range     = toPrice - fromPrice;

      If[range == 0,
        Return[Missing["ZeroRange"], Module]
      ];

      fibPrices = Map[fromPrice + # * range &, fibonacciRatiosArg];

      goodness = segmentGoodness[segment, candles, fibonacciRatiosArg,
        Sequence @@ FilterRules[{opts}, Options[segmentGoodness]]
      ];

      <|
        "Goodness"        -> goodness,
        "Segment"         -> segment,
        "FibonacciPrices" -> fibPrices
      |>
    ];

    raw = Association[
      Map[{fromIndexArg, #} -> evaluate[#] &, Range[fromIndexArg + 1, toIndexArg]]
    ];

    filtered = Select[raw, AssociationQ];

    Association[
      ReverseSortBy[Normal[filtered], #[[2]]["Goodness"] &]
    ]
  ];

Clear[segmentsGoodnessStatistics];
  
Options[segmentsGoodnessStatistics] = Options[candlestickFibonacciGoodness];

segmentsGoodnessStatistics[segmentsArg_,
                            candlesticksArg_,
                            fibonacciRatiosArg_List : fibonacciRatios,
                            opts : OptionsPattern[]] :=
  Module[{segments, scores, details, bestIdx, worstIdx, deciles, histogram},

    segments = If[Head[segmentsArg] === Tabular,
      Normal[segmentsArg],
      segmentsArg
    ];

    If[segments === {},
      Return[<|
        "Count"             -> 0,
        "Mean"              -> Missing["NoSegments"],
        "Median"            -> Missing["NoSegments"],
        "StandardDeviation" -> Missing["NoSegments"],
        "Min"               -> Missing["NoSegments"],
        "Max"               -> Missing["NoSegments"],
        "Quartiles"         -> Missing["NoSegments"],
        "Best"              -> Missing["NoSegments"],
        "Worst"             -> Missing["NoSegments"],
        "Histogram"         -> {},
        "Details"           -> Tabular[{}]
      |>]
    ];

    scores = Map[
      segmentGoodness[#, candlesticksArg, fibonacciRatiosArg,
        Sequence @@ FilterRules[{opts}, Options[segmentGoodness]]] &,
      segments
    ];

    details = Tabular[
      SortBy[
        MapThread[
          Function[{seg, score},
            <|
              "FromIndex" -> seg["FromIndex"],
              "ToIndex"   -> seg["ToIndex"],
              "Direction" -> seg["Direction"],
              "FromPrice" -> seg["FromPrice"],
              "ToPrice"   -> seg["ToPrice"],
              "Range"     -> seg["Range"],
              "Goodness"  -> score
            |>
          ],
          {segments, scores}
        ],
        -#["Goodness"] &
      ]
    ];

    bestIdx  = First[Ordering[scores, -1]];
    worstIdx = First[Ordering[scores,  1]];

    deciles = Range[0.0, 1.0, 0.1];
    histogram = Map[
      Function[i,
        <|
          "Bucket" -> ToString[NumberForm[deciles[[i]], {3, 1}]] <> "-" <>
                       ToString[NumberForm[deciles[[i + 1]], {3, 1}]],
          "Count"  -> Count[scores, s_ /; deciles[[i]] <= s < deciles[[i + 1]]]
        |>
      ],
      Range[1, Length[deciles] - 1]
    ];
    histogram[[-1]] = <|
      "Bucket" -> histogram[[-1]]["Bucket"],
      "Count"  -> histogram[[-1]]["Count"] + Count[scores, 1.0]
    |>;

    <|
      "Count"             -> Length[segments],
      "Mean"              -> Mean[scores],
      "Median"            -> Median[scores],
      "StandardDeviation" -> StandardDeviation[scores],
      "Min"               -> Min[scores],
      "Max"               -> Max[scores],
      "Quartiles"         -> Quartiles[scores],
      "Best"              -> <|
        "Segment"  -> segments[[bestIdx]],
        "Goodness" -> scores[[bestIdx]]
      |>,
      "Worst"             -> <|
        "Segment"  -> segments[[worstIdx]],
        "Goodness" -> scores[[worstIdx]]
      |>,
      "Histogram"         -> Tabular[histogram],
      "Details"           -> details
    |>
  ];
  
Clear[segmentGoodnessCurve];

Options[segmentGoodnessCurve] = Join[
  Options[candlestickFibonacciGoodness],
  {"NumScanPoints"   -> 64
  ,"UpperMultiplier" -> If[ValueQ[Global`defaultMaxExtension], Global`defaultMaxExtension, 1.31]
  }
];

segmentGoodnessCurve[segmentArg_Association,
                      candlesticksArg_,
                      fibonacciRatiosArg_List : fibonacciRatios,
                      opts : OptionsPattern[]] :=
  Module[{candles, fromIdx, toIdx, fromPrice, toPrice, range,
          nScanPoints, upperMultiplier, scanEnd, scanPrices,
          segCandles, results},

    candles = If[Head[candlesticksArg] === Tabular,
      Normal[candlesticksArg],
      candlesticksArg
    ];

    fromIdx   = segmentArg["FromIndex"];
    toIdx     = segmentArg["ToIndex"];
    fromPrice = segmentArg["FromPrice"];
    toPrice   = segmentArg["ToPrice"];
    range     = toPrice - fromPrice;

    If[range == 0, Return[{}]];
    If[fromIdx > toIdx || toIdx > Length[candles], Return[{}]];

    nScanPoints     = OptionValue["NumScanPoints"];
    upperMultiplier = OptionValue["UpperMultiplier"];

    (* Scan range: from FromPrice through beyond ToPrice by upperMultiplier *)
    scanEnd    = fromPrice + range * upperMultiplier;
    scanPrices = Subdivide[fromPrice, scanEnd, nScanPoints - 1];
    segCandles = candles[[fromIdx ;; toIdx]];

    results = Map[
      Function[scanPrice,
        Module[{effectiveRange, fibPrices, g},
          (* Effective range: extended if scanPrice is beyond ToPrice in trend direction *)
          effectiveRange = If[Sign[range] * (scanPrice - toPrice) > 0,
            scanPrice - fromPrice,
            range
          ];
          If[effectiveRange == 0,
            {scanPrice, 0.0},
            fibPrices = Map[fromPrice + # * effectiveRange &, fibonacciRatiosArg];
            g = candlestickFibonacciGoodness[
              segCandles, fibPrices, Abs[effectiveRange],
              Sequence @@ FilterRules[{opts},
                Options[candlestickFibonacciGoodness]]
            ];
            {scanPrice, g}
          ]
        ]
      ],
      scanPrices
    ];
    results
  ];

Clear[plotSegmentGoodnessCurve];
    
Options[plotSegmentGoodnessCurve] = Options[segmentGoodnessCurve];

plotSegmentGoodnessCurve[segmentArg_Association,
                          candlesticksArg_,
                          fibonacciRatiosArg_List : fibonacciRatios,
                          opts : OptionsPattern[]] :=
  Module[{curve, fromPrice, toPrice, direction, segColor, maxGoodness},
    curve = segmentGoodnessCurve[segmentArg, candlesticksArg,
      fibonacciRatiosArg,
      Sequence @@ FilterRules[{opts}, Options[segmentGoodnessCurve]]
    ];
    If[curve === {}, Return[Graphics[{}, ImageSize -> 400]]];

    fromPrice   = segmentArg["FromPrice"];
    toPrice     = segmentArg["ToPrice"];
    direction   = segmentArg["Direction"];
    segColor    = If[direction === "U",
      RGBColor[0.0, 0.85, 0.0],
      RGBColor[0.85, 0.0, 0.0]
    ];
    maxGoodness = Max[Map[Last, curve]];

    ListLinePlot[curve,
      PlotStyle    -> Directive[Cyan, Thick],
      Filling      -> Axis,
      FillingStyle -> Directive[Cyan, Opacity[0.2]],
      GridLines    -> {
        {
          {fromPrice, Directive[segColor, Dashed, Thickness[0.003]]},
          {toPrice,   Directive[segColor, Dashed, Thickness[0.003]]}
        },
        Automatic
      },
      GridLinesStyle -> Directive[Gray, Opacity[0.3]],
      Background     -> Black,
      AxesStyle      -> White,
      FrameStyle     -> White,
      Frame          -> True,
      FrameLabel     -> {"Scan Price", "Goodness"},
      LabelStyle     -> Directive[White, FontSize -> 11],
      PlotLabel      -> Style[
        StringForm["Segment [`1`, `2`]  Direction `3`  Max Goodness `4`",
          segmentArg["FromIndex"], segmentArg["ToIndex"],
          direction,
          NumberForm[maxGoodness, {4, 3}]
        ],
        White
      ],
      ImageSize      -> 600,
      PlotRange      -> All,
      PlotRangePadding -> Scaled[0.02],
      Epilog -> {
        (* Labels for FromPrice and ToPrice *)
        Text[Style["From", segColor, Bold, 10],
             {fromPrice, maxGoodness * 1.02}, {0, -1}],
        Text[Style["To", segColor, Bold, 10],
             {toPrice, maxGoodness * 1.02}, {0, -1}]
      }
    ]
  ];
  
  
(* Echo["Reading scanSegmentGoodnessByEndpoint...."];  *) 
 
Clear[scanSegmentGoodnessByEndpoint];
 
Options[scanSegmentGoodnessByEndpoint] = Join[Options[candlestickFibonacciGoodness],
	  {
	    "UpperMultiplier" -> 1.618,
	    "NumScanPoints"   -> 64
	  }
	];

scanSegmentGoodnessByEndpoint[segmentArg_Association,
                               candlesticksArg_,
                               fibonacciRatiosArg_List : fibonacciRatios,
                               numExtraCandlesArg_Integer : 1,
                               opts : OptionsPattern[]] :=
  Module[{candles, fromIdx, toIdx, fromPrice, toPrice, direction, range,
          upperMultiplier, nScanPoints, scanEnd, scanPrices,
          extendedIdx, extendedCandles, futureCandles,
          futureHighs, futureLows, actualPrice, isReversal, actualGoodness,
          actualEffectiveRange, actualFibPrices,
          results, bestIdx},

    candles = If[Head[candlesticksArg] === Tabular,
      Normal[candlesticksArg],
      candlesticksArg
    ];

    fromIdx   = segmentArg["FromIndex"];
    toIdx     = segmentArg["ToIndex"];
    fromPrice = segmentArg["FromPrice"];
    toPrice   = segmentArg["ToPrice"];
    direction = segmentArg["Direction"];
    range     = toPrice - fromPrice;

    upperMultiplier = OptionValue["UpperMultiplier"];
    nScanPoints     = OptionValue["NumScanPoints"];

    (* Bounds check: need numExtraCandlesArg more candlesticks beyond ToIndex *)
    extendedIdx = toIdx + numExtraCandlesArg;
    If[extendedIdx > Length[candles] || numExtraCandlesArg < 1,
      Return[<|
        "ScanResults"      -> Tabular[{}],
        "BestScanPrice"    -> Missing["NotEnoughCandles"],
        "BestScanGoodness" -> Missing["NotEnoughCandles"],
        "ActualPrice"      -> Missing["NotEnoughCandles"],
        "ActualGoodness"   -> Missing["NotEnoughCandles"],
        "IsReversal"       -> Missing["NotEnoughCandles"],
        "OriginalToPrice"  -> toPrice,
        "OriginalRange"    -> range,
        "NumExtraCandles"  -> numExtraCandlesArg
      |>]
    ];

    (* Scan range extends from ToPrice to FromPrice + range * upperMultiplier *)
    scanEnd    = fromPrice + range * upperMultiplier;
    scanPrices = Subdivide[toPrice, scanEnd, nScanPoints - 1];

    extendedCandles = candles[[fromIdx ;; extendedIdx]];
    futureCandles   = candles[[toIdx + 1 ;; extendedIdx]];

    (* Scan hypothetical endpoints with effective-range logic *)
    results = Map[
      Function[scanPrice,
        Module[{effectiveRange, fibPrices, g},
          (* Effective range: extend beyond ToPrice, original within segment *)
          effectiveRange = If[Sign[range] * (scanPrice - toPrice) > 0,
            scanPrice - fromPrice,
            range
          ];
          If[effectiveRange == 0,
            Return[<|"ScanPrice" -> scanPrice, "Goodness" -> 0.0|>, Module]
          ];
          fibPrices = Map[fromPrice + # * effectiveRange &, fibonacciRatiosArg];
          g = candlestickFibonacciGoodness[
            extendedCandles, fibPrices, Abs[effectiveRange],
            Sequence @@ FilterRules[{opts},
              Options[candlestickFibonacciGoodness]]
          ];
          <|"ScanPrice" -> scanPrice, "Goodness" -> g|>
        ]
      ],
      scanPrices
    ];

    bestIdx = First[Ordering[Map[#["Goodness"] &, results], -1]];

    (* --- Actual endpoint from future candles --- *)
    (* For U: use maximum High across all future candles                       *)
    (* For D: use minimum Low across all future candles                        *)
    Which[
      direction === "U",
        futureHighs = Map[#["High"] &, futureCandles];
        actualPrice = Max[futureHighs];
        isReversal  = actualPrice < toPrice,

      direction === "D",
        futureLows  = Map[#["Low"] &, futureCandles];
        actualPrice = Min[futureLows];
        isReversal  = actualPrice > toPrice,

      True,
        actualPrice = Missing["UnknownDirection"];
        isReversal  = True
    ];

    actualGoodness = If[isReversal || MissingQ[actualPrice],
      0.0,
      (* Effective range for the actual candle uses same logic *)
      actualEffectiveRange = If[Sign[range] * (actualPrice - toPrice) > 0,
        actualPrice - fromPrice,
        range
      ];
      If[actualEffectiveRange == 0,
        0.0,
        actualFibPrices = Map[fromPrice + # * actualEffectiveRange &,
                              fibonacciRatiosArg];
        candlestickFibonacciGoodness[
          extendedCandles, actualFibPrices, Abs[actualEffectiveRange],
          Sequence @@ FilterRules[{opts},
            Options[candlestickFibonacciGoodness]]
        ]
      ]
    ];

    <|
      "ScanResults"      -> Tabular[results],
      "BestScanPrice"    -> results[[bestIdx]]["ScanPrice"],
      "BestScanGoodness" -> results[[bestIdx]]["Goodness"],
      "ActualPrice"      -> actualPrice,
      "ActualGoodness"   -> actualGoodness,
      "IsReversal"       -> isReversal,
      "OriginalToPrice"  -> toPrice,
      "OriginalRange"    -> range,
      "NumExtraCandles"  -> numExtraCandlesArg
    |>
  ];
  
Clear[findShortestOppositeSegmentEndingAt];
findShortestOppositeSegmentEndingAt[segmentsArg_,
                                      toIndexArg_Integer,
                                      directionArg_String] :=
  Module[{segments, oppositeDirection, matches, spans, shortestIdx},

    segments = If[Head[segmentsArg] === Tabular,
      Normal[segmentsArg],
      segmentsArg
    ];

    oppositeDirection = Which[
      directionArg === "U", "D",
      directionArg === "D", "U",
      True, Return[Missing["UnknownDirection"]]
    ];

    matches = Select[segments,
      #["ToIndex"] === toIndexArg &&
      #["Direction"] === oppositeDirection &];

    If[matches === {}, Return[Missing["NotFound"]]];

    spans = Map[#["ToIndex"] - #["FromIndex"] &, matches];
    shortestIdx = First[Ordering[spans, 1]];
    matches[[shortestIdx]]
  ];

Clear[segmentGoodnessWithContext];

Options[segmentGoodnessWithContext] = Options[candlestickFibonacciGoodness];

segmentGoodnessWithContext[featureSetArg_Dataset,
                            segmentArg_Association,
                            fibonacciRatiosArg_List : fibonacciRatios,
                            opts : OptionsPattern[]] :=
  Module[{candles, segments, priorSeg, priorFromIdx,
          currentFromIdx, currentToIdx, currentFromPrice, currentToPrice,
          currentRange, extendedFromIdx, extendedCandles,
          currentCandleCount, extendedCandleCount,
          fibPrices, rawGoodness, normalizedGoodness,
          priorFound},

    candles = featureSetArg["Candlesticks"];
    candles = If[Head[candles] === Tabular,
      Normal[candles],
      candles
    ];

    segments = If[KeyExistsQ[featureSetArg, "Segments"],
      featureSetArg["Segments"],
      Missing["NoSegments"]
    ];

    currentFromIdx   = segmentArg["FromIndex"];
    currentToIdx     = segmentArg["ToIndex"];
    currentFromPrice = segmentArg["FromPrice"];
    currentToPrice   = segmentArg["ToPrice"];
    currentRange     = currentToPrice - currentFromPrice;

    (* Find shortest prior opposite segment ending at current FromIndex *)
    priorSeg = If[MissingQ[segments],
      Missing["NoSegments"],
      findLongestOppositeSegmentEndingAt[segments, currentFromIdx,
        segmentArg["Direction"]]
    ];

    priorFound = !MissingQ[priorSeg];

    (* Extended range: from prior segment's FromIndex through current's ToIndex *)
    extendedFromIdx = If[priorFound,
      priorSeg["FromIndex"],
      currentFromIdx
    ];

    If[extendedFromIdx > currentToIdx || currentToIdx > Length[candles] ||
        currentRange == 0,
      Return[<|
        "Goodness"                   -> 0.0,
        "RawGoodness"                -> 0.0,
        "PriorSegmentFromIndex"      -> If[priorFound, priorSeg["FromIndex"], Missing["NotFound"]],
        "PriorSegmentToIndex"        -> If[priorFound, priorSeg["ToIndex"],   Missing["NotFound"]],
        "PriorSegmentFound"          -> priorFound,
        "CandlesticksScored"         -> 0,
        "CurrentSegmentCandlesticks" -> currentToIdx - currentFromIdx + 1
      |>]
    ];

    (* Slice includes the shared candlestick at currentFromIdx exactly once *)
    extendedCandles     = candles[[extendedFromIdx ;; currentToIdx]];
    extendedCandleCount = Length[extendedCandles];
    currentCandleCount  = currentToIdx - currentFromIdx + 1;

    fibPrices = Map[currentFromPrice + # * currentRange &, fibonacciRatiosArg];

    rawGoodness = candlestickFibonacciGoodness[
      extendedCandles, fibPrices, Abs[currentRange],
      Sequence @@ FilterRules[{opts},
        Options[candlestickFibonacciGoodness]]
    ];

    (* Normalize: multiply raw goodness by (currentCandleCount / extendedCandleCount) *)
    (* This offsets the dilution from including prior-segment candles: a segment      *)
    (* with a very short prior contribution keeps most of its raw goodness, while     *)
    (* one with a very long prior gets scaled down.                                   *)
    normalizedGoodness = rawGoodness * (currentCandleCount / extendedCandleCount);

    <|
      "Goodness"                   -> normalizedGoodness,
      "RawGoodness"                -> rawGoodness,
      "PriorSegmentFromIndex"      -> If[priorFound, priorSeg["FromIndex"], Missing["NotFound"]],
      "PriorSegmentToIndex"        -> If[priorFound, priorSeg["ToIndex"],   Missing["NotFound"]],
      "PriorSegmentFound"          -> priorFound,
      "CandlesticksScored"         -> extendedCandleCount,
      "CurrentSegmentCandlesticks" -> currentCandleCount
    |>
  ];


Clear[binomialUpperTail];
binomialUpperTail[mArg_Integer, pArg_?NumericQ, hArg_Integer] :=
  If[hArg <= 0,
    1.0,
    If[hArg > mArg,
      0.0,
      N[1 - CDF[BinomialDistribution[mArg, pArg], hArg - 1]]
    ]
  ];
  
Clear[fisherCombinedPValue];
fisherCombinedPValue[pValuesArg_List] :=
  Module[{floored, chiSq, k},
    floored = Map[Max[#, 1.*^-300] &, pValuesArg];
    chiSq   = -2 * Total[Log[floored]];
    k       = Length[floored];
    N[1 - CDF[ChiSquareDistribution[2 k], chiSq]]
  ];
  
Clear[computeFibonacciSegmentSignificance];
  
Options[computeFibonacciSegmentSignificance] = {
  "ExtendedToIndex" -> Automatic,
  "PriceTypes"      -> {"Open", "High", "Low", "Close"}
};

computeFibonacciSegmentSignificance[featureSetArg_Dataset,
                                      currentSegmentArg_Association,
                                      scanPriceArg_?NumericQ,
                                      fibonacciRatiosArg_List : fibonacciRatios,
                                      Optional[toleranceArg_?NumericQ, 0.01],
                                      opts : OptionsPattern[]] :=
  Module[{candles, segments, priorSeg, priorFound, priceTypes,
          currentFromIdx, currentToIdx, currentFromPrice, currentDirection,
          extendedToIdx,
          extendedRange, halfBand, fibPrices,
          priorFromIdx,
          currentPricesRaw, currentPrices, prevPrices,
          allPrices, universeLo, universeHi, universeLength,
          coverage, hitProbability,
          currentHits, prevHits,
          currentP, prevP, combinedP,
          countHits},

    candles = featureSetArg["Candlesticks"];
    candles = If[Head[candles] === Tabular, Normal[candles], candles];

    segments = If[KeyExistsQ[featureSetArg, "Segments"],
      featureSetArg["Segments"],
      Missing["NoSegments"]
    ];

    priceTypes       = OptionValue["PriceTypes"];
    currentFromIdx   = currentSegmentArg["FromIndex"];
    currentToIdx     = currentSegmentArg["ToIndex"];
    currentFromPrice = currentSegmentArg["FromPrice"];
    currentDirection = currentSegmentArg["Direction"];
    extendedToIdx    = If[OptionValue["ExtendedToIndex"] === Automatic,
      currentToIdx,
      OptionValue["ExtendedToIndex"]
    ];
    extendedToIdx = Min[extendedToIdx, Length[candles]];

    extendedRange = scanPriceArg - currentFromPrice;
    If[extendedRange == 0,
      Return[<|
        "Significance"        -> 1.0,
        "CurrentPValue"       -> 1.0,
        "PrevPValue"          -> 1.0,
        "CurrentHits"         -> 0,
        "CurrentPriceCount"   -> 0,
        "PrevHits"            -> 0,
        "PrevPriceCount"      -> 0,
        "HitProbability"      -> 0.0,
        "FibonacciPrices"     -> {},
        "PriceUniverseLength" -> 0.0,
        "BandWidth"           -> 0.0,
        "PriorSegmentFound"   -> False
      |>]
    ];

    fibPrices = Map[currentFromPrice + # * extendedRange &, fibonacciRatiosArg];

    (* halfBand is the tolerance radius (matches fibonacciTouchMarkers):     *)
    (*   hit when |price - fibonacciPrice| <= halfBand.                       *)
    (* Total band width is 2 * halfBand.                                       *)
    halfBand = toleranceArg * Abs[extendedRange];

    priorSeg = If[MissingQ[segments],
      Missing["NoSegments"],
      findLongestOppositeSegmentEndingAt[segments, currentFromIdx, currentDirection]
    ];
    priorFound   = !MissingQ[priorSeg];
    priorFromIdx = If[priorFound, priorSeg["FromIndex"], currentFromIdx];

    (* Collect current-segment prices excluding trivial-hit endpoints *)
    currentPricesRaw = Flatten[
      Map[
        Function[i,
          Module[{row, excluded},
            row = candles[[i]];
            excluded = Which[
              i === currentFromIdx,
                If[currentDirection === "U", {"Low"}, {"High"}],
              i === extendedToIdx,
                If[currentDirection === "U", {"High"}, {"Low"}],
              True,
                {}
            ];
            Map[row[#] &, Complement[priceTypes, excluded]]
          ]
        ],
        Range[currentFromIdx, extendedToIdx]
      ]
    ];
    currentPrices = currentPricesRaw;

    (* Collect prev-segment prices excluding the shared boundary candlestick *)
    prevPrices = If[priorFound && priorFromIdx < currentFromIdx,
      Flatten[
        Map[
          Function[i,
            Module[{row}, row = candles[[i]]; Map[row[#] &, priceTypes]]
          ],
          Range[priorFromIdx, currentFromIdx - 1]
        ]
      ],
      {}
    ];

    (* Universe = price extents of prev + current candle range *)
    allPrices = Join[
      Flatten[Map[Function[i,
        With[{row = candles[[i]]},
          {row["High"], row["Low"]}]],
        Range[priorFromIdx, extendedToIdx]
      ]],
      {currentFromPrice, scanPriceArg}
    ];
    universeLo     = Min[allPrices];
    universeHi     = Max[allPrices];
    universeLength = universeHi - universeLo;
    If[universeLength <= 0, universeLength = Abs[extendedRange]];

    (* Coverage: each band has TOTAL width 2 * halfBand.                  *)
    (* Pass 2 * halfBand to nonOverlappingCoverage so band spans          *)
    (* [fib - halfBand, fib + halfBand].                                    *)
    coverage       = nonOverlappingCoverage[fibPrices, 2 * halfBand,
                                              universeLo, universeHi];
    hitProbability = coverage / universeLength;

    (* Hit test: |price - fib| <= halfBand   (matches fibonacciTouchMarkers) *)
    countHits = Function[{pricesList},
      Length[Select[pricesList,
        Function[q,
          AnyTrue[fibPrices, Abs[q - #] <= halfBand &]
        ]
      ]]
    ];

    currentHits = countHits[currentPrices];
    prevHits    = countHits[prevPrices];

    currentP = binomialUpperTail[
      Length[currentPrices], hitProbability, currentHits];
    prevP = If[Length[prevPrices] > 0,
      binomialUpperTail[Length[prevPrices], hitProbability, prevHits],
      1.0
    ];

    combinedP = If[Length[prevPrices] > 0,
      fisherCombinedPValue[{currentP, prevP}],
      currentP
    ];

    <|
      "Significance"        -> combinedP,
      "CurrentPValue"       -> currentP,
      "PrevPValue"          -> prevP,
      "CurrentHits"         -> currentHits,
      "CurrentPriceCount"   -> Length[currentPrices],
      "PrevHits"            -> prevHits,
      "PrevPriceCount"      -> Length[prevPrices],
      "HitProbability"      -> hitProbability,
      "FibonacciPrices"     -> fibPrices,
      "PriceUniverseLength" -> universeLength,
      "BandWidth"           -> 2 * halfBand,
      "PriorSegmentFound"   -> priorFound
    |>
  ];
    
Clear[scanSegmentSignificance];

Options[scanSegmentSignificance] = Join[
  Options[computeFibonacciSegmentSignificance],
  {"NumScanPoints"   -> 64
  ,"UpperMultiplier" -> If[ValueQ[Global`defaultMaxExtension], Global`defaultMaxExtension, 1.31]
  }
];

scanSegmentSignificance[featureSetArg_Dataset,
                         currentSegmentArg_Association,
                         fibonacciRatiosArg_List : fibonacciRatios,
                         Optional[toleranceArg_?NumericQ, 0.01],
                         opts : OptionsPattern[]] :=
  Module[{fromPrice, toPrice, range, upperMultiplier, nScanPoints,
          scanEnd, scanPrices, results, bestIdx,
          segments, priorSeg, priorFound},

    fromPrice   = currentSegmentArg["FromPrice"];
    toPrice     = currentSegmentArg["ToPrice"];
    range       = toPrice - fromPrice;

    If[range == 0,
      Return[<|
        "ScanResults"           -> Tabular[{}],
        "BestScanPrice"         -> Missing["ZeroRange"],
        "BestSignificance"      -> Missing["ZeroRange"],
        "OriginalToPrice"       -> toPrice,
        "OriginalRange"         -> range,
        "PriorSegmentFromIndex" -> Missing["NotComputed"],
        "PriorSegmentToIndex"   -> Missing["NotComputed"],
        "PriorSegmentFound"     -> False
      |>]
    ];

    upperMultiplier = OptionValue["UpperMultiplier"];
    nScanPoints     = OptionValue["NumScanPoints"];
    scanEnd         = fromPrice + range * upperMultiplier;
    scanPrices      = Subdivide[fromPrice, scanEnd, nScanPoints - 1];

    (* Look up prior segment once for the return metadata *)
    segments = If[KeyExistsQ[featureSetArg, "Segments"],
      featureSetArg["Segments"], Missing["NoSegments"]];
    priorSeg = If[MissingQ[segments],
      Missing["NoSegments"],
      findLongestOppositeSegmentEndingAt[segments,
        currentSegmentArg["FromIndex"], currentSegmentArg["Direction"]]
    ];
    priorFound = !MissingQ[priorSeg];

    results = Map[
      Function[scanPrice,
        Module[{sig},
          sig = computeFibonacciSegmentSignificance[
            featureSetArg, currentSegmentArg, scanPrice,
            fibonacciRatiosArg, toleranceArg,
            Sequence @@ FilterRules[{opts},
              Options[computeFibonacciSegmentSignificance]]
          ];
          <|
            "ScanPrice"      -> scanPrice,
            "Significance"   -> sig["Significance"],
            "CurrentPValue"  -> sig["CurrentPValue"],
            "PrevPValue"     -> sig["PrevPValue"],
            "CurrentHits"    -> sig["CurrentHits"],
            "PrevHits"       -> sig["PrevHits"],
            "HitProbability" -> sig["HitProbability"]
          |>
        ]
      ],
      scanPrices
    ];

    bestIdx = First[Ordering[Map[#["Significance"] &, results], 1]];

    <|
      "ScanResults"           -> Tabular[results],
      "BestScanPrice"         -> results[[bestIdx]]["ScanPrice"],
      "BestSignificance"      -> results[[bestIdx]]["Significance"],
      "OriginalToPrice"       -> toPrice,
      "OriginalRange"         -> range,
      "PriorSegmentFromIndex" -> If[priorFound, priorSeg["FromIndex"], Missing["NotFound"]],
      "PriorSegmentToIndex"   -> If[priorFound, priorSeg["ToIndex"],   Missing["NotFound"]],
      "PriorSegmentFound"     -> priorFound
    |>
  ];  
  
Clear[significanceByRangeRatio];

Options[significanceByRangeRatio] = Join[
  Options[computeFibonacciSegmentSignificance],
  {
    "MinRatio"      -> 1.0,
    "MaxRatio"      -> If[ValueQ[Global`defaultMaxExtension], Global`defaultMaxExtension, 1.31],
    "NumScanPoints" -> 32
  }
];

significanceByRangeRatio[featureSetArg_,
                          currentSegmentArg_Association,
                          fibonacciRatiosArg_List : fibonacciRatios,
                          Optional[toleranceArg_?NumericQ, 0.01],
                          opts : OptionsPattern[]] :=
  Module[{fromPrice, toPrice, range, minRatio, maxRatio, nScanPoints,
          ratios, scanEntries, bestIdx,
          significanceByRatio, scanDetails,
          segments, priorSeg, priorFound},

    fromPrice = currentSegmentArg["FromPrice"];
    toPrice   = currentSegmentArg["ToPrice"];
    range     = toPrice - fromPrice;

    minRatio    = N[OptionValue["MinRatio"]];
    maxRatio    = N[OptionValue["MaxRatio"]];
    nScanPoints = OptionValue["NumScanPoints"];

    (* Early exit if range is zero *)
    If[range == 0,
      Return[<|
        "SignificanceByRatio"    -> <||>,
        "BestRatio"              -> Missing["ZeroRange"],
        "BestSignificance"       -> Missing["ZeroRange"],
        "BestScanPrice"          -> Missing["ZeroRange"],
        "OriginalToPrice"        -> toPrice,
        "OriginalRange"          -> range,
        "PriorSegmentFound"      -> False,
        "PriorSegmentFromIndex"  -> Missing["NotComputed"],
        "PriorSegmentToIndex"    -> Missing["NotComputed"],
        "MaxRatio"               -> maxRatio,
        "NumScanPoints"          -> nScanPoints,
        "ScanDetails"            -> Tabular[{}]
      |>]
    ];

    (* Look up prior segment once for return metadata *)
    segments = If[KeyExistsQ[featureSetArg, "Segments"],
      featureSetArg["Segments"],
      Missing["NoSegments"]
    ];
    priorSeg = If[MissingQ[segments],
      Missing["NoSegments"],
      findLongestOppositeSegmentEndingAt[segments,
        currentSegmentArg["FromIndex"], currentSegmentArg["Direction"]]
    ];
    priorFound = !MissingQ[priorSeg];

    (* Generate ratios from minRatio to maxRatio *)
    ratios = Subdivide[minRatio, maxRatio, nScanPoints - 1];

    (* Compute significance at each ratio *)
    scanEntries = Map[
      Function[ratio,
        Module[{scanPrice, sig},
          scanPrice = fromPrice + ratio * range;
          sig = computeFibonacciSegmentSignificance[
            featureSetArg, currentSegmentArg, scanPrice,
            fibonacciRatiosArg, toleranceArg,
            Sequence @@ FilterRules[{opts},
              Options[computeFibonacciSegmentSignificance]]
          ];
          <|
            "RangeRatio"     -> ratio,
            "ScanPrice"      -> scanPrice,
            "Significance"   -> sig["Significance"],
            "CurrentPValue"  -> sig["CurrentPValue"],
            "PrevPValue"     -> sig["PrevPValue"],
            "CurrentHits"    -> sig["CurrentHits"],
            "PrevHits"       -> sig["PrevHits"],
            "HitProbability" -> sig["HitProbability"]
          |>
        ]
      ],
      ratios
    ];

    (* Association keyed by ratio *)
    significanceByRatio = Association[
      Map[#["RangeRatio"] -> #["Significance"] &, scanEntries]
    ];

    (* Full details as Tabular *)
    scanDetails = Tabular[scanEntries];

    (* Best (most significant = smallest p-value) *)
    bestIdx = First[Ordering[Map[#["Significance"] &, scanEntries], 1]];

    <|
      "SignificanceByRatio"    -> significanceByRatio,
      "BestRatio"              -> scanEntries[[bestIdx]]["RangeRatio"],
      "BestSignificance"       -> scanEntries[[bestIdx]]["Significance"],
      "BestScanPrice"          -> scanEntries[[bestIdx]]["ScanPrice"],
      "OriginalToPrice"        -> toPrice,
      "OriginalRange"          -> range,
      "PriorSegmentFound"      -> priorFound,
      "PriorSegmentFromIndex"  -> If[priorFound, priorSeg["FromIndex"], Missing["NotFound"]],
      "PriorSegmentToIndex"    -> If[priorFound, priorSeg["ToIndex"],   Missing["NotFound"]],
      "MaxRatio"               -> maxRatio,
      "NumScanPoints"          -> nScanPoints,
      "ScanDetails"            -> scanDetails
    |>
  ];  
  
Clear[nonOverlappingCoverage];
nonOverlappingCoverage[fArg_List, wArg_?NumericQ,
                        segMin_?NumericQ, segMax_?NumericQ] :=
  Module[{halfW, individualLengths},
    halfW = wArg / 2;
    individualLengths = Map[
      Function[f,
        Max[0, Min[f + halfW, segMax] - Max[f - halfW, segMin]]
      ],
      fArg
    ];
    Total[individualLengths]
  ];  
  
Clear[computeFibonacciLift];

Options[computeFibonacciLift] = Options[computeFibonacciSegmentSignificance];

computeFibonacciLift[featureSetArg_Dataset,
                      currentSegmentArg_Association,
                      scanPriceArg_?NumericQ,
                      fibonacciRatiosArg_List : fibonacciRatios,
                      Optional[toleranceArg_?NumericQ, 0.01],
                      opts : OptionsPattern[]] :=
  Module[{sig, currentExpected, prevExpected, observedTotal, expectedTotal,
          currentLift, prevLift, combinedLift, lift},

    sig = computeFibonacciSegmentSignificance[
      featureSetArg, currentSegmentArg, scanPriceArg,
      fibonacciRatiosArg, toleranceArg,
      Sequence @@ FilterRules[{opts},
        Options[computeFibonacciSegmentSignificance]]
    ];

    currentExpected = sig["CurrentPriceCount"] * sig["HitProbability"];
    prevExpected    = sig["PrevPriceCount"]    * sig["HitProbability"];
    observedTotal   = sig["CurrentHits"] + sig["PrevHits"];
    expectedTotal   = currentExpected + prevExpected;

    (* Per-bucket lift: undefined when expected = 0 *)
    currentLift = If[currentExpected > 0,
      sig["CurrentHits"] / currentExpected,
      Missing["ZeroExpected"]];
    prevLift    = If[prevExpected > 0,
      sig["PrevHits"] / prevExpected,
      Missing["ZeroExpected"]];

    (* Combined lift: naturally weighted by sample size *)
    combinedLift = If[expectedTotal > 0,
      observedTotal / expectedTotal,
      Missing["ZeroExpected"]];

    lift = combinedLift;

    <|
      "Lift"              -> lift,
      "CurrentLift"       -> currentLift,
      "PrevLift"          -> prevLift,
      "CombinedLift"      -> combinedLift,
      "ObservedTotal"     -> observedTotal,
      "ExpectedTotal"     -> N[expectedTotal],
      "CurrentHits"       -> sig["CurrentHits"],
      "CurrentExpected"   -> N[currentExpected],
      "PrevHits"          -> sig["PrevHits"],
      "PrevExpected"      -> N[prevExpected],
      "HitProbability"    -> sig["HitProbability"],
      "Significance"      -> sig["Significance"]
    |>
  ];  
  
  
Clear[liftByRangeRatio];

Options[liftByRangeRatio] = Join[
  Options[computeFibonacciLift],
  {"MinRatio" -> 1.0,
   "MaxRatio" -> If[ValueQ[Global`defaultMaxExtension], Global`defaultMaxExtension, 1.31],
   "NumScanPoints" -> 32
  }
]; 
  
liftByRangeRatio[featureSetArg_Dataset,
                  currentSegmentArg_Association,
                  fibonacciRatiosArg_List : fibonacciRatios,
                  Optional[toleranceArg_?NumericQ, 0.01],
                  opts : OptionsPattern[]] :=
  Module[{fromPrice, toPrice, range, minRatio, maxRatio, nScanPoints,
          ratios, entries, liftAssoc, bestIdx, details},

    fromPrice = currentSegmentArg["FromPrice"];
    toPrice   = currentSegmentArg["ToPrice"];
    range     = toPrice - fromPrice;

    If[range == 0,
      Return[<|
        "LiftByRatio"   -> <||>,
        "BestRatio"     -> Missing["ZeroRange"],
        "BestLift"      -> Missing["ZeroRange"],
        "BestScanPrice" -> Missing["ZeroRange"],
        "ScanDetails"   -> Tabular[{}]
      |>]
    ];

    minRatio    = N[OptionValue["MinRatio"]];
    maxRatio    = N[OptionValue["MaxRatio"]];
    nScanPoints = OptionValue["NumScanPoints"];
    ratios      = Subdivide[minRatio, maxRatio, nScanPoints - 1];

    entries = Map[
      Function[ratio,
        Module[{scanPrice, lift},
          scanPrice = fromPrice + ratio * range;
          lift = computeFibonacciLift[
            featureSetArg, currentSegmentArg, scanPrice,
            fibonacciRatiosArg, toleranceArg,
            Sequence @@ FilterRules[{opts},
              Options[computeFibonacciLift]]
          ];
          <|
            "RangeRatio"     -> ratio,
            "ScanPrice"      -> scanPrice,
            "Lift"           -> lift["Lift"],
            "ObservedTotal"  -> lift["ObservedTotal"],
            "ExpectedTotal"  -> lift["ExpectedTotal"],
            "HitProbability" -> lift["HitProbability"],
            "Significance"   -> lift["Significance"]
          |>
        ]
      ],
      ratios
    ];

    liftAssoc = Association[
      Map[#["RangeRatio"] -> #["Lift"] &, entries]
    ];
    details = Tabular[entries];

    bestIdx = First[Ordering[
      Map[If[MissingQ[#["Lift"]], -Infinity, #["Lift"]] &, entries], -1]];

    <|
      "LiftByRatio"   -> liftAssoc,
      "BestRatio"     -> entries[[bestIdx]]["RangeRatio"],
      "BestLift"      -> entries[[bestIdx]]["Lift"],
      "BestScanPrice" -> entries[[bestIdx]]["ScanPrice"],
      "ScanDetails"   -> details
    |>
  ];  
  
(* ============================================================================
   computeTouchCountsForSegmentAtExtension \[LongDash] three-count touch tally for a segment
   ============================================================================ *)

Clear[computeTouchCountsForSegmentAtExtension];
computeTouchCountsForSegmentAtExtension[featureSetArg_Dataset,
                              segmentArg_Association,
                              fibonacciRatiosArg_List,
                              extensionRatioArg_?NumericQ,
                              touchToleranceArg_?NumericQ,
                              touchPriceTypesArg_List,
                              extraOnRightArg_Integer : 0] :=
  Module[{cs, allCandles, totalCandles,
          curFromIdx, curToIdx, curFromPrice, curToPrice, curDirection,
          curRange, effectiveRatio, effectiveRange, effectiveToPrice,
          fibPrices, halfBand,
          segments, priorSeg, priorFound, priorFromIdx, priorToIdx,
          preRangeStart, preRangeEnd, inRangeStart, inRangeEnd,
          postRangeStart, postRangeEnd,
          preCandles, inCandles, postCandles,
          countTouchesInRange,
          preTouches, inTouchesRaw, inTouches, postTouches,
          preCandleCount, inCandleCount, postCandleCount},

    (* --- Extract segment info --- *)
    curFromIdx    = segmentArg["FromIndex"];
    curToIdx      = segmentArg["ToIndex"];
    curFromPrice  = segmentArg["FromPrice"];
    curToPrice    = segmentArg["ToPrice"];
    curDirection  = segmentArg["Direction"];
    curRange      = curToPrice - curFromPrice;

    (* --- Fibonacci prices at the given extension ratio --- *)
    effectiveRatio   = Max[1.0, extensionRatioArg];
    effectiveRange   = curRange * effectiveRatio;
    effectiveToPrice = curFromPrice + effectiveRange;
    fibPrices        = Map[curFromPrice + # * effectiveRange &, fibonacciRatiosArg];
    halfBand         = touchToleranceArg * Abs[effectiveRange];

    (* --- Load candles --- *)
    cs = featureSetArg["Candlesticks"];
    cs = If[Head[cs] === Tabular, cs, Tabular[cs]];
    allCandles   = Normal[cs];
    totalCandles = Length[allCandles];

    (* --- Find prior opposite-direction shortest segment --- *)
    segments = If[KeyExistsQ[featureSetArg, "Segments"],
      featureSetArg["Segments"],
      Missing["NoSegments"]
    ];
    priorSeg = If[MissingQ[segments],
      Missing["NoSegments"],
      findLongestOppositeSegmentEndingAt[segments, curFromIdx, curDirection]
    ];
    priorFound = !MissingQ[priorSeg];

    (* --- Range boundaries --- *)
    If[priorFound,
      priorFromIdx  = priorSeg["FromIndex"];
      priorToIdx    = priorSeg["ToIndex"];
      preRangeStart = priorFromIdx;
      preRangeEnd   = priorToIdx - 1;    (* exclude shared boundary *)
      ,
      preRangeStart = curFromIdx;
      preRangeEnd   = curFromIdx - 1;    (* empty range when no prior *)
    ];

    inRangeStart   = curFromIdx;
    inRangeEnd     = curToIdx;
    postRangeStart = curToIdx + 1;
    postRangeEnd   = Min[curToIdx + extraOnRightArg, totalCandles];

    (* --- Candle count helper (empty ranges return 0) --- *)
    preCandleCount  = Max[preRangeEnd  - preRangeStart  + 1, 0];
    inCandleCount   = Max[inRangeEnd   - inRangeStart   + 1, 0];
    postCandleCount = Max[postRangeEnd - postRangeStart + 1, 0];

    (* --- Candle slices (clamped to valid indices) --- *)
    preCandles  = Select[allCandles,
      preRangeStart  <= #["Index"] <= preRangeEnd  &];
    inCandles   = Select[allCandles,
      inRangeStart   <= #["Index"] <= inRangeEnd   &];
    postCandles = Select[allCandles,
      postRangeStart <= #["Index"] <= postRangeEnd &];

    (* --- Touch-counting function --- *)
    countTouchesInRange = Function[candleList,
      Length[Flatten[
        Map[
          Function[row,
            Select[
              Map[row[#] &, touchPriceTypesArg],
              Function[priceVal,
                AnyTrue[fibPrices, Abs[priceVal - #] <= halfBand &]
              ]
            ]
          ],
          candleList
        ],
        1
      ]]
    ];

    preTouches   = countTouchesInRange[preCandles];
    inTouchesRaw = countTouchesInRange[inCandles];
    (* Subtract 1 for the always-present extreme touches on current segment *)
    inTouches    = Max[inTouchesRaw - 1, 0];
    postTouches  = countTouchesInRange[postCandles];

    <|
      "PreTouches"      -> preTouches,
      "InTouches"       -> inTouches,
      "InTouchesRaw"    -> inTouchesRaw,
      "PostTouches"     -> postTouches,
      "PreCandleCount"  -> preCandleCount,
      "InCandleCount"   -> inCandleCount,
      "PostCandleCount" -> postCandleCount,
      "FibPrices"       -> fibPrices,
      "HalfBand"        -> halfBand,
      "EffectiveRatio"  -> effectiveRatio,
      "ExtraOnRight"    -> extraOnRightArg
    |>
  ];  
  

Clear[findLongestOppositeSegmentEndingAt];
findLongestOppositeSegmentEndingAt[segmentsArg_,
                                     toIndexArg_Integer,
                                     directionArg_String] :=
  Module[{segments, oppositeDirection, matches, spans, longestIdx},

    segments = If[Head[segmentsArg] === Tabular,
      Normal[segmentsArg],
      segmentsArg
    ];

    oppositeDirection = Which[
      directionArg === "U", "D",
      directionArg === "D", "U",
      True, Return[Missing["UnknownDirection"]]
    ];

    matches = Select[segments,
      #["ToIndex"] === toIndexArg &&
      #["Direction"] === oppositeDirection &];

    If[matches === {}, Return[Missing["NotFound"]]];

    spans = Map[#["ToIndex"] - #["FromIndex"] &, matches];
    longestIdx = First[Ordering[spans, -1]];
    matches[[longestIdx]]
  ];


      
 End[]

EndPackage[]
