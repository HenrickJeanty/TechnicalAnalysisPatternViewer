(* ::Package:: *)

(* ============================================================================
   Visualization.wl
   StockAnalyzer Package -- Visualization Layer
   
   Provides candlestick chart rendering, parametric line geometry, Fibonacci
   rectangle overlays, segment tooltips, and the top-level graph display
   function used by GraphBuilder_Demo.nb.
   
   Author : Henry Henrick Jeanty, Ph.D.
   Version: 1.0.0
   Requires: Mathematica 14.3+
   ============================================================================ *)

BeginPackage["StockAnalyzer`Visualization`",
  {"StockAnalyzer`StockDataManager`",
   "StockAnalyzer`Fibonacci`",
   "StockAnalyzer`Analysis`"}
]

hoveredEdgeKey;
hoveredSegs;
hoveredSeg;
hoveredRow;
hoveredVertex;
(* Initialize hover state globals *)
hoveredEdgeKey = None;
hoveredSegsGlobal = <||>;
hoveredSeg     = None;
hoveredRow     = None;
hoveredVertex  = None;
hoveredField   = None;

StockAnalyzer`Visualization`lastClickedCandidates = {};
StockAnalyzer`Visualization`lastSelectedSegKey = None;

showMarkovMatrix = False;

(* ============================================================================
   Public symbol declarations with usage messages
   ============================================================================ *)

(* -- Color Constants -------------------------------------------------------- *)
bullishColor::usage =
  "bullishColor is the default color for bullish (upward) trend arrows and edges.";

bearishColor::usage =
  "bearishColor is the default color for bearish (downward) trend arrows and edges.";

bullishCandlestickColor::usage =
  "bullishCandlestickColor is the fill color for bullish (close >= open) candlestick bodies.";

bearishCandlestickColor::usage =
  "bearishCandlestickColor is the fill color for bearish (close < open) candlestick bodies.";

rectangleBullish::usage =
  "rectangleBullish is the default color for bullish Fibonacci rectangle overlays.";

rectangleBearish::usage =
  "rectangleBearish is the default color for bearish Fibonacci rectangle overlays.";

(* -- Edge Color Palettes ---------------------------------------------------- *)
edgeTypeColor::usage =
  "edgeTypeColor is an Association mapping named EdgeType strings to display \
colors. Up edge types use the \"Aquamarine\" ColorData scheme; Down edge types \
use the \"SolarColors\" scheme. Keys: \"MaxU\", \"UpInner\", \"Support\", \
\"InnerUp\", \"OuterUp\", \"MaxD\", \"DownInner\", \"Resistance\", \
\"InnerDown\", \"OuterDown\".";

(* -- Overlay Helpers -------------------------------------------------------- *)
$overlays::usage =
  "$overlays is a global Association keyed by chart name, storing lists of \
overlay Graphics primitives for each named chart.";

clearOverlay::usage =
  "clearOverlay[keyArg] resets the overlay list for the chart named keyArg to {}.";

clearOverlays::usage =
  "clearOverlays[] clears all overlays for all charts by resetting $overlays to <||>.";

(* -- Line & Geometry -------------------------------------------------------- *)
makeSegment::usage =
  "makeSegment[{x1,y1},{x2,y2}] returns Graphics primitives for a direct \
point-to-point line segment. Options: \"color\" (default Black), \
\"thickness\" (default 0.002).";

makeLineFunction::usage =
  "makeLineFunction[{x1,y1},{x2,y2}] returns a pure Function f such that \
f[x] gives the y value of the line through the two points at x.";

makeParametricLine::usage =
  "makeParametricLine[{xi,yi},{xj,yj}] returns an Association with keys \
\"slope\", \"intercept\", \"f\", \"normal\", and \"vertical\" describing the \
line through the two points. makeParametricLine[slope, intercept] constructs \
the same structure from slope-intercept form.";

perpendicularLine::usage =
  "perpendicularLine[line, xInt] returns a parametric line Association \
perpendicular to line at x = xInt.";

lineY::usage =
  "lineY[line, x] returns the y value of line at x.";

linePoint::usage =
  "linePoint[line, x] returns the point {x, y} on line at x.";

linePointDist::usage =
  "linePointDist[line, pt] returns the perpendicular distance from point pt \
to line.";

lineParametric::usage =
  "lineParametric[line] returns a pure Function r such that r[t] gives the \
point on line at parameter t using the parametric form r[t] = p0 + t * dir.";

lineString::usage =
  "lineString[line] returns a human-readable String description of line in \
slope-intercept form.";

lineAngle::usage =
  "lineAngle[line1, line2] returns the signed angle in radians from line1 to \
line2, measured counterclockwise. Returns Sign[numer]*Pi/2 when the lines are \
perpendicular.";

drawLine::usage =
  "drawLine[line, {xstart, xend, xext}] draws line from xstart to xend as a \
solid segment, then extends it dashed from xend to xext. Options: \
\"thickness\", \"upColor\", \"downColor\", \"dashedColor\".";

(* -- Rectangle Overlays ----------------------------------------------------- *)
makeRectangle::usage =
  "makeRectangle[{x1,y1},{x2,y2}] returns Graphics primitives for a Fibonacci \
retracement rectangle with horizontal fill lines. Options: \"upColor\", \
\"downColor\", \"thickness\", \"lineOpacity\", \"fillLines\", \"extraOnLeft\", \
\"extraOnRight\".";

makeRectangleFromSegment::usage =
  "makeRectangleFromSegment[segmentArg] calls makeRectangle using the \
FromIndex, FromPrice, ToIndex, and ToPrice fields of segmentArg. Accepts all \
Options of makeRectangle.";

drawRectangles::usage =
  "drawRectangles[rects] maps makeRectangle over a list of {{x1,y1},{x2,y2}} \
pairs. Accepts all Options of makeRectangle.";

(* -- Chart Primitives ------------------------------------------------------- *)
candlestickGlyph::usage =
  "candlestickGlyph[cx, open, high, low, close, halfWidth] returns Graphics \
primitives for a single OHLC candlestick centered at x = cx. halfWidth \
defaults to 0.35.";

dateTickFunction::usage =
  "dateTickFunction[candlesticksArg] returns a tick-generating Function \
suitable for FrameTicks that labels each candlestick index with its formatted \
date string, rotated -45 degrees.";

indexTickFunction::usage =
  "indexTickFunction[candlesticksArg] returns a tick-generating Function \
suitable for FrameTicks that labels each candlestick position with its integer \
index.";

infoBarText::usage =
  "infoBarText[row] formats a single candlestick row Association as a \
horizontal Row expression matching TradingChart's top info bar style. \
infoBarText[None] returns an empty string.";

segmentTooltipContent::usage =
  "segmentTooltipContent[seg] returns a Framed tooltip widget displaying the \
Direction, FromPrice, ToPrice, Range, LogRatio, Angle, and Weight fields of \
segment Association seg.";

candlestickVertexGraph::usage =
  "candlestickVertexGraph[featureSet, {{from1, to1}, ...}, opts] returns a plain \
Graph (no event handlers) in which every vertex n is drawn as the OHLC \
candlestick of featureSet[\"Candlesticks\"][[n]] at x = n. The optional second \
argument lists the segments to show, each as {fromIndex, toIndex} (or a segment \
Association); each is looked up in featureSet[\"Segments\"] and drawn as an \
arrow from its FromPrice to its ToPrice (low to high for an up segment, high \
to low for a down segment), styled and coloured like the arrows of makeGraphDisplay. With no list \
or an empty list only the candlesticks are shown. The listed segments are the \
only ones drawn; \"FromIndex\" and \"ToIndex\" (1, All) only choose which \
candlesticks are shown, so an arrow reaching outside that range is clipped at \
the plot edge. Other string options: \"GlyphWidth\" (0.35), \"VertexShapeFunction\" (\"Glyph\"; \"Glyph\" draws candlesticks, \"Square\", \"Circle\", \"Diamond\" and \"Triangle\" draw that shape in the up/down colour, anything else goes to Graph; the shapes sit at the ToIndex candle's High (up segment) or Low (down segment), and at the FromIndex candle's Low (up) or High (down)), \"ShapeSize\" (0.2, half-width of the shapes in index units), \"EdgeTypes\" (All or a list such \
as {\"MaxU\", \"MaxD\"}), \"ArrowSize\" (0.005), \"EdgeThickness\" (0.0005), \
\"BackgroundEdges\" (Automatic: with Graphs in the second argument the whole graph is drawn in gray behind them, as HighlightGraph does; True or False forces it), \"BackgroundStyle\", \"HighlightColors\" (Automatic), \"HighlightThickness\" (2, times EdgeThickness), \"RealEdges\" (True: the Graph has the edges of featureSet[\"Graph\"], so graph functions such as FindIsomorphicSubgraph work on it; the second argument may also list Graphs, e.g. matches), \"Graph\" (Automatic = featureSet[\"Graph\"]; a Graph supplies the vertices and edges instead, also as candlestickVertexGraph[featureSet, graph, opts]; the candlesticks and the edge prices still come from the featureSet), \"OwnEdges\" (Automatic: with no segment list the Graph draws its own gray edges, one arrow per edge, so HighlightGraph[g, FindIsomorphicSubgraph[g, pattern, n]] shows the matches in colour over the graph; True or False forces it), \"PointLabels\" ({} or entries {index, price, label}, drawn as text), \n\"ImageSize\" (800), \"AspectRatio\" (0.5). Other options (for example FrameTicks or FrameLabel, which override the default \
index and price axes) are passed on to Graph.";

candlestickVertexGraph::noseg =
  "No segment found in the feature set for {fromIndex, toIndex} = `1`.";

(* -- High-Level Display ----------------------------------------------------- *)
makeGraphDisplay::usage =
  "makeGraphDisplay[featureSetArg, cs] renders a candlestick chart from \
Tabular cs overlaid with directed trend arrows from featureSetArg[\"EdgeSegments\"]. \
Returns a DynamicModule with hover tooltips, click-to-pin edge rectangles, \
and an optional price slider. Options: GlyphWidth, ImageSize, XAxis \
(\"Index\" or \"Date\"), EdgeThickness, Lines, Background, UpColor, DownColor, \
ArrowSize, ShowPriceSlider, ShowGraphEx (default False - controls initial edge \
visibility, toggled at runtime via Clear and Show All Edges buttons).";

ShowExtensions::usage =
  "ShowExtensions is an option for makeGraphDisplay, displayCandlestickGraph \
and related displays. With ShowExtensions -> True each trend arrow is followed \
by a dotted line extending its slope to the right edge of the chart; the \
default False draws the arrows only.";

makeGraphDisplayWithSegmentPicker::usage =
  "makeGraphDisplayWithSegmentPicker[featureSet, opts] returns a Column \
containing the standard graph display plus a clickable Dataset of segments \
from featureSet[\"Segments\"] (MaxU/MaxD only). Clicking a row's Show \
button sets lastSegmentClicked, which the chart's reactive watcher picks \
up and re-renders.";

displayCandlestickGraphWithSegmentPicker::usage =
  "displayCandlestickGraphWithSegmentPicker[featureSet, chartKey, opts] \
behaves exactly like displayCandlestickGraph but returns a Column with the \
chart on top and a clickable Dataset of featureSet[\"Segments\"] (MaxU/MaxD \
only) underneath. Clicking a row's Show button sets lastSegmentClicked, \
which the chart's reactive watcher picks up and re-renders. If the featureSet has support and resistance \
lines (updateFeatureSet option \"SupportResistance\") they are listed in two more tables, resistances and supports, controlled by the same buttons; \
the lines valid at the last candle are on the chart at first, Show / Hide and Select all change that, and they stay \
until the Clear Support/Resistance button is pressed.";

displayScrollableCandlestickGraph::usage =
  "displayScrollableCandlestickGraph[featureSetArg, chartKey] is the scrollable \
equivalent of displayCandlestickGraph, adding peak/valley markers. Accepts all \
Options of makeGraphDisplay.";

ShowScrollbar::usage =
  "ShowScrollbar is an option for makeGraphDisplay and displayCandlestickGraph. \
When True, displays an IntervalSlider below the chart for scrolling through \
candlestick history. Default False.";

animateFibonacci::usage =
  "animateFibonacci[segmentArg, fibRatiosArg, toleranceArg, chartKeyArg] animates \
a Fibonacci level scan over a price segment on the candlestick chart display. \
A white horizontal scan line moves from FromPrice + First[fibRatiosArg] * Range to \
FromPrice + Last[fibRatiosArg] * Range. At each scan position, horizontal Fibonacci \
lines are drawn at FromPrice + ratio * currentRange for each ratio in fibRatiosArg, \
each with a colored tolerance band. A cyan rectangle shows the accumulated price range. \
Price labels are displayed at the right end of each line. The animation is controlled \
by a Manipulate slider. Requires a separate cell displaying the chart via \
displayCandlestickGraph before calling this function, as it updates \
$overlays[chartKeyArg] dynamically. \
Default fibRatiosArg: {0.236, 0.382, 0.500, 0.618, 0.786, 1., 1.272}. \
Default toleranceArg: 0.01. Default chartKeyArg: \"default\".";

displayCandlestickGraphWithPDF::usage =
  "displayCandlestickGraphWithPDF[featureSetArg, targetSegmentArg, \
priceTypesArg, fibonacciRatiosArg, toleranceArg, chartKey, opts] renders \
the candlestick graph for featureSetArg with a Gaussian-mixture PDF panel \
attached below. The PDF is computed by predicting prices for \
targetSegmentArg using each candlestick from the segment, each price type \
in priceTypesArg (default {\"Open\", \"High\", \"Low\", \"Close\"}), and \
each Fibonacci ratio in fibonacciRatiosArg (default fibonacciRatios). \
toleranceArg (default 0.05) controls band widths. The PDF panel shares \
the price y-axis with the candlesticks. Option \"ShowSummaryMarkers\" \
(default True) overlays the global max (yellow), expected value \
(magenta), and credible interval (white dashed) as horizontal lines. \
Option \"DesiredLevelOfConfidence\" (default 0.95) controls Gaussian \
width per band so the band radius represents that confidence level.";

plotTouchPointCounts::usage =
  "plotTouches[segmentArg, touchPointsArg, candlesticksArg, imageSizeArg] \
displays a scatter plot of touch counts vs scanned price for a given segment. \
segmentArg is the target segment Association containing FromIndex, ToIndex, \
FromPrice, ToPrice, Direction and Range. touchPointsArg is a list of \
{scanPrice, touchCount} pairs as returned by scanFibonacciTouches or \
scanTrendlineTouches. candlesticksArg is the full list of candlestick rows. \
imageSizeArg controls the plot width in pixels (default 512). \
The plot shows: dashed vertical lines at FromPrice and ToPrice in green (uptrend) \
or red (downtrend); a directional arrow between FromPrice and ToPrice; \
a yellow vertical line at the next candlestick Open price; and a horizontal \
candlestick glyph for the next candlestick after the segment. \
A fake starting point at {FromPrice, 1} is prepended to anchor the x-axis.";

touchPointsForSegment::usage =
  "touchPointsForSegment[touchesForSegmentArg] returns a list of {ScanPrice, \
Touches} pairs extracted from the ScanResultsTable of touchesForSegmentArg \
(one segment's result from scanFibonacciTouches or scanTrendlineTouches, \
or any other function that produces a ScanResultsTable with ScanPrice and \
Touches columns). Use to feed plotTouchPointCounts or other visualization \
functions that consume {x, y} pairs.";

displayGoodnessProfile::usage =
  "displayGoodnessProfile[segmentArg, candlesticksArg, fibonacciRatiosArg, opts] \
returns a rotated goodness profile chart: price on the vertical axis, \
goodness value on the horizontal axis. The vertical price range matches \
the segment's FromPrice to ToPrice. Suitable for side-by-side display \
next to a candlestick chart. Options are forwarded to segmentGoodnessCurve.";

fibonacciTouchMarkers::usage =
  "fibonacciTouchMarkers[featureSetArg, segmentArg, fibonacciRatiosArg, \
toleranceArg, opts] returns Graphics primitives (colored markers) at every \
candlestick price (Open, High, Low, Close) that falls within toleranceArg \
(relative to segment range) of any Fibonacci price computed from segmentArg. \
Uses the shortest prior opposite-direction segment ending at \
segmentArg[\"FromIndex\"] to extend the candlestick range considered. \
Options: \"MarkerColor\" -> Orange (default), \"MarkerSize\" -> 6, \
\"PriceTypes\" -> {\"Open\", \"High\", \"Low\", \"Close\"}.";
  
compareLiftFunctions::usage =
  "compareLiftFunctions[liftFnsListArg_List, segmentArg, opts] is a thin " <>
  "wrapper around plainCandlestickChartWithLiftProfile that passes " <>
  "liftFnsListArg as the \"LiftFunctions\" option, so the right-side " <>
  "panel area becomes a vertical stack of one lift curve per function " <>
  "(all sharing the chart's price axis and the Manipulate frame slider). " <>
  "segmentArg defaults to Automatic, which uses " <>
  "StockAnalyzer`Visualization`lastSegmentClicked. Example: " <>
  "compareLiftFunctions[{liftFunctionInTouchesOnly, " <>
  "liftFunctionPrePlusIn, liftFunctionWeightedByPriceType}]. All other " <>
  "options are forwarded to plainCandlestickChartWithLiftProfile.";  

plotLiftAndSignificance::usage =
  "plotLiftAndSignificance[featureSetArg, currentSegmentArg, \
fibonacciRatiosArg, toleranceArg, opts] produces a two-panel visualization: \
top panel shows observed vs expected hit counts across range ratios (with \
lift curve overlay), bottom panel shows -log10(p-value) with significance \
thresholds. Both share the same x-axis (range ratio from MinRatio to MaxRatio). \
Options: \"MinRatio\", \"MaxRatio\", \"NumScanPoints\", plus all options of \
computeFibonacciLift. ImageSize controls the total plot width.";



lastSegmentClicked::usage = 
  "lastSegmentClicked is the most recently clicked segment (an Association) \
in a displayCandlestickGraph. Read this after clicking a candlestick to \
get its segment data.";

shownTrendlines::usage =
  "shownTrendlines is an Association of the support and resistance lines that stay on the chart of displayCandlestickGraphWithSegmentPicker until clearShownTrendlines[] (the Clear Support/Resistance button) is used. Keys are strings, values are Associations with Segment and TouchMarkers.";

shownPatterns::usage =
  "shownPatterns is an Association of the support/resistance patterns (a resistance and a support) that stay on the chart of displayCandlestickGraphWithSegmentPicker until clearShownTrendlines[] is used. Keys are strings, values are Associations with Pattern, Resistance, Support and LastIndex.";

shownHarmonics::usage =
  "shownHarmonics is an Association of the harmonic patterns (rows of the key HarmonicPatterns of a featureSet) that stay on the chart of displayCandlestickGraphWithSegmentPicker until clearShownTrendlines[] or the Clear button of the harmonic table is used. Keys are strings, values are the rows.";

lastHarmonic::usage =
  "lastHarmonic is the row of the harmonic pattern that was shown last (its Subgraph is drawn under the harmonic table), or None.";

plainCandlestickChart::usage =
  "plainCandlestickChart[featureSetArg, targetSegmentArg, \
fibonacciRatiosArg, extensionRatioArg, opts] renders a static candlestick \
chart focused on a target segment and its shortest prior opposite segment. \
Displays candlestick glyphs, peak/valley dots, a solid arrow for the current \
segment, a dashed arrow for the prior segment, a dashed extended arrow \
projecting to (curToIdx+1, curFromPrice + curRange*extensionRatioArg), a \
rectangle enclosing the current segment (extended to Max[1.0, \
extensionRatioArg] times the original range) with Fibonacci horizontal lines \
at that effective range, and orange touch markers on candlestick prices \
within TouchTolerance of any Fibonacci level. For extensionRatioArg < 1.0, \
if the extended endpoint is within tolerance of a Fibonacci line, an extra \
white-ringed orange marker is placed at (curToIdx+1, extendedArrowToPrice). \
Displays lift value (observed/expected touches) as a title inside the chart. \
No event handlers \[LongDash] suitable for animation, export, or side-by-side display. \
Options include GlyphWidth, ImageSize, Background, UpColor, DownColor, \
ScalingFunctions, XAxis, and string-keyed options ShowPeakValleyDots, \
PeakDotSize, ValleyDotSize, PeakDotColor, ValleyDotColor, ArrowSize, \
ArrowThickness, RectangleBorderThickness, FibonacciLineThickness, \
FibonacciLineOpacity, TouchTolerance, TouchMarkerColor, TouchMarkerSize, \
TouchPriceTypes, HypotheticalMarkerSize, TitleFontSize, Verbose.";

plainCandlestickChartFrames::usage =
  "plainCandlestickChartFrames[featureSetArg, targetSegmentArg, \
fibonacciRatiosArg, opts] generates a list of static chart frames across \
a range of extensionRatio values, suitable for ListAnimate, Manipulate, or \
export as GIF/MP4. Returns an Association with Ratios, Frames, NumFrames, \
MinRatio, MaxRatio, TotalTime, TimePerFrame. Options: \"MinRatio\" (default \
1.0), \"MaxRatio\" (default Global`defaultMaxExtension if set, else 1.31), \"NumFrames\" (default 256), plus \
all options of plainCandlestickChart.";

computeLiftAtRatio::usage =
  "computeLiftAtRatio[featureSetArg, targetSegmentArg, fibonacciRatiosArg, \
extensionRatioArg, touchToleranceArg, touchPriceTypesArg] returns the lift \
value (observed/expected Fibonacci touches under null hypothesis) at a \
given extension ratio, using the same logic as plainCandlestickChart. \
Returns a numeric value or Missing[\"ZeroExpected\"] / Missing[\"NoData\"] \
in edge cases.";

plainCandlestickChartWithLiftCurve::usage =
  "plainCandlestickChartWithLiftCurve[featureSetArg, targetSegmentArg, \
fibonacciRatiosArg, opts] renders a dual-panel interactive Manipulate: the \
top panel shows the plainCandlestickChart for the current frame's extension \
ratio; the bottom panel shows a lift curve (lift vs extension ratio) with \
a vertical marker tracking the current frame. Slide the control to scrub \
through frames and observe how the lift changes across the Fibonacci \
extension range. Peaks in the lift curve near classical ratios (1.0, 1.272, \
1.618) suggest strong Fibonacci fits at those target prices. \
Options include all of plainCandlestickChartFrames plus: LiftCurveHeight \
(default 180), LiftCurveColor (default light blue), MarkerLineColor \
(default yellow), ShowLiftMarkers (default True), AnnotateFibRatios \
(default {1.0, 1.272, 1.618}).";

plainCandlestickChartAnimation::usage =
  "plainCandlestickChartAnimation[featureSetArg, targetSegmentArg, \
fibonacciRatiosArg, opts] returns a Manipulate widget that animates the \
sweep of extensionRatio values from MinRatio to MaxRatio. Same arguments \
and inherited options as plainCandlestickChartFrames, plus animation \
controls. Opens paused with controls at top, starting at the frame with \
highest lift value by default. \
Additional options: AnimationRate (default 10), AutoPlay (default False), \
InitialFrame (default \"MaxLift\"; also accepts \"First\", \"Last\", or an \
integer index).";

plainCandlestickChartWithLiftProfile::usage =
  "plainCandlestickChartWithLiftProfile[featureSetArg, targetSegmentArg, " <>
  "fibonacciRatiosArg, opts] renders an animated candlestick chart with " <>
  "fib lines + touches on the left and one or more lift profile panels " <>
  "on the right, sharing a vertical price axis. The Manipulate frame " <>
  "slider sweeps the extension ratio and updates both the chart overlays " <>
  "and the current-extension markers on each lift panel. Option " <>
  "\"LiftFunctions\" (default Automatic -> {$defaultLiftFunction}) " <>
  "accepts a List of lift functions \[LongDash] N > 1 stacks N lift " <>
  "panels vertically on the right, each labeled with its function's " <>
  "SymbolName (override via \"LiftFunctionNames\" -> {\"a\", \"b\", ...}). " <>
  "Other options: \"LiftProfileWidth\" (default 250), \"LiftCurveColor\", " <>
  "\"MarkerLineColor\", \"ShowLiftMarkers\", \"AnnotateFibRatios\", " <>
  "\"AnimationRate\", \"AutoPlay\", \"InitialFrame\" " <>
  "(\"MaxLift\" | \"First\" | \"Last\" | integer; MaxLift uses the first " <>
  "lift function's max), \"MergedAxis\", \"SharedAxisColor\", " <>
  "\"SharedAxisThickness\", plus all Options of plainCandlestickChartFrames " <>
  "(including MinRatio, MaxRatio, NumFrames, FixedYRange, TouchTolerance, " <>
  "TouchPriceTypes, ImageSize, GlyphWidth, UpColor, DownColor, " <>
  "ExtraOnLeft, ExtraOnRight).";
  
handleExternalSegmentSet::usage =
  "handleExternalSegmentSet[featureSetArg, touchToleranceArg, touchMarkerSizeArg, " <>
  "chartExtraLeftArg, chartExtraRightArg] is the reactive watcher body. " <>
  "Reads StockAnalyzer`Visualization`lastSegmentClicked; if the segment's " <>
  "6-element EdgeSegments-style segKey is NOT already in hoveredSegs, this " <>
  "is an external set (not a click/keyboard update) -- clears hoveredSegs " <>
  "and adds that segment. Called from a Dynamic in makeGraphDisplay. Returns {}.";



lastSegmentClicked = None;
shownTrendlines = <||>;
shownPatterns = <||>;
shownHarmonics = <||>;
lastHarmonic = None;

(* ============================================================================
   Group 1: Color Constants
   ============================================================================ *)

bullishColor            = RGBColor[0.0,  0.85, 0.0];   (* bright green arrows    *)
bearishColor            = RGBColor[0.85, 0.0,  0.0];   (* bright red arrows      *)
bullishCandlestickColor = RGBColor[0.0,  0.80, 0.0];   (* bright green - bullish body outline/fill *)
bearishCandlestickColor = RGBColor[0.85, 0.1,  0.1];   (* bright red   - bearish body fill *)
rectangleBullish        = Darker[Cyan,  .75];
rectangleBearish        = Darker[Pink,  .75];

(* ============================================================================
   Edge Type Color Palette
   ============================================================================
   Up edges use the "Aquamarine" ColorData scheme (cool blue-greens).
   Down edges use the "SolarColors" ColorData scheme (warm oranges-reds).
   Each named EdgeType maps to a fixed position t \[Element] [0,1] in its scheme:

   EdgeType    Direction  FromField  ToField       t      Description
   \[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]
   "MaxU"     U          Low        High         0.85   Primary up move
   "UpInner"   U          Low        Open/Close   0.65   Secondary up
   "Support"   U          Low        Low          0.45   Low->Low (support)
   "InnerUp"   U          Open/Close any          0.30   Open/Close origin up
   "OuterUp"   U          High       any          0.15   High origin on up seg
   "MaxD"   D          High       Low          0.85   Primary down move
   "DownInner" D          High       Open/Close   0.65   Secondary down
   "Resistance"D          High       High         0.45   High->High (resistance)
   "InnerDown" D          Open/Close any          0.30   Open/Close origin down
   "OuterDown" D          Low        any          0.15   Low origin on down seg
   ============================================================================ *)
edgeTypeColor = <|
  "MaxU"      -> ColorData["Aquamarine"][0.85],
  "UpInner"    -> ColorData["Aquamarine"][0.65],
  "Support"    -> ColorData["Aquamarine"][0.45],
  "InnerUp"    -> ColorData["Aquamarine"][0.30],
  "OuterUp"    -> ColorData["Aquamarine"][0.15],
  "MaxD"    -> ColorData["SolarColors"][0.85],
  "DownInner"  -> ColorData["SolarColors"][0.65],
  "Resistance" -> ColorData["SolarColors"][0.45],
  "InnerDown"  -> ColorData["SolarColors"][0.30],
  "OuterDown"  -> ColorData["SolarColors"][0.15]
|>;

(* ============================================================================
   Group 2: Overlay Helpers
   ============================================================================ *)

$overlays = <||>;

(* ============================================================================
   Group 3: Options declarations (must be public)
   ============================================================================ *)

Options[makeSegment] = {
  "color"     -> Black,
  "thickness" -> 0.002
};

Options[drawLine] = {
  "thickness"   -> 0.002,
  "upColor"     -> Darker[Cyan, .75],
  "downColor"   -> Darker[Pink, .75],
  "dashedColor" -> Gray
};

Options[makeRectangle] = {
  "upColor"      -> Darker[Cyan, .75],
  "downColor"    -> Darker[Pink, .75],
  "thickness"    -> 0.002,
  "lineOpacity"  -> 0.25,
  "fillLines"    -> (* fibonacciRatios *){(*-0.236,*) 0., 0.236, 0.382, 0.500, 0.618, 0.786, 1., 1.272} ,
  "extraOnLeft"  -> 16,
  "extraOnRight" -> 1
};

Options[makeRectangleFromSegment] = Options[makeRectangle];

Options[drawRectangles] = Options[makeRectangle];

Options[makeGraphDisplay] = {
  GlyphWidth      -> 0.35,
  ImageSize       -> {800, 400},
  XAxis           -> "Both",   (* "Both": dates below, candlestick indices above; or "Date" or "Index" (below only) *)
  EdgeThickness   -> 0.0005,
  Lines           -> {},
  Background      -> Black,
  UpColor         -> RGBColor[0.0,  0.85, 0.0],
  DownColor       -> RGBColor[0.85, 0.0,  0.0],
  ArrowSize       -> 0.005,
  ShowPriceSlider -> False,
  ShowGraphEx     -> False,
  ShowExtensions  -> False,
  ScalingFunctions -> {"Linear", "Linear"},
  ShowScrollbar   -> False,
  "TouchTolerance"  -> (1./100.),
  "TouchMarkerSize" -> 6,
  "ExtraOnLeft"     -> 0,
  "ExtraOnRight"    -> 1
};

Options[displayCandlestickGraph] = Options[makeGraphDisplay];

plotCandlesticks::usage = "
plotCandlesticks[candlesticksArg, plotCommands, chartType]

DESCRIPTION:
  Displays a candlestick chart using Mathematica's TradingChart or
  InteractiveTradingChart from a Tabular of candlestick data.

ARGUMENTS:
  candlesticksArg  Tabular   Candlestick data with columns Date, Open,
                             High, Low, Close, Volume.

  plotCommands     List      (Optional) List of graphics primitives to
                             overlay on the chart via Prolog.
                             Default: {}

  chartType        String    (Optional) Chart type selector.
                             \"T\" -> TradingChart (default)
                             \"I\" -> InteractiveTradingChart

OPTIONS:
  \"ImageSize\"        -> Medium     Size of the chart image
  \"DateSlider\"       -> Automatic  Date range slider behavior
  \"ScalingTransform\" -> {\"Reverse\", \"Automatic\"}

RETURNS:
  A TradingChart or InteractiveTradingChart graphic.

EXAMPLES:
  (* Basic chart *)
  plotCandlesticks[candlesticks]

  (* Interactive chart with custom size *)
  plotCandlesticks[candlesticks, {}, \"I\", \"ImageSize\" -> Large]

  (* With graphics overlay *)
  plotCandlesticks[candlesticks, {Red, Line[{{1,100},{5,110}}]}, \"T\"]

NOTES:
  - DateObject granularity is normalized to Day symbol for TradingChart
    compatibility in Mathematica 14.3.
  - Volume is included in the OHLCV sublist passed to TradingChart.
";

Options[plotCandlesticks] = {
  "ImageSize"        -> Medium,
  "DateSlider"       -> Automatic,
  "ScalingTransform" -> {"Reverse", "Automatic"}
};

regimeBar::usage =
  "regimeBar[bearP, sidewaysP, bullP, barWidth] draws a horizontal \
3-color rectangle showing Bear/Sideways/Bull regime probabilities. \
Red=Bear, White=Sideways, Green=Bull. barWidth defaults to 200 pixels.";

showMarkovMatrix::usage =
  "showMarkovMatrix is a Boolean flag controlling whether the Markov \
transition matrix overlay is displayed on the chart. Default False.";

regimeBarFromRow::usage =
  "regimeBarFromRow[rowArg] returns a list of colored Rectangle Graphics \
primitives representing the Bear/Sideways/Bull regime proportions stored \
in rowArg. The proportions are read directly from the RegimeBear, \
RegimeSideways, and RegimeBull columns. Returns {} if rowArg is None or \
missing regime columns.";

markovMatrixFromRow::usage =
  "markovMatrixFromRow[rowArg] returns the 3x3 Markov transition matrix \
as a nested list, reading RegimeM11 through RegimeM33 directly from rowArg. \
Returns a uniform 1/3 matrix if rowArg is None or missing regime columns.";

markovMatrixDisplay::usage =
  "markovMatrixDisplay[pArg] renders a 3x3 Markov transition matrix as a \
color-coded Grid, blending from dark red (0%) through yellow (50%) to dark \
green (100%). Row and column order is Bear, Sideways, Bull. Each cell shows \
the transition probability as a 2-decimal number.";

regimeBarDisplay::usage =
  "regimeBarDisplay[rowArg] renders a colored regime bar with Bear/Side/Bull \
proportions and numeric readout from a candlestick row Association.";



displaySegmentPicker::usage =
  "displaySegmentPicker[desiredSegmentsArg_Association] returns a Dataset " <>
  "with a Show button in each row. Clicking Show sets " <>
  "StockAnalyzer`Visualization`lastSegmentClicked to that segment, which " <>
  "the chart's reactive watcher picks up and displays.";

Clear[displaySegmentPicker];
displaySegmentPicker[desiredSegmentsArg_Association] := Module[{rows},
  rows = KeyValueMap[
    Function[{key, seg},
      <|
        "Show"      -> Button["Show",
                         StockAnalyzer`Visualization`lastSegmentClicked = seg,
                         Appearance -> "Palette",
                         ImageSize -> {55, 22}],
        "From"      -> seg["FromIndex"],
        "To"        -> seg["ToIndex"],
        "Dir"       -> seg["Direction"],
        "EdgeType"  -> seg["EdgeType"],
        "FromField" -> seg["FromField"],
        "ToField"   -> seg["ToField"],
        "DeltaX"    -> seg["DeltaX"],
        "FromPrice" -> NumberForm[N[seg["FromPrice"]], {7, 2}],
        "ToPrice"   -> NumberForm[N[seg["ToPrice"]], {7, 2}],
        "Range"     -> NumberForm[N[seg["Range"]], {7, 2}]
      |>
    ],
    desiredSegmentsArg
  ];
  Dataset[rows]
];

(* \[HorizontalLine]\[HorizontalLine] animateFibonacci \[HorizontalLine]\[HorizontalLine] *)
animateFibonacci[segmentArg_Association,
                 fibRatiosArg_List : fibonacciRatios (*{0.236, 0.382, 0.500, 0.618,
                                      0.786, 1., 1.272, 1.618} *),
                 toleranceArg_Real : (1./100.),
                 chartKeyArg_String : "default"] :=
  Module[{fromIdx, toIdx, fromPrice, toPrice, range, nRatios, colors, lightColors, absoluteTolerance},
    fromIdx     = segmentArg["FromIndex"];
    toIdx       = segmentArg["ToIndex"];
    fromPrice   = segmentArg["FromPrice"];
    toPrice     = segmentArg["ToPrice"];
    range       = segmentArg["Range"];
    nRatios     = Length[fibRatiosArg];
    colors      = ColorData["Rainbow"] /@ Rescale[Range[nRatios], {1, nRatios}];
    lightColors = Map[Lighter[#, 0.4] &, colors];
    Manipulate[
      Module[{vPrice, range},
        vPrice = fromPrice + t * (toPrice - fromPrice);
        range  = vPrice - fromPrice;
        absoluteTolerance = range * toleranceArg;
        $overlays[chartKeyArg] = Join[
          (* Fibonacci bands, center lines and price labels *)
          Flatten[MapThread[
            Function[{ratio, color, lightColor},
              Module[{fibPrice, interval, bounds},
                fibPrice = fromPrice + ratio * range;
                interval = CenteredInterval[fibPrice, absoluteTolerance];
                bounds = Information[interval,"Bounds"];
                {
                  (* Tolerance band *)
                  {lightColor, Opacity[0.15],
                   Rectangle[{Max[0.5, fromIdx - 8.5], bounds[[1]]},
                              {toIdx + 8.5, bounds[[2]]}]},
                  (* Center line *)
                  {color, Thick,
                   Line[{{Max[0.5, fromIdx - 8.5], fibPrice},
                         {toIdx + 8.5, fibPrice}}]},
                  (* Price label *)
                  Text[
                    Style[ToString[NumberForm[fibPrice, {6, 2}]],
                          color, Bold, 10],
                    {toIdx + 8.6, fibPrice},
                    {-1, 0}
                  ]
                }
              ]
            ],
            {fibRatiosArg, colors, lightColors}
          ], 1],
          (* Rectangle *)
          {{Cyan, Opacity[0.15],
            Rectangle[{fromIdx - 0.5, fromPrice}, {toIdx + 0.5, vPrice}]}},
          (* Scan line *)
          {{White, Thick,
            Line[{{fromIdx - 0.5, vPrice}, {toIdx + 8.5, vPrice}}]}},
          (* Scan line price label *)
          {Text[
            Style[ToString[NumberForm[vPrice, {6, 2}]],
                  White, Bold, 10],
            {toIdx + 8.6, vPrice},
            {-1, 0}
          ]}
        ]
      ];
      "scanning...",
      {t, First[fibRatiosArg], Last[fibRatiosArg], 0.02,
       (* ControlType -> VerticalSlider, *)
       Appearance    -> "Labeled",
       AnimationRate -> 5},
      ControlPlacement -> Right,
      Initialization :> (t = 1.0)
    ]
  ]; 



		(* \[HorizontalLine]\[HorizontalLine] regimeBar \[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]
		   Draws a horizontal 3-color rectangle showing Bear/Sideways/Bull
		   regime probabilities. Red=Bear, White=Sideways, Green=Bull.
		   Total width is barWidth pixels.
		   \[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]\[HorizontalLine]*)
		   
Clear[regimeBar];		   
regimeBar[bearP_, sidewaysP_, bullP_, barWidth_: 200] :=
  Framed[
    Row[{
      Framed[
        Spacer[{bearP * barWidth, 8}],
        Background -> RGBColor[0.9, 0.1, 0.1],
        FrameStyle -> None,
        ContentPadding -> False,
        FrameMargins -> 0
      ],
      Framed[
        Spacer[{sidewaysP * barWidth, 8}],
        Background -> GrayLevel[0.75],
        FrameStyle -> None,
        ContentPadding -> False,
        FrameMargins -> 0
      ],
      Framed[
        Spacer[{bullP * barWidth, 8}],
        Background -> RGBColor[0.1, 0.8, 0.2],
        FrameStyle -> None,
        ContentPadding -> False,
        FrameMargins -> 0
      ]
    }, Spacer[0]],
    FrameStyle -> Directive[
      RGBColor[
        0.9*bearP + 0.75*sidewaysP + 0.1*bullP,
        0.1*bearP + 0.75*sidewaysP + 0.8*bullP,
        0.1*bearP + 0.75*sidewaysP + 0.2*bullP
      ],
      AbsoluteThickness[3]
    ],
    ContentPadding -> False,
    FrameMargins -> {{3, 3}, {3, 2}},
    RoundingRadius -> 3
  ]


addGraphChart::usage =
  "addGraphChart[featureSet, opts] returns the featureSet (a Dataset) with the key \"GraphChart\": the graph of the last \"Window\" (250) candlesticks drawn as candlestick glyphs with the edges at the prices of the moves, the same picture as candlestickVertexGraph. FindIsomorphicSubgraph and HighlightGraph work on it and draw in the same style. Only the candles inside the window are in it; use the plain featureSet[\"Graph\"] to search the whole history. Options: \"Window\" (250; All for every candle), and the display options of candlestickVertexGraph \"GlyphWidth\" (0.35), \"VertexShapeFunction\" (\"Glyph\"), \"ShapeSize\" (0.2), \"EdgeTypes\" (All), \"ArrowSize\" (0.005), \"EdgeThickness\" (0.002), \"RealEdges\" (True), \"OwnEdges\" (Automatic), \"ImageSize\" (800), \"AspectRatio\" (0.5). The key is not stored in the cache files.";

highlightKeepColors::usage =
  "highlightKeepColors[graph, subgraph] (or a list of subgraphs, for example the result of FindIsomorphicSubgraph[graph, pattern, All]) shows graph with all its edges light gray and the edges of the subgraph(s) in their own colours (the EdgeStyle of graph, for example lime up and red down in featureSet[\"Graph\"]) and wider. " <>
  "highlightKeepColors[graph, subgraphs, width, gray, ringColors]: width in points (3), the gray of the other edges (GrayLevel[0.75]), and the vertex colours of the subgraphs: one subgraph has light blue vertices, several have a colour for each (yellow, magenta, cyan, orange, then again). " <>
  "For a chart (featureSet[\"GraphChart\"]) the picture is the stock chart with the edges hidden except those of the subgraph(s), in their own colours and wider.";

Begin["`Private`"]

(* ============================================================================
   Group 2: Overlay Helper Definitions
   ============================================================================ *)

clearOverlay[key_String] := AppendTo[$overlays, key -> {}];

clearOverlays[] := ($overlays = <||>);



(* ============================================================================
   Group 3: Line & Geometry
   ============================================================================ *)

(* Returns raw Graphics primitives for a direct point-to-point line segment.
   Unlike drawLine, no line equation is computed. *)
Clear[makeSegment];

makeSegment[{x1_, y1_}, {x2_, y2_}, OptionsPattern[]] := {
  OptionValue["color"],
  Thickness[OptionValue["thickness"]],
  Line[{N[{x1, y1}], N[{x2, y2}]}]
};

(* Given 2 points, returns a pure function f[x] giving y on the line
   through those points. Uses the two-point form of a line equation. *)
Clear[makeLineFunction];

makeLineFunction[{x1_, y1_}, {x2_, y2_}] :=
  Function[x, y1 + (y2 - y1) / (x2 - x1) * (x - x1)];

(* Constructs a parametric line Association from slope-intercept form. *)
Clear[makeParametricLine];

makeParametricLine[slope_?NumericQ, intercept_?NumericQ] := 
  With[{s = slope, i = intercept},
    <|
      "slope"     -> slope,
      "intercept" -> intercept,
      "f"         -> Function[x, s x + i],
      "normal"    -> Normalize[{-slope, 1}],
      "vertical"  -> False
    |>
  ];

makeParametricLine[{xi_, yi_}, {xj_, yj_}] := 
  Module[{slope, intercept},
    If[xi == xj,
      <|
        "slope"     -> ComplexInfinity,
        "intercept" -> Indeterminate,
        "x"         -> xi,
        "f"         -> Function[x, Indeterminate],
        "normal"    -> {1, 0},
        "vertical"  -> True
      |>
      ,
      slope     = (yj - yi) / (xj - xi);
      intercept = yi - slope xi;
      With[{s = slope, i = intercept},
        <|
          "slope"     -> slope,
          "intercept" -> intercept,
          "f"         -> Function[x, s x + i],
          "normal"    -> Normalize[{-slope, 1}],
          "vertical"  -> False
        |>
      ]
    ]
  ];
  
(* Returns the line perpendicular to line intersecting at x = xInt. *)
perpendicularLine[line_, xInt_] := Module[
  {x0, y0, slopePerp},
  x0 = xInt;
  y0 = line["f"][x0];
  (* Horizontal line -> perpendicular is vertical *)
  If[line["slope"] == 0,
    makeParametricLine[{x0, y0}, {x0, y0 + 1}],
    slopePerp = -1 / line["slope"];
    makeParametricLine[slopePerp, y0 - slopePerp * x0]
  ]
];

(* Returns the y value of line at x. *)
Clear[lineY, linePoint];

lineY[line_, x_] := line["f"][x];

(* Returns the point {x, y} on line at x. *)
linePoint[line_, x_] := {x, line["f"][x]};

(* Returns the perpendicular distance from point pt to line. *)
Clear[linePointDist];

linePointDist[line_, pt_] := If[line["vertical"],
  Abs[pt[[1]] - line["x"]],
  Abs[line["slope"] pt[[1]] - pt[[2]] + line["intercept"]] /
    Sqrt[line["slope"]^2 + 1]
];

(* Returns a pure function r[t] = p0 + t * unit_direction on line. *)
Clear[lineParametric];

lineParametric[line_] := Module[
  {p0 = {0, line["intercept"]},
   dir = Normalize[{1, line["slope"]}]},
  Function[t, p0 + t dir]
];

(* Returns a human-readable slope-intercept string for line. *)
lineString[line_] :=
  StringJoin["Y = ", ToString[line["slope"]], "X + ", ToString[line["intercept"]]];

(* Returns the signed angle in radians from line1 to line2.
   Positive angle is counterclockwise from line1 to line2.
   Returns Sign[numer]*Pi/2 when lines are perpendicular (denom = 0). *)
lineAngle[line1_, line2_] := Module[
  {m1 = line1["slope"], m2 = line2["slope"], denom, numer},
  numer = m2 - m1;
  denom = 1 + m1 * m2;
  If[denom == 0,
    Sign[numer] * Pi / 2,
    ArcTan[numer / denom]
  ]
];

(* Draws line as a solid segment from xstart to xend, then dashed from
   xend to xext. Force numeric evaluation at construction time so no
   unevaluated expressions survive into DynamicModule Graphics primitives. *)
Clear[drawLine];

drawLine[line_, {xstart_, xend_, xext_}, opts : OptionsPattern[]] := Module[
  {p1, p2, p3},
  p1 = N[{xstart, line["f"][xstart]}];
  p2 = N[{xend,   line["f"][xend]}];
  p3 = N[{xext,   line["f"][xext]}];
  {
    OptionValue["upColor"],
    Thickness[OptionValue["thickness"]],
    Line[{p1, p2}],
    OptionValue["dashedColor"],
    Dashed,
    Line[{p2, p3}]
  }
];

(* ============================================================================
   Group 4: Rectangle Overlays
   ============================================================================ *)

(* fillLines is a list of percentages in [0,1] specifying where horizontal
   fill lines are drawn relative to the rectangle height.
   0% = y1 (first point), 100% = y2 (last point), regardless of direction.
   Percentages outside [0,1] are allowed and extrapolate beyond the rectangle.
   extraOnLeft extends fill lines to the left of x1 by that many indices.
   extraOnRight extends fill lines to the right of x2 by that many indices.
   Border segments always span exactly x1 to x2. *)
Clear[makeRectangle];

makeRectangle[{x1_, y1_}, {x2_, y2_}, OptionsPattern[]] := Module[
  {color, fillColor, fillPrimitives, yAt, lineX1, lineX2},
  color     = If[N[y2] >= N[y1], OptionValue["upColor"], OptionValue["downColor"]];
  fillColor = Directive[Opacity[OptionValue["lineOpacity"]], color];
  (* 0% = y1, 100% = y2; extrapolates naturally for pct outside [0,1] *)
  yAt    = Function[pct, N[y1 + pct * (y2 - y1)]];
  (* Fill lines extend beyond the rectangle by extraOnLeft and extraOnRight *)
  lineX1 = Max[1,N[x1 - OptionValue["extraOnLeft"]]];
  lineX2 = N[x2 + OptionValue["extraOnRight"]];
  (* Generate one horizontal line per percentage in fillLines *)
  fillPrimitives = Map[
    {fillColor, Thickness[OptionValue["thickness"]],
     Line[{{lineX1, yAt[#]}, {lineX2, yAt[#]}}]
    } &,
    OptionValue["fillLines"]
  ];
  {
    fillPrimitives,
    (* Border segments: thicker than fill lines *)
    makeSegment[{x1, y1}, {x2, y1}, "color" -> color, "thickness" -> OptionValue["thickness"] * 2.5],
    makeSegment[{x2, y1}, {x2, y2}, "color" -> color, "thickness" -> OptionValue["thickness"] * 2.5],
    makeSegment[{x2, y2}, {x1, y2}, "color" -> color, "thickness" -> OptionValue["thickness"] * 2.5],
    makeSegment[{x1, y2}, {x1, y1}, "color" -> color, "thickness" -> OptionValue["thickness"] * 2.5]
  }
];

Clear[makeRectangleFromSegment];

makeRectangleFromSegment[segmentArg_, opts : OptionsPattern[]] :=
  makeRectangle[
    {segmentArg["FromIndex"], segmentArg["FromPrice"]},
    {segmentArg["ToIndex"],   segmentArg["ToPrice"]},
    opts
  ];

Clear[drawRectangles];

drawRectangles[rects_List, opts : OptionsPattern[]] :=
  Map[makeRectangle[#[[1]], #[[2]], opts] &, rects];

(* ============================================================================
   Group 5: Chart Primitives
   ============================================================================ *)

(* Returns a tick-generating function that labels each index position with
   its formatted date string, rotated -45 degrees. *)
dateTickFunction[candlesticksArg_Tabular] :=
  Module[{cData, indexToDate},
    cData       = Normal[candlesticksArg];
    indexToDate = Association[#["Index"] -> #["Date"] & /@ cData];
    Function[{min, max},
      Table[
        If[KeyExistsQ[indexToDate, i],
          {i,
           Rotate[
             Style[
               DateString[indexToDate[i],
                 {"MonthNameShort", " ", "Day", ", ", "Year"}],
               FontSize -> 8, FontFamily -> "Arial"
             ],
             -45 Degree
           ],
           {0.01, 0},
           {}
          },
          Nothing
        ],
        {i, Round[min], Round[max]}
      ]
    ]
  ];

(* Formats a single candlestick row Association as a horizontal Row
   expression matching TradingChart's top info bar style. *)
infoBarText[row_] := Module[{direction, labelColor, valueColor,
                              regimeData, historical, today, forecast},
  direction  = If[row["Close"] >= row["Open"], "U", "D"];
  labelColor = GrayLevel[0.55];
  valueColor = If[direction === "U", bullishCandlestickColor, bearishCandlestickColor];

  (* Look up pre-computed regime data for this candlestick index *)
	regimeData = If[
	  AssociationQ[Global`$regimeHistory] &&
	  KeyExistsQ[Global`$regimeHistory, row["Index"]],
	  Global`$regimeHistory[row["Index"]],
	  None
	];

  Row[{
    Style[
      DateString[row["Date"], {"DayNameShort", " ", "MonthNameShort", " ",
                               "Day", ", ", "Year"}] <> "   ",
      Bold, 11, FontFamily -> "Arial", GrayLevel[0.75]
    ],
    Style["Idx: ", 11, labelColor],
    Style[ToString[row["Index"]] <> "   ", 11, FontFamily -> "Arial", GrayLevel[0.75]],
    Style["O: ", 11, labelColor],
    Style[ToString[NumberForm[row["Open"],  {6, 2}]] <> "   ", 11, FontFamily -> "Arial", valueColor],
    Style["H: ", 11, labelColor],
    Style[ToString[NumberForm[row["High"],  {6, 2}]] <> "   ", 11, FontFamily -> "Arial", valueColor],
    Style["L: ", 11, labelColor],
    Style[ToString[NumberForm[row["Low"],   {6, 2}]] <> "   ", 11, FontFamily -> "Arial", valueColor],
    Style["C: ", 11, labelColor],
    Style[ToString[NumberForm[row["Close"], {6, 2}]] <> "   ", 11, FontFamily -> "Arial", valueColor],
    Style["V: ", 11, labelColor],
    Style[ToString[row["Volume"]] <> "   ", 11, FontFamily -> "Arial", GrayLevel[0.65]],

    (* \[HorizontalLine]\[HorizontalLine] Regime bars \[HorizontalLine]\[HorizontalLine] *)
    If[regimeData =!= None,
      Row[{
        Spacer[8],
        (* Historical regime bar *)
        Column[{
          Style["Hist", 8, GrayLevel[0.55]],
          regimeBar[
            regimeData["HistoricalBear"],
            regimeData["HistoricalSideways"],
            regimeData["HistoricalBull"]
          ]
        }],
        Spacer[4],
        (* Today's regime bar *)
        Column[{
          Style["Now", 8, GrayLevel[0.55]],
          regimeBar[
            regimeData["TodayBear"],
            regimeData["TodaySideways"],
            regimeData["TodayBull"]
          ]
        }],
        Spacer[4],
        (* 5-day forecast bar *)
        Column[{
          Style["5d", 8, GrayLevel[0.55]],
          regimeBar[
            regimeData["Forecast5Bear"],
            regimeData["Forecast5Sideways"],
            regimeData["Forecast5Bull"]
          ]
        }]
      }],
      (* No regime data available *)
      Spacer[0]
    ]
  }]
];

infoBarText[None] := "";

(* Returns a Framed tooltip widget displaying the key fields of a segment
   Association. Used as hover tooltip content on directed edge arrows. *)
segmentTooltipContent[seg_] := Module[{letterColor},
  letterColor = Black;
  Framed[
    Column[{
      Style[
        StringForm["Edge  `1`  ->  `2`", seg["FromIndex"], seg["ToIndex"]],
        Bold, 11, FontFamily -> "Arial", Black
      ],
      Grid[{
        {"Direction:",  Style[seg["Direction"],           10]},
        {"From Price:", Style[ToString[seg["FromPrice"]], 10]},
        {"To Price:",   Style[ToString[seg["ToPrice"]],   10]},
        {"Range:",      Style[ToString[seg["Range"]],     10]},
        {"LogRatio:",   Style[ToString[seg["LogRatio"]],  10]},
        {"Angle:",      Style[ToString[seg["Angle"]],     10]},
        {"Weight:",     Style[ToString[seg["Weight"]],    10]}
      },
        Alignment -> {{Right, Left}, Automatic},
        Spacings  -> {0.8, 0.3}
      ]
    }, Spacings -> 0.5],
    Background     -> Gray,
    RoundingRadius -> 4,
    FrameStyle     -> Directive[LightGray, Thin],
    FrameMargins   -> 6
  ]
];

(* Returns Graphics primitives for a single OHLC candlestick body and wick
   centered at x = cx. Color is bullish or bearish based on close vs open. *)
candlestickGlyph[cx_, open_, high_, low_, close_, halfWidth_ : 0.35] :=
  Module[{isBullish, bodyColor, wickColor, bodyBottom, bodyTop},
    isBullish  = close >= open;
    bodyBottom = Min[open, close];
    bodyTop    = Max[open, close];
    wickColor  = GrayLevel[0.75];
    If[isBullish,
      (* Bullish: hollow body with green outline, light gray wick *)
      {
        wickColor, Thickness[0.001],
        Line[{{cx, low},  {cx, bodyBottom}}],
        Line[{{cx, bodyTop}, {cx, high}}],
        EdgeForm[Directive[bullishCandlestickColor, Thickness[0.002]]],
        FaceForm[Transparent],
        Rectangle[{cx - halfWidth, bodyBottom}, {cx + halfWidth, bodyTop}]
      },
      (* Bearish: hollow body with red outline, light gray wick *)
      {
        wickColor, Thickness[0.001],
        Line[{{cx, low},  {cx, bodyBottom}}],
        Line[{{cx, bodyTop}, {cx, high}}],
        EdgeForm[Directive[bearishCandlestickColor, Thickness[0.002]]],
        FaceForm[Transparent],
        Rectangle[{cx - halfWidth, bodyBottom}, {cx + halfWidth, bodyTop}]
      }
    ]
  ];
  
(* Returns a tick-generating function that labels each candlestick position
   with its integer index value. *)
indexTickFunction[candlesticksArg_Tabular] :=
  Module[{cData},
    cData = Normal[candlesticksArg];
    Function[{min, max},
      Table[
        {i, Style[ToString[i], FontSize -> 9, FontFamily -> "Arial"], {0.01, 0}, {}},
        {i, Round[min], Round[max]}
      ]
    ]
  ];

(* ============================================================================
   Group 6: High-Level Display
   ============================================================================ *)

segmentEdgeTooltip[seg_] :=
  Framed[
    Column[{
      Style[
        StringForm["`1`  \[Rule]  `2`  (`3`)",
          seg["FromIndex"], seg["ToIndex"], seg["Direction"]],
        Bold, 11, FontFamily -> "Arial", White
      ],
      Grid[{
        {"From:",      Style[ToString[seg["FromIndex"]], 10, White]},
        {"To:",        Style[ToString[seg["ToIndex"]],   10, White]},
        {"FromField:", Style[seg["FromField"],            10, White]},
        {"ToField:",   Style[seg["ToField"],              10, White]},
        {"EdgeType:",  Style[seg["EdgeType"],             10, White]},
        {"Weight:",    Style[ToString[NumberForm[seg["Weight"], {6,4}]], 10, White]}
      },
        Alignment -> {{Right, Left}, Automatic},
        Spacings  -> {0.8, 0.3}
      ]
    }, Spacings -> 0.5],
    Background     -> GrayLevel[0.2],
    RoundingRadius -> 4,
    FrameStyle     -> Directive[GrayLevel[0.35], Thin],
    FrameMargins   -> 6
  ];

Options[drawGraphExEdges] = {
  UpColor       -> bullishColor,
  DownColor     -> bearishColor,
  ArrowSize     -> 0.005,
  EdgeThickness -> 0.0005,
  ShowExtensions -> False
};

(* Returns the display color for a segment Association based on its named
   EdgeType. Shared by drawGraphExEdges and rectangle rendering. *)
segmentEdgeColor[seg_] :=
  Lookup[edgeTypeColor, seg["EdgeType"], GrayLevel[0.5]];

drawGraphExEdges[featureSetArg_, nMax_, opts : OptionsPattern[]] :=
  Module[{
    edgeSegs, allKeys,
    upColor, downColor, arrowSize, edgeThickness, showExt
    },
    upColor       = OptionValue[UpColor];
    downColor     = OptionValue[DownColor];
    arrowSize     = OptionValue[ArrowSize];
    edgeThickness = OptionValue[EdgeThickness];
    showExt       = TrueQ[OptionValue[ShowExtensions]];
    edgeSegs = Which[
      AssociationQ[featureSetArg["EdgeSegments"]],
        featureSetArg["EdgeSegments"],
      True,
        Association[Normal[featureSetArg["EdgeSegments"]]]
    ];
    allKeys = Keys[edgeSegs];
    Map[
      Function[key,
        Module[{seg, fx, fy, tx, ty, color, dx, dy, slope,
                extX, extY, edgeType, thickness, opacity},
          seg       = edgeSegs[key];
          fx        = seg["FromIndex"];
          fy        = seg["FromPrice"];
          tx        = seg["ToIndex"];
          ty        = seg["ToPrice"];
          edgeType  = seg["EdgeType"];
          color     = Lookup[edgeTypeColor, edgeType,
                        If[seg["Direction"] === "U", upColor, downColor]];
          dx        = tx - fx;
          dy        = ty - fy;
          (* StrictRanges -> False also makes one-candle segments (from and to the same candle, dx = 0):
             a vertical edge has no slope, so it gets no extension (the dotted line has length 0) *)
          slope     = If[dx == 0, 0, dy / dx];
          extX      = If[dx == 0, tx, nMax + 0.5];
          extY      = If[dx == 0, ty, ty + slope * (extX - tx)];
          (* Primary edges (MaxU/MaxD) are thicker and fully opaque *)
          thickness = If[edgeType === "MaxU" || edgeType === "MaxD",
                        edgeThickness, edgeThickness * 0.75];
          opacity   = If[edgeType === "MaxU" || edgeType === "MaxD",
                        1.0, 0.75];
          With[{
            k      = ToString[key],
            fa     = {fx, fy},
            ta     = {tx, ty},
            ex     = {extX, extY},
            c      = color,
            th     = thickness,
            op     = opacity,
            as     = arrowSize,
            se     = showExt,
            toi    = tx,
            et     = edgeType,
            segDir = seg["Direction"],
            segFF  = seg["FromField"],
            segTF  = seg["ToField"],
            segVal = seg
            },
            {
              (* Arrow + dotted extension: visible when showAllEdges AND type enabled,
                 pinned, or destination candlestick is hovered AND type enabled *)
              Dynamic[
                If[
                  (* EdgeType button OR all three fine-grained filters active and matching *)
                  (MemberQ[StockAnalyzer`Visualization`showEdgeTypes, et] ||
                   (StockAnalyzer`Visualization`showDirections =!= {} &&
                    StockAnalyzer`Visualization`showFromFields =!= {} &&
                    StockAnalyzer`Visualization`showToFields   =!= {} &&
                    MemberQ[StockAnalyzer`Visualization`showDirections, segDir] &&
                    MemberQ[StockAnalyzer`Visualization`showFromFields, segFF] &&
                    MemberQ[StockAnalyzer`Visualization`showToFields,   segTF])) &&
                  (StockAnalyzer`Visualization`showAllEdges ||
                   KeyExistsQ[StockAnalyzer`Visualization`hoveredSegs, k] ||
                   hoveredVertex === toi) &&
                  (StockAnalyzer`Visualization`showDirections === {} ||
                   MemberQ[StockAnalyzer`Visualization`showDirections, segDir]) &&
                  (StockAnalyzer`Visualization`showFromFields === {} ||
                   MemberQ[StockAnalyzer`Visualization`showFromFields, segFF]) &&
                  (StockAnalyzer`Visualization`showToFields === {} ||
                   MemberQ[StockAnalyzer`Visualization`showToFields, segTF]),
                  {
                    Opacity[op], c, Thickness[th],
                    Arrowheads[{{as, 1}}],
                    Arrow[{fa, ta}],
                    (* Dotted extension to the right edge, only if ShowExtensions *)
                    If[se,
                      {Opacity[1.0], c, Thickness[th * 1.5],
                       Dashing[Tiny],
                       Line[{ta, ex}]},
                      {}]
                  },
                  {}
                ],
                TrackedSymbols :> {
                  StockAnalyzer`Visualization`showAllEdges,
                  StockAnalyzer`Visualization`showEdgeTypes,
                  StockAnalyzer`Visualization`showDirections,
                  StockAnalyzer`Visualization`showFromFields,
                  StockAnalyzer`Visualization`showToFields,
                  StockAnalyzer`Visualization`hoveredSegs,
                  hoveredVertex
                }
              ],
              (* Hit polygon + tooltip: only active when edge is visible *)
              Dynamic[
                If[
                  (* EdgeType button OR all three fine-grained filters active and matching *)
                  (MemberQ[StockAnalyzer`Visualization`showEdgeTypes, et] ||
                   (StockAnalyzer`Visualization`showDirections =!= {} &&
                    StockAnalyzer`Visualization`showFromFields =!= {} &&
                    StockAnalyzer`Visualization`showToFields   =!= {} &&
                    MemberQ[StockAnalyzer`Visualization`showDirections, segDir] &&
                    MemberQ[StockAnalyzer`Visualization`showFromFields, segFF] &&
                    MemberQ[StockAnalyzer`Visualization`showToFields,   segTF])) &&
                  (StockAnalyzer`Visualization`showAllEdges ||
                   KeyExistsQ[StockAnalyzer`Visualization`hoveredSegs, k] ||
                   hoveredVertex === toi) &&
                  (StockAnalyzer`Visualization`showDirections === {} ||
                   MemberQ[StockAnalyzer`Visualization`showDirections, segDir]) &&
                  (StockAnalyzer`Visualization`showFromFields === {} ||
                   MemberQ[StockAnalyzer`Visualization`showFromFields, segFF]) &&
                  (StockAnalyzer`Visualization`showToFields === {} ||
                   MemberQ[StockAnalyzer`Visualization`showToFields, segTF]),
                  (* Thin polygon tightly hugging the arrow for precise hit detection *)
                  Module[{dx, dy, len, nx, ny, hw},
                    dx  = ta[[1]] - fa[[1]];
                    dy  = ta[[2]] - fa[[2]];
                    len = Sqrt[dx^2 + dy^2];
                    nx  = -dy / len;
                    ny  =  dx / len;
                    hw  = len * 0.015; (* 1.5% of arrow length - tight hit area *)
                    Module[{hitHandler},
                      hitHandler = EventHandler[
                        {Opacity[0], White,
                          Polygon[{
                            {fa[[1]] + nx * hw, fa[[2]] + ny * hw},
                            {ta[[1]] + nx * hw, ta[[2]] + ny * hw},
                            {ta[[1]] - nx * hw, ta[[2]] - ny * hw},
                            {fa[[1]] - nx * hw, fa[[2]] - ny * hw}
                          }]
                        },
                        {
                          "MouseClicked" :> (
                            If[KeyExistsQ[StockAnalyzer`Visualization`hoveredSegs, k],
                              StockAnalyzer`Visualization`hoveredSegs =
                                KeyDrop[StockAnalyzer`Visualization`hoveredSegs, k],
                              StockAnalyzer`Visualization`hoveredSegs =
                                Append[StockAnalyzer`Visualization`hoveredSegs, k -> segVal]
                            ];
                            StockAnalyzer`Visualization`lastSegmentClicked = segVal
                          )
                        },
                        PassEventsDown -> True
                      ];
                      If[StockAnalyzer`Visualization`showEdgeTooltip,
                        Tooltip[hitHandler, segmentEdgeTooltip[segVal]],
                        hitHandler
                      ]
                    ]
                  ],
                  {} (* not visible - no hit area *)
                ],
                TrackedSymbols :> {
                  StockAnalyzer`Visualization`showAllEdges,
                  StockAnalyzer`Visualization`showEdgeTypes,
                  StockAnalyzer`Visualization`showEdgeTooltip,
                  StockAnalyzer`Visualization`showDirections,
                  StockAnalyzer`Visualization`showFromFields,
                  StockAnalyzer`Visualization`showToFields,
                  StockAnalyzer`Visualization`hoveredSegs,
                  hoveredVertex
                }
              ]
            }
          ]
        ]
      ],
      allKeys
    ]
  ];
  
(* ---- Support and resistance lines: the rows of featureSet["Segments"] with Kind "Res" or "Sup" ---- *)

Clear[trendlineSegmentQ];
trendlineSegmentQ[seg_] := AssociationQ[seg] && MemberQ[{"Res", "Sup"}, Lookup[seg, "Kind", None]];

(* Resistance orange, support cyan *)
trendlineColor[seg_] := If[seg["Kind"] === "Res", RGBColor[1., 0.55, 0.15], RGBColor[0.25, 0.85, 1.]];

(* Price of the line of a support / resistance row at candle x *)
trendlinePriceAt[seg_, x_] :=
  seg["FromPrice"] + (seg["ToPrice"] - seg["FromPrice"]) * (x - seg["FromIndex"]) / (seg["ToIndex"] - seg["FromIndex"]);

(* Yellow points on the line at the candles that touch it (read from featureSet["ValidTrendlines"], up to the ToIndex of the row) *)
Clear[trendlineTouchMarkers];
trendlineTouchMarkers[linesArg_List, seg_, sizeArg_ : 0.012] :=
  Module[{rec, touchIndices},
    rec = SelectFirst[linesArg,
      #["Kind"] === seg["Kind"] && #["FromIndex"] === seg["FromIndex"] && #["AnchorIndex"] === Lookup[seg, "AnchorIndex", None] &,
      Missing[]];
    touchIndices = If[AssociationQ[rec], Select[Lookup[rec, "TouchIndices", {}], # <= seg["ToIndex"] &], {}];
    {Yellow, PointSize[sizeArg], Point[{#, trendlinePriceAt[seg, #]} & /@ touchIndices]}
  ];
trendlineTouchMarkers[featureSetArg_, seg_, sizeArg_ : 0.012] :=
  trendlineTouchMarkers[
    If[KeyExistsQ[featureSetArg, "ValidTrendlines"], Normal[featureSetArg["ValidTrendlines"]], {}], seg, sizeArg];

(* The candle data of the pickers: the Show button reads it by key when it is pressed, so that the buttons do not carry the data *)
$trendlineStore = <||>;

(* The extent of a support / resistance line: the line is extended backward from its first anchor and forward from its
   ToIndex until a candle pierces the extension (a High above the line + tolerance for a resistance, a Low below the
   line - tolerance for a support). The tolerance of a candle k is pctArg percent of the High-Low range of the candles
   between k and the first anchor. TouchIndices lists the candles that touch the line (within the tolerance) from
   the start of the backward extension to the end of the forward extension, without the two anchors.
   BackX / ForwardX are the x coordinates where the dotted extensions end (ForwardX: the right edge of the chart). BreakIndex is the candle that breaks the line, if any. *)
Clear[trendlineExtent];
trendlineExtent[highsArg_List, lowsArg_List, seg_, pctArg_] :=
  Module[{n = Length[highsArg], a1 = seg["FromIndex"], a2 = Lookup[seg, "AnchorIndex", seg["FromIndex"]],
          nTo = seg["ToIndex"], res = (seg["Kind"] === "Res"), series, tolF, tolB, tolAt, pierced, backStart, forwardEnd, breakIndex = Missing[], touchIndices},
    series = If[res, highsArg, lowsArg];
    (* tolerance of candle k >= a1: range of a1 .. k; of candle k <= a1: range of k .. a1 *)
    tolF = (pctArg / 100) * (FoldList[Max, highsArg[[a1 ;; n]]] - FoldList[Min, lowsArg[[a1 ;; n]]]);
    tolB = (pctArg / 100) * (FoldList[Max, Reverse[highsArg[[1 ;; a1]]]] - FoldList[Min, Reverse[lowsArg[[1 ;; a1]]]]);
    tolAt[k_] := If[k >= a1, tolF[[k - a1 + 1]], tolB[[a1 - k + 1]]];
    pierced[k_] := If[res, series[[k]] - trendlinePriceAt[seg, k], trendlinePriceAt[seg, k] - series[[k]]] > tolAt[k];
    backStart = a1;
    Do[If[pierced[k], Break[]]; backStart = k, {k, a1 - 1, 1, -1}];
    forwardEnd = nTo;
    Do[If[pierced[k], breakIndex = k; Break[]]; forwardEnd = k, {k, nTo + 1, n}];
    touchIndices = Select[Complement[Range[backStart, forwardEnd], {a1, a2}],
      Abs[series[[#]] - trendlinePriceAt[seg, #]] <= tolAt[#] &];
    <|"BackStart" -> backStart, "ForwardEnd" -> forwardEnd,
      "BackX"     -> If[backStart < a1, backStart - 0.5, a1],
      "BreakIndex" -> breakIndex,
      (* the dotted right extension goes to the right edge of the chart, also beyond the candle that breaks the line *)
      "ForwardX"  -> n + 0.5,
      "TouchIndices" -> touchIndices,
      "StartIndex" -> Min[Prepend[Select[touchIndices, # < a1 &], a1]]|>
  ];

(* The key of a support / resistance line in shownTrendlines *)
Clear[trendlineKey];
trendlineKey[seg_] := ToString[{seg["Kind"], seg["FromIndex"], seg["AnchorIndex"], seg["ToIndex"]}];

(* The entry of a line for shownTrendlines: key -> the segment, its touch markers and its extent (see trendlineExtent).
   storeKeyArg selects the candle data and the lines of the picker. *)
Clear[shownTrendlineEntry];
shownTrendlineEntry[seg_, storeKeyArg_] :=
  Module[{data, rec, pct, extent, markers},
    data = Lookup[$trendlineStore, storeKeyArg, <||>];
    rec  = SelectFirst[Lookup[data, "Lines", {}],
      #["Kind"] === seg["Kind"] && #["FromIndex"] === seg["FromIndex"] && #["AnchorIndex"] === seg["AnchorIndex"] &, <||>];
    pct  = Lookup[rec, "TolerancePercent", 5];
    extent  = trendlineExtent[data["Highs"], data["Lows"], seg, pct];
    markers = {Yellow, PointSize[0.012], Point[{#, trendlinePriceAt[seg, #]} & /@ extent["TouchIndices"]]};
    trendlineKey[seg] -> <|"Segment" -> seg, "TouchMarkers" -> markers, "Extent" -> extent, "ExtraOnLeft" -> 0, "ExtraOnRight" -> 0|>
  ];

(* The Show / Hide button of the support / resistance tables: adds the line, with its extensions and touches, to
   shownTrendlines, or removes it if it is there. *)
Clear[toggleShownTrendline];
toggleShownTrendline[seg_, storeKeyArg_] :=
  (If[KeyExistsQ[shownTrendlines, trendlineKey[seg]],
     shownTrendlines = KeyDrop[shownTrendlines, trendlineKey[seg]],
     shownTrendlines = Append[shownTrendlines, shownTrendlineEntry[seg, storeKeyArg]]];);

(* The Select all button: puts the lines of segsArg that are not on the chart yet on the chart *)
Clear[selectShownTrendlines];
selectShownTrendlines[segsArg_List, storeKeyArg_] :=
  (shownTrendlines = Join[shownTrendlines,
     Association[shownTrendlineEntry[#, storeKeyArg] & /@ Select[segsArg, ! KeyExistsQ[shownTrendlines, trendlineKey[#]] &]]];);

(* The key of a pattern in shownPatterns *)
Clear[patternKeyString];
patternKeyString[row_] :=
  ToString[{row["Pattern"], row["ToIndex"], row["ResistanceFromIndex"], row["ResistanceAnchorIndex"], row["SupportFromIndex"], row["SupportAnchorIndex"]}];

(* The entry of a pattern for shownPatterns: the row of Patterns and the two lines (records of ValidTrendlines) *)
Clear[shownPatternEntry];
shownPatternEntry[row_, storeKeyArg_] :=
  Module[{data = Lookup[$trendlineStore, storeKeyArg, <||>], lines, res, sup, backX},
    lines = Lookup[data, "Lines", {}];
    res = SelectFirst[lines, #["Kind"] === "Res" && #["FromIndex"] === row["ResistanceFromIndex"] && #["AnchorIndex"] === row["ResistanceAnchorIndex"] &, <||>];
    sup = SelectFirst[lines, #["Kind"] === "Sup" && #["FromIndex"] === row["SupportFromIndex"] && #["AnchorIndex"] === row["SupportAnchorIndex"] &, <||>];
    (* the dotted backward extension of each line, up to the first candle that breaks it (see trendlineExtent) *)
    backX = Function[line,
      If[line === <||> || ! KeyExistsQ[data, "Highs"], Lookup[line, "FromIndex", 1],
        trendlineExtent[data["Highs"], data["Lows"],
          <|"Kind" -> line["Kind"], "FromIndex" -> line["FromIndex"], "AnchorIndex" -> line["AnchorIndex"], "ToIndex" -> row["ToIndex"],
            "FromPrice" -> line["FromPrice"], "ToPrice" -> line["FromPrice"] + line["Slope"] (row["ToIndex"] - line["FromIndex"])|>,
          Lookup[line, "TolerancePercent", 5]]["BackX"]]];
    patternKeyString[row] -> <|"Pattern" -> row, "Resistance" -> res, "Support" -> sup, "LastIndex" -> Length[Lookup[data, "Highs", {}]],
      "ResistanceBackX" -> backX[res], "SupportBackX" -> backX[sup]|>
  ];

(* The Show / Hide button of the patterns table *)
Clear[toggleShownPattern];
toggleShownPattern[row_, storeKeyArg_] :=
  (If[KeyExistsQ[shownPatterns, patternKeyString[row]],
     shownPatterns = KeyDrop[shownPatterns, patternKeyString[row]],
     shownPatterns = Append[shownPatterns, shownPatternEntry[row, storeKeyArg]]];);

(* A pattern on the chart: each of its two lines in four pieces: dotted for the left extension (until a candle breaks it), solid in a darker colour between its two anchors, solid from the
   second anchor to the candle N of the row, dotted to the right up to the least of the candle where the two
   lines meet (the apex, if it is to the right of N) and the right edge of the chart; the area between the lines from the start of the pair is shaded;
   the touches of the two lines up to N are yellow dots *)
Clear[patternOverlay];
patternOverlay[entry_Association] :=
  Module[{row = entry["Pattern"], res = entry["Resistance"], sup = entry["Support"], n, last, endX, apex, t0, xEnd, ends, shadeEnd, rp, sp, tip, resA, supA, resB, supB, resColor = trendlineColorOf["Res"], supColor = trendlineColorOf["Sup"]},
    If[! AssociationQ[res] || ! AssociationQ[sup] || res === <||> || sup === <||>, Return[{}]];
    n = row["ToIndex"]; last = entry["LastIndex"]; t0 = row["StartIndex"]; apex = row["Apex"];
    endX = last + 0.5;
    xEnd = If[NumericQ[apex] && apex > n, Min[apex, endX], endX];
    (* the shading stops at the candle that ends the pattern (the first to break one of the two lines) or at the end of the lines, whichever comes first *)
    ends = Cases[{Lookup[res, "EndIndex", None], Lookup[sup, "EndIndex", None]}, _Integer];
    shadeEnd = If[ends === {}, xEnd, Min[xEnd, Min[ends] + 1]];
    rp[x_] := res["FromPrice"] + res["Slope"] (x - res["FromIndex"]);
    sp[x_] := sup["FromPrice"] + sup["Slope"] (x - sup["FromIndex"]);
    tip = row["PatternName"] <> ": candles " <> ToString[t0] <> " to " <> ToString[n] <> ", gap ratio " <> ToString[NumberForm[row["GapRatio"], {4, 2}]];
    resA = Lookup[res, "AnchorIndex", res["FromIndex"]]; supA = Lookup[sup, "AnchorIndex", sup["FromIndex"]];
    resB = Min[Lookup[entry, "ResistanceBackX", res["FromIndex"]], res["FromIndex"]]; supB = Min[Lookup[entry, "SupportBackX", sup["FromIndex"]], sup["FromIndex"]];
    {{Opacity[0.13], RGBColor[0.6, 0.8, 1.], EdgeForm[None], Polygon[{{t0, rp[t0]}, {shadeEnd, rp[shadeEnd]}, {shadeEnd, sp[shadeEnd]}, {t0, sp[t0]}}]},
     (* each line: solid in a darker colour between its two anchors, solid from the second anchor to N, dotted from N to xEnd, dotted for the left extension *)
     {resColor,
      {Darker[resColor], Opacity[0.95], Thickness[0.006], Dashing[{}], Tooltip[Line[{{res["FromIndex"], rp[res["FromIndex"]]}, {resA, rp[resA]}}], tip]},
      {Opacity[0.95], Thickness[0.002], Dashing[{}], Tooltip[Line[{{resA, rp[resA]}, {n, rp[n]}}], tip]},
      {Opacity[0.95], Thickness[0.0035], Dashing[{0.0, 0.012}], CapForm["Round"], Line[{{n, rp[n]}, {xEnd, rp[xEnd]}}],
       If[resB < res["FromIndex"], Line[{{resB, rp[resB]}, {res["FromIndex"], rp[res["FromIndex"]]}}], {}]}},
     {supColor,
      {Darker[supColor], Opacity[0.95], Thickness[0.006], Dashing[{}], Tooltip[Line[{{sup["FromIndex"], sp[sup["FromIndex"]]}, {supA, sp[supA]}}], tip]},
      {Opacity[0.95], Thickness[0.002], Dashing[{}], Tooltip[Line[{{supA, sp[supA]}, {n, sp[n]}}], tip]},
      {Opacity[0.95], Thickness[0.0035], Dashing[{0.0, 0.012}], CapForm["Round"], Line[{{n, sp[n]}, {xEnd, sp[xEnd]}}],
       If[supB < sup["FromIndex"], Line[{{supB, sp[supB]}, {sup["FromIndex"], sp[sup["FromIndex"]]}}], {}]}},
     {Opacity[0.95], resColor, PointSize[0.012], Point[{{res["FromIndex"], rp[res["FromIndex"]]}, {res["AnchorIndex"], rp[res["AnchorIndex"]]}}]},
     {Opacity[0.95], supColor, PointSize[0.012], Point[{{sup["FromIndex"], sp[sup["FromIndex"]]}, {sup["AnchorIndex"], sp[sup["AnchorIndex"]]}}]},
     (* the touches (confirmations) of each line from the left start of its extension to N: the candles whose High (resistance) or Low (support)
        is within the tolerance of the line, without the two anchors *)
     {Opacity[1], Yellow, PointSize[0.012],
      Point[{#, rp[#]} & /@ Join[Lookup[res, "BackTouchIndices", {}], Select[Lookup[res, "TouchIndices", {}], # <= n &]]],
      Point[{#, sp[#]} & /@ Join[Lookup[sup, "BackTouchIndices", {}], Select[Lookup[sup, "TouchIndices", {}], # <= n &]]]},
     If[NumericQ[apex] && n < apex <= endX, {White, PointSize[0.012], Tooltip[Point[{apex, rp[apex]}], "apex: the two lines meet at candle " <> ToString[NumberForm[apex, {5, 1}]]]}, {}]}
  ];

trendlineColorOf[kind_String] := If[kind === "Res", RGBColor[1., 0.55, 0.15], RGBColor[0.25, 0.85, 1.]];

(* The Clear Support/Resistance button: the lines and the patterns *)
Clear[clearShownTrendlines];
clearShownTrendlines[] := (shownTrendlines = <||>; shownPatterns = <||>; shownHarmonics = <||>; lastHarmonic = None;);

(* ---- harmonic patterns: one row of the key HarmonicPatterns of the feature set is a path X, A, B, C, D of the stock graph ------------------------------ *)

Clear[harmonicKeyString];
harmonicKeyString[row_] := ToString[{row["Pattern"], row["Indices"]}];

Clear[toggleShownHarmonic];
toggleShownHarmonic[row_] :=
  (If[KeyExistsQ[shownHarmonics, harmonicKeyString[row]],
     shownHarmonics = KeyDrop[shownHarmonics, harmonicKeyString[row]],
     shownHarmonics = Append[shownHarmonics, harmonicKeyString[row] -> row]; lastHarmonic = row];);

Clear[selectShownHarmonics];
selectShownHarmonics[rowsArg_List] :=
  (shownHarmonics = Join[shownHarmonics, Association[(harmonicKeyString[#] -> #) & /@ Select[rowsArg, ! KeyExistsQ[shownHarmonics, harmonicKeyString[#]] &]]];);

Clear[clearShownHarmonics];
clearShownHarmonics[] := (shownHarmonics = <||>; lastHarmonic = None;);

(* A harmonic pattern on the chart: the path X A B C D (the segments of the stock graph) with its points named, the two triangles X A B and B C D lightly shaded;
   the tooltip gives the pattern, the direction (Bullish: D is a low, the price is expected to turn up), the violation and the ratios *)
Clear[harmonicOverlay];
harmonicOverlay[row_Association] :=
  Module[{idx = row["Indices"], pr = row["Prices"], labels = row["Points"], color, pts, k, ratios, tip, peakQ},
    k = Length[idx];
    color = If[row["Direction"] === "Bullish", RGBColor[0.35, 0.95, 0.45], RGBColor[1., 0.45, 0.7]];
    pts = Transpose[{idx, pr}];
    ratios = StringRiffle[
      KeyValueMap[#1 <> " " <> ToString[NumberForm[#2, {4, 2}]] &,
        Select[Association[# -> Lookup[row, #, Missing[]] & /@ {"BofXA", "CofAB", "DofXA", "CDofBC", "CDofAB", "CofXA", "DofXC"}], NumericQ]], ", "];
    tip = row["Pattern"] <> " " <> ToLowerCase[row["Direction"]] <> ", " <> StringRiffle[MapThread[#1 <> " " <> ToString[#2] &, {labels, idx}], " > "] <>
      ", born at candle " <> ToString[row["ToIndex"]] <> ", violation " <> ToString[NumberForm[row["Violation"], {4, 3}]] <> If[ratios === "", "", "; " <> ratios];
    (* a point is a peak when it is above the point before it (the first: above the second) *)
    peakQ[i_] := If[i == 1, pr[[1]] > pr[[2]], pr[[i]] > pr[[i - 1]]];
    {If[k == 5, {Opacity[0.12], color, EdgeForm[None], Polygon[pts[[{1, 2, 3}]]], Polygon[pts[[{3, 4, 5}]]]}, {}],
     {Opacity[0.95], color, Thickness[0.004], Tooltip[Line[pts], tip]},
     {Opacity[1], color, PointSize[0.012], Point[pts]},
     Table[Text[Style[labels[[i]], Bold, 13, color], pts[[i]], If[peakQ[i], {0, -1.6}, {0, 1.6}]], {i, k}]}
  ];

(* The line in four pieces: dotted for the left extension (from its Start, see extentArg and trendlineExtent, to its first anchor), solid in a darker colour between its two anchors (the stub),
   solid from the second anchor (the Anchor) to the ToIndex (the candle at which the line is alive), dotted from the ToIndex to the right edge of the chart (dimmer after the
   candle that breaks the line); the two anchors are dots *)
Clear[trendlineOverlay];
trendlineOverlay[seg_, extraOnRightArg_ : 0, extentArg_ : <||>] :=
  Module[{x1 = seg["FromIndex"], x2 = seg["ToIndex"], xEnd, anchor = Lookup[seg, "AnchorIndex", None],
          xBack, xForward, xA, touches, color = trendlineColor[seg], price},
    price[x_] := trendlinePriceAt[seg, x];
    xEnd     = x2 + Max[0, extraOnRightArg];
    (* the second anchor (the first anchor if there is none) *)
    xA       = If[IntegerQ[anchor] && anchor > x1, anchor, x1];
    xBack    = Lookup[extentArg, "BackX", x1];
    xForward = Lookup[extentArg, "ForwardX", x2];
    touches  = If[KeyExistsQ[extentArg, "TouchIndices"], Length[extentArg["TouchIndices"]], Lookup[seg, "Touches", 0]];
    With[{tip = seg["Direction"] <> ": candles " <> ToString[x1] <> " to " <> ToString[x2] <> ", touches " <> ToString[touches] <>
       If[KeyExistsQ[extentArg, "BackStart"], " (counted from candle " <> ToString[extentArg["BackStart"]] <> " to " <> ToString[extentArg["ForwardEnd"]] <> ")", ""] <>
       If[IntegerQ[Lookup[extentArg, "BreakIndex", None]], ", broken by candle " <> ToString[extentArg["BreakIndex"]], ""]},
      {color,
       (* solid, in a darker colour, between the two anchors *)
       {Darker[color], Opacity[0.9], Thickness[0.006], Dashing[{}], Tooltip[Line[{{x1, price[x1]}, {xA, price[xA]}}], tip]},
       (* solid from the second anchor to the candle at which the line is alive *)
       {Opacity[0.9], Thickness[0.002], Dashing[{}], If[xEnd > xA, Tooltip[Line[{{xA, price[xA]}, {xEnd, price[xEnd]}}], tip], {}]},
       (* dotted extensions: to the left up to the candle that pierces the line, to the right edge of the chart (dimmer after the candle that breaks the line) *)
       {Opacity[0.95], Thickness[0.0035], Dashing[{0.0, 0.012}], CapForm["Round"],
        If[xBack < x1, Line[{{xBack, price[xBack]}, {x1, price[x1]}}], {}],
        If[xForward > xEnd,
          With[{brk = Lookup[extentArg, "BreakIndex", None]},
            If[IntegerQ[brk] && xEnd < brk < xForward,
              {Line[{{xEnd, price[xEnd]}, {brk, price[brk]}}],
               Opacity[0.4], Line[{{brk, price[brk]}, {xForward, price[xForward]}}]},
              Line[{{xEnd, price[xEnd]}, {xForward, price[xForward]}}]]],
          {}]},
       {Opacity[0.9], Dashing[{}], PointSize[0.014], Point[{x1, seg["FromPrice"]}],
        If[IntegerQ[anchor], Point[{anchor, price[anchor]}], {}]}}]
  ];

(* What is drawn for one entry of hoveredSegs: the rectangle with its extension for a wave segment, the line for a
   support / resistance row; the touch markers follow in both cases *)
Clear[segmentOverlay];
segmentOverlay[entry_Association] :=
  Module[{seg = entry["Segment"], markers = entry["TouchMarkers"], extraLeft = entry["ExtraOnLeft"],
          extraRight = Lookup[entry, "ExtraOnRight", 0]},
    If[trendlineSegmentQ[seg],
      {trendlineOverlay[seg, extraRight, Lookup[entry, "Extent", <||>]], markers},
      {EventHandler[
         makeRectangle[
           {seg["FromIndex"], seg["FromPrice"]},
           {seg["ToIndex"],   seg["ToPrice"]},
           "upColor"      -> segmentEdgeColor[seg],
           "downColor"    -> segmentEdgeColor[seg],
           "lineOpacity"  -> 0.85,
           "thickness"    -> 0.001,
           "extraOnLeft"  -> extraLeft,
           "extraOnRight" -> extraRight
         ],
         {"MouseClicked" :> Null},
         PassEventsDown -> True
       ],
       markers}
    ]
  ];

Clear[handleExternalSegmentSet];
handleExternalSegmentSet[featureSetArg_, touchToleranceArg_, touchMarkerSizeArg_,
                         chartExtraLeftArg_, chartExtraRightArg_] :=
  Module[{seg, segKey, touchMarkers, priorSeg, extraOnLeft},
    seg = StockAnalyzer`Visualization`lastSegmentClicked;
    If[AssociationQ[seg] &&
       KeyExistsQ[seg, "FromIndex"] &&
       KeyExistsQ[seg, "ToIndex"] &&
       KeyExistsQ[seg, "Direction"],
      (* Derive missing fields from Direction *)
      If[!KeyExistsQ[seg, "EdgeType"],
        seg = Append[seg, "EdgeType"    -> If[trendlineSegmentQ[seg], If[seg["Kind"] === "Res", "Resistance", "Support"],
                                              If[seg["Direction"] === "U", "MaxU", "MaxD"]]]];
      If[!KeyExistsQ[seg, "FromField"],
        seg = Append[seg, "FromField"   -> If[trendlineSegmentQ[seg], If[seg["Kind"] === "Res", "High", "Low"],
                                              If[seg["Direction"] === "U", "Low",  "High"]]]];
      If[!KeyExistsQ[seg, "ToField"],
        seg = Append[seg, "ToField"     -> If[trendlineSegmentQ[seg], If[seg["Kind"] === "Res", "High", "Low"],
                                              If[seg["Direction"] === "U", "High", "Low" ]]]];
      If[!KeyExistsQ[seg, "SegmentType"],
        seg = Append[seg, "SegmentType" -> If[trendlineSegmentQ[seg], seg["Direction"],
                                              If[seg["Direction"] === "U", "UULH", "DDHL"]]]];
      segKey = ToString[{
        DirectedEdge[seg["FromIndex"], seg["ToIndex"]],
        seg["Direction"],
        seg["FromField"],
        seg["ToField"],
        seg["EdgeType"],
        seg["SegmentType"]
      }];
      If[!KeyExistsQ[StockAnalyzer`Visualization`hoveredSegs, segKey],
	       debugPrint["Watchers", "external set --> ", seg["FromIndex"], "->",
	    seg["ToIndex"], " ", seg["EdgeType"], "  key=", segKey];
        If[trendlineSegmentQ[seg],
          (* support / resistance line: the candles that touch it, no Fibonacci markers, no left extension *)
          touchMarkers = trendlineTouchMarkers[featureSetArg, seg];
          extraOnLeft  = 0,
          touchMarkers = fibonacciTouchMarkers[
            featureSetArg, seg, fibonacciRatios, touchToleranceArg,
            "MarkerSize"   -> touchMarkerSizeArg,
            "ExtraOnLeft"  -> chartExtraLeftArg,
            "ExtraOnRight" -> chartExtraRightArg
          ];
          priorSeg = findLongestOppositeSegmentEndingAt[
            featureSetArg["Segments"],
            seg["FromIndex"],
            seg["Direction"]
          ];
          extraOnLeft = If[MissingQ[priorSeg], 0,
            seg["FromIndex"] - priorSeg["FromIndex"]
          ]
        ];
        StockAnalyzer`Visualization`hoveredSegs = <||>;
        StockAnalyzer`Visualization`hoveredSegs = Append[
          StockAnalyzer`Visualization`hoveredSegs,
          segKey -> <|
            "Segment"      -> seg,
            "TouchMarkers" -> touchMarkers,
            "ExtraOnLeft"  -> extraOnLeft + chartExtraLeftArg,
            "ExtraOnRight" -> chartExtraRightArg
          |>
        ];
        StockAnalyzer`Visualization`lastSelectedSegKey = segKey;
        StockAnalyzer`Visualization`lastClickedCandidates = {seg};
      ]
    ];
    {}
  ]; 
  
(* Renders a full candlestick chart with directed trend edge overlays.
   Supports hover tooltips on both candles and edges, click-to-pin edge
   Fibonacci rectangles, and an optional vertical price slider. *)

Clear[selectCandidateByIndex];
selectCandidateByIndex[nArg_Integer, isSwapArg_,
                       featureSetArg_, touchToleranceArg_, touchMarkerSizeArg_,
                       chartExtraLeftArg_, chartExtraRightArg_, diagLabelArg_String] :=
  Module[{candidates, segArg, primarySeg, segKey, touchMarkers, priorSeg, extraOnLeft},
    candidates = StockAnalyzer`Visualization`lastClickedCandidates;
    If[Length[candidates] == 0 || nArg < 1 || nArg > Length[candidates],
      debugPrint[diagLabelArg, " --> ignored (n=", nArg, ", candidates=", Length[candidates], ")"];
      Return[Null]
    ];
    segArg = candidates[[nArg]];
    primarySeg = Join[segArg,
      If[segArg["Direction"] === "U",
        <|"EdgeType" -> "MaxU", "FromField" -> "Low",  "ToField" -> "High", "SegmentType" -> "UULH"|>,
        <|"EdgeType" -> "MaxD", "FromField" -> "High", "ToField" -> "Low",  "SegmentType" -> "DDHL"|>
      ]
    ];
    segKey = ToString[{
      DirectedEdge[primarySeg["FromIndex"], primarySeg["ToIndex"]],
      primarySeg["Direction"],
      primarySeg["FromField"],
      primarySeg["ToField"],
      primarySeg["EdgeType"],
      primarySeg["SegmentType"]
    }];
    (* Keyboard swap: remove previously-selected key before adding new.
       Click (isSwap=False): leave existing selections alone (Q2=B). *)
    If[isSwapArg === True &&
       StockAnalyzer`Visualization`lastSelectedSegKey =!= None &&
       KeyExistsQ[StockAnalyzer`Visualization`hoveredSegs,
                  StockAnalyzer`Visualization`lastSelectedSegKey],
      StockAnalyzer`Visualization`hoveredSegs =
        KeyDrop[StockAnalyzer`Visualization`hoveredSegs,
                StockAnalyzer`Visualization`lastSelectedSegKey]
    ];
    touchMarkers = fibonacciTouchMarkers[
      featureSetArg, primarySeg, fibonacciRatios, touchToleranceArg,
      "MarkerSize"   -> touchMarkerSizeArg,
      "ExtraOnLeft"  -> chartExtraLeftArg,
      "ExtraOnRight" -> chartExtraRightArg
    ];
    priorSeg = findLongestOppositeSegmentEndingAt[
      featureSetArg["Segments"],
      primarySeg["FromIndex"],
      primarySeg["Direction"]
    ];
    extraOnLeft = If[MissingQ[priorSeg],
      0,
      primarySeg["FromIndex"] - priorSeg["FromIndex"]
    ];
    
    (*
    debugPrint[diagLabelArg, " --> ", nArg, "/", Length[candidates],
          "  seg=", primarySeg["FromIndex"], "->", primarySeg["ToIndex"],
          " ", primarySeg["Direction"], " DeltaX=", primarySeg["DeltaX"]];
    *)
    
    StockAnalyzer`Visualization`hoveredSegs = Append[
      StockAnalyzer`Visualization`hoveredSegs,
      segKey -> <|
        "Segment"      -> primarySeg,
        "TouchMarkers" -> touchMarkers,
        "ExtraOnLeft"  -> extraOnLeft + chartExtraLeftArg,
        "ExtraOnRight" -> chartExtraRightArg
      |>
    ];
    StockAnalyzer`Visualization`lastSelectedSegKey = segKey;
    StockAnalyzer`Visualization`lastSegmentClicked = primarySeg;
  ];  

Clear[makeGraphDisplay];
makeGraphDisplay[featureSetArg_, cs_Tabular, opts : OptionsPattern[]] :=
  Module[
    {cData, nMin, nMax,
     allPrices, priceMin, priceMax, priceRange,
     tickFn, topTickFn, candleGlyphs, graphExEdges, evaluatedLines,
     halfWidth, imageSize, xAxis, edgeThickness,
     lines, background, upColor, downColor, arrowSize, showPriceSlider,
     showScrollbar, defaultWindow,
     chartPixelHeight, imagePadding, plotAreaHeight,
     plotPriceMin, plotPriceMax, plotPriceSpan,
     touchTolerance, touchMarkerSize,
     chartExtraLeft, chartExtraRight
    },
    halfWidth       = OptionValue[GlyphWidth];
    imageSize       = OptionValue[ImageSize];
    xAxis           = OptionValue[XAxis];
    edgeThickness   = OptionValue[EdgeThickness];
    lines           = OptionValue[Lines];
    background      = OptionValue[Background];
    upColor         = OptionValue[UpColor];
    downColor       = OptionValue[DownColor];
    arrowSize       = OptionValue[ArrowSize];
    showPriceSlider = OptionValue[ShowPriceSlider];
    showScrollbar   = OptionValue[ShowScrollbar];
    touchTolerance  = OptionValue["TouchTolerance"];
    touchMarkerSize = OptionValue["TouchMarkerSize"];
    chartExtraLeft  = OptionValue["ExtraOnLeft"];
    chartExtraRight = OptionValue["ExtraOnRight"];
    StockAnalyzer`Visualization`showAllEdges    = OptionValue[ShowGraphEx];
    StockAnalyzer`Visualization`showEdgeTypes   = {"MaxU", "MaxD"};
    StockAnalyzer`Visualization`showEdgeTooltip = False;
    StockAnalyzer`Visualization`showDirections  = {};
    StockAnalyzer`Visualization`showFromFields  = {};
    StockAnalyzer`Visualization`showToFields    = {};
    (* with both axes the dates (rotated) need room below and the indices room above *)
    imagePadding = If[xAxis === "Both", {{45, 85}, {50, 24}}, {{45, 85}, {35, 10}}];
    chartPixelHeight = If[NumericQ[imageSize],
      imageSize / 2,
      If[ListQ[imageSize], imageSize[[2]], 400]
    ];
    cData      = Normal[cs];
    nMin       = Min[cData[[All, "Index"]]];
    nMax       = Max[cData[[All, "Index"]]];
    defaultWindow = {Max[nMin, nMax - 59], nMax};
    allPrices  = Join[cData[[All, "High"]], cData[[All, "Low"]]];
    priceMin   = Min[allPrices];
    priceMax   = Max[allPrices];
    priceRange = priceMax - priceMin;
    plotPriceMin  = priceMin - 0.05 * priceRange;
    plotPriceMax  = priceMax + 0.05 * priceRange;
    plotPriceSpan = plotPriceMax - plotPriceMin;
    plotAreaHeight = N[chartPixelHeight - imagePadding[[2,1]] - imagePadding[[2,2]]] - 16;
    tickFn = If[xAxis === "Date" || xAxis === "Both",
      (* the dates are thinned to about 30 labels or fewer: every 2nd, 3rd, 6th, 12th... candle when there are many *)
      With[{base = dateTickFunction[cs]},
        Function[{min, max},
          With[{step = SelectFirst[{1, 2, 3, 4, 6, 12, 24, 36, 60, 120, 240}, (Round[max] - Round[min] + 1) / # <= 30 &, 480]},
            Select[base[min, max], Mod[#[[1]], step] == 0 || step == 1 &]]]],
      indexTickFunction[cs]
    ];
    (* the upper axis: the candlestick index, at the multiples of a step chosen so that about 20 labels or fewer are shown *)
    topTickFn = If[xAxis === "Both",
      Function[{min, max},
        Module[{step = SelectFirst[{1, 2, 5, 10, 20, 50, 100, 200, 500}, (Round[max] - Round[min]) / # <= 20 &, 1000]},
          Table[{i, Style[ToString[i], FontSize -> 9, FontFamily -> "Arial"], {0.01, 0}, {}},
            {i, Select[Range[Round[min], Round[max]], Mod[#, step] == 0 &]}]]],
      Function[{min, max}, {}]
    ];
    evaluatedLines = lines;
    candleGlyphs = Map[
      Function[row,
        Module[{glyph},
          glyph = candlestickGlyph[
            row["Index"],
            row["Open"], row["High"], row["Low"], row["Close"],
            halfWidth
          ];
          If[showScrollbar,
            {row["Index"],
             EventHandler[
               {glyph, Opacity[0], White,
                Rectangle[{row["Index"] - halfWidth, row["Low"]},
                          {row["Index"] + halfWidth, row["High"]}]},
               {"MouseEntered" :> (hoveredRow = row; hoveredVertex = Round[row["Index"]]),
                "MouseExited"  :> (hoveredVertex = None; hoveredField = None),
                "MouseClicked" :> Module[{idx},
                  idx = Round[row["Index"]];
                  StockAnalyzer`Visualization`lastClickedCandidates =
                    findSegmentsEndingAt[featureSetArg["Segments"], idx];
                  debugPrint["MakeGraphDisplay -- Scrollable -- click at idx=", idx,
                        " candidates=", Length[StockAnalyzer`Visualization`lastClickedCandidates]];
                  If[Length[StockAnalyzer`Visualization`lastClickedCandidates] > 0,
                    selectCandidateByIndex[1, False,
                      featureSetArg, touchTolerance, touchMarkerSize,
                      chartExtraLeft, chartExtraRight,
                      "MakeGraphDisplay -- Scrollable -- click"]
                  ]
                ]},
               PassEventsDown -> True
             ]
            },
            EventHandler[
              {glyph, Opacity[0], White,
               Rectangle[{row["Index"] - halfWidth, row["Low"]},
                         {row["Index"] + halfWidth, row["High"]}]},
              {"MouseEntered" :> (hoveredRow = row; hoveredVertex = Round[row["Index"]]),
               "MouseExited"  :> (hoveredVertex = None; hoveredField = None),
               "MouseClicked" :> Module[{idx},
                 idx = Round[row["Index"]];
                 StockAnalyzer`Visualization`lastClickedCandidates =
                   findSegmentsEndingAt[featureSetArg["Segments"], idx];
                 deubgPrint["MakeGraphDisplay -- NonScrollable -- click at idx=", idx,
                       " candidates=", Length[StockAnalyzer`Visualization`lastClickedCandidates]];
                 If[Length[StockAnalyzer`Visualization`lastClickedCandidates] > 0,
                   selectCandidateByIndex[1, False,
                     featureSetArg, touchTolerance, touchMarkerSize,
                     chartExtraLeft, chartExtraRight,
                     "MakeGraphDisplay -- NonScrollable -- click"]
                 ]
               ]},
              PassEventsDown -> True
            ]
          ]
        ]
      ],
      cData
    ];
    graphExEdges = drawGraphExEdges[featureSetArg, nMax,
      UpColor       -> upColor,
      DownColor     -> downColor,
      ArrowSize     -> arrowSize,
      EdgeThickness -> edgeThickness,
      ShowExtensions -> OptionValue[ShowExtensions]
    ];
    hoveredSegs = <||>;
    DynamicModule[
      {rangeWindow = defaultWindow,
       hoveredRow = Last[cData],
       hoveredSeg = None,
       hoveredOHLCEdge = None,
       priceLevel = N[(plotPriceMin + plotPriceMax) / 2],
       showMarkovMatrix = False
      },
      Column[{
        Framed[
          Row[{Spacer[8], Dynamic[infoBarText[hoveredRow]]}],
          Background     -> GrayLevel[0.12],
          FrameStyle     -> Directive[GrayLevel[0.3], Thin],
          FrameMargins   -> {4, 4},
          RoundingRadius -> 0,
          ImageSize      -> {Full, 35}
        ],
        Framed[
          Row[{
            Spacer[8],
            Style["Regime: ", 9, GrayLevel[0.7]],
            Dynamic[regimeBarDisplay[hoveredRow],
              TrackedSymbols :> {hoveredRow}
            ],
            Spacer[12],
            Button[
              Dynamic[Style[If[showMarkovMatrix,
                "Markov \[DownArrow]", "Markov \[RightArrow]"], 9]],
              showMarkovMatrix = !showMarkovMatrix,
              Appearance -> "Palette"
            ]
          }],
          Background   -> GrayLevel[0.10],
          FrameStyle   -> Directive[GrayLevel[0.3], Thin],
          FrameMargins -> {4, 2},
          ImageSize    -> {Full, 22}
        ],
        Dynamic[
          If[showMarkovMatrix,
            Framed[
              Row[{
                Spacer[8],
                Style["Transition Matrix (Bear/Side/Bull): ", 9, GrayLevel[0.6]],
                markovMatrixDisplay[markovMatrixFromRow[hoveredRow]]
              }],
              Background   -> GrayLevel[0.10],
              FrameStyle   -> Directive[GrayLevel[0.3], Thin],
              FrameMargins -> {4, 4}
            ],
            Spacer[0]
          ],
          TrackedSymbols :> {showMarkovMatrix, hoveredRow}
        ],
        EventHandler[
          If[showScrollbar,
            Dynamic[
              Module[{winMin, winMax, visData, winPrices,
                      winPriceMin, winPriceMax, winPriceRange,
                      winPlotMin, winPlotMax, winPlotSpan,
                      visCandles, winGridLines, winTicks},
                winMin = Round[rangeWindow[[1]]];
                winMax = Round[rangeWindow[[2]]];
                visData = Select[cData, #["Index"] >= winMin && #["Index"] <= winMax &];
                winPrices     = Join[visData[[All, "High"]], visData[[All, "Low"]]];
                winPriceMin   = If[Length[winPrices] > 0, Min[winPrices], priceMin];
                winPriceMax   = If[Length[winPrices] > 0, Max[winPrices], priceMax];
                winPriceRange = winPriceMax - winPriceMin;
                winPlotMin    = winPriceMin - 0.05 * winPriceRange;
                winPlotMax    = winPriceMax + 0.05 * winPriceRange;
                winPlotSpan   = winPlotMax - winPlotMin;
                visCandles = Cases[candleGlyphs,
                  {idx_, glyph_} /; idx >= winMin && idx <= winMax :> glyph
                ];
                winGridLines = {
                  Table[{i, Directive[GrayLevel[0.25], Dotted, Thickness[0.001]]},
                    {i, winMin, winMax}],
                  Automatic
                };
                winTicks = tickFn[winMin, winMax];
                Graphics[
                  {evaluatedLines, visCandles, graphExEdges,
                   (* ---- NEW: reactive watcher for external set of lastSegmentClicked ---- *)
                   Dynamic[
                     handleExternalSegmentSet[featureSetArg, touchTolerance,
                       touchMarkerSize, chartExtraLeft, chartExtraRight],
                     TrackedSymbols :> {StockAnalyzer`Visualization`lastSegmentClicked}
                   ],
                   Dynamic[Map[segmentOverlay, Values[hoveredSegs]]],
                   (* support / resistance lines stay until the Clear Support/Resistance button *)
                   Dynamic[Map[patternOverlay, Values[StockAnalyzer`Visualization`shownPatterns]],
                     TrackedSymbols :> {StockAnalyzer`Visualization`shownPatterns}],
                   Dynamic[Map[harmonicOverlay, Values[StockAnalyzer`Visualization`shownHarmonics]],
                     TrackedSymbols :> {StockAnalyzer`Visualization`shownHarmonics}],
                   Dynamic[Map[segmentOverlay, Values[StockAnalyzer`Visualization`shownTrendlines]],
                     TrackedSymbols :> {StockAnalyzer`Visualization`shownTrendlines}]
                  },
                  Frame      -> True,
                  FrameLabel -> {{Style["Price", 10, FontFamily -> "Arial"], None}, {None, None}},
                  FrameTicks -> {
                    {Map[{#, Style[ToString[NumberForm[N[#],{6,2}]], 9, FontFamily->"Arial"], {0.01,0}} &,
                       FindDivisions[{winPriceMin, winPriceMax}, 8]],
                     Map[{#, Style[ToString[NumberForm[N[#],{6,2}]], 9, FontFamily->"Arial"], {0.01,0}} &,
                       FindDivisions[{winPriceMin, winPriceMax}, 8]]},
                    {winTicks, topTickFn[winMin, winMax]}
                  },
                  PlotRange         -> {{winMin-0.5, winMax+0.5}, {winPlotMin, winPlotMax}},
                  AspectRatio       -> 1/2,
                  ImageSize         -> imageSize,
                  ImagePadding      -> imagePadding,
                  Background        -> background,
                  GridLines         -> winGridLines,
                  GridLinesStyle    -> Directive[GrayLevel[0.25], Dotted, Thickness[0.001]],
                  FrameStyle        -> Directive[GrayLevel[0.5], Thickness[0.002]],
                  BaseStyle         -> {FontColor -> White, FontSize -> 9, FontFamily -> "Arial"},
                  ContentSelectable -> False,
                  Epilog            -> {},
                  ScalingFunctions  -> OptionValue[ScalingFunctions]
                ]
              ],
              TrackedSymbols :> {
                rangeWindow, hoveredSegs, hoveredRow,
                StockAnalyzer`Visualization`showAllEdges,
                StockAnalyzer`Visualization`showEdgeTypes,
                StockAnalyzer`Visualization`showEdgeTooltip,
                StockAnalyzer`Visualization`showDirections,
                StockAnalyzer`Visualization`showFromFields,
                StockAnalyzer`Visualization`showToFields,
                StockAnalyzer`Visualization`lastSegmentClicked
              },
              SynchronousUpdating -> False
            ],
            Graphics[
              {evaluatedLines, candleGlyphs, graphExEdges,
               If[showPriceSlider,
                 Dynamic[{
                   Directive[Darker[Red], Dashed, Thickness[0.002]],
                   Line[{{nMin-0.5, priceLevel}, {nMax+0.5, priceLevel}}],
                   Text[Style[ToString[NumberForm[priceLevel,{6,2}]], 9,
                          FontFamily->"Arial", Red, Bold],
                        Scaled[{1., (priceLevel-plotPriceMin)/plotPriceSpan}],
                        {-1, 0}]
                 }],
                 {}
               ],
               (* ---- NEW: reactive watcher for external set of lastSegmentClicked ---- *)
               Dynamic[
                 handleExternalSegmentSet[featureSetArg, touchTolerance,
                   touchMarkerSize, chartExtraLeft, chartExtraRight],
                 TrackedSymbols :> {StockAnalyzer`Visualization`lastSegmentClicked}
               ],
               Dynamic[Map[segmentOverlay, Values[hoveredSegs]]],
                   (* support / resistance lines stay until the Clear Support/Resistance button *)
                   Dynamic[Map[patternOverlay, Values[StockAnalyzer`Visualization`shownPatterns]],
                     TrackedSymbols :> {StockAnalyzer`Visualization`shownPatterns}],
                   Dynamic[Map[harmonicOverlay, Values[StockAnalyzer`Visualization`shownHarmonics]],
                     TrackedSymbols :> {StockAnalyzer`Visualization`shownHarmonics}],
                   Dynamic[Map[segmentOverlay, Values[StockAnalyzer`Visualization`shownTrendlines]],
                     TrackedSymbols :> {StockAnalyzer`Visualization`shownTrendlines}]
              },
              Frame      -> True,
              FrameLabel -> {{Style["Price", 10, FontFamily -> "Arial"], None}, {None, None}},
              FrameTicks -> {
                {Map[{#, Style[ToString[NumberForm[N[#],{6,2}]], 9, FontFamily->"Arial"], {0.01,0}} &,
                   FindDivisions[{priceMin, priceMax}, 8]],
                 Map[{#, Style[ToString[NumberForm[N[#],{6,2}]], 9, FontFamily->"Arial"], {0.01,0}} &,
                   FindDivisions[{priceMin, priceMax}, 8]]},
                {tickFn[nMin, nMax], topTickFn[nMin, nMax]}
              },
              PlotRange         -> {{nMin-0.5, nMax+0.5},
                                    {priceMin-0.05*priceRange, priceMax+0.05*priceRange}},
              AspectRatio       -> 1/2,
              ImageSize         -> imageSize,
              ImagePadding      -> imagePadding,
              Background        -> background,
              GridLines         -> {
                Table[{i, Directive[GrayLevel[0.25], Dotted, Thickness[0.001]]},
                  {i, nMin, nMax}],
                Automatic
              },
              GridLinesStyle    -> Directive[GrayLevel[0.25], Dotted, Thickness[0.001]],
              FrameStyle        -> Directive[GrayLevel[0.5], Thickness[0.002]],
              BaseStyle         -> {FontColor -> White, FontSize -> 9, FontFamily -> "Arial"},
              ContentSelectable -> False,
              Epilog            -> {},
              ScalingFunctions  -> OptionValue[ScalingFunctions],
              Prolog -> If[showPriceSlider,
                Inset[
                  Slider[
                    Dynamic[priceLevel, (priceLevel = #; myFunction[#]) &],
                    {plotPriceMin, plotPriceMax},
                    Appearance -> "Vertical",
                    ImageSize  -> {16, plotAreaHeight}
                  ],
                  Scaled[{1.06, 0}], {Center, Bottom}
                ],
                {}
              ]
            ]
          ],
          {
            "MouseExited" :> Null,
            {"KeyDown", "1"} :> selectCandidateByIndex[1, True, featureSetArg, touchTolerance, touchMarkerSize, chartExtraLeft, chartExtraRight, "KeyDown-1"],
            {"KeyDown", "2"} :> selectCandidateByIndex[2, True, featureSetArg, touchTolerance, touchMarkerSize, chartExtraLeft, chartExtraRight, "KeyDown-2"],
            {"KeyDown", "3"} :> selectCandidateByIndex[3, True, featureSetArg, touchTolerance, touchMarkerSize, chartExtraLeft, chartExtraRight, "KeyDown-3"],
            {"KeyDown", "4"} :> selectCandidateByIndex[4, True, featureSetArg, touchTolerance, touchMarkerSize, chartExtraLeft, chartExtraRight, "KeyDown-4"],
            {"KeyDown", "5"} :> selectCandidateByIndex[5, True, featureSetArg, touchTolerance, touchMarkerSize, chartExtraLeft, chartExtraRight, "KeyDown-5"],
            {"KeyDown", "6"} :> selectCandidateByIndex[6, True, featureSetArg, touchTolerance, touchMarkerSize, chartExtraLeft, chartExtraRight, "KeyDown-6"],
            {"KeyDown", "7"} :> selectCandidateByIndex[7, True, featureSetArg, touchTolerance, touchMarkerSize, chartExtraLeft, chartExtraRight, "KeyDown-7"],
            {"KeyDown", "8"} :> selectCandidateByIndex[8, True, featureSetArg, touchTolerance, touchMarkerSize, chartExtraLeft, chartExtraRight, "KeyDown-8"],
            {"KeyDown", "9"} :> selectCandidateByIndex[9, True, featureSetArg, touchTolerance, touchMarkerSize, chartExtraLeft, chartExtraRight, "KeyDown-9"]
          },
          PassEventsDown -> False,
          PassEventsUp   -> False
        ],
        If[showScrollbar,
          Row[{
            Spacer[imagePadding[[1,1]]],
            Dynamic[
              Style[
                ToString[Round[rangeWindow[[1]]]] <> " \[Dash] " <>
                ToString[Round[rangeWindow[[2]]]] <> "  (" <>
                ToString[Round[rangeWindow[[2]]]-Round[rangeWindow[[1]]]+1] <> " candles)",
                9, White, FontFamily -> "Arial"
              ]
            ],
            Spacer[8],
            IntervalSlider[
              Dynamic[rangeWindow, (rangeWindow = #) &],
              {nMin, nMax, 1},
              ImageSize        -> imageSize - imagePadding[[1,1]] - imagePadding[[1,2]] - 150,
              Appearance       -> "Paired",
              ContinuousAction -> False
            ]
          }],
          Nothing
        ],
        Row[{
          Button["Clear",
            hoveredSegs = <||>;
            hoveredEdgeKey = None;
            hoveredSeg = None;
            StockAnalyzer`Visualization`hoveredSegs = <||>;
            StockAnalyzer`Visualization`showAllEdges = False;
            StockAnalyzer`Visualization`lastClickedCandidates = {};
            StockAnalyzer`Visualization`lastSelectedSegKey = None;
            StockAnalyzer`Visualization`lastSegmentClicked = None
          ],
          Spacer[8],
          Button["Show All Edges",
            StockAnalyzer`Visualization`showAllEdges = True
          ],
          Spacer[16],
          Dynamic[
            Button[
              Style["Edge Info", Bold, 10,
                If[StockAnalyzer`Visualization`showEdgeTooltip, White, GrayLevel[0.45]]],
              StockAnalyzer`Visualization`showEdgeTooltip =
                !StockAnalyzer`Visualization`showEdgeTooltip,
              Background -> Dynamic[If[StockAnalyzer`Visualization`showEdgeTooltip,
                GrayLevel[0.25], GrayLevel[0.12]]],
              FrameMargins -> 4
            ],
            TrackedSymbols :> {StockAnalyzer`Visualization`showEdgeTooltip}
          ],
          Spacer[16],
          Dynamic[
            Button[
              Style["Markov", Bold, 10,
                If[showMarkovMatrix, White, GrayLevel[0.45]]],
              showMarkovMatrix = !showMarkovMatrix,
              Background -> Dynamic[If[showMarkovMatrix,
                GrayLevel[0.25], GrayLevel[0.12]]],
              FrameMargins -> 4
            ],
            TrackedSymbols :> {showMarkovMatrix}
          ],
          Spacer[16],
          Dynamic[
            Column[{
              Row[Map[Function[name,
                Row[{Spacer[4], Button[
                  Style[name, Bold, 9,
                    If[MemberQ[StockAnalyzer`Visualization`showEdgeTypes, name],
                      White, GrayLevel[0.45]]],
                  If[MemberQ[StockAnalyzer`Visualization`showEdgeTypes, name],
                    StockAnalyzer`Visualization`showEdgeTypes =
                      DeleteCases[StockAnalyzer`Visualization`showEdgeTypes, name],
                    StockAnalyzer`Visualization`showEdgeTypes =
                      Append[StockAnalyzer`Visualization`showEdgeTypes, name]],
                  Background -> If[MemberQ[StockAnalyzer`Visualization`showEdgeTypes, name],
                    ColorData["Aquamarine"][0.5], GrayLevel[0.12]],
                  FrameMargins -> 3]}]],
                {"MaxU", "UpInner", "Support", "InnerUp", "OuterUp"}]],
              Row[Map[Function[name,
                Row[{Spacer[4], Button[
                  Style[name, Bold, 9,
                    If[MemberQ[StockAnalyzer`Visualization`showEdgeTypes, name],
                      White, GrayLevel[0.45]]],
                  If[MemberQ[StockAnalyzer`Visualization`showEdgeTypes, name],
                    StockAnalyzer`Visualization`showEdgeTypes =
                      DeleteCases[StockAnalyzer`Visualization`showEdgeTypes, name],
                    StockAnalyzer`Visualization`showEdgeTypes =
                      Append[StockAnalyzer`Visualization`showEdgeTypes, name]],
                  Background -> If[MemberQ[StockAnalyzer`Visualization`showEdgeTypes, name],
                    ColorData["SolarColors"][0.5], GrayLevel[0.12]],
                  FrameMargins -> 3]}]],
                {"MaxD", "DownInner", "Resistance", "InnerDown", "OuterDown"}]]
            }, Alignment -> Left],
            TrackedSymbols :> {StockAnalyzer`Visualization`showEdgeTypes}
          ],
          Spacer[16],
          Dynamic[
            Column[{
              Row[{Style["Dir: ", Bold, 9, GrayLevel[0.6]], Spacer[4],
                Button[Style["UP", Bold, 9,
                  If[MemberQ[StockAnalyzer`Visualization`showDirections,"U"],
                    White, GrayLevel[0.45]]],
                  If[MemberQ[StockAnalyzer`Visualization`showDirections,"U"],
                    StockAnalyzer`Visualization`showDirections =
                      DeleteCases[StockAnalyzer`Visualization`showDirections,"U"],
                    StockAnalyzer`Visualization`showDirections =
                      Append[StockAnalyzer`Visualization`showDirections,"U"]],
                  Background -> If[MemberQ[StockAnalyzer`Visualization`showDirections,"U"],
                    ColorData["Aquamarine"][0.6], GrayLevel[0.12]],
                  FrameMargins -> 3],
                Spacer[4],
                Button[Style["DOWN", Bold, 9,
                  If[MemberQ[StockAnalyzer`Visualization`showDirections,"D"],
                    White, GrayLevel[0.45]]],
                  If[MemberQ[StockAnalyzer`Visualization`showDirections,"D"],
                    StockAnalyzer`Visualization`showDirections =
                      DeleteCases[StockAnalyzer`Visualization`showDirections,"D"],
                    StockAnalyzer`Visualization`showDirections =
                      Append[StockAnalyzer`Visualization`showDirections,"D"]],
                  Background -> If[MemberQ[StockAnalyzer`Visualization`showDirections,"D"],
                    ColorData["SolarColors"][0.6], GrayLevel[0.12]],
                  FrameMargins -> 3]
              }],
              Row[{Style["From: ", Bold, 9, GrayLevel[0.6]], Spacer[4],
                Row[Map[Function[f, Row[{Spacer[4], Button[
                  Style[f, Bold, 9,
                    If[MemberQ[StockAnalyzer`Visualization`showFromFields,f],
                      White, GrayLevel[0.45]]],
                  If[MemberQ[StockAnalyzer`Visualization`showFromFields,f],
                    StockAnalyzer`Visualization`showFromFields =
                      DeleteCases[StockAnalyzer`Visualization`showFromFields,f],
                    StockAnalyzer`Visualization`showFromFields =
                      Append[StockAnalyzer`Visualization`showFromFields,f]],
                  Background -> If[MemberQ[StockAnalyzer`Visualization`showFromFields,f],
                    GrayLevel[0.35], GrayLevel[0.12]],
                  FrameMargins -> 3]}]],
                  {"Open","High","Low","Close"}]]
              }],
              Row[{Style["To:   ", Bold, 9, GrayLevel[0.6]], Spacer[4],
                Row[Map[Function[f, Row[{Spacer[4], Button[
                  Style[f, Bold, 9,
                    If[MemberQ[StockAnalyzer`Visualization`showToFields,f],
                      White, GrayLevel[0.45]]],
                  If[MemberQ[StockAnalyzer`Visualization`showToFields,f],
                    StockAnalyzer`Visualization`showToFields =
                      DeleteCases[StockAnalyzer`Visualization`showToFields,f],
                    StockAnalyzer`Visualization`showToFields =
                      Append[StockAnalyzer`Visualization`showToFields,f]],
                  Background -> If[MemberQ[StockAnalyzer`Visualization`showToFields,f],
                    GrayLevel[0.35], GrayLevel[0.12]],
                  FrameMargins -> 3]}]],
                  {"Open","High","Low","Close"}]]
              }]
            }, Alignment -> Left],
            TrackedSymbols :> {
              StockAnalyzer`Visualization`showDirections,
              StockAnalyzer`Visualization`showFromFields,
              StockAnalyzer`Visualization`showToFields}
          ]
        }]
      }, Spacings -> 0]
    ]
  ];
  
        
(* Wrapper adding peak/valley markers \[LongDash] scrollbar off by default *)
Clear[displayCandlestickGraph];

displayCandlestickGraph[featureSetArg_, chartKey_String : "default",
    opts : OptionsPattern[]] :=
  Module[{fs = featureSetArg, evaluatedLines},
    If[!KeyExistsQ[$overlays, chartKey],
      $overlays[chartKey] = {}
    ];
    evaluatedLines = {
      {AbsolutePointSize[4], Red,
       Point[Map[{#["Index"], #["Low"]} &,  Normal[fs["Valleys"]]]]},
      {AbsolutePointSize[4], Blue,
       Point[Map[{#["Index"], #["High"]} &, Normal[fs["Peaks"]]]]}
    };
    
	  makeGraphDisplay[
	    fs,
	    fs["Candlesticks"],
	    Lines        -> {evaluatedLines, Dynamic[$overlays[chartKey]]},
	    ShowScrollbar -> False,
        ScalingFunctions  -> OptionValue[ScalingFunctions],    
	    Sequence @@ FilterRules[{opts}, Except[Lines]]
	  ]
  ];
  
 makeGraphDisplayWithSegmentPicker[featureSetArg_, opts___?OptionQ] :=
  Module[{chartExpr, segmentsAssoc, rows, pickerDataset},

    (* The chart \[LongDash] watcher is already wired inside its Graphics blocks *)
    chartExpr = makeGraphDisplay[featureSetArg, opts];

    (* Build the picker rows from EdgeSegments *)
    segmentsAssoc = Association[Normal[featureSetArg["Segments"]]];

    rows =
      KeyValueMap[
        Function[{key, seg},
          With[{thisSeg = seg},
            <|
              "Pick" ->
                Button["Show",
                  StockAnalyzer`Visualization`lastSegmentClicked = thisSeg,
                  Appearance -> "Palette",
                  ImageSize -> {55, 22}],
              "From"        -> seg["FromIndex"],
              "To"          -> seg["ToIndex"],
              "Dir"         -> seg["Direction"],
              "EdgeType"    -> seg["EdgeType"],
              "SegmentType" -> seg["SegmentType"],
              "FromField"   -> seg["FromField"],
              "ToField"     -> seg["ToField"]
            |>
          ]
        ],
        segmentsAssoc
      ];

    pickerDataset = Dataset[rows];

    Column[{
      chartExpr,
      Style["Segments \[LongDash] click Show to display", Bold, 12],
      Pane[pickerDataset,
        ImageSize   -> {Automatic, 350},
        ImageSizeAction -> "Scrollable", Scrollbars  -> {False, True}]
    }, Spacings -> 1]
  ]; 
  
(* A table with its own vertical slider (a scrollbar made of a Slider, because the scrollbars of a Pane did not scroll inside the display):
   scrollGrid[gridOptions, header, rows, nVisible] shows nVisible rows of rows (the expression is evaluated again whenever what it depends on changes, for
   example the "Alive at candle" field), starting at the row that the slider chooses; the slider is only there when there are more rows than that. *)
SetAttributes[scrollGrid, HoldRest];
Clear[scrollGrid];
scrollGrid[gridOptions_List, header_, rowsExpr_, nVisible_Integer] :=
  DynamicModule[{top = 1},
    Style[Row[{
      Dynamic[
        Module[{rows = rowsExpr, len, first},
          len   = Length[rows];
          first = Max[1, Min[top, Max[1, len - nVisible + 1]]];
          Grid[Prepend[If[len == 0, {}, Take[rows, {first, Min[len, first + nVisible - 1]}]], header], Sequence @@ gridOptions]]],
      Spacer[6],
      Dynamic[
        Module[{len = Length[rowsExpr], m},
          m = Max[1, len - nVisible + 1];
          If[len <= nVisible,
            Spacer[0],
            Column[{
              Style[ToString[len] <> " rows", 9, GrayLevel[0.7]],
              (* a vertical slider has its smallest value at the bottom, so the value is minus the first visible row *)
              Slider[Dynamic[-Min[top, m], (top = -#) &], {-m, -1, 1}, Appearance -> "Vertical", ImageSize -> {16, nVisible * 28}]}]]]]
    }, Alignment -> Top], LineBreakWithin -> False]];

displayCandlestickGraphWithSegmentPicker[featureSetArg_,
    chartKey_String : "default", opts : OptionsPattern[]] :=
  Module[{chartExpr, rowsAll, rowsList, srRows, lastIndex, trendlines, headerRow, dataRows, pickerOpts,
          srHeader, srEntries, srPanel, storeKey, candleRows, patternRowsAll, patternHeader, patternEntries,
          harmonicRowsAll, harmonicHeader, harmonicEntries, harmonicPanel},

    chartExpr = displayCandlestickGraph[featureSetArg, chartKey, opts];

    rowsAll = SortBy[
      Normal[featureSetArg["Segments"]],
      {#["FromIndex"] &, #["ToIndex"] &}
    ];
    (* The wave rows (U / D) and the support / resistance rows (Kind Res / Sup) go to two separate tables *)
    rowsList  = Select[rowsAll, ! trendlineSegmentQ[#] &];
    srRows    = Select[rowsAll, trendlineSegmentQ];
    lastIndex = Length[featureSetArg["Candlesticks"]];

    headerRow =
      Style[#, Bold, 11, White] & /@
        {"Pick", "Dir", "From", "To", "FromPrice", "ToPrice", "Range"};

    dataRows =
      Map[
        Function[seg,
          With[{thisSeg = seg},
            {
              Button["Show",
                StockAnalyzer`Visualization`lastSegmentClicked = thisSeg,
                ImageSize -> {55, 22}],
              Style[seg["Direction"],               White],
              Style[seg["FromIndex"],               White],
              Style[seg["ToIndex"],                 White],
              Style[NumberForm[seg["FromPrice"], {7, 2}], White],
              Style[NumberForm[seg["ToPrice"],   {7, 2}], White],
              Style[NumberForm[seg["Range"],     {7, 2}], White]
            }
          ]
        ],
        rowsList
      ];

    pickerOpts =
      {
        Frame      -> All,
        FrameStyle -> GrayLevel[0.35],
        Alignment  -> {{Center, Center, Right, Right, Right, Right, Right}, Center},
        Background -> {None,
                       {GrayLevel[0.22],                   (* header strip *)
                        {GrayLevel[0.08], GrayLevel[0.14]} (* zebra data rows *)}},
        Spacings   -> {1, 0.4},
        BaseStyle  -> {FontColor -> White, FontFamily -> "Helvetica", FontSize -> 11}
      };

    (* Support / resistance tables, one for the resistances and one for the supports, with one row per line alive at a
       candle. Both are controlled by the same buttons: Show / Hide puts a line on the chart (its dotted extensions
       and touches too), where it stays together with the others until Clear Support/Resistance; Select all puts every
       listed line on the chart. The field "Alive at candle" selects the rows of both tables (the default is the last
       candle, All lists every row). When the chart is built, every line valid at the last candle is selected. *)
    trendlines = If[KeyExistsQ[featureSetArg, "ValidTrendlines"], Normal[featureSetArg["ValidTrendlines"]], {}];
    (* the Show buttons read the candles and the lines from this store when they are pressed *)
    storeKey   = CreateUUID[];
    If[srRows =!= {},
      candleRows = Normal[featureSetArg["Candlesticks"]];
      $trendlineStore[storeKey] = <|"Highs" -> candleRows[[All, "High"]], "Lows" -> candleRows[[All, "Low"]], "Lines" -> trendlines|>;
      shownTrendlines = Association[shownTrendlineEntry[#, storeKey] & /@ Select[srRows, #["ToIndex"] == lastIndex &]]];
    srHeader = Style[#, Bold, 10, White] & /@ {"Pick", "Dir", "Start", "From", "Anchor", "To", "FromPrice", "ToPrice", "Slope", "LogSlope", "Touches", "Goodness"};
    srEntries = Map[
      Function[seg,
        With[{thisSeg = seg,
              sKey    = storeKey,
              key     = trendlineKey[seg]},
          {seg["ToIndex"], seg["Kind"],
           {Button[
              Dynamic[If[KeyExistsQ[StockAnalyzer`Visualization`shownTrendlines, key], "Hide", "Show"],
                TrackedSymbols :> {StockAnalyzer`Visualization`shownTrendlines}],
              toggleShownTrendline[thisSeg, sKey],
              ImageSize -> {55, 22}],
            Style[seg["Direction"],                 trendlineColor[seg]],
            Style[Lookup[seg, "StartIndex", "-"],   White],
            Style[seg["FromIndex"],                 White],
            Style[seg["AnchorIndex"],               White],
            Style[seg["ToIndex"],                   White],
            Style[NumberForm[seg["FromPrice"], {7, 2}], White],
            Style[NumberForm[seg["ToPrice"],   {7, 2}], White],
            Style[NumberForm[Lookup[seg, "Slope", 0], {6, 3}], White],
            Style[NumberForm[Lookup[seg, "LogSlope", 0], {6, 4}], White],
            Style[seg["Touches"],                   White],
            Style[Lookup[seg, "Goodness", "-"],     White]},
           seg}
        ]
      ],
      srRows];

    (* the patterns valid at each candle (key Patterns of the feature set): one row per pattern and candle, shown with the same "Alive at candle" field *)
    patternRowsAll = If[KeyExistsQ[featureSetArg, "Patterns"], Normal[featureSetArg["Patterns"]], {}];
    If[! ListQ[patternRowsAll], patternRowsAll = {}];
    patternRowsAll = SortBy[patternRowsAll, {#["StartIndex"] &, #["ToIndex"] &}];
    patternHeader  = Style[#, Bold, 10, White] & /@ {"Pick", "Pattern", "R/S", "Start", "To", "Width", "GapRatio", "Apex", "R touches", "S touches"};
    patternEntries = Map[
      Function[row,
        With[{thisRow = row, sKey = storeKey, key = patternKeyString[row]},
          {row["ToIndex"],
           {Button[
              Dynamic[If[KeyExistsQ[StockAnalyzer`Visualization`shownPatterns, key], "Hide", "Show"],
                TrackedSymbols :> {StockAnalyzer`Visualization`shownPatterns}],
              toggleShownPattern[thisRow, sKey],
              ImageSize -> {55, 22}],
            Style[row["PatternName"], White],
            Style[row["ResistanceDirection"] <> "/" <> row["SupportDirection"], White],
            Style[row["StartIndex"], White],
            Style[row["ToIndex"], White],
            Style[row["Width"], White],
            Style[NumberForm[row["GapRatio"], {4, 2}], White],
            Style[If[NumericQ[row["Apex"]], NumberForm[row["Apex"], {6, 1}], "-"], White],
            Style[Lookup[row, "ResistanceTouches", "-"], White],
            Style[Lookup[row, "SupportTouches", "-"], White]}}
        ]
      ],
      patternRowsAll];

    srPanel = If[srRows === {}, Nothing,
      With[{entries = srEntries, header = srHeader, last = lastIndex, sKey = storeKey, pEntries = patternEntries, pHeader = patternHeader},
        DynamicModule[{at = last},
          Column[{
            Style["Support / Resistance \[Dash] Show keeps a line or a pattern, with its dotted extensions, on the chart until you clear it", Bold, 12, White],
            Row[{
              Button["Clear Support/Resistance", StockAnalyzer`Visualization`Private`clearShownTrendlines[]],
              Spacer[6],
              Button["Select all",
                StockAnalyzer`Visualization`Private`selectShownTrendlines[
                  Cases[entries, {t_, _, _, s_} /; (at === All || at == t) :> s], sKey]],
              Spacer[10], Style["Alive at candle:", White], Spacer[4],
              InputField[Dynamic[at], Expression, FieldSize -> 4],
              Spacer[4], Button["Last", at = last], Button["All", at = All]}],
            (* the patterns (a resistance and a support) valid at the chosen candle *)
            Sequence @@ If[pEntries === {}, {},
              {Style["Patterns", Bold, 12, White],
               scrollGrid[{Frame      -> All,
FrameStyle -> GrayLevel[0.35],
Alignment  -> {{Center, Left, Center, Right, Right, Right, Right, Right, Right, Right}, Center},
Background -> {None, {GrayLevel[0.22], {GrayLevel[0.08], GrayLevel[0.14]}}},
Spacings   -> {1, 0.4},
BaseStyle  -> {FontColor -> White, FontFamily -> "Helvetica", FontSize -> 10}},
                pHeader, Cases[pEntries, {t_, r_} /; (at === All || at == t) :> r], 5]}],
            Style["Resistances", Bold, 12, trendlineColor[<|"Kind" -> "Res"|>]],
            scrollGrid[{Frame      -> All,
FrameStyle -> GrayLevel[0.35],
Alignment  -> {{Center, Center, Right, Right, Right, Right, Right, Right, Right, Right, Right, Right}, Center},
Background -> {None, {GrayLevel[0.22], {GrayLevel[0.08], GrayLevel[0.14]}}},
Spacings   -> {1, 0.4},
BaseStyle  -> {FontColor -> White, FontFamily -> "Helvetica", FontSize -> 10}},
                header, Cases[entries, {t_, "Res", r_, _} /; (at === All || at == t) :> r], 5],
            Style["Supports", Bold, 12, trendlineColor[<|"Kind" -> "Sup"|>]],
            scrollGrid[{Frame      -> All,
FrameStyle -> GrayLevel[0.35],
Alignment  -> {{Center, Center, Right, Right, Right, Right, Right, Right, Right, Right, Right, Right}, Center},
Background -> {None, {GrayLevel[0.22], {GrayLevel[0.08], GrayLevel[0.14]}}},
Spacings   -> {1, 0.4},
BaseStyle  -> {FontColor -> White, FontFamily -> "Helvetica", FontSize -> 10}},
                header, Cases[entries, {t_, "Sup", r_, _} /; (at === All || at == t) :> r], 5]
          }, Spacings -> 1]
        ]
      ]
    ];

    (* The harmonic patterns born at each candle (key HarmonicPatterns of the feature set, see getHarmonicPatternsBornAtLastCandlestick): a path X A B C D of the
       stock graph. The table lists the patterns born at the chosen candle (All: every pattern); Show draws the path on the chart and its subgraph under the table. *)
    harmonicHeader = Style[#, Bold, 10, White] & /@ {"Pick", "Pattern", "Dir", "Born", "X", "A", "B", "C", "D", "Viol", "BofXA", "CofAB", "DofXA"};
    harmonicRowsAll = If[KeyExistsQ[featureSetArg, "HarmonicPatterns"], Normal[featureSetArg["HarmonicPatterns"]], {}];
    If[! ListQ[harmonicRowsAll], harmonicRowsAll = {}];
    harmonicRowsAll = SortBy[harmonicRowsAll, {#["ToIndex"] &, #["Pattern"] &}];
    harmonicEntries = Map[
      Function[row,
        With[{thisRow = row, key = harmonicKeyString[row], ix = row["Indices"], fmt = Function[v, If[NumericQ[v], NumberForm[v, {4, 2}], "-"]]},
          {row["ToIndex"],
           {Button[
              Dynamic[If[KeyExistsQ[StockAnalyzer`Visualization`shownHarmonics, key], "Hide", "Show"],
                TrackedSymbols :> {StockAnalyzer`Visualization`shownHarmonics}],
              toggleShownHarmonic[thisRow],
              ImageSize -> {55, 22}],
            Style[row["Pattern"], White],
            Style[If[row["Direction"] === "Bullish", "Bull", "Bear"], If[row["Direction"] === "Bullish", RGBColor[0.35, 0.95, 0.45], RGBColor[1., 0.45, 0.7]]],
            Style[row["ToIndex"], White],
            Sequence @@ (Style[#, White] & /@ If[Length[ix] == 4, Prepend[ix, "-"], ix]),
            Style[NumberForm[row["Violation"], {4, 3}], White],
            Style[fmt[Lookup[row, "BofXA", Missing[]]], White],
            Style[fmt[Lookup[row, "CofAB", Missing[]]], White],
            Style[fmt[Lookup[row, "DofXA", Missing[]]], White]}}
        ]
      ],
      harmonicRowsAll];
    harmonicPanel = If[! KeyExistsQ[featureSetArg, "HarmonicPatterns"], Nothing,
      With[{entries = harmonicEntries, header = harmonicHeader, last = lastIndex, allRows = harmonicRowsAll},
        DynamicModule[{born = All},
          Column[{
            Style["Harmonic patterns \[Dash] a path X, A, B, C, D of the stock graph, born at the candle where its last segment ends", Bold, 12, White],
            Row[{
              Button["Clear harmonics", StockAnalyzer`Visualization`Private`clearShownHarmonics[]],
              Spacer[6],
              Button["Select all", StockAnalyzer`Visualization`Private`selectShownHarmonics[Cases[allRows, r_ /; (born === All || born == r["ToIndex"])]]],
              Spacer[10], Style["Born at candle:", White], Spacer[4],
              InputField[Dynamic[born], Expression, FieldSize -> 4],
              Spacer[4], Button["Last", born = last], Button["All", born = All]}],
            If[entries === {},
              Style["(no harmonic pattern was found in these candles)", Italic, Gray],
              scrollGrid[{Frame      -> All,
FrameStyle -> GrayLevel[0.35],
Alignment  -> {{Center, Left, Center, Right, Right, Right, Right, Right, Right, Right, Right, Right, Right}, Center},
Background -> {None, {GrayLevel[0.22], {GrayLevel[0.08], GrayLevel[0.14]}}},
Spacings   -> {1, 0.4},
BaseStyle  -> {FontColor -> White, FontFamily -> "Helvetica", FontSize -> 10}},
                header, Cases[entries, {t_, r_} /; (born === All || born == t) :> r], 8]],
            (* the last pattern shown, as a subgraph of the stock graph *)
            Dynamic[
              If[AssociationQ[StockAnalyzer`Visualization`lastHarmonic],
                Column[{
                  Style["Subgraph of the stock graph: " <> StockAnalyzer`Visualization`lastHarmonic["Pattern"] <> " " <>
                    ToString[StockAnalyzer`Visualization`lastHarmonic["Indices"]] <> " (vertices: candlestick numbers; coordinates: number, price)", 11, White],
                  StockAnalyzer`Visualization`lastHarmonic["Subgraph"]}],
                Style["Press Show on a pattern to see it on the chart and as a subgraph of the stock graph", Italic, Gray]],
              TrackedSymbols :> {StockAnalyzer`Visualization`lastHarmonic}]
          }, Spacings -> 1]
        ]
      ]
    ];

    Grid[
      {{
        chartExpr,
        Column[Flatten[{
          Style["Segments \[Dash] click Show to display", Bold, 12, White],
          With[{h = headerRow, rows = dataRows, n = If[srRows === {}, 12, 9]}, scrollGrid[pickerOpts, h, rows, n]],
          srPanel,
          harmonicPanel
        }], Spacings -> 1]
      }},
      Alignment -> {Left, Top},
      Spacings  -> {2, 0}
    ]
  ];

Clear[compareLiftFunctions];

Options[compareLiftFunctions] := Options[plainCandlestickChartWithLiftProfile];

compareLiftFunctions[liftFnsListArg_List,
    segmentArg : (_Association | Automatic) : Automatic,
    opts : OptionsPattern[]] :=
  Module[{seg, defaultStyling},
    seg = If[segmentArg === Automatic, lastSegmentClicked, segmentArg];

    If[! AssociationQ[seg] || ! KeyExistsQ[seg, "FromIndex"]
       || ! KeyExistsQ[seg, "ToIndex"],
      Return[Style[
        "compareLiftFunctions: no segment selected (lastSegmentClicked " <>
        "has Head " <> ToString[Head[seg]] <> "). Click Show on a " <>
        "segment in the chart first.",
        Red, Italic]]
    ];

    If[! ValueQ[Global`updatedFeatureSet],
      Return[Style[
        "compareLiftFunctions: Global`updatedFeatureSet is not loaded. " <>
        "Click Load first.",
        Red, Italic]]
    ];

    (* Styling defaults that match loadStockGUI's singleProfile display.
       Any option explicitly passed via opts wins over these defaults
       (Mathematica's OptionValue takes the first matching rule). *)
    defaultStyling = {
      "MinRatio"                 -> 1.0,
      "MaxRatio"                 -> If[ValueQ[Global`defaultMaxExtension],
                                        Global`defaultMaxExtension, 1.31],
      "NumFrames"                -> 32,
      "FixedYRange"              -> Automatic,
      GlyphWidth                 -> 0.35,
      ImageSize                  -> {800, 400},
      Background                 -> Black,
      UpColor                    -> Cyan,
      DownColor                  -> Yellow,
      ScalingFunctions           -> {"Linear", "Linear"},
      XAxis                      -> "Index",
      "ShowPeakValleyDots"       -> True,
      "PeakDotSize"              -> 4, "ValleyDotSize" -> 4,
      "PeakDotColor"             -> Blue, "ValleyDotColor" -> Red,
      "ArrowSize"                -> 0.005,
      "ArrowThickness"           -> 0.003,
      "RectangleBorderThickness" -> 0.0012,
      "FibonacciLineThickness"   -> 0.002,
      "FibonacciLineOpacity"     -> 0.45,
      "TouchTolerance"           -> 0.01,
      "TouchMarkerColor"         -> RGBColor[1.0, 0.5, 0.0],
      "TouchMarkerSize"          -> 6,
      "TouchPriceTypes"          -> {"Open", "High", "Low", "Close"},
      "HypotheticalMarkerSize"   -> 6,
      "TitleFontSize"            -> 16,
      "Verbose"                  -> False,
      "LiftProfileWidth"         -> 250,
      "LiftCurveColor"           -> RGBColor[0.4, 0.7, 1.0],
      "MarkerLineColor"          -> RGBColor[1.0, 0.85, 0.0],
      "ShowLiftMarkers"          -> True,
      "AnnotateFibRatios"        -> {1.0, 1.1, 1.272, 1.5, 1.618},
      "AnimationRate"            -> 10,
      "AutoPlay"                 -> False,
      "InitialFrame"             -> "MaxLift",
      "MergedAxis"               -> True,
      "SharedAxisColor"          -> GrayLevel[0.7],
      "SharedAxisThickness"      -> 0.003,
      "ExtraOnLeft"              -> 1,
      "ExtraOnRight"             -> 8
    };

    plainCandlestickChartWithLiftProfile[
      Global`updatedFeatureSet,
      seg,
      StockAnalyzer`Fibonacci`fibonacciRatios,
      "LiftFunctions" -> liftFnsListArg,
      opts,                        (* user's explicit opts win *)
      Sequence @@ defaultStyling    (* styling defaults as fallback *)
    ]
  ]; 
      
(* Scrollable wrapper \[LongDash] alias with ShowScrollbar -> True *)
Clear[displayScrollableCandlestickGraph];

displayScrollableCandlestickGraph[featureSetArg_, chartKey_String : "default",
    opts : OptionsPattern[]] :=
  Module[{fs = featureSetArg, priceRange, evaluatedLines},
    If[!KeyExistsQ[$overlays, chartKey],
      $overlays[chartKey] = {}
    ];
    evaluatedLines = {
      {AbsolutePointSize[4], Red,
       Point[Map[{#["Index"], #["Low"]} &,  Normal[fs["Valleys"]]]]},
      {AbsolutePointSize[4], Blue,
       Point[Map[{#["Index"], #["High"]} &, Normal[fs["Peaks"]]]]}
    };
    makeGraphDisplay[
      fs,
      fs["Candlesticks"],
      Lines        -> {evaluatedLines, Dynamic[$overlays[chartKey]]},
      ShowScrollbar -> True,
      Sequence @@ FilterRules[{opts}, Except[Lines]]
    ]
  ];	
  
Clear[plotCandlesticks];
plotCandlesticks[candlesticksArg_Tabular, plotCommands_List : {},
  chartType_String : "T", OptionsPattern[]] :=
  Module[{plot, ohlcvData, rows},
    rows     = Normal[candlesticksArg];
    ohlcvData = Map[
      {DateObject[DateValue[#["Date"], {"Year","Month","Day"}], Day],
       {#["Open"], #["High"], #["Low"], #["Close"], #["Volume"]}} &,
      rows
    ];
    plot = If[chartType === "I",
      InteractiveTradingChart[
        ohlcvData,
        Prolog          -> {plotCommands},
        ImageSize       -> OptionValue["ImageSize"]
      ],
      TradingChart[
        ohlcvData,
        Prolog          -> {plotCommands},
        ImageSize       -> OptionValue["ImageSize"]
      ]
    ];
    plot
  ];
  
	(* Read regime bar proportions directly from stored candlestick row *)
	regimeBarFromRow[rowArg_] :=
	  If[rowArg === None || !KeyExistsQ[rowArg, "RegimeBear"],
	    {},
	    Module[{bear, side, bull},
	      bear = rowArg["RegimeBear"];
	      side = rowArg["RegimeSideways"];
	      bull = rowArg["RegimeBull"];
	      {
	        {RGBColor[0.8, 0.2, 0.2],
	          Rectangle[{0, 0}, {bear, 1}]},
	        {RGBColor[0.55, 0.55, 0.55],
	          Rectangle[{bear, 0}, {bear + side, 1}]},
	        {RGBColor[0.2, 0.65, 0.2],
	          Rectangle[{bear + side, 0}, {1, 1}]}
	      }
	    ]
	  ];
	
	(* Build 3x3 matrix from stored RegimeM columns in a candlestick row *)
	markovMatrixFromRow[rowArg_] :=
	  If[rowArg === None || !KeyExistsQ[rowArg, "RegimeM11"],
	    N[ConstantArray[1/3., {3, 3}]],
	    {
	      {rowArg["RegimeM11"], rowArg["RegimeM12"], rowArg["RegimeM13"]},
	      {rowArg["RegimeM21"], rowArg["RegimeM22"], rowArg["RegimeM23"]},
	      {rowArg["RegimeM31"], rowArg["RegimeM32"], rowArg["RegimeM33"]}
	    }
	  ];
	
	(* Render the 3x3 Markov matrix as a colored heatmap grid *)
	markovMatrixDisplay[pArg_?MatrixQ] :=
	  Module[{states, colors},
	    states = {"Bear", "Side", "Bull"};
	    colors = Map[
	      Blend[{Darker[Red, 0.3], Yellow, Darker[Green, 0.3]}, #] &,
	      pArg, {2}
	    ];
	    Grid[
	      MapIndexed[
	        Function[{val, idx},
	          Item[
	            Style[ToString[NumberForm[val, {3, 2}]], 8,
	              FontFamily -> "Arial",
	              If[val > 0.5, White, Black]],
	            Background -> colors[[idx[[1]], idx[[2]]]]
	          ]
	        ],
	        pArg, {2}
	      ],
	      Frame -> All,
	      FrameStyle -> GrayLevel[0.3],
	      ItemSize -> {3.5, 1.2},
	      Spacings -> {0, 0}
	    ]
	  ]; 

		regimeBarDisplay[rowArg_] :=
		  If[rowArg === None || !AssociationQ[rowArg] || !KeyExistsQ[rowArg, "RegimeBear"],
		    Style["hover a candle", 9, GrayLevel[0.5]],
		    Row[{
		      regimeBar[rowArg["RegimeBear"], rowArg["RegimeSideways"], rowArg["RegimeBull"]],
		      Spacer[12],
		      Style["Bear:", 9, RGBColor[0.8, 0.2, 0.2]],
		      Style[ToString[NumberForm[rowArg["RegimeBear"],     {4,3}]] <> "  ", 9, RGBColor[0.8, 0.2, 0.2]],
		      Style["Side:", 9, GrayLevel[0.5]],
		      Style[ToString[NumberForm[rowArg["RegimeSideways"], {4,3}]] <> "  ", 9, GrayLevel[0.5]],
		      Style["Bull:", 9, RGBColor[0.2, 0.65, 0.2]],
		      Style[ToString[NumberForm[rowArg["RegimeBull"],     {4,3}]], 9, RGBColor[0.2, 0.65, 0.2]]
		    }]
  ];    
  
	Clear[plotTouchPointCounts];
	plotTouchPointCounts[segmentArg_Association,
	            touchPointsArg_List,
	            candlesticksArg_List,
	            imageSizeArg_Integer : 512] :=
  Module[{fromPrice, toPrice, direction, segColor,
          nextCandlestick, nextOpen, nextOpenLine,
          horizontalNextCandlestick, toIndex},
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

    ListPlot[
      touchPointsArg,
      PlotRange      -> Automatic,
      PlotLabel      -> Style[
        "Touch Count vs Scanned Price  " <>
        ToString[segmentArg["FromIndex"]] <> "->" <>
        ToString[segmentArg["ToIndex"]] <>
        "  " <> direction,
        Bold, 14, White
      ],
      ImageSize      -> imageSizeArg,
      PlotStyle      -> {Thick, Cyan},
      Filling        -> Axis,
      FillingStyle   -> Automatic,
      Background     -> Black,
      BaseStyle      -> {FontColor -> White},
      Frame          -> True,
      FrameLabel     -> {{"Total Touches", None}, {"Scanned Price", None}},
      GridLines      -> {Automatic, Automatic},
      PlotHighlighting -> "Dropline",
      Epilog         -> {
        horizontalNextCandlestick,
        {segColor, Dashed, Thickness[0.005],
         Line[{{fromPrice, 0}, {fromPrice, 1000}}]},
        {segColor, Dashed, Thickness[0.005],
         Line[{{toPrice, 0}, {toPrice, 1000}}]},
        {segColor, Thickness[0.005],
         Arrow[{{fromPrice, 0.08}, {toPrice, 0.08}}]},
        {Yellow, Thickness[0.002], nextOpenLine}
      }
    ]
  ];
  
      
Options[displayCandlestickGraphWithPDF] = Join[
  Options[makeGraphDisplay],
  {
    "ShowSummaryMarkers"        -> True,
    "DesiredLevelOfConfidence"  -> 0.95,        (* was "SigmaFactor" -> 0.5 *)
    "ClearDegenerateBands"      -> True,
    "CredibleMass"              -> 0.9
  }
];

displayCandlestickGraphWithPDF[
  featureSetArg_,
  targetSegmentArg_Association,
  priceTypesArg_List : {"Open", "High", "Low", "Close"},
  fibonacciRatiosArg_List : fibonacciRatios,
  toleranceArg_ : (1.0/100.0),
  chartKey_String : "default",
  opts : OptionsPattern[]
] := Module[
  {candlesticks, predictions, bands, filteredBands, pdf, summary,
   intervals, allBounds, priceMin, priceMax, priceRange, plotPriceMin,
   plotPriceMax, plotPriceSpan, candleNMin, candleNMax,
   pdfPanelStart, pdfPanelEnd, totalXMin, totalXMax,
   priceSamples, pdfSamples, pdfMaxValue,
   pdfCurve, pdfFillPolygon, separator,
   showSummaryMarkers, sigmaFactor, clearDegenerate, credibleMass,
   summaryMarkers, candleGraphic, imageSize, evaluatedOverlay},

  candlesticks       = featureSetArg["Candlesticks"];
  showSummaryMarkers = OptionValue["ShowSummaryMarkers"];
  sigmaFactor        = OptionValue["SigmaFactor"];
  clearDegenerate    = OptionValue["ClearDegenerateBands"];
  credibleMass       = OptionValue["CredibleMass"];
  imageSize          = OptionValue[ImageSize];

  (* --- 1. Compute predictions and PDF --- *)
  predictions = predictionsForSegment[
    targetSegmentArg, candlesticks,
    priceTypesArg, fibonacciRatiosArg, toleranceArg
  ];
  bands = collectBands[predictions];

  filteredBands = If[clearDegenerate,
    filterDegenerateBands[bands],
    bands
  ];

  pdf = bandsToPDF[filteredBands, sigmaFactor];

  summary = summarizeBandsPDF[bands, desiredLevelOfConfidenceInBand,
    "ClearDegenerateBands" -> clearDegenerate,
    "CredibleMass"         -> credibleMass
  ];

  (* --- 2. Compute candlestick + PDF combined price range --- *)
  intervals = If[filteredBands === {}, {}, Map[intervalBounds, filteredBands]];
  allBounds = Flatten[intervals];

  Module[{candleData, candlePrices},
    candleData   = Normal[candlesticks];
    candlePrices = Join[candleData[[All, "High"]], candleData[[All, "Low"]]];
    priceMin     = If[allBounds === {}, Min[candlePrices], Min[Min[candlePrices], Min[allBounds]]];
    priceMax     = If[allBounds === {}, Max[candlePrices], Max[Max[candlePrices], Max[allBounds]]];
    candleNMin   = Min[candleData[[All, "Index"]]];
    candleNMax   = Max[candleData[[All, "Index"]]];
  ];

  priceRange    = priceMax - priceMin;
  plotPriceMin  = priceMin - 0.05 * priceRange;
  plotPriceMax  = priceMax + 0.05 * priceRange;
  plotPriceSpan = plotPriceMax - plotPriceMin;

  (* --- 3. Compute PDF panel coordinates --- *)
  (* Candles occupy ~80% of x range, PDF panel occupies ~20% *)
  pdfPanelStart = candleNMax + 1;
  pdfPanelEnd   = pdfPanelStart + (candleNMax - candleNMin) * 0.25;
  totalXMin     = candleNMin - 0.5;
  totalXMax     = pdfPanelEnd + 0.5;

  (* --- 4. Sample the PDF in price space, map to panel x coordinates --- *)
  priceSamples = Subdivide[plotPriceMin, plotPriceMax, 500];
  pdfSamples   = Quiet[Map[pdf, priceSamples], General::munfl];
  pdfSamples   = Map[If[NumericQ[#] && # >= 0, #, 0.0] &, pdfSamples];
  pdfMaxValue  = Max[pdfSamples];
  If[pdfMaxValue <= 0, pdfMaxValue = 1.0];

  pdfCurve = Transpose[{
    Rescale[pdfSamples, {0, pdfMaxValue}, {pdfPanelStart, pdfPanelEnd}],
    priceSamples
  }];

  pdfFillPolygon = Polygon[Join[
    {{pdfPanelStart, plotPriceMin}},
    pdfCurve,
    {{pdfPanelStart, plotPriceMax}}
  ]];

  (* --- 5. Build summary markers (horizontal lines across both panels) --- *)
  summaryMarkers = If[showSummaryMarkers && !MissingQ[summary["GlobalMaximum"]],
    {
      (* Global peak: yellow thick line *)
      {Yellow, Thickness[0.0025],
        Line[{
          {totalXMin, summary["GlobalMaximum"][[2]]},
          {totalXMax, summary["GlobalMaximum"][[2]]}
        }]
      },
      (* Expected value: magenta dashed line *)
      {Magenta, Dashed, Thickness[0.0015],
        Line[{
          {totalXMin, summary["ExpectedValue"]},
          {totalXMax, summary["ExpectedValue"]}
        }]
      },
      (* Credible interval: white dashed lines *)
      {White, Dashed, Opacity[0.5], Thickness[0.0010],
        Line[{
          {totalXMin, summary["CredibleInterval"][[1]]},
          {totalXMax, summary["CredibleInterval"][[1]]}
        }],
        Line[{
          {totalXMin, summary["CredibleInterval"][[2]]},
          {totalXMax, summary["CredibleInterval"][[2]]}
        }]
      }
    },
    {}
  ];

  (* --- 6. Build the candlestick graphic via makeGraphDisplay --- *)
  candleGraphic = displayCandlestickGraph[
    featureSetArg, chartKey,
    Sequence @@ FilterRules[{opts},
      Options[makeGraphDisplay]
    ]
  ];
  
  (* --- 7. Build the PDF-only Graphics overlay --- *)
  separator = {Gray, Dashed, Thickness[0.0015],
    Line[{{pdfPanelStart - 0.5, plotPriceMin},
          {pdfPanelStart - 0.5, plotPriceMax}}]};

  evaluatedOverlay = Graphics[
    {
      summaryMarkers,
      separator,
      {Cyan, Opacity[0.35], pdfFillPolygon},
      {Cyan, Thickness[0.003], Line[pdfCurve]}
    },
    PlotRange  -> {{totalXMin, totalXMax}, {plotPriceMin, plotPriceMax}},
    ImageSize  -> imageSize,
    AspectRatio -> 1/2,
    ImagePadding -> {{45, 85}, {35, 10}},
    Background -> OptionValue[Background],
    Axes -> False
  ];

  (* --- 8. Compose: candlestick graphic on top, PDF panel beneath --- *)
  Column[{
    candleGraphic,
    Style[
      Row[{
        Spacer[10],
        "Prediction PDF for segment ",
        Style[ToString[{
          targetSegmentArg["FromIndex"], targetSegmentArg["ToIndex"]
        }], Bold, Yellow],
        Spacer[20],
        Style["Global Peak: " <> ToString[NumberForm[summary["GlobalMaximum"][[2]], {4, 2}]], Yellow],
        Spacer[15],
        Style["Expected: " <> ToString[NumberForm[summary["ExpectedValue"], {4, 2}]], Magenta],
        Spacer[15],
        Style[
          ToString[Round[100 * credibleMass]] <> "% CI: [" <>
            ToString[NumberForm[summary["CredibleInterval"][[1]], {4, 2}]] <> ", " <>
            ToString[NumberForm[summary["CredibleInterval"][[2]], {4, 2}]] <> "]",
          White
        ]
      }],
      9, FontFamily -> "Arial"
    ],
    evaluatedOverlay
  }, Spacings -> 0]
];

Clear[touchPointsForSegment];
touchPointsForSegment[touchesForSegmentArg_Association] := Map[{#["ScanPrice"], #["Touches"]} &, Normal[touchesForSegmentArg["ScanResultsTable"]]];

Options[displayGoodnessProfile] = Join[
  Options[segmentGoodnessCurve],
  {
    ImageSize    -> {200, 400},
    "PriceRange" -> Automatic
  }
];

Clear[displayGoodnessProfile];
displayGoodnessProfile[segmentArg_Association,
                        candlesticksArg_,
                        fibonacciRatiosArg_List : fibonacciRatios,
                        opts : OptionsPattern[]] :=
  Module[{curve, rotatedCurve, fromPrice, toPrice, direction, segColor,
          priceMin, priceMax, padding, maxGoodness, imgSize, priceRangeOpt,
          candles, allPrices},

    curve = segmentGoodnessCurve[segmentArg, candlesticksArg,
      fibonacciRatiosArg,
      Sequence @@ FilterRules[{opts}, Options[segmentGoodnessCurve]]
    ];
    If[curve === {}, Return[Graphics[{}, ImageSize -> OptionValue[ImageSize]]]];

    fromPrice = segmentArg["FromPrice"];
    toPrice   = segmentArg["ToPrice"];
    direction = segmentArg["Direction"];
    segColor  = If[direction === "U",
      RGBColor[0.0, 0.85, 0.0],
      RGBColor[0.85, 0.0, 0.0]
    ];

    rotatedCurve  = Map[{#[[2]], #[[1]]} &, curve];
    maxGoodness   = Max[Map[First, rotatedCurve]];
    imgSize       = OptionValue[ImageSize];
    priceRangeOpt = OptionValue["PriceRange"];

    (* Determine y-axis price range *)
    Which[
      priceRangeOpt === Automatic,
        priceMin = Min[fromPrice, toPrice];
        priceMax = Max[fromPrice, toPrice];
        padding  = 0.05 * (priceMax - priceMin);
        priceMin -= padding;
        priceMax += padding,

      priceRangeOpt === "All" || priceRangeOpt === All,
        candles = If[Head[candlesticksArg] === Tabular,
          Normal[candlesticksArg], candlesticksArg];
        allPrices = Join[Map[#["High"] &, candles], Map[#["Low"] &, candles]];
        priceMin  = Min[allPrices];
        priceMax  = Max[allPrices];
        padding   = 0.05 * (priceMax - priceMin);
        priceMin -= padding;
        priceMax += padding,

      ListQ[priceRangeOpt] && Length[priceRangeOpt] == 2,
        priceMin = priceRangeOpt[[1]];
        priceMax = priceRangeOpt[[2]],

      True,
        priceMin = Min[fromPrice, toPrice];
        priceMax = Max[fromPrice, toPrice]
    ];
    
   ListLinePlot[rotatedCurve,
	  PlotStyle    -> Directive[Cyan, Thick],
	  Filling      -> None,
	  FillingStyle -> Directive[Cyan, Opacity[0.3]],
	  Frame        -> {{False, False}, {True, False}},   (* no left, no right, bottom only *)
	  FrameStyle   -> White,
	  FrameTicks   -> {{None, None}, {Automatic, None}}, (* no side ticks *)
	  FrameLabel   -> {{None, None}, {"Goodness", None}},
	  LabelStyle   -> Directive[White, FontSize -> 10],
	  Background   -> Black,
	  GridLines    -> {
	    Automatic,
	    {
	      {fromPrice, Directive[segColor, Dashed, Thickness[0.003]]},
	      {toPrice,   Directive[segColor, Dashed, Thickness[0.003]]}
	    }
	  },
	  GridLinesStyle -> Directive[Gray, Opacity[0.3]],
	  PlotRange    -> {{0, maxGoodness * 1.05}, {priceMin, priceMax}},
	  ImageSize    -> imgSize,
	  ImagePadding -> {{5, 5}, {30, 5}},   (* minimal padding: bottom for "Goodness" label, tiny on sides *)
	  AspectRatio  -> imgSize[[2]] / imgSize[[1]],
	  Epilog       -> {
	    Text[Style["From", segColor, Bold, 9],
	         {maxGoodness * 1.02, fromPrice}, {1, 0}],
	    Text[Style["To", segColor, Bold, 9],
	         {maxGoodness * 1.02, toPrice}, {1, 0}]
	  }
	]
  ];
  
Options[fibonacciTouchMarkers] = {
  "MarkerColor"  -> RGBColor[1.0, 0.5, 0.0],
  "MarkerSize"   -> 6,
  "PriceTypes"   -> {"Open", "High", "Low", "Close"},
  "ExtraOnLeft"  -> 0,
  "ExtraOnRight" -> 0
};  
  
Clear[fibonacciTouchMarkers];
fibonacciTouchMarkers[featureSetArg_Dataset,
                       segmentArg_Association,
                       fibonacciRatiosArg_List : fibonacciRatios,
                       toleranceArg_Real : 0.01,
                       opts : OptionsPattern[]] :=
  Module[{candles, segments, priorSeg, priorFound,
          currentFromIdx, currentToIdx, currentFromPrice, currentToPrice,
          currentRange, rangeAllowedDelta, extendedFromIdx,
          extendedCandles, fibonacciPrices, markerColor, markerSize,
          priceTypes, touches,
          extraOnLeft, extraOnRight,
          scanStartIdx, scanEndIdx},

    candles = featureSetArg["Candlesticks"];
    candles = If[Head[candles] === Tabular, Normal[candles], candles];

    segments = If[KeyExistsQ[featureSetArg, "Segments"],
      featureSetArg["Segments"],
      Missing["NoSegments"]
    ];

    currentFromIdx   = segmentArg["FromIndex"];
    currentToIdx     = segmentArg["ToIndex"];
    currentFromPrice = segmentArg["FromPrice"];
    currentToPrice   = segmentArg["ToPrice"];
    currentRange     = currentToPrice - currentFromPrice;

    If[currentRange == 0, Return[{}]];

    priorSeg = If[MissingQ[segments],
      Missing["NoSegments"],
      findLongestOppositeSegmentEndingAt[segments, currentFromIdx,
        segmentArg["Direction"]]
    ];
    priorFound      = !MissingQ[priorSeg];
    extendedFromIdx = If[priorFound, priorSeg["FromIndex"], currentFromIdx];

    markerColor  = OptionValue["MarkerColor"];
    markerSize   = OptionValue["MarkerSize"];
    priceTypes   = OptionValue["PriceTypes"];
    extraOnLeft  = OptionValue["ExtraOnLeft"];
    extraOnRight = OptionValue["ExtraOnRight"];

    (* Scan range extends by extraOnLeft on the left and extraOnRight on the right, clamped to data *)
    scanStartIdx    = Max[extendedFromIdx - extraOnLeft, 1];
    scanEndIdx      = Min[currentToIdx + extraOnRight, Length[candles]];
    extendedCandles = candles[[scanStartIdx ;; scanEndIdx]];

    fibonacciPrices   = Map[currentFromPrice + # * currentRange &, fibonacciRatiosArg];
    rangeAllowedDelta = Abs[toleranceArg * currentRange];

    touches = Flatten[
      Map[
        Function[row,
          Map[
            Function[field,
              Module[{fieldPrice, closestFib, delta},
                fieldPrice = row[field];
                closestFib = First[SortBy[fibonacciPrices, Abs[# - fieldPrice] &]];
                delta      = Abs[fieldPrice - closestFib];
                If[delta <= rangeAllowedDelta,
                  {row["Index"], fieldPrice},
                  Nothing
                ]
              ]
            ],
            priceTypes
          ]
        ],
        extendedCandles
      ],
      1
    ];

    If[touches === {},
      {},
      {markerColor, AbsolutePointSize[markerSize], Point[touches]}
    ]
  ];

Options[plotLiftAndSignificance] = Join[
  Options[liftByRangeRatio],
  {ImageSize -> 700}
];

Clear[plotLiftAndSignificance];
plotLiftAndSignificance[featureSetArg_Dataset,
                         currentSegmentArg_Association,
                         fibonacciRatiosArg_List : fibonacciRatios,
                         Optional[toleranceArg_?NumericQ, 0.01],
                         opts : OptionsPattern[]] :=
  Module[{lift, details, imgSize,
          observedData, expectedData, liftData, sigData,
          fromIdx, toIdx, direction, minRatio, maxRatio,
          fromPrice, range, ratioToPrice,
          bottomTicks, topTicks, sharedFrameTicks,
          keyRatios, keyRatiosInRange},

    lift    = liftByRangeRatio[featureSetArg, currentSegmentArg,
                fibonacciRatiosArg, toleranceArg,
                Sequence @@ FilterRules[{opts}, Options[liftByRangeRatio]]];
    details = Normal[lift["ScanDetails"]];

    If[details === {},
      Return[Graphics[{}, ImageSize -> OptionValue[ImageSize]]]
    ];

    imgSize   = OptionValue[ImageSize];
    minRatio  = N[OptionValue["MinRatio"]];
    maxRatio  = N[OptionValue["MaxRatio"]];
    fromIdx   = currentSegmentArg["FromIndex"];
    toIdx     = currentSegmentArg["ToIndex"];
    direction = currentSegmentArg["Direction"];
    fromPrice = currentSegmentArg["FromPrice"];
    range     = currentSegmentArg["ToPrice"] - fromPrice;

    (* Ratio-to-price mapping *)
    ratioToPrice = Function[r, fromPrice + r * range];

    (* Data series *)
    observedData = Map[{#["RangeRatio"], #["ObservedTotal"]} &, details];
    expectedData = Map[{#["RangeRatio"], #["ExpectedTotal"]} &, details];
    liftData     = Map[{#["RangeRatio"],
                          If[MissingQ[#["Lift"]], 0, #["Lift"]]} &, details];
    sigData      = Map[{#["RangeRatio"],
                          -Log10[Max[#["Significance"], 1.*^-10]]} &, details];

    (* Key ratios to always show as ticks (classical Fibonacci) *)
    keyRatios = {1.0, 1.272, 1.618};
    keyRatiosInRange = Select[keyRatios, minRatio <= # <= maxRatio &];

    (* Bottom axis: range ratio labels *)
    (* Combine automatic divisions + key Fibonacci ratios *)
    bottomTicks = Function[{xMin, xMax},
      Module[{autoDivisions, autoTicks, keyTicks, allTicks},
        autoDivisions = FindDivisions[{xMin, xMax}, 6];
        autoTicks = Map[
          {#, Style[ToString[NumberForm[N[#], {5, 3}]], 9, FontFamily -> "Arial"]} &,
          autoDivisions];
        keyTicks = Map[
          {#, Style[ToString[NumberForm[N[#], {5, 3}]], 9, FontFamily -> "Arial",
                     Bold, Darker[Blue]]} &,
          keyRatiosInRange];
        allTicks = Join[autoTicks, keyTicks];
        (* Deduplicate \[LongDash] key ticks take priority *)
        DeleteDuplicatesBy[allTicks, First]
      ]
    ];

    (* Top axis: price labels using SAME positions as bottom *)
    topTicks = Function[{xMin, xMax},
      Module[{autoDivisions, autoTicks, keyTicks, allTicks},
        autoDivisions = FindDivisions[{xMin, xMax}, 6];
        autoTicks = Map[
          Function[r,
            {r, Style["$" <> ToString[NumberForm[ratioToPrice[r], {6, 2}]],
                       9, FontFamily -> "Arial", GrayLevel[0.3]]}
          ],
          autoDivisions];
        keyTicks = Map[
          Function[r,
            {r, Style["$" <> ToString[NumberForm[ratioToPrice[r], {6, 2}]],
                       9, FontFamily -> "Arial", Bold, Darker[Blue]]}
          ],
          keyRatiosInRange];
        allTicks = Join[autoTicks, keyTicks];
        DeleteDuplicatesBy[allTicks, First]
      ]
    ];

    (* Shared frame ticks: bottom = ratio, top = price, sides = numeric y ticks *)
    sharedFrameTicks = {{Automatic, None},
                        {bottomTicks, topTicks}};
                        
    Column[{
      Style[
        Row[{"Segment [", fromIdx, ", ", toIdx, "] Direction ", direction,
             "  |  FromPrice $", NumberForm[fromPrice, {6, 2}],
             "  |  Range $",     NumberForm[range,     {6, 2}]}],
        Bold, 12
      ],

      (* Panel 1: Observed vs Expected counts *)
      ListLinePlot[{observedData, expectedData},
        PlotStyle    -> {Directive[Blue, Thick],
                          Directive[Gray, Dashed, Thick]},
        Frame        -> True,
        FrameLabel   -> {
          {"Hit Count",                                             None},
          {"Range Ratio  (bottom)   |   Scan Price $  (top)",       None}
        },
        FrameTicks   -> sharedFrameTicks,
        PlotLabel    -> "Observed (blue) vs Expected (gray) hits",
        PlotLegends  -> {"Observed", "Expected (null)"},
        GridLines    -> {
          {{1.0,   Directive[Gray, Dashed]},
           {1.272, Directive[Gray, Dashed]},
           {1.618, Directive[Gold, Dashed, Thickness[0.003]]}
          },
          Automatic
        },
        GridLinesStyle -> Directive[Gray, Opacity[0.3]],
        Filling        -> {1 -> {2}},
        FillingStyle   -> Directive[Blue, Opacity[0.15]],
        ImageSize      -> imgSize,
        PlotRange      -> {{minRatio, maxRatio}, All}
      ],

      (* Panel 2: Lift curve *)
      ListLinePlot[liftData,
        PlotStyle    -> Directive[Purple, Thick],
        Frame        -> True,
        FrameLabel   -> {
          {"Lift (Observed/Expected)",                              None},
          {"Range Ratio  (bottom)   |   Scan Price $  (top)",       None}
        },
        FrameTicks   -> sharedFrameTicks,
        PlotLabel    -> "Lift (1.0 = random baseline)",
        GridLines    -> {
          {{1.0,   Directive[Gray, Dashed]},
           {1.272, Directive[Gray, Dashed]},
           {1.618, Directive[Gold, Dashed, Thickness[0.003]]}
          },
          {{1.0, Directive[Red, Dashed, Thickness[0.002]]}}
        },
        GridLinesStyle -> Directive[Gray, Opacity[0.3]],
        Filling        -> {1 -> {Axis, 1}},
        FillingStyle   -> Directive[Purple, Opacity[0.3]],
        ImageSize      -> imgSize,
        PlotRange      -> {{minRatio, maxRatio}, All}
      ],

      (* Panel 3: Significance -log10(p) *)
      ListLinePlot[sigData,
        PlotStyle    -> Directive[Darker[Green], Thick],
        Frame        -> True,
        FrameLabel   -> {
          {"-log10(p-value)",                                       None},
          {"Range Ratio  (bottom)   |   Scan Price $  (top)",       None}
        },
        FrameTicks   -> sharedFrameTicks,
        PlotLabel    -> "Statistical significance",
        GridLines    -> {
          {{1.0,   Directive[Gray, Dashed]},
           {1.272, Directive[Gray, Dashed]},
           {1.618, Directive[Gold, Dashed, Thickness[0.003]]}
          },
          {{-Log10[0.05],  Directive[Green, Dashed]},
           {-Log10[0.01],  Directive[Orange, Dashed]},
           {-Log10[0.001], Directive[Red, Dashed]}}
        },
        GridLinesStyle -> Directive[Gray, Opacity[0.3]],
        Filling        -> Axis,
        FillingStyle   -> Directive[Darker[Green], Opacity[0.3]],
        ImageSize      -> imgSize,
        PlotRange      -> {{minRatio, maxRatio}, All}
      ]
    }, Alignment -> Left, Spacings -> 0.5]
  ];
  
(* ============================================================================
   Group: Static Chart Rendering (no event handlers)

   plainCandlestickChart       - single static chart for a target segment
   plainCandlestickChartFrames - animation frame generator across extensionRatio
   ============================================================================ *)

Options[plainCandlestickChart] = {
  GlyphWidth               -> 0.35,
  ImageSize                -> {1200, 500},
  ImagePadding             -> {{45, 20}, {30, 10}},    (* NEW *)
  Background               -> Black,
  UpColor                  -> RGBColor[0.0,  0.85, 0.0],
  DownColor                -> RGBColor[0.85, 0.0,  0.0],
  ScalingFunctions         -> {"Linear", "Linear"},
  XAxis                    -> "Index",
  "ShowPeakValleyDots"     -> True,
  "PeakDotSize"            -> 4,
  "ValleyDotSize"          -> 4,
  "PeakDotColor"           -> Blue,
  "ValleyDotColor"         -> Red,
  "ArrowSize"              -> 0.005,
  "ArrowThickness"         -> 0.003,
  "RectangleBorderThickness" -> 0.0012,
  "FibonacciLineThickness"   -> 0.0004,
  "FibonacciLineOpacity"   -> 0.45,
  "TouchTolerance"         -> (1.0 / 100.0),
  "TouchMarkerColor"       -> RGBColor[1.0, 0.5, 0.0],
  "TouchMarkerSize"        -> 6,
  "TouchPriceTypes"        -> {"Open", "High", "Low", "Close"},
  "HypotheticalMarkerSize" -> 6,
  "TitleFontSize"          -> 9,
  "FixedYRange"            -> Automatic,
  "ExtraOnLeft"            -> 0,
  "ExtraOnRight"           -> 0,
  "Verbose"                -> False
};

Clear[plainCandlestickChart];
plainCandlestickChart[featureSetArg_Dataset,
                       targetSegmentArg_Association,
                       fibonacciRatiosArg_List : fibonacciRatios,
                       Optional[extensionRatioArg_?NumericQ, 1.0],
                       opts : OptionsPattern[]] :=
  Module[{cs, allCandles, totalCandles,
          curFromIdx, curToIdx, curFromPrice, curToPrice, curDirection,
          curRange,
          effectiveRatio, effectiveRange, effectiveToPrice,
          extendedArrowToPrice,
          segments, priorSeg, priorFound,
          priorFromIdx, priorToIdx, priorFromPrice, priorToPrice, priorDirection,
          startIdx, endIdx, visData,
          nMin, nMax, plotXMin, plotXMax,
          allPrices, priceMin, priceMax, priceRange,
          candleGlyphs, peakValleyDots,
          currentArrow, priorArrow, extendedArrow,
          rectangleBorder, fibonacciLines,
          fibPrices, halfBand,
          touchPoints, touchMarkers,
          hypotheticalHit, hypotheticalMarker,
          coverage, hitProbability,
          totalPrices, observedHits, expectedHits, hypotheticalBonus,
          totalObserved, liftValue, touchCountsForDisplay,
          curColor, priorColor,
          tickFn,
          halfWidth, imageSize, background, upColor, downColor,
          xAxis, showDots, peakSize, valleySize, peakColor, valleyColor,
          arrowSize, arrowThickness,
          rectangleBorderThickness, fibonacciLineThickness,
          fibLineOpacity,
          touchTolerance, touchMarkerColor, touchMarkerSize, touchPriceTypes,
          hypotheticalMarkerSize, titleFontSize, fixedYRange, verbose,
          extendedArrowToIdx,
          titleString, titlePrimitive,
          individualBandLengths,
          hypoMarkerPos,
          lineXLeft, lineXRight, fillLineColor,
          effectivePlotYRange, tickPriceMin, tickPriceMax,
          imagePaddingOpt,
          extraOnLeft, extraOnRight
         },

    (* --- Extract options --- *)
    halfWidth                = OptionValue[GlyphWidth];
    imageSize                = OptionValue[ImageSize];
    background               = OptionValue[Background];
    upColor                  = OptionValue[UpColor];
    downColor                = OptionValue[DownColor];
    xAxis                    = OptionValue[XAxis];
    showDots                 = OptionValue["ShowPeakValleyDots"];
    peakSize                 = OptionValue["PeakDotSize"];
    valleySize               = OptionValue["ValleyDotSize"];
    peakColor                = OptionValue["PeakDotColor"];
    valleyColor              = OptionValue["ValleyDotColor"];
    arrowSize                = OptionValue["ArrowSize"];
    arrowThickness           = OptionValue["ArrowThickness"];
    rectangleBorderThickness = OptionValue["RectangleBorderThickness"];
    fibonacciLineThickness   = OptionValue["FibonacciLineThickness"];
    fibLineOpacity           = OptionValue["FibonacciLineOpacity"];
    touchTolerance           = OptionValue["TouchTolerance"];
    touchMarkerColor         = OptionValue["TouchMarkerColor"];
    touchMarkerSize          = OptionValue["TouchMarkerSize"];
    touchPriceTypes          = OptionValue["TouchPriceTypes"];
    hypotheticalMarkerSize   = OptionValue["HypotheticalMarkerSize"];
    titleFontSize            = OptionValue["TitleFontSize"];
    fixedYRange              = OptionValue["FixedYRange"];
    verbose                  = OptionValue["Verbose"];
    imagePaddingOpt          = OptionValue[ImagePadding];
    extraOnLeft              = OptionValue["ExtraOnLeft"];
    extraOnRight             = OptionValue["ExtraOnRight"];
    (* --- Target-segment info --- *)
    curFromIdx    = targetSegmentArg["FromIndex"];
    curToIdx      = targetSegmentArg["ToIndex"];
    curFromPrice  = targetSegmentArg["FromPrice"];
    curToPrice    = targetSegmentArg["ToPrice"];
    curDirection  = targetSegmentArg["Direction"];
    curRange      = curToPrice - curFromPrice;

    (* --- Effective range for rectangle AND Fibonacci lines --- *)
    effectiveRatio       = Max[1.0, extensionRatioArg];
    effectiveRange       = curRange * effectiveRatio;
    effectiveToPrice     = curFromPrice + effectiveRange;

    (* --- Extended arrow endpoint (raw ratio, can be < 1.0) --- *)
    extendedArrowToPrice = curFromPrice + curRange * extensionRatioArg;

    (* --- Look up shortest prior opposite segment --- *)
    segments = If[KeyExistsQ[featureSetArg, "Segments"],
      featureSetArg["Segments"],
      Missing["NoSegments"]
    ];
    priorSeg = If[MissingQ[segments],
      Missing["NoSegments"],
      findLongestOppositeSegmentEndingAt[segments, curFromIdx, curDirection]
    ];
    priorFound = !MissingQ[priorSeg];
    If[priorFound,
      priorFromIdx   = priorSeg["FromIndex"];
      priorToIdx     = priorSeg["ToIndex"];
      priorFromPrice = priorSeg["FromPrice"];
      priorToPrice   = priorSeg["ToPrice"];
      priorDirection = priorSeg["Direction"];
      ,
      priorFromIdx   = curFromIdx;
      priorToIdx     = curFromIdx;
      priorFromPrice = Missing["NoPriorSegment"];
      priorToPrice   = Missing["NoPriorSegment"];
      priorDirection = Missing["NoPriorSegment"];
    ];

    (* --- Pull candles, determine display window --- *)
    cs = featureSetArg["Candlesticks"];
    cs = If[Head[cs] === Tabular, cs, Tabular[cs]];
    allCandles   = Normal[cs];
    totalCandles = Length[allCandles];

    startIdx = If[priorFound, priorFromIdx - 1, curFromIdx - 1];
    startIdx = Max[startIdx, 1];
    endIdx   = Min[curToIdx + extraOnRight, totalCandles];

    visData = Select[allCandles, startIdx <= #["Index"] <= endIdx &];

    If[visData === {},
      Return[Graphics[
        Text[Style["No candles in display window", 14, Red], {0, 0}],
        ImageSize -> imageSize
      ]]
    ];

    nMin = Min[visData[[All, "Index"]]];
    nMax = Max[visData[[All, "Index"]]];

    plotXMin = Max[1, Min[nMin, curFromIdx - extraOnLeft]] - 0.5;
    plotXMax = Min[curToIdx + extraOnRight, totalCandles] + 0.5;
    
    (* --- Fibonacci fill lines: span startIdx..endIdx, extended by extras, clamped to data --- *)
    fibPrices     = Map[curFromPrice + # * effectiveRange &, fibonacciRatiosArg];
    fillLineColor = Directive[Opacity[fibLineOpacity], curColor];
    lineXLeft     = Max[startIdx - extraOnLeft, 1];
    lineXRight    = Min[curToIdx + extraOnRight, totalCandles];
        
    (* --- Price range (natural, per-frame) --- *)
    allPrices  = Join[
      visData[[All, "High"]],
      visData[[All, "Low"]],
      {curFromPrice, curToPrice, effectiveToPrice, extendedArrowToPrice},
      If[priorFound, {priorFromPrice, priorToPrice}, {}]
    ];
    priceMin   = Min[allPrices];
    priceMax   = Max[allPrices];
    priceRange = priceMax - priceMin;

    (* --- Effective Y range: user override wins if a valid pair provided --- *)
    effectivePlotYRange = If[
      MatchQ[fixedYRange, {_?NumericQ, _?NumericQ}],
      fixedYRange,
      {priceMin - 0.05 * priceRange, priceMax + 0.05 * priceRange}
    ];

    (* --- Y tick range: use effective range so ticks match visible area --- *)
    tickPriceMin = effectivePlotYRange[[1]];
    tickPriceMax = effectivePlotYRange[[2]];

    (* --- Segment colors --- *)
    curColor = If[curDirection === "U", upColor, downColor];
    priorColor = If[priorFound,
      If[priorDirection === "U", upColor, downColor],
      GrayLevel[0.5]
    ];

    (* --- Tick function --- *)
    tickFn = If[xAxis === "Date",
      dateTickFunction[cs],
      indexTickFunction[cs]
    ];

    (* --- Candle glyphs (plain, no event handlers) --- *)
    candleGlyphs = Map[
      Function[row,
        candlestickGlyph[
          row["Index"],
          row["Open"], row["High"], row["Low"], row["Close"],
          halfWidth
        ]
      ],
      visData
    ];

    (* --- Peak/valley dots (filtered to window) --- *)
    peakValleyDots = If[showDots &&
                          KeyExistsQ[featureSetArg, "Peaks"] &&
                          KeyExistsQ[featureSetArg, "Valleys"],
      {
        {AbsolutePointSize[valleySize], valleyColor,
         Point[Map[{#["Index"], #["Low"]} &,
                   Select[Normal[featureSetArg["Valleys"]],
                          startIdx <= #["Index"] <= endIdx &]]]},
        {AbsolutePointSize[peakSize], peakColor,
         Point[Map[{#["Index"], #["High"]} &,
                   Select[Normal[featureSetArg["Peaks"]],
                          startIdx <= #["Index"] <= endIdx &]]]}
      },
      {}
    ];

    (* --- Current (target) segment arrow: SOLID --- *)
    currentArrow = {
      curColor,
      Thickness[arrowThickness],
      Arrowheads[{{arrowSize, 1}}],
      Arrow[{{curFromIdx, curFromPrice}, {curToIdx, curToPrice}}]
    };

    (* --- Prior segment arrow: DASHED --- *)
    priorArrow = If[priorFound,
      {
        priorColor,
        Thickness[arrowThickness],
        Dashed,
        Arrowheads[{{arrowSize, 1}}],
        Arrow[{{priorFromIdx, priorFromPrice}, {priorToIdx, priorToPrice}}]
      },
      {}
    ];

    (* --- Extended (hypothetical) arrow: DASHED --- *)
    extendedArrowToIdx = curToIdx + 1;
    extendedArrow = {
      curColor,
      Thickness[arrowThickness],
      Dashed,
      Arrowheads[{{arrowSize, 1}}],
      Arrow[{{curFromIdx, curFromPrice},
             {extendedArrowToIdx, extendedArrowToPrice}}]
    };

    (* --- Rectangle border: independent thickness --- *)
    rectangleBorder = {
      curColor,
      Thickness[rectangleBorderThickness],
      Line[{
        {curFromIdx, curFromPrice},
        {curToIdx,   curFromPrice},
        {curToIdx,   effectiveToPrice},
        {curFromIdx, effectiveToPrice},
        {curFromIdx, curFromPrice}
      }]
    };

    (* --- Fibonacci fill lines: independent thickness, span whole window --- *)
    fibPrices     = Map[curFromPrice + # * effectiveRange &, fibonacciRatiosArg];
    fillLineColor = Directive[Opacity[fibLineOpacity], curColor];
    lineXLeft     = startIdx;
    lineXRight    = endIdx;
    fibonacciLines = {
      fillLineColor,
      Thickness[fibonacciLineThickness],
      Map[
        Line[{{lineXLeft, #}, {lineXRight, #}}] &,
        fibPrices
      ]
    };

    (* --- Halfband and natural touch markers --- *)
    halfBand = touchTolerance * Abs[effectiveRange];
    touchPoints = Flatten[
      Map[
        Function[row,
          Module[{idx, hits},
            idx = row["Index"];
            hits = Select[
              Map[{idx, row[#]} &, touchPriceTypes],
              Function[pt,
                AnyTrue[fibPrices, Abs[pt[[2]] - #] <= halfBand &]
              ]
            ];
            hits
          ]
        ],
        visData
      ],
      1
    ];

    touchMarkers = If[Length[touchPoints] > 0,
      {touchMarkerColor, AbsolutePointSize[touchMarkerSize],
       Point[touchPoints]},
      {}
    ];

    (* --- Hypothetical touch bonus + optional marker ---
       Only reward hypothetical hits at shrunken extensions (extRatio < 1.0),
       where the hypothetical endpoint can coincidentally land on a fib line.
       At extRatio >= 1.0, the toIndex extreme touch is already counted in
       touchPoints, so adding a +1 bonus would double-count. *)
    hypotheticalHit = If[extensionRatioArg < 1.0,
      AnyTrue[fibPrices, Abs[extendedArrowToPrice - #] <= halfBand &],
      False
    ];
    hypotheticalBonus = If[hypotheticalHit, 1, 0];
    
    hypoMarkerPos = {curToIdx + 1, extendedArrowToPrice};

    hypotheticalMarker = If[TrueQ[extensionRatioArg < 1.0] && TrueQ[hypotheticalHit],
      {
        {White,
         AbsolutePointSize[hypotheticalMarkerSize + 2],
         Point[{hypoMarkerPos}]
        },
        {touchMarkerColor,
         AbsolutePointSize[hypotheticalMarkerSize],
         Point[{hypoMarkerPos}]
        }
      },
      {}
    ];

    (* --- Diagnostics --- *)
    If[verbose,
      Print["=== plainCandlestickChart diagnostics ==="];
      Print["extensionRatioArg:      ", extensionRatioArg];
      Print["effectiveRatio:         ", effectiveRatio];
      Print["effectiveRange:         ", effectiveRange];
      Print["curRange:               ", curRange];
      Print["extendedArrowToPrice:   ", extendedArrowToPrice];
      Print["halfBand:               ", halfBand];
      Print["fibPrices:              ", fibPrices];
      Print["hypotheticalHit:        ", hypotheticalHit];
      Print["hypoMarkerPos:          ", hypoMarkerPos];
      Print["fixedYRange (in):       ", fixedYRange];
      Print["effectivePlotYRange:    ", effectivePlotYRange];
    ];

    (* --- Lift computation --- *)
    individualBandLengths = Map[
      Function[f,
        Max[0, Min[f + halfBand, priceMax] - Max[f - halfBand, priceMin]]
      ],
      fibPrices
    ];
    coverage       = Total[individualBandLengths];
    hitProbability = If[priceMax > priceMin,
      coverage / (priceMax - priceMin),
      0.0
    ];

    totalPrices   = Length[visData] * Length[touchPriceTypes];
    observedHits  = Length[touchPoints];
    totalObserved = observedHits + hypotheticalBonus;
    expectedHits  = totalPrices * hitProbability;
    liftValue     = If[expectedHits > 0,
      N[totalObserved / expectedHits],
      Missing["ZeroExpected"]
    ];

    (* --- Three-count breakdown for title display --- *)
    touchCountsForDisplay = computeTouchCountsForSegmentAtExtension[
      featureSetArg, targetSegmentArg, fibonacciRatiosArg,
      extensionRatioArg, touchTolerance, touchPriceTypes, extraOnRight];

    (* --- Title as inset Text primitive inside chart --- *)
         titleString = StringJoin[
           "Lift: ",
           If[MissingQ[liftValue],
             "N/A",
             ToString[NumberForm[liftValue, {5, 3}]]
           ],
           "   pre: ",  ToString[touchCountsForDisplay["PreTouches"]],
           " | in: ",   ToString[touchCountsForDisplay["InTouches"]],
           " | post: ", ToString[touchCountsForDisplay["PostTouches"]],
           "\n observed=", ToString[totalObserved],
           
           If[hypotheticalBonus > 0,
			  "  (" <> ToString[observedHits] <> "+" <> ToString[hypotheticalBonus] <> ")",
			  ""
			],
           "     expected=", ToString[NumberForm[expectedHits, {5, 3}]],
           "     ext=",      ToString[NumberForm[N[extensionRatioArg], {5, 3}]]
         ];

    titlePrimitive = Inset[
      Style[titleString,
        FontSize   -> titleFontSize,
        FontFamily -> "Arial",
        FontColor  -> White,
        FontWeight -> Bold],
      Scaled[{0.5, 0.98}],
      {Center, Top}
    ];

    (* --- Assemble Graphics (no event handlers) --- *)
    Graphics[
      {peakValleyDots, candleGlyphs,
       priorArrow, currentArrow, extendedArrow,
       fibonacciLines, rectangleBorder,
       touchMarkers, hypotheticalMarker,
       titlePrimitive},
      Frame          -> True,
      FrameLabel     -> {{Style["Price", 10, FontFamily -> "Arial"], None},
                          {None, None}},
      FrameTicks     -> {
        {Map[{#, Style[ToString[NumberForm[N[#], {6, 2}]], 9,
                        FontFamily -> "Arial"], {0.01, 0}} &,
             FindDivisions[{tickPriceMin, tickPriceMax}, 8]],
         Map[{#, Style[ToString[NumberForm[N[#], {6, 2}]], 9,
                        FontFamily -> "Arial"], {0.01, 0}} &,
             FindDivisions[{tickPriceMin, tickPriceMax}, 8]]},
        {tickFn[nMin, nMax], None}
      },
      PlotRange         -> {{plotXMin, plotXMax}, effectivePlotYRange},
      AspectRatio       -> 1/2,
      ImageSize         -> imageSize,
      ImagePadding      -> imagePaddingOpt,
      Background        -> background,
      GridLines         -> {
        Table[{i, Directive[GrayLevel[0.25], Dotted, Thickness[0.001]]},
              {i, nMin, plotXMax}],
        Automatic
      },
      GridLinesStyle    -> Directive[GrayLevel[0.25], Dotted, Thickness[0.001]],
      FrameStyle        -> Directive[GrayLevel[0.5], Thickness[0.002]],
      BaseStyle         -> {FontColor -> White, FontSize -> 9, FontFamily -> "Arial"},
      ContentSelectable -> False,
      ScalingFunctions  -> OptionValue[ScalingFunctions]
    ]
  ];

(* Delayed option assignment (:=) so that Options[plainCandlestickChart]  *)
(* is evaluated when Options[plainCandlestickChartFrames] is queried,      *)
(* avoiding load-order dependency.                                          *)

Options[computeLiftAtRatio] = {
  "LiftFunction" -> Automatic
};

Clear[computeLiftAtRatio];
computeLiftAtRatio[featureSetArg_Dataset,
                    targetSegmentArg_Association,
                    fibonacciRatiosArg_List,
                    extensionRatioArg_?NumericQ,
                    touchToleranceArg_?NumericQ,
                    touchPriceTypesArg_List,
                    opts : OptionsPattern[]] :=
  Module[{liftFn, touchCounts, allCandles,
          curFromIdx, curToIdx, curFromPrice, curToPrice, curDirection,
          curRange, segRange, scanRange,
          preTouches, inTouches, postTouches,
          preCandleCount, inCandleCount, postCandleCount,
          liftArgs, observedHits, expectedHits, liftValue},

    liftFn = OptionValue["LiftFunction"];
    If[liftFn === Automatic, liftFn = $defaultLiftFunction];

    allCandles = Normal[featureSetArg["Candlesticks"]];

    touchCounts = computeTouchCountsForSegmentAtExtension[
      featureSetArg, targetSegmentArg, fibonacciRatiosArg,
      extensionRatioArg, touchToleranceArg, touchPriceTypesArg, 0];

    curFromIdx    = targetSegmentArg["FromIndex"];
    curToIdx      = targetSegmentArg["ToIndex"];
    curFromPrice  = targetSegmentArg["FromPrice"];
    curToPrice    = targetSegmentArg["ToPrice"];
    curDirection  = targetSegmentArg["Direction"];
    curRange      = curToPrice - curFromPrice;
    segRange      = curRange;
    scanRange     = extensionRatioArg * segRange;

    preTouches       = touchCounts["PreTouches"];
    inTouches        = touchCounts["InTouches"];
    postTouches      = touchCounts["PostTouches"];
    preCandleCount   = touchCounts["PreCandleCount"];
    inCandleCount    = touchCounts["InCandleCount"];
    postCandleCount  = touchCounts["PostCandleCount"];

    liftArgs = Association[
      "PreTouches"      -> preTouches,
      "InTouches"       -> inTouches,
      "PostTouches"     -> postTouches,
      "PreCandleCount"  -> preCandleCount,
      "InCandleCount"   -> inCandleCount,
      "PostCandleCount" -> postCandleCount,
      "ExtRatio"        -> extensionRatioArg,
      "ScanRange"       -> scanRange,
      "SegRange"        -> segRange,
      "FromPrice"       -> curFromPrice,
      "FromIdx"         -> curFromIdx,
      "ToIdx"           -> curToIdx,
      "ScanStartIdx"    -> curFromIdx - preCandleCount,
      "ScanEndIdx"      -> curToIdx + postCandleCount,
      "Tolerance"       -> touchToleranceArg,
      "FibRatios"       -> fibonacciRatiosArg,
      "PriceTypes"      -> touchPriceTypesArg,
      "Candles"         -> allCandles,
      "FibTouches"      -> <||>,
      "AllTouchDetails" -> {},
      "FeatureSet"      -> featureSetArg,
      "Segment"         -> targetSegmentArg
    ];

    {observedHits, expectedHits, liftValue} = liftFn[liftArgs];

    liftValue
  ];
  
Options[plainCandlestickChartFrames] := Join[
  Options[plainCandlestickChart],
  {
    "MinRatio"      -> 1.0,
    "MaxRatio"      -> If[ValueQ[Global`defaultMaxExtension], Global`defaultMaxExtension, 1.31],
    "NumFrames"     -> 256
  }
];

Clear[plainCandlestickChartFrames];
plainCandlestickChartFrames[featureSetArg_Dataset,
                             targetSegmentArg_Association,
                             fibonacciRatiosArg_List : fibonacciRatios,
                             opts : OptionsPattern[]] :=
  Module[{minRatio, maxRatio, nFrames, ratios,
          userFixedYRange, resolvedYRange,
          curFromPrice, curRange,
          endpointsEffective, endpointsExtended, allEndpoints,
          segments, priorSeg, priorFromIdx, priorToIdx,
          priorFromPrice, priorToPrice,
          allCandles, totalCandles, startIdx, endIdx, visData,
          rangeAllPrices, yLo, yHi,
          plainOpts, timerStart, frames, elapsedTime,
          liftValues, touchTolerance, touchPriceTypes},

    minRatio = N[OptionValue["MinRatio"]];
    maxRatio = N[OptionValue["MaxRatio"]];
    nFrames  = OptionValue["NumFrames"];
    ratios   = Subdivide[minRatio, maxRatio, nFrames - 1];

    (* Options we'll need for the parallel lift computation *)
    touchTolerance  = OptionValue["TouchTolerance"];
    touchPriceTypes = OptionValue["TouchPriceTypes"];

    (* --- Resolve the FixedYRange option --- *)
    userFixedYRange = OptionValue["FixedYRange"];
    resolvedYRange = Which[
      MatchQ[userFixedYRange, {_?NumericQ, _?NumericQ}],
        userFixedYRange,
      userFixedYRange === True || userFixedYRange === Auto,
        Module[{},
          curFromPrice = targetSegmentArg["FromPrice"];
          curRange     = targetSegmentArg["ToPrice"] - curFromPrice;
          endpointsEffective = Map[curFromPrice + curRange * Max[1.0, #] &, ratios];
          endpointsExtended  = Map[curFromPrice + curRange * # &, ratios];
          allEndpoints = Join[endpointsEffective, endpointsExtended];
          segments = If[KeyExistsQ[featureSetArg, "Segments"],
            featureSetArg["Segments"], Missing["NoSegments"]];
          priorSeg = If[MissingQ[segments], Missing["NoPrior"],
            findLongestOppositeSegmentEndingAt[segments,
              targetSegmentArg["FromIndex"], targetSegmentArg["Direction"]]];
          priorFromIdx   = If[MissingQ[priorSeg],
            targetSegmentArg["FromIndex"], priorSeg["FromIndex"]];
          priorToIdx     = If[MissingQ[priorSeg],
            targetSegmentArg["FromIndex"], priorSeg["ToIndex"]];
          priorFromPrice = If[MissingQ[priorSeg], curFromPrice, priorSeg["FromPrice"]];
          priorToPrice   = If[MissingQ[priorSeg], curFromPrice, priorSeg["ToPrice"]];
          allCandles   = Normal[featureSetArg["Candlesticks"]];
          totalCandles = Length[allCandles];
          startIdx     = Max[priorFromIdx - 1, 1];
          endIdx       = Min[targetSegmentArg["ToIndex"] + 1, totalCandles];
          visData      = Select[allCandles, startIdx <= #["Index"] <= endIdx &];
          rangeAllPrices = Join[
            visData[[All, "High"]], visData[[All, "Low"]], allEndpoints,
            {curFromPrice, priorFromPrice, priorToPrice}
          ];
          yLo = Min[rangeAllPrices];
          yHi = Max[rangeAllPrices];
          {yLo - 0.05 * (yHi - yLo), yHi + 0.05 * (yHi - yLo)}
        ],
      True,
        Automatic
    ];

    (* --- Forward options to plainCandlestickChart --- *)
    plainOpts = FilterRules[{opts}, Options[plainCandlestickChart]];
    plainOpts = Join[
      {"FixedYRange" -> resolvedYRange},
      FilterRules[plainOpts, Except["FixedYRange"]]
    ];

    (* --- Generate frames --- *)
    timerStart = AbsoluteTime[];
    frames = Map[
      plainCandlestickChart[featureSetArg, targetSegmentArg,
        fibonacciRatiosArg, #, Sequence @@ plainOpts] &,
      ratios
    ];
    elapsedTime = AbsoluteTime[] - timerStart;

    (* --- Compute lift value at each ratio (parallel to frame generation) --- *)
    liftValues = Map[
      Function[r,
        computeLiftAtRatio[featureSetArg, targetSegmentArg,
          fibonacciRatiosArg, r, touchTolerance, touchPriceTypes]
      ],
      ratios
    ];

    <|
      "Ratios"       -> ratios,
      "Frames"       -> frames,
      "LiftValues"   -> liftValues,
      "NumFrames"    -> nFrames,
      "MinRatio"     -> minRatio,
      "MaxRatio"     -> maxRatio,
      "TotalTime"    -> elapsedTime,
      "TimePerFrame" -> elapsedTime / nFrames,
      "YRange"       -> resolvedYRange
    |>
  ];
  
Options[plainCandlestickChartWithLiftCurve] := Join[
  Options[plainCandlestickChartFrames],
  {
    "LiftCurveHeight"     -> 180,
    "LiftCurveColor"      -> RGBColor[0.4, 0.7, 1.0],
    "MarkerLineColor"     -> RGBColor[1.0, 0.85, 0.0],
    "ShowLiftMarkers"     -> True,
    "AnnotateFibRatios"   -> {1.0, 1.272, 1.618},
    "AnimationRate"       -> 10,
    "AutoPlay"            -> False,
    "InitialFrame"        -> "MaxLift"
  }
];

Clear[plainCandlestickChartWithLiftCurve];
plainCandlestickChartWithLiftCurve[featureSetArg_Dataset,
                                    targetSegmentArg_Association,
                                    fibonacciRatiosArg_List : fibonacciRatios,
                                    opts : OptionsPattern[]] :=
  Module[{result, ratios, frames, liftValues,
          liftCurveData, imageSize, curveWidth, curveHeight,
          liftCurveColor, markerLineColor,
          annotateFibRatios, showLiftMarkers,
          minRatio, maxRatio, minLift, maxLift, liftRange,
          minPrice, maxPrice,
          fibMarkersInRange, fibPriceMarkers,
          framesOpts,
          touchTolerance, touchPriceTypes,
          curFromPrice, curRange, ratioToPrice,
          animRate, autoPlay, initialFrameOpt,
          prices,
          numericLifts, startFrame},

    (* Extract our options *)
    curveHeight       = OptionValue["LiftCurveHeight"];
    liftCurveColor    = OptionValue["LiftCurveColor"];
    markerLineColor   = OptionValue["MarkerLineColor"];
    annotateFibRatios = OptionValue["AnnotateFibRatios"];
    showLiftMarkers   = OptionValue["ShowLiftMarkers"];
    animRate          = OptionValue["AnimationRate"];
    autoPlay          = OptionValue["AutoPlay"];
    initialFrameOpt   = OptionValue["InitialFrame"];

    (* Forward remaining options to Frames *)
    framesOpts = FilterRules[{opts}, Options[plainCandlestickChartFrames]];

    (* Generate frames *)
    result = plainCandlestickChartFrames[featureSetArg, targetSegmentArg,
               fibonacciRatiosArg, Sequence @@ framesOpts];
    ratios = result["Ratios"];
    frames = result["Frames"];

    (* Compute lift values *)
    touchTolerance  = OptionValue["TouchTolerance"];
    touchPriceTypes = OptionValue["TouchPriceTypes"];
    liftValues = If[KeyExistsQ[result, "LiftValues"],
      result["LiftValues"],
      Map[
        computeLiftAtRatio[featureSetArg, targetSegmentArg,
          fibonacciRatiosArg, #, touchTolerance, touchPriceTypes] &,
        ratios
      ]
    ];

    (* Ratio-to-price conversion *)
    curFromPrice = targetSegmentArg["FromPrice"];
    curRange     = targetSegmentArg["ToPrice"] - curFromPrice;
    ratioToPrice = Function[r, curFromPrice + curRange * r];
    prices       = Map[ratioToPrice, ratios];

    (* Image size *)
    imageSize = OptionValue["ImageSize"];
    curveWidth = If[NumericQ[imageSize], imageSize,
                    If[ListQ[imageSize], imageSize[[1]], 1200]];

    (* Curve data: PRICE on x-axis, LIFT on y-axis *)
    liftCurveData = Transpose[{
      prices,
      Map[If[MissingQ[#], 0.0, N[#]] &, liftValues]
    }];

    minRatio  = N[result["MinRatio"]];
    maxRatio  = N[result["MaxRatio"]];
    minPrice  = Min[prices];
    maxPrice  = Max[prices];
    minLift   = Min[liftCurveData[[All, 2]]];
    maxLift   = Max[liftCurveData[[All, 2]]];
    liftRange = maxLift - minLift;
    If[liftRange == 0, liftRange = 1.0];

    (* Fibonacci ratios inside the scan range, with corresponding prices *)
    fibMarkersInRange = Select[annotateFibRatios, minRatio <= # <= maxRatio &];
    fibPriceMarkers   = Map[ratioToPrice, fibMarkersInRange];

    (* --- Determine initial frame --- *)
    (* Options:                                                          *)
    (*   "MaxLift"    (default) \[LongDash] frame with highest lift value          *)
    (*   "First"                \[LongDash] frame 1                                *)
    (*   "Last"                 \[LongDash] final frame                            *)
    (*   integer n              \[LongDash] that specific frame index              *)
    numericLifts = Map[If[MissingQ[#], -Infinity, N[#]] &, liftValues];
    startFrame = Which[
      initialFrameOpt === "MaxLift",
        First[Ordering[numericLifts, -1]],
      initialFrameOpt === "First",
        1,
      initialFrameOpt === "Last",
        Length[frames],
      IntegerQ[initialFrameOpt] && 1 <= initialFrameOpt <= Length[frames],
        initialFrameOpt,
      True,
        First[Ordering[numericLifts, -1]]
    ];

    Manipulate[
      Column[{
        frames[[k]],
        ListLinePlot[liftCurveData,
          PlotStyle      -> Directive[liftCurveColor, Thickness[0.004]],
          Frame          -> True,
          FrameLabel     -> {
            {Style["Lift", 10, FontFamily -> "Arial"], None},
            {Style["Projected Price", 10, FontFamily -> "Arial"], None}
          },
          FrameStyle     -> Directive[GrayLevel[0.5], Thickness[0.002]],
          BaseStyle      -> {FontColor -> White, FontSize -> 9,
                              FontFamily -> "Arial"},
          Background     -> Black,
          FrameTicks     -> {
            {Automatic, None},
            {Map[
                Function[p,
                  {p, Style[ToString[NumberForm[N[p], {6, 2}]], 9,
                             FontFamily -> "Arial"]}
                ],
                FindDivisions[{minPrice, maxPrice}, 8]
              ], None}
          },
          GridLines      -> {
            Map[{#, Directive[GrayLevel[0.35], Dashed, Thickness[0.001]]} &,
                fibPriceMarkers],
            {{1.0, Directive[GrayLevel[0.4], Dashed, Thickness[0.001]]}}
          },
          GridLinesStyle -> Directive[GrayLevel[0.25], Dotted, Thickness[0.001]],
          Filling        -> Axis,
          FillingStyle   -> Directive[liftCurveColor, Opacity[0.25]],
          PlotRange      -> {{minPrice, maxPrice},
                             {Max[0, minLift - 0.1 * liftRange],
                              maxLift + 0.15 * liftRange}},
          ImageSize      -> {curveWidth, curveHeight},
          ImagePadding   -> {{45, 20}, {30, 10}},
          Epilog         -> {
            (* Vertical marker at current price *)
            {markerLineColor, Thickness[0.003],
             Line[{{prices[[k]], Max[0, minLift - 0.1 * liftRange]},
                   {prices[[k]], maxLift + 0.15 * liftRange}}]},
            (* Dot at current point on curve *)
            {markerLineColor, AbsolutePointSize[8],
             Point[{prices[[k]], liftCurveData[[k, 2]]}]},
            (* Text annotation *)
            {White, FontFamily -> "Arial", FontSize -> 10,
             Text[Style[
                Row[{"price: $", NumberForm[N[prices[[k]]], {6, 2}],
                     "   ratio: ", NumberForm[N[ratios[[k]]], {5, 3}],
                     "   lift: ", NumberForm[liftCurveData[[k, 2]], {5, 3}]}],
                Bold],
               Scaled[{0.02, 0.95}], {Left, Top}]},
            (* Fibonacci-ratio annotations (labels near top of curve) *)
            If[showLiftMarkers,
              MapThread[
                Function[{fr, fp},
                  {GrayLevel[0.7], FontFamily -> "Arial", FontSize -> 8,
                   Text[Row[{NumberForm[N[fr], {4, 3}], " ($",
                              NumberForm[N[fp], {6, 2}], ")"}],
                     {fp, maxLift + 0.12 * liftRange}, {0, 0}]}
                ],
                {fibMarkersInRange, fibPriceMarkers}
              ],
              {}
            ]
          }
        ]
      }, Alignment -> Left, Spacings -> 0],
      {{k, startFrame, "Frame"}, 1, Length[frames], 1,
        AnimationRate    -> animRate,
        AnimationRunning -> autoPlay,
        Appearance       -> "Open"},
      ControlPlacement -> Top,
      SaveDefinitions  -> True
    ]
  ];
  
Options[plainCandlestickChartAnimation] := Join[
  Options[plainCandlestickChartFrames],
  {
    "AnimationRate" -> 10,
    "AutoPlay"      -> False,
    "InitialFrame"  -> "MaxLift"
  }
];

Clear[plainCandlestickChartAnimation];
plainCandlestickChartAnimation[featureSetArg_Dataset,
                                targetSegmentArg_Association,
                                fibonacciRatiosArg_List : fibonacciRatios,
                                opts : OptionsPattern[]] :=
  Module[{result, frames, liftValues, ratios,
          animRate, autoPlay, initialFrameOpt,
          numericLifts, startFrame,
          framesOpts,
          touchTolerance, touchPriceTypes},

    (* Extract our options *)
    animRate        = OptionValue["AnimationRate"];
    autoPlay        = OptionValue["AutoPlay"];
    initialFrameOpt = OptionValue["InitialFrame"];

    (* Forward remaining options to Frames *)
    framesOpts = FilterRules[{opts}, Options[plainCandlestickChartFrames]];

    (* Generate frames *)
    result     = plainCandlestickChartFrames[featureSetArg, targetSegmentArg,
                   fibonacciRatiosArg, Sequence @@ framesOpts];
    ratios     = result["Ratios"];
    frames     = result["Frames"];

    (* Get or compute lift values (needed for MaxLift initial-frame option) *)
    touchTolerance  = OptionValue["TouchTolerance"];
    touchPriceTypes = OptionValue["TouchPriceTypes"];
    liftValues = If[KeyExistsQ[result, "LiftValues"],
      result["LiftValues"],
      Map[
        computeLiftAtRatio[featureSetArg, targetSegmentArg,
          fibonacciRatiosArg, #, touchTolerance, touchPriceTypes] &,
        ratios
      ]
    ];

    (* --- Determine initial frame --- *)
    (* Options:                                                          *)
    (*   "MaxLift"    (default) \[LongDash] frame with highest lift value          *)
    (*   "First"                \[LongDash] frame 1                                *)
    (*   "Last"                 \[LongDash] final frame                            *)
    (*   integer n              \[LongDash] that specific frame index              *)
    numericLifts = Map[If[MissingQ[#], -Infinity, N[#]] &, liftValues];
    startFrame = Which[
      initialFrameOpt === "MaxLift",
        First[Ordering[numericLifts, -1]],
      initialFrameOpt === "First",
        1,
      initialFrameOpt === "Last",
        Length[frames],
      IntegerQ[initialFrameOpt] && 1 <= initialFrameOpt <= Length[frames],
        initialFrameOpt,
      True,
        First[Ordering[numericLifts, -1]]
    ];

    (* --- Return the Manipulate widget --- *)
    Manipulate[
      frames[[k]],
      {{k, startFrame, "Frame"}, 1, Length[frames], 1,
        AnimationRate    -> animRate,
        AnimationRunning -> autoPlay,
        Appearance       -> "Open"},
      ControlPlacement -> Top,
      SaveDefinitions  -> True
    ]
  ];

  
Options[plainCandlestickChartWithLiftProfile] := Join[
  Options[plainCandlestickChartFrames],
  {
    "LiftProfileWidth"    -> 250,
    "LiftCurveColor"      -> RGBColor[0.4, 0.7, 1.0],
    "MarkerLineColor"     -> RGBColor[1.0, 0.85, 0.0],
    "ShowLiftMarkers"     -> True,
    "AnnotateFibRatios"   -> {1.0, 1.272, 1.618},
    "AnimationRate"       -> 10,
    "AutoPlay"            -> False,
    "InitialFrame"        -> "MaxLift",
    "MergedAxis"          -> True,
    "SharedAxisColor"     -> GrayLevel[0.7],
    "SharedAxisThickness" -> 0.003,
    "LiftFunctions"       -> Automatic,
    "LiftFunctionNames"   -> Automatic,
    (* "Lift": the lift profile on the right (default). "FibHits": the sum of triangles of the extensions at which a
       Fibonacci line of the extended range hits a price of the segment (Kernel/FibHitProfile.wl, Studies/FibHitExtensions.wl);
       the triangles are x +/- TouchTolerance wide. *)
    "SideProfile"         -> "Lift"
  }
];    
  
Clear[plainCandlestickChartWithLiftProfile];
plainCandlestickChartWithLiftProfile[featureSetArg_Dataset,
                                      targetSegmentArg_Association,
                                      fibonacciRatiosArg_List : fibonacciRatios,
                                      opts : OptionsPattern[]] :=
  Module[{result, ratios, frames,
          liftFunctionsOpt, liftFunctionsList, nLiftFns,
          liftFunctionNamesOpt, liftFnNames,
          liftValuesPerFn, liftCurveDataPerFn,
          minLiftPerFn, maxLiftPerFn, liftRangePerFn,
          globalMinLift, globalMaxLift, globalLiftRange,
          curveColors, plotStyle,
          imageSize, chartWidth, chartHeight, profileWidth,
          liftCurveColor, markerLineColor,
          annotateFibRatios, showLiftMarkers,
          minRatio, maxRatio,
          curFromPrice, curRange, ratioToPrice, prices,
          minPrice, maxPrice,
          fibMarkersInRange, fibPriceMarkers,
          framesOpts,
          touchTolerance, touchPriceTypes,
          animRate, autoPlay, initialFrameOpt,
          numericLifts, startFrame,
          userFixedYRange, sharedYRange,
          resolvedYRangeForFrames,
          endpointsEffective, endpointsExtended, allEndpoints,
          segments, priorSeg, priorFromIdx, priorToIdx,
          priorFromPrice, priorToPrice,
          allCandles, totalCandles, startIdx, endIdx, visData,
          rangeAllPrices, yLo, yHi,
          mergedAxis, sharedAxisColor, sharedAxisThickness,
          chartRightPad, profileLeftPad,
          chartImagePadding,
          normalFrame, invisibleFrame,
          chartFrameStyle, profileFrameStyle,
          useHits, fhRes, fhProfile, frameVals, fhCurve, hitMarks, hitRatioList,
          sideLabel, valueSuffix, curvePlotData, hitGridLines},

    profileWidth        = OptionValue["LiftProfileWidth"];
    liftCurveColor      = OptionValue["LiftCurveColor"];
    markerLineColor     = OptionValue["MarkerLineColor"];
    annotateFibRatios   = OptionValue["AnnotateFibRatios"];
    showLiftMarkers     = OptionValue["ShowLiftMarkers"];
    animRate            = OptionValue["AnimationRate"];
    autoPlay            = OptionValue["AutoPlay"];
    initialFrameOpt     = OptionValue["InitialFrame"];
    mergedAxis          = OptionValue["MergedAxis"];
    sharedAxisColor     = OptionValue["SharedAxisColor"];
    sharedAxisThickness = OptionValue["SharedAxisThickness"];

    userFixedYRange     = OptionValue["FixedYRange"];
    touchTolerance      = OptionValue["TouchTolerance"];
    touchPriceTypes     = OptionValue["TouchPriceTypes"];

    (* --- Resolve the LiftFunctions option (Automatic -> single default) --- *)
    liftFunctionsOpt = OptionValue["LiftFunctions"];
    liftFunctionsList = Which[
      liftFunctionsOpt === Automatic, {$defaultLiftFunction},
      ListQ[liftFunctionsOpt],         liftFunctionsOpt,
      True,                             {liftFunctionsOpt}
    ];
    nLiftFns = Length[liftFunctionsList];

    (* --- Resolve the LiftFunctionNames option (Automatic -> SymbolName) --- *)
    liftFunctionNamesOpt = OptionValue["LiftFunctionNames"];
    liftFnNames = Which[
      liftFunctionNamesOpt === Automatic
        || ! ListQ[liftFunctionNamesOpt]
        || Length[liftFunctionNamesOpt] =!= nLiftFns,
        Map[Quiet @ Check[SymbolName[#], "lift"] &, liftFunctionsList],
      True,
        liftFunctionNamesOpt
    ];

    (* --- Shared Y range across all frames --- *)
    minRatio = N[OptionValue["MinRatio"]];
    maxRatio = N[OptionValue["MaxRatio"]];
    ratios   = Subdivide[minRatio, maxRatio, OptionValue["NumFrames"] - 1];

    sharedYRange = Which[
      MatchQ[userFixedYRange, {_?NumericQ, _?NumericQ}],
        userFixedYRange,
      True,
        Module[{},
          curFromPrice = targetSegmentArg["FromPrice"];
          curRange     = targetSegmentArg["ToPrice"] - curFromPrice;
          endpointsEffective = Map[curFromPrice + curRange * Max[1.0, #] &, ratios];
          endpointsExtended  = Map[curFromPrice + curRange * # &, ratios];
          allEndpoints = Join[endpointsEffective, endpointsExtended];
          segments = If[KeyExistsQ[featureSetArg, "Segments"],
            featureSetArg["Segments"], Missing["NoSegments"]];
          priorSeg = If[MissingQ[segments], Missing["NoPrior"],
            findLongestOppositeSegmentEndingAt[segments,
              targetSegmentArg["FromIndex"],
              targetSegmentArg["Direction"]]];
          priorFromIdx = If[MissingQ[priorSeg],
            targetSegmentArg["FromIndex"], priorSeg["FromIndex"]];
          priorToIdx = If[MissingQ[priorSeg],
            targetSegmentArg["FromIndex"], priorSeg["ToIndex"]];
          priorFromPrice = If[MissingQ[priorSeg],
            curFromPrice, priorSeg["FromPrice"]];
          priorToPrice = If[MissingQ[priorSeg],
            curFromPrice, priorSeg["ToPrice"]];
          allCandles   = Normal[featureSetArg["Candlesticks"]];
          totalCandles = Length[allCandles];
          startIdx     = Max[priorFromIdx - 1, 1];
          endIdx       = Min[targetSegmentArg["ToIndex"] + 1, totalCandles];
          visData      = Select[allCandles, startIdx <= #["Index"] <= endIdx &];
          rangeAllPrices = Join[
            visData[[All, "High"]], visData[[All, "Low"]],
            allEndpoints, {curFromPrice, priorFromPrice, priorToPrice}];
          yLo = Min[rangeAllPrices];
          yHi = Max[rangeAllPrices];
          {yLo - 0.05 * (yHi - yLo), yHi + 0.05 * (yHi - yLo)}
        ]
    ];

    (* --- Padding for merged vs separated axis modes --- *)
    normalFrame    = Directive[GrayLevel[0.5], Thickness[0.002]];
    invisibleFrame = Directive[Opacity[0]];

    chartRightPad     = If[TrueQ[mergedAxis], 0, 20];
    profileLeftPad    = If[TrueQ[mergedAxis], 0, 5];
    chartImagePadding = {{45, chartRightPad}, {30, 10}};

    chartFrameStyle = If[TrueQ[mergedAxis],
      {{normalFrame, invisibleFrame}, {normalFrame, normalFrame}},
      {{normalFrame, normalFrame},    {normalFrame, normalFrame}}
    ];

    profileFrameStyle = If[TrueQ[mergedAxis],
      {{Directive[sharedAxisColor, Thickness[sharedAxisThickness]], normalFrame},
       {normalFrame, normalFrame}},
      {{normalFrame, normalFrame}, {normalFrame, normalFrame}}
    ];

    (* --- Generate frames --- *)
    resolvedYRangeForFrames = sharedYRange;
    framesOpts = FilterRules[{opts}, Options[plainCandlestickChartFrames]];
    framesOpts = Join[
      {"FixedYRange" -> resolvedYRangeForFrames,
       ImagePadding  -> chartImagePadding},
      FilterRules[framesOpts, Except[{"FixedYRange", ImagePadding}]]
    ];

    result = plainCandlestickChartFrames[featureSetArg, targetSegmentArg,
               fibonacciRatiosArg, Sequence @@ framesOpts];
    ratios = result["Ratios"];

    frames = Map[
      Show[#,
        FrameStyle   -> chartFrameStyle,
        ImagePadding -> chartImagePadding
      ] &,
      result["Frames"]
    ];

    (* --- Side profile "FibHits": the sum of triangles of the Fibonacci-line hits replaces the lift --- *)
    useHits = False; fhRes = <||>; fhProfile = {}; frameVals = {};
    If[OptionValue["SideProfile"] === "FibHits"
       && DownValues[StockAnalyzer`FibHitProfile`segmentFibProfile] =!= {},
      Module[{cands, ohlc, res, interp},
        cands = SortBy[Normal[featureSetArg["Candlesticks"]], #["Index"] &];
        If[cands[[All, "Index"]] === Range[Length[cands]],
          ohlc = N[{#["Open"], #["High"], #["Low"], #["Close"]} & /@ cands];
          res = Quiet @ Check[
            StockAnalyzer`FibHitProfile`segmentFibProfile[ohlc, targetSegmentArg,
              "XMin" -> minRatio, "XMax" -> maxRatio, "Tolerance" -> touchTolerance],
            $Failed];
          If[AssociationQ[res],
            fhRes = res; fhProfile = res["Profile"];
            (* the sum at each frame: the profile is linear between its breakpoints *)
            frameVals = If[fhProfile === {}, ConstantArray[0., Length[ratios]],
              interp = Interpolation[fhProfile, InterpolationOrder -> 1];
              Map[Quiet[interp[Clip[#, {minRatio, maxRatio}]]] &, ratios]];
            useHits = True]]]];
    If[useHits, nLiftFns = 1];

    (* --- Compute lift values for each lift function --- *)
    liftValuesPerFn =
      If[useHits,
        {frameVals},
      If[nLiftFns == 1
         && liftFunctionsList[[1]] === $defaultLiftFunction
         && KeyExistsQ[result, "LiftValues"],
        {result["LiftValues"]},
        Map[
          Function[lf,
            Map[
              computeLiftAtRatio[featureSetArg, targetSegmentArg,
                fibonacciRatiosArg, #, touchTolerance, touchPriceTypes,
                "LiftFunction" -> lf] &,
              ratios]
          ],
          liftFunctionsList
        ]
      ]];

    curFromPrice = targetSegmentArg["FromPrice"];
    curRange     = targetSegmentArg["ToPrice"] - curFromPrice;
    ratioToPrice = Function[r, curFromPrice + curRange * r];
    prices       = Map[ratioToPrice, ratios];

    imageSize = OptionValue["ImageSize"];
    {chartWidth, chartHeight} = If[ListQ[imageSize],
      imageSize, {imageSize, imageSize/2}];

    (* --- Per-fn curve data + per-fn min/max/range --- *)
    liftCurveDataPerFn = Map[
      Transpose[{Map[If[MissingQ[#], 0.0, N[#]] &, #], prices}] &,
      liftValuesPerFn
    ];

    minLiftPerFn = Map[Min[#[[All, 1]]] &, liftCurveDataPerFn];
    maxLiftPerFn = Map[Max[#[[All, 1]]] &, liftCurveDataPerFn];
    liftRangePerFn = MapThread[
      If[#1 - #2 == 0, 1.0, #1 - #2] &,
      {maxLiftPerFn, minLiftPerFn}
    ];

    (* --- Global lift range so all N curves share the same lift axis --- *)
    globalMinLift   = Min[minLiftPerFn];
    globalMaxLift   = Max[maxLiftPerFn];
    globalLiftRange = If[globalMaxLift - globalMinLift == 0, 1.0,
                         globalMaxLift - globalMinLift];

    (* --- Labels, and for "FibHits" the exact curve {sum, price of the extension} and the marks of the hits --- *)
    sideLabel   = If[useHits, "Fib-line hits (sum of triangles)", "Lift"];
    valueSuffix = If[useHits, " hits", "x"];
    curvePlotData = liftCurveDataPerFn;
    hitMarks = {}; hitGridLines = {{1.0, Directive[GrayLevel[0.4], Dashed, Thickness[0.001]]}};
    If[useHits,
      fhCurve = If[fhProfile === {},
        {{0., prices[[1]]}, {0., prices[[-1]]}},
        Map[{#[[2]], curFromPrice + curRange * #[[1]]} &, fhProfile]];
      curvePlotData = {fhCurve};
      globalMinLift   = 0.;
      globalMaxLift   = Max[1., Max[fhCurve[[All, 1]]]];
      globalLiftRange = globalMaxLift;
      hitGridLines = Map[{#, Directive[GrayLevel[0.4], Dashed, Thickness[0.001]]} &,
                         Range[1, Floor[globalMaxLift]]];
      (* one small mark on the left axis at the price of every hit, coloured by its Fibonacci ratio *)
      hitRatioList = Sort[DeleteDuplicates[fhRes["Ratio"]]];
      hitMarks = MapThread[
        {ColorData[97][First[Flatten[Position[hitRatioList, #2]]]], AbsolutePointSize[5],
         Point[{0.03 * globalMaxLift, curFromPrice + curRange * #1}]} &,
        {fhRes["Extensions"], fhRes["Ratio"]}]
    ];

    (* --- Per-curve colors: default for N=1, distinct palette for N>1 --- *)
    curveColors = If[nLiftFns == 1,
      {liftCurveColor},
      Table[ColorData[97][j], {j, nLiftFns}]
    ];
    plotStyle = Map[Directive[#, Thickness[0.006]] &, curveColors];

    minPrice = Min[prices];
    maxPrice = Max[prices];

    fibMarkersInRange = Select[annotateFibRatios, minRatio <= # <= maxRatio &];
    fibPriceMarkers   = Map[ratioToPrice, fibMarkersInRange];

    (* --- startFrame driven by the first lift function's max --- *)
    numericLifts = Map[If[MissingQ[#], -Infinity, N[#]] &, liftValuesPerFn[[1]]];
    startFrame = Which[
      initialFrameOpt === "MaxLift", First[Ordering[numericLifts, -1]],
      initialFrameOpt === "First", 1,
      initialFrameOpt === "Last", Length[frames],
      IntegerQ[initialFrameOpt] && 1 <= initialFrameOpt <= Length[frames],
        initialFrameOpt,
      True, First[Ordering[numericLifts, -1]]
    ];

    Manipulate[
      (
        Global`currentAnimationFrame = k;

        Grid[{{
          frames[[k]],
          ListLinePlot[
            curvePlotData,
            PlotStyle      -> plotStyle,
            Frame          -> True,
            FrameLabel     -> {
              {None, None},
              {Style[sideLabel, 10, FontFamily -> "Arial"], None}
            },
            FrameStyle     -> profileFrameStyle,
            BaseStyle      -> {FontColor -> White, FontSize -> 9,
                                FontFamily -> "Arial"},
            Background     -> Black,
            FrameTicks     -> {{None, None}, {Automatic, None}},
            GridLines      -> {
              hitGridLines,
              Map[{#, Directive[GrayLevel[0.35], Dashed, Thickness[0.001]]} &,
                  fibPriceMarkers]
            },
            GridLinesStyle -> Directive[GrayLevel[0.25], Dotted, Thickness[0.001]],
            Filling        -> If[nLiftFns == 1 && ! useHits, Axis, None],
            FillingStyle   -> If[nLiftFns == 1 && ! useHits,
              Directive[liftCurveColor, Opacity[0.25]],
              None
            ],
            Prolog         -> If[useHits,
              {Directive[liftCurveColor, Opacity[0.25]],
               Polygon[Join[{{0., fhCurve[[1, 2]]}}, fhCurve, {{0., fhCurve[[-1, 2]]}}]]},
              {}],
            PlotRange      -> {
              {Max[0, globalMinLift - 0.1 * globalLiftRange],
               globalMaxLift + 0.15 * globalLiftRange},
              sharedYRange
            },
            AspectRatio    -> chartHeight / profileWidth,
            ImageSize      -> {profileWidth, chartHeight},
            ImagePadding   -> {{profileLeftPad, 20}, {30, 10}},
            Epilog         -> {
              (* --- Horizontal marker line at current price (shared) --- *)
              {markerLineColor, Thickness[0.003],
               Line[{{Max[0, globalMinLift - 0.1 * globalLiftRange],
                      prices[[k]]},
                     {globalMaxLift + 0.15 * globalLiftRange,
                      prices[[k]]}}]},

              (* --- "FibHits": one mark per hit at the left edge --- *)
              hitMarks,

              (* --- One point marker per curve at current frame --- *)
              MapThread[
                {#2, AbsolutePointSize[8],
                 Point[{#1[[k, 1]], prices[[k]]}]} &,
                {liftCurveDataPerFn, curveColors}
              ],

              (* --- Top-center text: price + per-curve lift value --- *)
              {White, FontFamily -> "Arial", FontSize -> 9,
               Text[Style[
                   Column[
                     Join[
                       {StringJoin["$",
                          ToString[NumberForm[N[prices[[k]]], {6, 2}]]]},
                       If[nLiftFns == 1,
                         {StringJoin[
                            ToString[NumberForm[
                              liftCurveDataPerFn[[1, k, 1]], {5, 3}]],
                            valueSuffix]},
                         MapThread[
                           Function[{name, data, color},
                             Style[
                               Row[{name, ": ",
                                    NumberForm[data[[k, 1]], {5, 3}], "x"}],
                               color, 8
                             ]
                           ],
                           {liftFnNames, liftCurveDataPerFn, curveColors}
                         ]
                       ]
                     ],
                     Alignment -> Center],
                   Bold],
                 Scaled[{0.5, 0.98}], {Center, Top}]},

              (* --- Fib ratio annotations on the right --- *)
              If[showLiftMarkers,
                MapThread[
                  Function[{fr, fp},
                    {GrayLevel[0.7], FontFamily -> "Arial", FontSize -> 8,
                     Text[NumberForm[N[fr], {4, 3}],
                       {globalMaxLift + 0.12 * globalLiftRange, fp}, {1, 0}]}
                  ],
                  {fibMarkersInRange, fibPriceMarkers}
                ],
                {}
              ]
            }
          ]
        }}, Spacings -> {0, 0}, Alignment -> {Left, Top}]
      ),
      {{k, startFrame, "Frame"}, 1, Length[frames], 1,
        AnimationRate    -> animRate,
        AnimationRunning -> autoPlay,
        Appearance       -> "Open"},
      ControlPlacement -> Top,
      SaveDefinitions  -> True
    ]
  ];
  
Clear[compareLiftFunctions];

Options[compareLiftFunctions] := Options[plainCandlestickChartWithLiftProfile];

compareLiftFunctions[liftFnsListArg_List,
    segmentArg : (_Association | Automatic) : Automatic,
    opts : OptionsPattern[]] :=
  Module[{seg},
    seg = If[segmentArg === Automatic, lastSegmentClicked, segmentArg];

    If[! AssociationQ[seg] || ! KeyExistsQ[seg, "FromIndex"]
       || ! KeyExistsQ[seg, "ToIndex"],
      Return[Style[
        "compareLiftFunctions: no segment selected (lastSegmentClicked " <>
        "has Head " <> ToString[Head[seg]] <> "). Click Show on a " <>
        "segment in the chart first.",
        Red, Italic]]
    ];

    If[! ValueQ[Global`updatedFeatureSet],
      Return[Style[
        "compareLiftFunctions: Global`updatedFeatureSet is not loaded. " <>
        "Click Load first.",
        Red, Italic]]
    ];

    plainCandlestickChartWithLiftProfile[
      Global`updatedFeatureSet,
      seg,
      StockAnalyzer`Fibonacci`fibonacciRatios,
      "LiftFunctions" -> liftFnsListArg,
      opts
    ]
  ];  
  
(* Plain (handler-free) Graph whose vertices are drawn as candlesticks.
   Vertex n is candlestick row n, placed at x = n; the glyph itself is drawn in
   absolute price coordinates. The Graph has no edges of its own: the optional
   second argument lists {fromIndex, toIndex} segments, each looked up in
   featureSet["Segments"] and drawn as an
   arrow from the segment's FromPrice to its ToPrice (up segment: low to high,
   down segment: high to low), styled like the arrows of makeGraphDisplay. *)
Clear[candlestickVertexGraph];

Options[candlestickVertexGraph] = {
  "FromIndex"     -> 1,
  "ToIndex"       -> All,
  "GlyphWidth"    -> 0.35,
  "VertexShapeFunction" -> "Glyph",
  "ShapeSize"     -> 0.2,
  "EdgeTypes"     -> All,
  "ArrowSize"     -> 0.005,
  "EdgeThickness" -> 0.0005,
  "PointLabels"   -> {},
  PlotLabel       -> None,
  "RealEdges"     -> True,
  "OwnEdges"      -> Automatic,
  "Graph"         -> Automatic,
  "BackgroundEdges" -> Automatic,
  "BackgroundStyle" -> Directive[GrayLevel[0.6], Opacity[0.6]],
  "HighlightColors" -> Automatic,
  "HighlightThickness" -> 2,
  "DateAxis"      -> True,   (* True: dates below, candlestick indices above; False: indices below only *)
  "ImageSize"     -> 800,
  "AspectRatio"   -> 0.5
};

candlestickVertexGraph[featureSetArg_, segsArg_List : {},
    opts : OptionsPattern[]] :=
  Module[{
    rows, nRows, from, to, vertices, rowOf, halfW, keepTypes,
    allSegs, wanted, found, arrows, priceLo, priceHi, pad, coords, shapeFn, shapeOpt, shapeW, shapeH, drawShape,
    matched, arrowOf, clipSeg, xr, yr, endPrices, vertexPrices,
    priceLo0, priceHi0, pad0, padFrac, sy, yTicks, graphEdges,
    groups, plainItems, graphItems, hlMode, hlColors, vcolor, showBg, bgSegs, bgArrows,
    ownMode, segLookup, edgeFn, vertexStyles, customGraph, vset, dateAxis, dateTicks, indexTicks, gapDays, dateFormat},

    rows  = Normal[featureSetArg["Candlesticks"]];
    nRows = Length[rows];
    from  = Max[1, OptionValue["FromIndex"]];
    to    = Min[nRows, Replace[OptionValue["ToIndex"], All -> nRows]];
    vertices  = Range[from, to];
    rowOf     = AssociationThread[vertices, rows[[from ;; to]]];
    (* "Graph" -> g (or the overload candlestickVertexGraph[featureSet, g, opts]): the vertices and
       edges come from g instead of featureSet["Graph"]; only the candle numbers inside the window
       that exist in featureSet["Candlesticks"] are kept as vertices *)
    customGraph = OptionValue["Graph"];
    If[GraphQ[customGraph],
      vertices = Sort[Select[VertexList[customGraph], IntegerQ[#] && from <= # <= to &]]];
    vset = Association[(# -> True) & /@ vertices];
    halfW     = OptionValue["GlyphWidth"];
    keepTypes = OptionValue["EdgeTypes"];

    (* Proportions: an explicit AspectRatio option makes Graph drop custom vertex
       shapes and a {width, height} ImageSize is only a bounding box, so the
       prices are multiplied by sy, chosen so that the plot window
       (to - from + 2 candles wide, the price range plus padding tall) has the
       requested height/width ratio, and the price axis is labelled with the
       true prices. AspectRatio -> Automatic keeps the prices (sy = 1). *)
    priceLo0 = Min[#["Low"]  & /@ Values[rowOf]];
    priceHi0 = Max[#["High"] & /@ Values[rowOf]];
    (* more room above and below when point labels are drawn *)
    padFrac  = If[OptionValue["PointLabels"] === {}, 0.05, 0.18];
    pad0     = padFrac (priceHi0 - priceLo0);
    sy = If[NumberQ[OptionValue["AspectRatio"]] && priceHi0 > priceLo0,
      OptionValue["AspectRatio"] (to - from + 2) / (priceHi0 - priceLo0 + 2 pad0), 1.];
    rowOf = Map[
      <|"Open" -> sy #["Open"], "High" -> sy #["High"],
        "Low" -> sy #["Low"], "Close" -> sy #["Close"]|> &, rowOf];

    (* featureSet["Segments"] holds the MaxU/MaxD segments; it has no EdgeType
       column, so the type is taken from Direction (U -> MaxU, D -> MaxD) *)
    allSegs = Select[Normal[featureSetArg["Segments"]], AssociationQ];
    allSegs = Map[
      If[KeyExistsQ[#, "EdgeType"], #,
        Append[#, "EdgeType" -> If[#["Direction"] === "U", "MaxU", "MaxD"]]] &,
      allSegs];
    allSegs = Map[
      Join[#, <|"FromPrice" -> sy #["FromPrice"], "ToPrice" -> sy #["ToPrice"]|>] &,
      allSegs];

    (* The second argument lists what to draw. Plain entries ({from, to} pairs or segment
       Associations) are drawn as before, in the colours of their edge types. Each Graph in
       the list (for example a match of FindIsomorphicSubgraph) is a highlighted group, as
       in HighlightGraph: its edges and vertices get a colour of their own, and the whole
       graph is drawn in gray behind them (BackgroundEdges -> Automatic). *)
    plainItems = Select[segsArg, ! GraphQ[#] &];
    graphItems = Select[segsArg, GraphQ];
    hlMode     = graphItems =!= {};
    hlColors   = Replace[OptionValue["HighlightColors"],
      Automatic :> (ColorData[97] /@ Range[Max[1, Length[graphItems]]])];
    groups = Join[
      If[plainItems === {}, {},
        {{DeleteDuplicates[
            If[AssociationQ[#], {#["FromIndex"], #["ToIndex"]}, Take[#, 2]] & /@ plainItems], None}}],
      MapIndexed[
        {DeleteDuplicates[Take[#1, 2] & /@ (List @@@ EdgeList[#1])],
         hlColors[[Mod[#2[[1]] - 1, Length[hlColors]] + 1]]} &,
        graphItems]];
    vcolor = Association[Flatten[Map[
      Function[grp, If[grp[[2]] === None, {}, (# -> grp[[2]]) & /@ Union[Flatten[grp[[1]]]]]],
      groups]]];

    (* RealEdges: the Graph carries the edges of featureSet["Graph"] (those inside the
       window), so FindIsomorphicSubgraph and the other graph functions work on it.
       With OwnEdges off they are not drawn (EdgeShapeFunction -> None); the arrows
       below are then the only drawing, one per listed segment. *)
    graphEdges = If[GraphQ[customGraph],
      Select[EdgeList[customGraph], KeyExistsQ[vset, #[[1]]] && KeyExistsQ[vset, #[[2]]] &],
      If[TrueQ[OptionValue["RealEdges"]],
      Module[{g = featureSetArg["Graph"], el},
        If[Head[g] === Dataset, g = Normal[g]];
        el = If[GraphQ[g], EdgeList[g],
          DirectedEdge @@@ DeleteDuplicates[{#["FromIndex"], #["ToIndex"]} & /@ allSegs]];
        Select[el, from <= #[[1]] <= to && from <= #[[2]] <= to &]],
      {}]];

    (* OwnEdges: the Graph draws its own edges (EdgeShapeFunction, one arrow per edge from the
       segment's FromPrice to its ToPrice), so Mathematica's own functions work on it:
       HighlightGraph[dotGraph, FindIsomorphicSubgraph[dotGraph, pattern, n]] shows the matches
       in colour over the gray graph. Automatic: on when no segment list is given. *)
    ownMode = TrueQ[Replace[OptionValue["OwnEdges"],
      Automatic :> (segsArg === {} && (GraphQ[customGraph] || TrueQ[OptionValue["RealEdges"]]))]] && graphEdges =!= {};

    (* The listed segments found in featureSet["Segments"] *)
    matched = Flatten[Map[
      Function[grp, Flatten[Map[
      Function[pair,
        Module[{matches},
          matches = Select[allSegs,
            #["FromIndex"] == pair[[1]] && #["ToIndex"] == pair[[2]] &&
            (keepTypes === All || MemberQ[keepTypes, Lookup[#, "EdgeType", None]]) &];
          (* One row per EdgeType, so identical arrows are drawn once *)
          matches = DeleteDuplicatesBy[matches,
            {#["FromPrice"], #["ToPrice"], Lookup[#, "EdgeType", None]} &];
          If[matches === {} && keepTypes === All,
            Message[candlestickVertexGraph::noseg, pair]];
          Map[Append[#, "HColor" -> grp[[2]]] &, matches]
        ]],
      grp[[1]]], 1]],
      groups], 1];

    arrowOf = Function[seg,
      Module[{et = Lookup[seg, "EdgeType", None], primary, color, hc = Lookup[seg, "HColor", None]},
        primary = MemberQ[{"MaxU", "MaxD"}, et];
        color   = If[hc =!= None, hc, edgeDirectionColor[seg]];   (* lime up, red down, yellow flat *)
        {Opacity[If[hc =!= None || primary, 1.0, 0.75]], color,
         Thickness[If[hc =!= None, OptionValue["HighlightThickness"], If[primary, 1.0, 0.75]] *
           OptionValue["EdgeThickness"]],
         Arrowheads[{{OptionValue["ArrowSize"], 1}}],
         With[{clipped = clipSeg[{{seg["FromIndex"], seg["FromPrice"]},
                                  {seg["ToIndex"],   seg["ToPrice"]}}, xr, yr]},
           Which[
             clipped === None, {},
             (* End point inside the window: arrow head at the ToIndex end *)
             clipped[[4]], Arrow[clipped[[1 ;; 2]]],
             (* End point outside the window: plain line to the plot edge *)
             True, Line[clipped[[1 ;; 2]]]]]}
      ]];

    (* Where the shape of a vertex is drawn, so that it sits at the end of
       the arrow: the ToIndex candle at its High for an up segment and at its
       Low for a down segment; the FromIndex candle at its Low for an up
       segment and at its High for a down segment. Vertices that are not
       the end of a listed segment stay at the candle's mid price. *)
    endPrices = Flatten[Map[
      Function[seg, {
        {seg["FromIndex"], If[seg["Direction"] === "U", "Low", "High"]},
        {seg["ToIndex"],   If[seg["Direction"] === "U", "High", "Low"]}}],
      matched], 1];
    vertexPrices = Map[DeleteDuplicates,
      GroupBy[Select[endPrices, KeyExistsQ[rowOf, First[#]] &],
        First -> (rowOf[First[#]][Last[#]] &)]];

    priceLo = Min[#["Low"]  & /@ Values[rowOf]];
    priceHi = Max[#["High"] & /@ Values[rowOf]];
    pad     = padFrac (priceHi - priceLo);

    (* Plot window. Epilog is not clipped to PlotRange, so an arrow reaching
       past the window would be drawn outside the frame; each arrow is clipped
       to the window here instead (Liang-Barsky). Result: {start, end,
       start kept?, end kept?} or None when the arrow lies outside. *)
    xr = {from - 1, to + 1};
    yr = {priceLo - pad, priceHi + pad};
    clipSeg = Function[{seg, xrng, yrng},
      Module[{p1 = N[seg[[1]]], p2 = N[seg[[2]]], dl, ps, qs, t0 = 0., t1 = 1., ok = True, r},
        dl = p2 - p1;
        ps = {-dl[[1]], dl[[1]], -dl[[2]], dl[[2]]};
        qs = {p1[[1]] - xrng[[1]], xrng[[2]] - p1[[1]],
              p1[[2]] - yrng[[1]], yrng[[2]] - p1[[2]]};
        Do[
          If[ps[[i]] == 0,
            If[qs[[i]] < 0, ok = False],
            r = qs[[i]]/ps[[i]];
            If[ps[[i]] < 0,
              If[r > t1, ok = False, t0 = Max[t0, r]],
              If[r < t0, ok = False, t1 = Min[t1, r]]]],
          {i, 4}];
        If[ok && t0 <= t1, {p1 + t0 dl, p1 + t1 dl, t0 == 0., t1 == 1.}, None]]];

    arrows = arrowOf /@ matched;

    (* Own edges: segment geometry per {from, to}, primary types (MaxU, MaxD) first *)
    segLookup = If[ownMode,
      Association[Reverse[Map[
        {#["FromIndex"], #["ToIndex"]} ->
          {{#["FromIndex"], #["FromPrice"]}, {#["ToIndex"], #["ToPrice"]}} &,
        SortBy[Select[allSegs,
            keepTypes === All || MemberQ[keepTypes, Lookup[#, "EdgeType", None]] &],
          ! MemberQ[{"MaxU", "MaxD"}, Lookup[#, "EdgeType", None]] &]]]],
      <||>];
    segColors = If[ownMode && ! hlMode,
      Association[Reverse[Map[
        {#["FromIndex"], #["ToIndex"]} -> edgeDirectionColor[#] &,
        SortBy[Select[allSegs,
            keepTypes === All || MemberQ[keepTypes, Lookup[#, "EdgeType", None]] &],
          ! MemberQ[{"MaxU", "MaxD"}, Lookup[#, "EdgeType", None]] &]]]],
      <||>];
    edgeFn = With[{lk = segLookup, xr0 = xr, yr0 = yr, hs = OptionValue["ArrowSize"],
                   cs = clipSeg, all = (keepTypes === All)},
      Function[{pts, e},
        With[{sg = Lookup[lk, Key[{e[[1]], e[[2]]}], None]},
          {Arrowheads[{{hs, 1}}],
           If[sg === None,
             If[all, Arrow[pts], {}],
             With[{cl = cs[sg, xr0, yr0]},
               Which[cl === None, {}, cl[[4]], Arrow[cl[[1 ;; 2]]], True, Line[cl[[1 ;; 2]]]]]]}]]];
    (* default vertex colours by candle direction; HighlightGraph overrides them for matches *)
    vertexStyles = If[ownMode,
      Map[# -> If[rowOf[#]["Close"] >= rowOf[#]["Open"], bullishColor, bearishColor] &, vertices],
      {}];

    (* The whole graph in gray behind the highlighted groups *)
    showBg = Replace[OptionValue["BackgroundEdges"], Automatic :> hlMode];
    bgSegs = If[TrueQ[showBg],
      DeleteDuplicatesBy[
        Select[allSegs, from <= #["FromIndex"] <= to && from <= #["ToIndex"] <= to &],
        {#["FromIndex"], #["ToIndex"]} &],
      {}];
    bgArrows = {OptionValue["BackgroundStyle"],
      Thickness[0.6 OptionValue["EdgeThickness"]],
      Arrowheads[{{0.6 OptionValue["ArrowSize"], 1}}],
      Map[
        Function[seg,
          With[{cl = clipSeg[{{seg["FromIndex"], seg["FromPrice"]},
                              {seg["ToIndex"],   seg["ToPrice"]}}, xr, yr]},
            Which[cl === None, {}, cl[[4]], Arrow[cl[[1 ;; 2]]], True, Line[cl[[1 ;; 2]]]]]],
        bgSegs]};

    coords = Map[# -> {#, (rowOf[#]["High"] + rowOf[#]["Low"]) / 2} &, vertices];

    shapeOpt = OptionValue["VertexShapeFunction"];
    shapeW   = OptionValue["ShapeSize"];

    (* One unit is as long up as across (the prices are scaled by sy), so a
       shape is as tall as it is wide *)
    shapeH = shapeW;

    (* Date axis: the dates below (about 8 labels, turned by 45 degrees), the candlestick indices above (about 8 labels) *)
    dateAxis = TrueQ[OptionValue["DateAxis"]] && MatchQ[rows[[1, "Date"]], _DateObject];
    If[dateAxis,
      gapDays = (AbsoluteTime[rows[[to, "Date"]]] - AbsoluteTime[rows[[from, "Date"]]]) / (86400 Max[1, to - from]);
      dateFormat = If[gapDays >= 20, {"MonthNameShort", " ", "Year"}, {"Day", " ", "MonthNameShort", " ", "Year"}];
      dateTicks = Table[{i, Rotate[Style[DateString[rows[[i, "Date"]], dateFormat], 10, FontFamily -> "Arial"], -45 Degree], {0.01, 0}, {}},
        {i, DeleteDuplicates[Round[Subdivide[from, to, Min[8, Max[to - from, 1]]]]]}];
      indexTicks = Table[{i, Style[ToString[i], 10, FontFamily -> "Arial"], {0.01, 0}, {}},
        {i, Select[FindDivisions[{from, to}, 8], IntegerQ[#] && from <= # <= to &]}]];

    (* Price axis: nice true prices at their scaled positions *)
    yTicks = Map[{sy #, #} &,
      Select[FindDivisions[{(priceLo - pad)/sy, (priceHi + pad)/sy}, 8],
        (priceLo - pad)/sy <= # <= (priceHi + pad)/sy &]];

    (* Shapes drawn directly in plot coordinates, coloured by direction
       (close >= open: up colour) *)
    drawShape = Function[{name, x, y, col},
      {EdgeForm[{Thin, GrayLevel[0.3]}], col,
       Switch[name,
         "Square",   Rectangle[{x - shapeW, y - shapeH}, {x + shapeW, y + shapeH}],
         "Circle",   Disk[{x, y}, {shapeW, shapeH}],
         "Diamond",  Polygon[{{x - shapeW, y}, {x, y + shapeH}, {x + shapeW, y},
                              {x, y - shapeH}}],
         "Triangle", Polygon[{{x - shapeW, y - shapeH}, {x + shapeW, y - shapeH},
                              {x, y + shapeH}}]]}];

    (* "Glyph": the candlestick; a name in the list below: that shape drawn at
       the end of the arrows (see vertexPrices); anything else is handed to
       Graph unchanged *)
    shapeFn = Which[
      shapeOpt === "Glyph",
        With[{r = rowOf, w = halfW, vc = vcolor, vp = vertexPrices},
          Function[{pos, v, size},
            With[{row = r[v]},
              {candlestickGlyph[pos[[1]], row["Open"], row["High"], row["Low"],
                row["Close"], w],
               (* highlighted vertex: a ring in the group's colour at the end of its arrows *)
               If[KeyExistsQ[vc, v],
                 Map[{Directive[vc[v], AbsoluteThickness[2]],
                      Circle[{pos[[1]], #}, 2 w]} &, Lookup[vp, v, {}]],
                 {}]}]]],
      MemberQ[{"Square", "Circle", "Diamond", "Triangle"}, shapeOpt],
        With[{r = rowOf, nm = shapeOpt, df = drawShape, vp = vertexPrices, vc = vcolor, hl = hlMode, om = ownMode},
          Function[{pos, v, size},
            Map[df[nm, v, #,
                Which[om, {},   (* own edges: colour comes from VertexStyle / HighlightGraph *)
                  KeyExistsQ[vc, v], vc[v],
                  hl, GrayLevel[0.5],
                  r[v]["Close"] >= r[v]["Open"], bullishColor,
                  True, bearishColor]] &,
              Lookup[vp, v, {pos[[2]]}]]]],
      True, shapeOpt];

    Graph[vertices, graphEdges,
      Sequence @@ FilterRules[{opts}, Options[Graph]],
      EdgeShapeFunction   -> If[ownMode, edgeFn, None],
      Sequence @@ If[ownMode,
        {EdgeStyle   -> Join[{Directive[OptionValue["BackgroundStyle"], Thickness[OptionValue["EdgeThickness"]]]},
           Map[# -> Directive[segColors[{#[[1]], #[[2]]}], Thickness[OptionValue["EdgeThickness"]]] &,
             Select[DeleteDuplicates[graphEdges], KeyExistsQ[segColors, {#[[1]], #[[2]]}] &]]],   (* a rule for each edge of this graph that has a segment (a rule for an edge that is not in the graph makes the Graph invalid); lime up, red down, yellow flat *)
         VertexStyle -> vertexStyles},
        {}],
      VertexCoordinates   -> coords,
      VertexShapeFunction -> shapeFn,
      VertexSize          -> {If[shapeOpt === "Glyph", halfW, shapeW],
                              0.001 (priceHi - priceLo)},
      (* PointLabels: {{index, price, label}, ...} or {index, price, label, {dx, dy}}
         entries drawn as bold text, e.g. the points X, A, B, C, D of a pattern *)
      Epilog              -> {bgArrows, arrows,
        Map[Text[Style[#[[3]], Bold, 14], {#[[1]], sy #[[2]]},
            If[Length[#] >= 4, #[[4]], {0, -1.5}]] &, OptionValue["PointLabels"]]},
      Frame               -> True,
      (* Candle indices below (at most about 20 labels), prices at the left *)
      FrameTicks          -> {{yTicks, None},
                              If[dateAxis, {dateTicks, indexTicks},
                                {Range[from, to, Max[1, Ceiling[(to - from + 1) / 20]]], None}]},
      FrameLabel          -> If[dateAxis, {{"Price", None}, {None, "Candlestick index"}}, {"Candlestick index", "Price"}],
      Sequence @@ If[dateAxis, {ImagePadding -> {{50, 20}, {52, 30}}}, {}],
      PlotRange           -> {xr, yr},
      ImageSize           -> OptionValue["ImageSize"]
    ]
  ];

(* Overload: the vertices and edges come from a Graph the caller controls (a subgraph, an
   edge-filtered graph, a pattern match, ...). The candlesticks and the price of each edge are
   still taken from the featureSet (an edge {from, to} that is not in featureSet["Segments"] is
   drawn between its vertices). Same options as the main form. *)
candlestickVertexGraph[featureSetArg_, g_Graph?GraphQ, opts : OptionsPattern[]] :=
  candlestickVertexGraph[featureSetArg, {}, "Graph" -> g, opts];

(* "GraphChart" key: the glyph picture of the last "Window" candles, added to a featureSet (Dataset).
   The whole history would be unreadable and slow to draw, so only the window is in the graph. *)
Options[addGraphChart] = {
  "Window"              -> 250,
  "GlyphWidth"          -> 0.35,
  "VertexShapeFunction" -> "Glyph",
  "ShapeSize"           -> 0.2,
  "EdgeTypes"           -> All,
  "ArrowSize"           -> 0.005,
  "EdgeThickness"       -> 0.002,
  "RealEdges"           -> True,
  "OwnEdges"            -> Automatic,
  "DateAxis"            -> True,
  "ImageSize"           -> 800,
  "AspectRatio"         -> 0.5
};

addGraphChart[featureSetArg_Dataset, opts : OptionsPattern[]] :=
  Module[{n, w, chart, base},
    n = Length[Normal[featureSetArg["Candlesticks"]]];
    If[! IntegerQ[n] || n < 1, Return[featureSetArg]];
    w = OptionValue["Window"];
    chart = candlestickVertexGraph[featureSetArg, {},
      "FromIndex"           -> If[IntegerQ[w], Max[1, n - w + 1], 1],
      "ToIndex"             -> n,
      "GlyphWidth"          -> OptionValue["GlyphWidth"],
      "VertexShapeFunction" -> OptionValue["VertexShapeFunction"],
      "ShapeSize"           -> OptionValue["ShapeSize"],
      "EdgeTypes"           -> OptionValue["EdgeTypes"],
      "ArrowSize"           -> OptionValue["ArrowSize"],
      "EdgeThickness"       -> OptionValue["EdgeThickness"],
      "RealEdges"           -> OptionValue["RealEdges"],
      "OwnEdges"            -> OptionValue["OwnEdges"],
      "DateAxis"            -> OptionValue["DateAxis"],
      "ImageSize"           -> OptionValue["ImageSize"],
      "AspectRatio"         -> OptionValue["AspectRatio"]];
    base = If[MemberQ[Normal[Keys[featureSetArg]], "GraphChart"], featureSetArg[KeyDrop["GraphChart"]], featureSetArg];
    Append[base, <|"GraphChart" -> chart|>]
  ];

(* highlightKeepColors: HighlightGraph that keeps the colours of the highlighted edges. HighlightGraph colours the highlighted parts with its own style, so here the whole
   graph is made light gray (an explicit gray rule for every edge, because a general EdgeStyle does not replace the colour rules) and each edge of the subgraph(s) is
   highlighted with its own style from EdgeStyle of the graph and a wider line. With a list of subgraphs the vertices get a colour for each subgraph.
   A chart (featureSet["GraphChart"], the glyphs drawn by an EdgeShapeFunction) is shown as a stock chart: the candlesticks as they are, the edges hidden, except the edges
   of the subgraph(s), in their own colours and wider (only the subgraph edges that are in the chart, that is inside its window). *)
highlightKeepColors[g_Graph, sub_Graph, rest___] := highlightKeepColors[g, {sub}, rest];
highlightKeepColors[g_Graph, subs : {__Graph}, width_ : 3, gray_ : GrayLevel[0.75],
    ringColors_ : {RGBColor[1., 0.85, 0.], RGBColor[0.9, 0.3, 1.], RGBColor[0., 0.9, 0.9], RGBColor[1., 0.55, 0.]}] :=
  Module[{style, colorOf, grayG, edgeItems, vertexItems, chartQ, subEdges},
    (* the style rule of each edge of g (EdgeStyle holds a default directive and edge -> style rules) *)
    style = Association[Cases[Quiet[EdgeStyle /. Options[g, EdgeStyle]], _Rule]];
    colorOf = Function[e, Lookup[style, e, Directive[gray]]];
    (* a chart draws its edges with its own EdgeShapeFunction; a plain graph has Automatic *)
    chartQ = Quiet[EdgeShapeFunction /. Options[g, EdgeShapeFunction]] =!= Automatic;
    (* every edge of every subgraph (a rule for an edge that is not in the graph would make the Graph invalid) *)
    subEdges = Intersection[DeleteDuplicates[Join @@ (EdgeList /@ subs)], EdgeList[g]];
    If[chartQ,
      (* the chart: the other edges invisible, the subgraph edges in their colour, wider; the candlesticks untouched *)
      Graph[g, EdgeStyle -> Join[
        (# -> Directive[Opacity[0]]) & /@ Complement[EdgeList[g], subEdges],
        (# -> Directive[colorOf[#], AbsoluteThickness[width]]) & /@ subEdges]],
      grayG = Graph[g, EdgeStyle -> ((# -> gray) & /@ EdgeList[g])];
      (* every edge of every subgraph: its own colour, wider *)
      edgeItems = Style[#, Directive[colorOf[#], AbsoluteThickness[width]]] & /@ subEdges;
      (* the vertices: one subgraph, light blue; several, a colour for each subgraph (a vertex in two subgraphs takes the last one) *)
      vertexItems = If[Length[subs] == 1,
        Style[#, RGBColor[0.55, 0.75, 1.]] & /@ VertexList[subs[[1]]],
        Flatten[MapIndexed[Function[{sg, i}, Style[#, ringColors[[Mod[i[[1]] - 1, Length[ringColors]] + 1]]] & /@ VertexList[sg]], subs]]];
      HighlightGraph[grayG, Join[edgeItems, vertexItems]]]];

End[]

EndPackage[]

(* Echo["Read Visualization.wl"] *) 
