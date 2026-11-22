# API reference

The 16 public functions of the Technical Analysis Pattern Viewer. This text is generated from the usage messages of the code (`?name` in a notebook shows the same text), so it is always the one the code gives. For worked examples with every option spelled out, see `Notebooks/TechnicalAnalysisPatternViewer.nb`.

After `Get[".../Kernel/init.m"]` every function is available by its short name. Their contexts are `TechnicalAnalysisPatternViewer`Candlesticks``, `...`Data``, `...`Engine`` and `...`Viewer``.

**The table of candlesticks** that all the functions accept and produce is a `Tabular` with the columns `Date`, `Open`, `High`, `Low`, `Close`, `Volume`, `Ticker`, `Period` and `logReturn`. Wherever a table is accepted, so is a `Dataset` or a list of associations with the keys `Open`, `High`, `Low` and `Close` (and `Date`).

**Candlestick numbers.** Every `FromIndex`, `ToIndex` and similar column of the tables is a candlestick NUMBER (1 for the first candlestick), not a date.

## Merging and drawing lists of candlesticks (`Kernel/Candlesticks.wl`)

### `mergeCandlesticks`

mergeCandlesticks[original, n] builds a new list of candlesticks from original by merging n candlesticks at a time: candlestick k of the result is the merge of original[[(k - 1) n + 1 ;; k n]].

The merged candlestick has the Open of the first candlestick of the group, the Close of the last, the highest High and the lowest Low of the group, the Date of the first one, the sum of the Volumes and the sum of the logReturns (the log return of the whole group); every other column (Ticker, Period, ...) is taken from the first candlestick of the group.

original is a Tabular, a Dataset or a list of associations with the keys Open, High, Low and Close, and the result has the same form (a Tabular gives a Tabular, a Dataset a Dataset, a list a list of associations).

The result can be merged again: mergeCandlesticks[mergeCandlesticks[original, 2], 2] has the candlesticks of original merged 4 at a time.

Option: "Remainder" -> "Drop" (the default: an incomplete last group is left out, so every result candlestick is a merge of exactly n candlesticks) or "Keep" (the incomplete last group is merged too).

Example: mergeCandlesticks[daily, 5] gives 5 daily candlesticks in one.

### `mergeCandlestickLevels`

mergeCandlestickLevels[original, n] gives the list of all the levels of merging n candlesticks at a time: {original, mergeCandlesticks[original, n], mergeCandlesticks[mergeCandlesticks[original, n], n], ...}. levels[[1]] is the original, levels[[2]] the original merged n at a time, levels[[3]] that merged again, and so on up to the last level, which still has at least one candlestick (the merging stops when fewer than n candlesticks are left to merge). n is an integer of at least 2.

Options: "Remainder" -> "Drop" (as in mergeCandlesticks) and "MaxLevels" -> All (or a number: at most that many merges after the original).

Example: levels = mergeCandlestickLevels[weekly, 2]; Length /@ levels gives {102, 51, 25, 12, 6, 3, 1} for 102 weekly candlesticks.

### `mergeGroupSizes`

mergeGroupSizes[levels] gives the grouping size of each level of levels (the result of mergeCandlestickLevels): the number of original candlesticks in one candlestick of the level, {1, n, n^2, ...}.

n is inferred from the numbers of candlesticks of the levels, assuming Remainder "Drop" (with Remainder "Keep" the numbers can fit another n too: give n). mergeGroupSizes[levels, n] uses the given n. The sizes are Missing["NotInferred"] when no n fits the levels (or there is a single level).

### `showCandlesticks`

showCandlesticks[candles] or showCandlesticks[candles, title] draws the candlesticks: for each one a wick from its Low to its High and a body from its Open to its Close, at x = its number, in the up colour when the Close is at or above the Open and in the down colour when it is below. candles is a Tabular, a Dataset or a list of associations with the keys Open, High, Low and Close (and Date, for the labels of the bottom axis), so it works for the original candlesticks and for merged ones.

Options: "ImageSize" -> 900, "AspectRatio" -> 0.5, "UpColor" -> RGBColor[0.1, 0.6, 0.2], "DownColor" -> RGBColor[0.85, 0.15, 0.15], "TopIndices" -> True (the number of each candlestick on the top axis, above the dates).

## Getting candlesticks from your data (`Kernel/Data.wl`)

### `loadCandlesticks`

loadCandlesticks[source] gives the table of candlesticks (a Tabular with the columns Date, Open, High, Low, Close, Volume, Ticker, Period and logReturn) made from source, in date order. source can be:

  • a Tabular, a Dataset, or a list of associations (or of lists of rules) with the keys Date, Open, High, Low and Close (Volume is optional; the names are matched without regard to case);

  • the path of a CSV file whose first line names the columns (Date, Open, High, Low, Close and optionally Volume), or File["path"];

  • a ticker such as "PCG", whose daily prices come from FinancialData (an internet connection is needed).

Rows with a missing or non-numeric Open, High, Low or Close are left out, with a message that counts them.

Options: "Source" -> Automatic ("Table", "CSV" or "FinancialData" to say which kind source is), "StartDate" -> All and "EndDate" -> All (a DateObject or {y, m, d}: only the candlesticks in this window), "Resample" -> None ("weekly", "monthly" or "yearly": make coarser candlesticks from the loaded ones, see resampleCandlesticks), "Period" -> Automatic (the text of the Period column: "daily", "weekly", "monthly" or "yearly"; Automatic takes it from the Resample option or from your table, and is "daily" otherwise), "Ticker" -> Automatic (the text of the Ticker column; for a ticker source, that ticker).

Examples: loadCandlesticks["PCG", "StartDate" -> {2024, 12, 1}, "Resample" -> "weekly"];  loadCandlesticks["prices.csv"];  loadCandlesticks[myDataset].

### `resampleCandlesticks`

resampleCandlesticks[candles, period] makes coarser candlesticks from finer ones. period is "weekly" (ISO weeks, Monday to Sunday), "monthly" or "yearly". Each result candlestick has the Date of the first candlestick of its week, month or year, the Open of the first, the Close of the last, the highest High, the lowest Low and the sum of the Volumes; the Period column says the new period. candles is any table that loadCandlesticks accepts; the result is a Tabular.

Use mergeCandlesticks[candles, n] instead to merge a fixed number n of candlesticks at a time, whatever their dates.

### `sampleCandlesticks`

sampleCandlesticks[] gives about five years of daily candlesticks of a SYNTHETIC price series (made up with a random-number generator, not real market data), to try the viewer without any data of your own. sampleCandlesticks[period] gives them resampled: "daily" (the default), "weekly", "monthly" or "yearly". The result is the table described in loadCandlesticks. The series is read from Data/SampleDaily.csv of this package.

## Finding the lines and the patterns (`Kernel/Engine.wl`)

### `loadEngine`

loadEngine[] loads the analysis code that finds the support and resistance lines and the patterns (the files of the folder vendor/ of this package) if it is not loaded yet, and gives True when it is loaded. The other functions call it themselves, so you rarely need it; loading takes a few seconds.

### `analyzeCandlesticks`

analyzeCandlesticks[candles] finds the peaks and valleys, the segments, the support and resistance lines and the patterns of the candlesticks, and gives everything as a feature set: a Dataset whose keys are, among others:

  "Candlesticks" the candlesticks used; "Peaks" and "Valleys" their numbers; "Segments" the segments between peaks and valleys and, with them, the support and resistance lines (rows with Kind "Res" or "Sup"); "ValidTrendlines" every support and resistance line, alive or ended; "Patterns" the patterns; "Graph" the directed graph of the candlesticks, with the complex edge weight a + b i (a = the number of candlesticks the edge spans, b = the price change along it).

candles is anything that loadCandlesticks accepts (a Tabular, a Dataset, a list of associations with Date, Open, High, Low and Close). The numbers in the tables are candlestick NUMBERS (1 for the first candlestick), not dates.

Options: "Sigma" -> 0 (peaks and valleys: 0 is a candlestick whose High is above both neighbours' Highs, or whose Low is below both neighbours' Lows; a larger integer asks for more extreme ones), "StrictRanges" -> True (True: one MaxU or MaxD edge per segment; False: every pair of fields), "SupportResistance" -> True (False switches the lines and patterns off; or a list of rules {"TolerancePercent" -> 5, "MinTouches" -> 1, "PatternBand" -> 0.15}: TolerancePercent is how far, as a percentage of the High-Low range, a High (Low) may pierce a resistance (support) and still count as touching it; MinTouches the number of touches between its two anchors that a line needs; PatternBand the band around 1 of the ratio of the gap between the two lines now to the gap at the start that counts as parallel), "HarmonicPatterns" -> False (True, the name of a harmonic pattern or a list of names: also find the harmonic patterns X, A, B, C, D born at each candlestick; or a list of rules such as {"Patterns" -> {"Gartley", "Bat"}, "Tolerance" -> 0.05}), "GraphChart" -> False (True or a list of options adds the key "GraphChart": the candlesticks drawn as a graph, see the usage of the viewer).

Example: featureSet = analyzeCandlesticks[sampleCandlesticks["monthly"]]; supportResistanceTable[featureSet].

### `supportResistanceTable`

supportResistanceTable[featureSet] gives the support and resistance lines of a feature set (the result of analyzeCandlesticks) as a Dataset, one row for every line and every candlestick at which it is alive. The columns are: Kind ("Res" resistance, "Sup" support), Direction ("ResU", "ResD", "ResF", "SupU", "SupD" or "SupF": rising U, falling D or flat F), FromIndex (the first anchor), AnchorIndex (the second anchor), StartIndex, ToIndex (the candlestick at which the row is valid), FromPrice and ToPrice (the prices of the line at FromIndex and ToIndex), Slope (price per candlestick), LogSlope (the same on a log scale), Touches (how many candlesticks touched the line) and Goodness (the number of touches counted with the anchors).

### `patternTable`

patternTable[featureSet] gives the patterns of a feature set (the result of analyzeCandlesticks) as a Dataset: one row for every pattern and every candlestick at which it is valid. The columns are: PatternName (see patternNames[]), Pattern (the same without spaces), ToIndex (the candlestick at which the pattern is valid), StartIndex (where the pair of lines starts), Width (the number of candlesticks from StartIndex), ResistanceDirection and SupportDirection (U, D or F), Relation ("converging", "diverging" or "parallel"), GapRatio (the gap between the lines now divided by the gap at the start), Apex (the candlestick number where the lines cross, ahead of ToIndex when they converge), ResistanceTouches, SupportTouches and Touches, ResistanceGoodness and SupportGoodness, the indices and prices where the two lines start, ResistanceValue and SupportValue (their prices at ToIndex) and the slopes. The Dataset is empty when no pattern was found.

### `segmentTable`

segmentTable[featureSet] gives the segments between peaks and valleys of a feature set (the result of analyzeCandlesticks) as a Dataset, without the support and resistance lines. The columns are: Direction ("U" up, "D" down), FromIndex and ToIndex (the candlestick numbers), FromPrice and ToPrice, Range (ToPrice - FromPrice), DeltaX (the number of candlesticks), LogRatio, Angle and Weight (the angle of the segment).

### `patternNames`

patternNames[] gives the names of the 13 patterns that are looked for: symmetric, ascending and descending triangles, rising and falling wedges, broadening formations (also right-angled and wedge-shaped), rectangles (trading ranges), and rising and falling channels. A pattern is a resistance and a support that are valid at the same candlestick: its name follows from their directions (up, down or flat) and from whether they converge, diverge or run parallel.

## The interactive panels (`Kernel/Viewer.wl`)

### `showPatternViewer`

showPatternViewer[candles] or showPatternViewer[candles, title] gives an interactive panel for the candlesticks candles (anything that loadCandlesticks accepts): the support lines, the resistance lines and the patterns that were found, in tables, and the chart on which you choose what to draw.

THE PANEL has, from the top: the title; the Sigma field and the Load button (change the sigma and press Load to find peaks and valleys again) and a status line; then the support / resistance part with its buttons and the field "Alive at candle" (the tables list the lines and patterns that are valid at that candlestick; the default is the last one; "Last" and "All" are shortcuts); then ONE LINE of scrollable windows (Patterns, Resistances, Supports) above the chart; then the chart, with the Segments table at its right (and, with "TableColumns" -> "All", the Patterns window under it).

USING IT: the chart starts without lines. Press Show in a row of a table to draw that line (or pattern) on the chart, with its dotted extension and the candlesticks that touch it; press Hide to remove it. "Select all" draws every listed line, "Clear Support/Resistance" removes them all. Move the mouse over the chart to read the date, the prices and the volume of a candlestick in the line above it; Show in the Segments table draws a segment.

Options (every option is given with its default):

  "Sigma" -> 0 (peaks and valleys, see analyzeCandlesticks), "StrictRanges" -> True, "HarmonicPatterns" -> False (True, a name, a list of names or rules: adds the harmonic patterns panel),

  "SupportResistance" -> True (False, or a list of rules {"TolerancePercent" -> 5, "MinTouches" -> 1, "PatternBand" -> 0.15}, see analyzeCandlesticks),

  "AutoLoad" -> Automatic (False: show the panel without computing, press Load to compute),

  "TopTables" -> True (the resistances and supports windows on a line above the chart; False: all the windows at the right, under the segments),

  "TableColumns" -> "Compact" (narrow tables, and the Patterns window on the same line as the Resistances and Supports windows; "All": every column, and the Patterns window under the segments table),

  "HarmonicInRow" -> False (True: the harmonic patterns panel is a window of the line above the chart; False: at the right, under the patterns),

  "LinesAtStart" -> False (False: the chart starts without support and resistance lines; True: the lines valid at the last candlestick are drawn at the start),

  "RegimeBars" -> False (the line above the chart can show three regime bars read from a global table $regimeHistory; they are left out because they would belong to another series),

  "EdgeThickness" -> 0.002 and "ArrowSize" -> 0.005 (the thickness of the segments drawn on the chart and the size of their arrowheads), "GraphChart" -> False (True or a list of options: also build the candlesticks-as-graph picture),

  "Tag" -> Automatic (the name of this panel for patternViewerFeatureSet).

Example: showPatternViewer[sampleCandlesticks["weekly"], "sample, weekly"].

### `showPatternViewers`

showPatternViewers[levels] gives a column of independent panels, one for each level of levels (the result of mergeCandlestickLevels, see mergeCandlesticks), each made by showPatternViewer: the same stock seen at several scales. Each panel is titled with its level, its grouping size (how many original candlesticks make one candlestick of the level) and its number of candlesticks. The panels do not share anything: pressing Show in one does not change another.

Options: "Levels" -> All (or a list of level numbers, for example {2, 3}), "MergeSize" -> Automatic (the n of mergeCandlestickLevels, inferred from the levels, for the grouping size in the titles), and every option of showPatternViewer.

Example: levels = mergeCandlestickLevels[sampleCandlesticks["weekly"], 2]; showPatternViewers[levels, "Levels" -> {2, 3}].

### `patternViewerFeatureSet`

patternViewerFeatureSet[tag] gives the feature set that the panel with this tag computed (see analyzeCandlesticks), so you can read its tables: supportResistanceTable[patternViewerFeatureSet[tag]]. The tag is the option "Tag" of showPatternViewer; showPatternViewers gives the panels the tags "level<k>-<number>". patternViewerFeatureSet[] gives an association of all the tags and their feature sets.

