# Technical Analysis Pattern Viewer

A Wolfram Language tool to **see support lines, resistance lines and chart patterns** on the candlesticks of a stock, and to choose which of them to draw.

* Finds the **support and resistance lines** (a line through two peaks / two valleys, valid while it is not pierced) and the **13 classic patterns** that a resistance and a support make together: symmetric, ascending and descending triangles; rising and falling wedges; five kinds of broadening formations (plain, two right-angled, rising and falling wedge-shaped); rectangles; rising and falling channels.
* Shows them in scrollable tables with a **Show / Hide** button in each row; the chart starts empty and draws what you pick.
* Works on **any candlesticks**: your own table, a CSV file, a ticker (through `FinancialData`) or the synthetic sample that comes with the package.
* Looks at the same stock **at several scales**: `mergeCandlestickLevels` merges n candlesticks into one, level after level (2 at a time: 1, 2, 4, 8, ... original candlesticks per candlestick), and `showPatternViewers` gives an independent panel for each level.

> The sample data in `Data/SampleDaily.csv` is **synthetic** (a seeded random walk). It is there so that every example runs without an internet connection. It is not a real stock.

## Install

Nothing to install. Keep the folder together and load it:

```wolfram
(* the path of the folder you downloaded; change it to yours *)
Get["path/to/TechnicalAnalysisPatternViewer/Kernel/init.m"]
```

It needs Wolfram Language 14.2 or later (it uses `Tabular`; developed on 15.0) and, for tickers only, an internet connection (`FinancialData`). After loading, the 16 public functions are available by their short names.

Then open `Notebooks/TechnicalAnalysisPatternViewer.nb`: it explains every cell.

## Quick start

```wolfram
(* the synthetic sample as monthly candlesticks (58 of them: the analysis takes about 2 s) *)
candles = sampleCandlesticks["monthly"];

(* a plain candlestick chart, with the candle numbers on the top axis *)
showCandlesticks[candles]

(* the interactive panel: tables of resistances, supports and patterns, and the chart *)
showPatternViewer[
  candles,
  "sample, monthly"
]
```

Your own data:

```wolfram
(* a ticker, from FinancialData: daily candlesticks of 2023-2025 merged into monthly ones *)
showPatternViewer[
  loadCandlesticks[
    "PCG",
    "StartDate" -> {2023, 1, 1},
    "EndDate" -> {2025, 12, 31},
    "Resample" -> "monthly"
  ],
  "PCG, monthly"
]

(* a CSV file with the columns Date, Open, High, Low, Close (and Volume) *)
showPatternViewer["path/to/prices.csv", "my file"]
```

The same stock at several scales, one independent panel for each:

```wolfram
(* level 1 is the original, level 2 merges 2 candlesticks into 1, level 3 merges 2 of those, ... *)
levels = mergeCandlestickLevels[sampleCandlesticks["weekly"], 2];

showPatternViewers[levels, "Levels" -> {3, 4}]
```

## The 16 public functions

Each one has a full usage message: `?showPatternViewer`, `?loadCandlesticks`, ...

| Function | What it does |
|---|---|
| `loadCandlesticks[source, options]` | reads candlesticks (table, Dataset, list of associations, CSV file, ticker) and gives a clean `Tabular` |
| `resampleCandlesticks[candles, "weekly"/"monthly"/"yearly"]` | merges daily candlesticks into longer ones |
| `sampleCandlesticks[period]` | the synthetic sample (`"daily"`, `"weekly"`, `"monthly"`, `"yearly"`) |
| `mergeCandlesticks[candles, n]` | merges n candlesticks at a time into one |
| `mergeCandlestickLevels[candles, n]` | the list of all levels of merging, from the original to the last one |
| `mergeGroupSizes[levels, n]` | how many original candlesticks make one candlestick of each level |
| `showCandlesticks[candles, options]` | a plain candlestick chart (candle numbers on the top axis) |
| `loadEngine[]` | loads the analysis code of `vendor/` (done by the functions that need it) |
| `analyzeCandlesticks[candles, options]` | everything that was found, as a feature set (a `Dataset`) |
| `supportResistanceTable[featureSet]` | the support and resistance lines |
| `patternTable[featureSet]` | the patterns |
| `segmentTable[featureSet]` | the segments between peaks and valleys |
| `patternNames[]` | the 13 pattern names |
| `showPatternViewer[candles, title, options]` | one interactive panel |
| `showPatternViewers[levels, options]` | one independent panel per level |
| `patternViewerFeatureSet[tag]` | the feature set that a panel computed |

## How it works

```
 your data ──loadCandlesticks──▶ Tabular ──analyzeCandlesticks──▶ feature set ──showPatternViewer──▶ panel
 (Kernel/Data.wl)                         (Kernel/Engine.wl, uses vendor/)       (Kernel/Viewer.wl)
```

* `Kernel/` is the code written for this package: `Candlesticks.wl` (merging, plain chart), `Data.wl` (reading data), `Engine.wl` (the analysis and its tables), `Viewer.wl` (the panels), and `init.m` which loads them.
* `vendor/` is a pinned copy of the analysis and drawing code of a larger private project; see `vendor/README.md`. It is not modified.
* Every panel loads its **own copy** of the drawing code with a renamed context, so panels do not share what you clicked. The copy is patched in a few places (narrower tables, chart starts without lines, ...); a patch that does not find its code is skipped with a message, never half applied.

### The rules, in short

* A **resistance** (support) passes through two peaks (valleys). It is valid while no candlestick between its first anchor and the last one pierces it with its High (Low) by more than `TolerancePercent` (default 5) of the High-Low range of the candlesticks from the first anchor to the one tested.
* A **pattern** is a resistance and a support that are valid at the same candlestick and whose directions and gap give one of the 13 shapes. `MinTouches` (default 1: the touches a line needs between its two anchors) and `PatternBand` (default 0.15: the band around 1 of the gap ratio that counts as parallel) tune it.
* The lines and patterns are found **candlestick by candlestick**, so what the tables list for candlestick *k* uses nothing after *k*. "Alive at candle" in the panel picks *k*.

## Folder

```
Kernel/      the code of the package (init.m loads it)
vendor/      the analysis and drawing code (pinned copy, see vendor/README.md)
Data/        SampleDaily.csv, the synthetic sample
Notebooks/   TechnicalAnalysisPatternViewer.nb, the guide
Tests/       *.wlt test files (run with TestReport)
```

## Tests

```wolfram
TestReport[#] & /@ FileNames["*.wlt", "path/to/TechnicalAnalysisPatternViewer/Tests"]
```

83 tests; the one that reads a ticker needs an internet connection.

## Limits and notes

* Not investment advice: the patterns are a way of **looking** at a chart. This package makes no claim that they predict anything.
* Needs at least 5 candlesticks to find anything. **The analysis time grows faster than the square of the number of candlesticks**: measured on the sample, about 1 s for 30 candlesticks, 5 s for 90, 17 s for 150 and 36 s for 200 (58 monthly candlesticks: 2 s). Thousands of daily candlesticks would take a very long time: look at weekly or monthly candlesticks (`"Resample"`), at a window of dates (`"StartDate"`, `"EndDate"`), or at merged levels (`mergeCandlestickLevels`), which is what the panels were made for.
* The panels use the front end (buttons, tooltips). Call `showPatternViewer` in a notebook, not in a plain script.
* If you used older versions of these functions, an old definition in the `Global`` context can hide the new one; `init.m` clears and forwards those and says so when it does. If a function seems unevaluated, restart the kernel and load again.
* The first load of a panel writes its patched copy of the drawing code to `$TemporaryDirectory/TechnicalAnalysisPatternViewer`.

## Licence

MIT, see `LICENSE`. This covers the whole folder, including the copy of the analysis and drawing code in `vendor/` (same author).
