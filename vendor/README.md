# vendor/ — the analysis and drawing code

This folder holds the code that finds the support lines, resistance lines and patterns and draws the chart. It was written for a larger private project (the *Stock Analyzer*, which also has a 5 GB database of stock data, regime models and prediction studies). Only the files that the viewer needs were copied here, so that the viewer is a self-contained folder.

## Provenance

The six files below are **unchanged copies** of the files of the Stock Analyzer project at its commit `9deae06` (git tag `archive-before-fresh-start`). Nothing in them was edited for this package.

| File | What the viewer uses it for |
|---|---|
| `Analysis.wl` | `updateFeatureSet`: finds peaks and valleys, the segments between them, the support and resistance lines (with their validity, touches and goodness) and the 13 patterns. This is the analysis. |
| `Visualization.wl` | `displayCandlestickGraphWithSegmentPicker`: the chart with its tables and its Show/Hide buttons. |
| `TrendlinePatterns.wl` | the 13 pattern definitions and the incremental finder of resistance/support pairs. |
| `HarmonicPatterns.wl` | the harmonic patterns (Gartley, Bat, Butterfly, Crab, ...) — only used with `"HarmonicPatterns" -> True`. |
| `Fibonacci.wl` | retracement and extension ratios, used by `HarmonicPatterns.wl`. |
| `MarkovRegimeModel.wl` | declared by `Analysis.wl`; the viewer never fits a regime model. |

One file is new and is **not** from the Stock Analyzer:

| File | What it is |
|---|---|
| `StockDataManagerStub.wl` | The Stock Analyzer reads its data through a package called `StockDataManager`. The viewer brings its own data, so this file declares the one symbol the code asks for (`getStock`) as a function that gives a message and `$Failed`. The code never needs it when it is given a table of candlesticks, as the viewer does. |

## How they are loaded

`Kernel/Engine.wl` (`loadEngine[]`) loads them, once, in this order: `MarkovRegimeModel`, `Fibonacci`, `StockDataManagerStub`, `TrendlinePatterns`, `HarmonicPatterns`, `Analysis`, `Visualization`. All load without messages.

`Visualization.wl` keeps the state of the chart (which segment was clicked, which lines are drawn, ...) in symbols of its own package, so two charts would share it. To make every panel independent, `Kernel/Viewer.wl` does **not** use the loaded copy: for each panel it reads this file as text, renames its context (`StockAnalyzer`Visualization`` becomes `TAPVPanel<tag>`Visualization``), applies a few text patches (see the header of `Kernel/Viewer.wl`) and loads that copy from the temporary folder. The file in this folder is never modified.

## Updating

If a newer version of the Stock Analyzer code is copied here, run `Tests/Viewer.wlt`. If the patches no longer find the code they expect, the panel says so with a message (`showPatternViewer::patch`) and applies none of that group; it never gives a half-changed table.

## Licence and attribution

The code in this folder is by Henry Henrick Jeanty, Ph.D., and is covered by the MIT licence of the package (see `LICENSE` in the folder above).
