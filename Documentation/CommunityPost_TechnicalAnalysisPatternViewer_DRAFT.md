# Technical Analysis Pattern Viewer: see support, resistance and chart patterns on any candlesticks

*DRAFT for Wolfram Community. Items in [brackets] are for you to fill in or check before posting. No .zip is attached (Community cannot take it): the code is on GitHub.*

---

**Code and guide notebook:** https://github.com/HenrickJeanty/TechnicalAnalysisPatternViewer (MIT licence)

## What it is

A small Wolfram Language package for **looking at** the support lines, resistance lines and classic chart patterns of a stock, and choosing which of them to draw.

You give it candlesticks (your own table, a CSV file, a ticker through `FinancialData`, or the synthetic sample that comes with the package). It finds:

* **resistance lines** (through two peaks) and **support lines** (through two valleys), each valid until a candlestick pierces it by more than a tolerance;
* **13 patterns**, each a resistance and a support valid at the same candlestick: symmetric, ascending and descending triangles, rising and falling wedges, five kinds of broadening formations, rectangles, and rising and falling channels.

It lists them in scrollable tables with a **Show / Hide** button in every row. **The chart starts empty and draws only what you pick.**

[FIGURE 1: screenshot of a panel on the monthly sample, with one resistance, one support and one pattern shown. Take it from section 5 of the guide notebook.]

> The sample data is **synthetic** (a seeded random walk), so that every example runs without an internet connection. It is not a real stock.

## Quick start

Download or clone the repository, keep the folder together, and load it:

```wolfram
(* Load the package: change the path to the folder you downloaded *)
Get["path/to/TechnicalAnalysisPatternViewer/Kernel/init.m"]
```

```wolfram
(* The synthetic sample as monthly candlesticks (58 of them) *)
candles = sampleCandlesticks["monthly"];

(* The interactive panel: tables of resistances, supports and patterns, and the chart *)
showPatternViewer[
  candles,
  "sample, monthly"
]
```

The analysis of 58 candlesticks takes about 2 seconds. Your own data works the same way:

```wolfram
(* A ticker from FinancialData: daily prices of 2023-2025, merged into monthly candlesticks *)
showPatternViewer[
  loadCandlesticks[
    "PCG",
    "StartDate" -> {2023, 1, 1},
    "EndDate" -> {2025, 12, 31},
    "Resample" -> "monthly"
  ],
  "PCG, monthly"
]

(* A CSV file with the columns Date, Open, High, Low, Close and, optionally, Volume *)
showPatternViewer[
  "path/to/prices.csv",
  "my file"
]
```

## Using the panel

* **Alive at candle** picks a candlestick; the tables list the lines and patterns that are valid *at that candlestick* (the default is the last one). The lines are found candlestick by candlestick, so what is listed for candlestick *k* uses nothing after *k*.
* **Show** in a row draws that line (or pattern) with its dotted extension and the candlesticks that touch it; **Hide** removes it. **Select all** and **Clear Support/Resistance** do it for the whole table.
* Move the mouse over the chart to read the date, prices and volume of a candlestick.
* The **Segments** table (the up and down moves between peaks and valleys) is at the right of the chart.

[FIGURE 2: the same panel with "TableColumns" -> "All", showing every column.]

## The same stock at several scales

`mergeCandlesticks[candles, n]` merges *n* candlesticks at a time into one (Open of the first, Close of the last, highest High, lowest Low, sum of the volumes). `mergeCandlestickLevels` does it again and again, so you get every scale from the original to a single candlestick, and `showPatternViewers` gives **one independent panel for each level**:

```wolfram
(* Weekly candlesticks of the sample, and all the levels of merging 2 at a time *)
levels = mergeCandlestickLevels[
  sampleCandlesticks["weekly"],
  2
];

(* How many candlesticks each level has: {250, 125, 62, 31, 15, 7, 3, 1} *)
Length /@ levels

(* One panel for level 3 (4 weeks per candlestick) and one for level 4 (8 weeks per candlestick) *)
showPatternViewers[
  levels,
  "Levels" -> {3, 4}
]
```

A resistance that looks minor on the weekly chart can be a major one on the 8-week chart; putting the panels one above the other makes that easy to see.

[FIGURE 3: two panels, levels 3 and 4, one above the other.]

## Not only a panel: the tables

Everything the panel shows is available as a `Dataset`:

```wolfram
(* Find everything in the monthly candlesticks *)
featureSet = analyzeCandlesticks[
  sampleCandlesticks["monthly"]
];

(* The support and resistance lines, one row for every line and every candlestick at which it is valid *)
supportResistanceTable[featureSet]

(* The patterns found, and how many candlesticks each one was valid at *)
Counts[Normal[patternTable[featureSet]][[All, "PatternName"]]]

(* The segments between peaks and valleys *)
segmentTable[featureSet]
```

There are 16 public functions in all, each with a usage message (`?showPatternViewer`, `?loadCandlesticks`, ...). The guide notebook `Notebooks/TechnicalAnalysisPatternViewer.nb` runs every one of them, spelling out every option with its default value.

## How it works (for the curious)

The package is built around one pipeline: `loadCandlesticks` (any data → a `Tabular`) → `analyzeCandlesticks` (the feature set) → `showPatternViewer` (the panel).

The analysis and the drawing code come from a larger private project of mine. Rather than rewrite them, I put a **pinned, unmodified copy** in `vendor/` and wrote the new package around it (`vendor/README.md` says exactly which files and which commit).

One trick that may interest other package authors: the chart code keeps its state (which segment you clicked, which lines are drawn) in symbols of its own context, so two charts would share it. To make the panels independent, every panel loads **its own copy of the drawing file, read as text with its context renamed** (`StockAnalyzer`Visualization`` becomes `TAPVPanel<tag>`Visualization``), and a few layout changes (narrower tables, a chart that starts empty) are made as text patches. Each group of patches is applied only if every piece of code it looks for is found exactly once; otherwise it is skipped with a message, so a changed vendor file can never give a half-changed table.

## Limits, honestly

* **Speed.** The analysis time grows faster than the square of the number of candlesticks: about 1 s for 30 candlesticks, 5 s for 90, 17 s for 150 and 36 s for 200. Thousands of daily candlesticks would take very long. Use weekly or monthly candlesticks, a window of dates, or the merged levels.
* **It needs the front end** (buttons, tooltips): use it in a notebook, not in a plain script.
* **It is a way of looking at charts, not a predictor.** The package makes no claim that these patterns tell you anything about future prices. [Optional, your call: "In my own tests of related pattern definitions I did not find evidence that they predict returns."] It is not investment advice.
* Wolfram Language 14.2 or later (it uses `Tabular`); developed on 15.0.
* 83 tests are in `Tests/` (run them with `TestReport`); the one that reads a ticker needs an internet connection.

## Feedback welcome

If a pattern is missing, a definition looks wrong, or a panel behaves oddly on your data, please tell me here or open an issue on GitHub. [Optional: the kind of data you would like it to read.]
