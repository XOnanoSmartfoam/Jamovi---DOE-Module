# jmvdoe — Design of Experiments for jamovi

JMP-inspired DOE module for [jamovi](https://www.jamovi.org/): generate a design, optionally write it to the spreadsheet, and evaluate it in the same analysis.

## Menu (Analyses → DOE)

| Analysis | Purpose |
| --- | --- |
| Full Factorial Design | General / mixed-level full factorials |
| Screening Design | Regular FrF2 fractions and Plackett–Burman |
| Response Surface Design | Central Composite and Box–Behnken |
| Custom Design | D- or I-optimal designs (AlgDesign) |
| Taguchi Arrays | Orthogonal arrays, optional outer/noise array |

Each generator includes **Evaluate this design** (ANOVA / Taguchi SN, plots, preferred settings).

## Workflow

1. Start with a **new/empty** jamovi file (recommended).
2. Open **DOE →** the design generator you need and define factors.
3. Set **Replicates (repeat whole design)** to stack the experimental runs (e.g. 8-run design × 3 → 24 rows). The **Run** column uses DoE-style labels (`1.1`, `2.1`, …, `1.2`, …).
4. Add **Responses** (name + Maximize, Minimize, or Match target). Each named column is created in the design.
5. Leave **Fill responses with random data** checked to pre-populate demo values (with planted factor effects so analysis plots look meaningful).
6. Check **Evaluate this design** to fit the model. Preferred factor levels follow each response’s goal.
7. Click **Add design to spreadsheet** when you want the runs in Data. That writes Run, factor, and response columns into the current spreadsheet.

**Note:** jamovi cannot create a brand-new separate data table like JMP’s *Make Table*. Spreadsheet write-back adds columns to the current sheet; use a fresh file so existing data is not mixed with the design. Uncheck **Columns in Data** to remove the generated columns. Click the button again after you change the design.

## Install (developers)

Prerequisites: jamovi 2.6+, R, and `jmvtools`.

```r
install.packages('jmvtools', repos=c('https://repo.jamovi.org', 'https://cloud.r-project.org'))

# Windows example — adjust to your jamovi path
options(jamovi_home = 'C:/Program Files/jamovi 2.6.44.0')

setwd('path/to/jmvdoe')
jmvtools::install()
```

Dependencies bundled via `DESCRIPTION` Imports: `DoE.base`, `FrF2`, `rsm`, `AlgDesign`, `ggplot2`.

## Tests

```r
devtools::test()
# or, after sourcing helpers:
# testthat::test_dir('tests/testthat')
```

## Out of scope (v1)

Mixture designs, definitive screening, augmentation, split-plot, factor constraints, Evaluate Design platform, prediction profiler, Taguchi dynamic SN.
