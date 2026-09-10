# Design of Experiments and Analysis

jamovi module (`jmvdoe`) to generate a designed experiment, write it to the spreadsheet, run the experiment, then fit a model to the measured results.

Author: Jake Merrell (jake.merrell@xonano.com)  
License: [GPL-3](LICENSE)

## Menu (Analyses → DOE)

| Analysis | Purpose |
| --- | --- |
| Full Factorial Design | General / mixed-level full factorials |
| Screening Design | Regular FrF2 fractions and Plackett–Burman |
| Response Surface Design | Central Composite and Box–Behnken |
| Custom Design | D- or I-optimal designs (AlgDesign) |
| Taguchi Arrays | Orthogonal arrays, optional outer/noise array |
| Analyze Design | Fit a model to the measured results (ANOVA / Taguchi SN, delta and rank, predicted SN, plots, preferred settings) |

The five generators build runs and write them to the spreadsheet. **Analyze Design** is a separate analysis that reads the columns back out, so it works on the values you actually measured.

## Workflow

1. Start with a **new/empty** jamovi file (recommended).
2. Open **DOE →** the design generator you need and define factors.
3. Set **Replicates (repeat whole design)** to stack the experimental runs (e.g. 8-run design × 3 → 24 rows). The **Run** column uses DoE-style labels (`1.1`, `2.1`, …, `1.2`, …).
4. Add **Responses** (name + Maximize, Minimize, or Match target). Each named column is created in the design.
5. Leave **Fill responses with random data** checked to pre-populate demo values (with planted factor effects so analysis plots look meaningful).
6. Open **Spreadsheet** and check **Add design to spreadsheet** when you want the runs in Data. That writes Run, factor, and response columns into the current spreadsheet and keeps them in step as you edit the design.
7. Run the experiment and type each measured value next to its run.
8. Open **DOE → Analyze Design**, move the measured column into **Responses** and the design columns into **Factors**. Choose Regression / ANOVA or Taguchi signal-to-noise, and set the goal so preferred factor levels are reported.

**Note:** jamovi cannot create a brand-new separate data table like JMP’s *Make Table*. Spreadsheet write-back adds columns to the current sheet; use a fresh file so existing data is not mixed with the design. Uncheck **Add design to spreadsheet** to remove the generated columns.

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

Mixture designs, definitive screening, augmentation, split-plot, factor constraints, prediction profiler, Taguchi dynamic SN.
