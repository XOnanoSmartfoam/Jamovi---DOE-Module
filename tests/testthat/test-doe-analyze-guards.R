# Regression tests for the failure that blanked the spreadsheet and left the
# analysis spinning: a response whose name did not match any column of the data
# reached lm() as an NA term and threw out of .run(). An error thrown after the
# design had already been written meant jamovi never applied the finished
# columns, so the runs stayed missing from the Data sheet.
#
# The rule these tests protect: filling a result never throws because of the
# columns it was handed. Bad or missing selections must be reported.

root <- if (file.exists("R/doe-utils.R")) "." else "../.."
source(file.path(root, "R/doe-utils.R"))
source(file.path(root, "R/doe-analyze.R"))

# --- minimal stand-ins for the jamovi result items -------------------------
make_item <- function() {
    self <- new.env()
    self$content <- NULL
    self$state <- NULL
    self$rows <- list()
    self$setContent <- function(x) self$content <- x
    self$setState <- function(x) self$state <- x
    self$addRow <- function(rowKey, values) self$rows[[length(self$rows) + 1L]] <- values
    self
}

make_results <- function() {
    keys <- c("analysisInfo", "analysisAnova", "analysisCoef", "analysisRecommend",
              "analysisHalfNormal", "analysisPareto", "analysisMainEffects",
              "analysisInteraction", "analysisResiduals", "analysisContour",
              "analysisPerRun", "analysisResponseSN", "analysisResponseMean",
              "analysisSnAnova", "analysisSnDelta", "analysisSnPredict",
              "analysisPlotSN", "analysisPlotMeans", "analysisSnDeltaPlot",
              "analysisEffects", "analysisEffectPlot")
    stats::setNames(lapply(keys, function(k) make_item()), keys)
}

# The design from the reported session: four two-level *text* factors, 16 runs.
nominal_design <- function(n_reps = 1L) {
    levs <- list(Surface = c("Asphalt", "Carpet"),
                 Shoe    = c("Rubber", "EVA"),
                 XO.SOLE = c("Ground", "None"),
                 User    = c("Jake", "Brandon"))
    grid <- expand.grid(levs, stringsAsFactors = FALSE)
    grid <- grid[rep(seq_len(nrow(grid)), n_reps), , drop = FALSE]
    out <- cbind(Run = seq_len(nrow(grid)), grid, stringsAsFactors = FALSE)
    rownames(out) <- NULL
    out
}

facs4 <- c("Surface", "Shoe", "XO.SOLE", "User")

test_that("a response name that is not a column is reported, not thrown", {
    data <- nominal_design()
    data$Data.Quality <- stats::rnorm(nrow(data))
    r <- make_results()

    # "Data Quality" (with the space the user typed) never became a column;
    # the column is "Data.Quality". This is the exact mismatch that threw.
    resp_info <- data.frame(name = "Data Quality", goal = "maximize",
                            target = NA_real_, stringsAsFactors = FALSE)
    expect_silent(
        out <- .doe_fill_lm_analysis(r, data, dep = "Data Quality",
                                     factors = facs4, model_preset = "main2fi",
                                     resp_info = resp_info)
    )
    expect_false(isTRUE(out))
    expect_match(r$analysisInfo$content, "still in the spreadsheet")
    expect_length(r$analysisAnova$rows, 0)
})

test_that("an NA response name is reported, not thrown", {
    data <- nominal_design()
    data$Data.Quality <- stats::rnorm(nrow(data))
    r <- make_results()
    resp_info <- data.frame(name = NA_character_, goal = "maximize",
                            target = NA_real_, stringsAsFactors = FALSE)
    expect_silent(
        out <- .doe_fill_lm_analysis(r, data, dep = NA_character_,
                                     factors = facs4, model_preset = "main2fi",
                                     resp_info = resp_info)
    )
    expect_false(isTRUE(out))
})

test_that("a factor that is not a column is dropped rather than modelled", {
    data <- nominal_design()
    data$Y <- stats::rnorm(nrow(data))
    r <- make_results()
    expect_silent(
        out <- .doe_fill_lm_analysis(r, data, dep = "Y",
                                     factors = c(facs4, "Deleted Column"),
                                     model_preset = "main",
                                     resp_info = NULL)
    )
    expect_true(isTRUE(out))
    expect_gt(length(r$analysisAnova$rows), 0)
})

test_that("all-nominal factors fit and produce effects for the plots", {
    data <- nominal_design()
    set.seed(1)
    data$Y <- stats::rnorm(nrow(data))
    r <- make_results()
    out <- .doe_fill_lm_analysis(r, data, dep = "Y", factors = facs4,
                                 model_preset = "main2fi", resp_info = NULL)
    expect_true(isTRUE(out))
    # 1 intercept + 4 mains + 6 two-factor interactions = 11 of 16 runs used.
    expect_equal(length(r$analysisCoef$rows), 11)
    expect_gt(nrow(r$analysisHalfNormal$state), 0)
})

test_that("an empty response column is reported and leaves other responses fittable", {
    data <- nominal_design()
    set.seed(2)
    data$Measured <- stats::rnorm(nrow(data))
    data$NotYetRun <- NA_real_
    r <- make_results()
    resp_info <- data.frame(name = c("Measured", "NotYetRun"),
                            goal = "maximize", target = NA_real_,
                            stringsAsFactors = FALSE)
    expect_silent(
        out <- .doe_fill_lm_analysis(r, data, dep = "Measured", factors = facs4,
                                     model_preset = "main", resp_info = resp_info)
    )
    expect_true(isTRUE(out))
    expect_match(r$analysisInfo$content, "NotYetRun")
    expect_match(r$analysisInfo$content, "Enter at least 3")
    # The measured response still used every run.
    expect_match(r$analysisInfo$content, "N = 16")
})

test_that("a partly measured response uses only the runs that have values", {
    data <- nominal_design()
    set.seed(3)
    y <- stats::rnorm(nrow(data))
    y[11:16] <- NA_real_
    data$Y <- y
    r <- make_results()
    out <- .doe_fill_lm_analysis(r, data, dep = "Y", factors = facs4,
                                 model_preset = "main", resp_info = NULL)
    expect_true(isTRUE(out))
    expect_match(r$analysisInfo$content, "N = 10")
})

test_that("nominal response columns are read from labels, not factor codes", {
    x <- factor(c("2.5", "7.5", "2.5"))
    expect_equal(.doe_numeric_col(x), c(2.5, 7.5, 2.5))
    expect_equal(.doe_numeric_col(NULL), numeric(0))
    expect_true(all(is.na(.doe_numeric_col(factor(c("a", "b"))))))
})

test_that("renaming keeps unknown columns addressable instead of turning them NA", {
    data <- data.frame(`Cure Time` = 1:3, Y = 4:6, check.names = FALSE)
    conv <- .doe_syntactic_columns(data, c("Cure Time", "Y", "Gone"))
    expect_equal(conv$cols, c("Cure.Time", "Y", "Gone"))
    expect_false(anyNA(conv$cols))
    expect_equal(names(conv$data), c("Cure.Time", "Y"))
})

test_that("the taguchi path reports missing columns instead of throwing", {
    data <- nominal_design()
    data$Y1 <- stats::rnorm(nrow(data))
    r <- make_results()
    expect_silent(
        out <- .doe_fill_taguchi_analysis(r, data, factor_names = facs4,
                                          resp_names = "Data Quality",
                                          sn_type = "larger")
    )
    expect_false(isTRUE(out))
    expect_match(r$analysisInfo$content, "still in the spreadsheet")

    r2 <- make_results()
    expect_silent(
        out2 <- .doe_fill_taguchi_analysis(r2, data, factor_names = NA_character_,
                                           resp_names = "Y1", sn_type = "larger")
    )
    expect_false(isTRUE(out2))
})

test_that("plot renderers refuse empty or unusable state instead of failing", {
    empty <- make_item()
    expect_false(.doe_plot_half_normal(empty))
    expect_false(.doe_plot_pareto(empty))
    expect_false(.doe_plot_effect_summary(empty))
    expect_false(.doe_plot_main_effects(empty))
    expect_false(.doe_plot_interaction(empty))
    expect_false(.doe_plot_residuals(empty))
    expect_false(.doe_plot_contour(empty))
    expect_false(.doe_plot_sn(empty))
    expect_false(.doe_plot_means(empty))
    expect_false(.doe_plot_sn_delta(empty))

    # Contour needs two continuous factors; an all-nominal design has none. The
    # surface is now worked out during the run, so such an image is never given
    # state at all rather than being handed a model it cannot use.
    data <- nominal_design()
    set.seed(4)
    data$Y <- stats::rnorm(nrow(data))
    data <- .doe_coerce_for_lm(data, facs4, "Y")
    fit <- stats::lm(Y ~ Surface + Shoe + XO.SOLE + User, data = data)
    expect_null(.doe_contour_grid(fit, data, facs4))

    # And once an image does have state, its renderer must never return FALSE:
    # state with no file written is reported as ANALYSIS_RENDERING, which shows
    # as a spinner that never resolves. It draws an explanation instead.
    img <- make_item()
    img$setState(list(grid = NULL, xname = "a", yname = "b", dep = "Y"))
    expect_true(.doe_plot_contour(img))
})

# Standard L9 inner array: four 3-level factors, 9 runs (8 model df when all
# four are fitted). Two response columns stand in for repeats of each run.
l9_design <- function() {
    data.frame(
        A = factor(c(1, 1, 1, 2, 2, 2, 3, 3, 3)),
        B = factor(c(1, 2, 3, 1, 2, 3, 1, 2, 3)),
        C = factor(c(1, 2, 3, 2, 3, 1, 3, 1, 2)),
        D = factor(c(1, 2, 3, 3, 1, 2, 2, 3, 1)),
        Y1 = c(10.0, 10.2, 9.8, 12.1, 12.4, 11.7, 14.0, 13.6, 14.3),
        Y2 = c(10.4, 9.9, 10.1, 12.0, 12.6, 11.9, 13.8, 13.9, 14.1),
        stringsAsFactors = FALSE
    )
}

anova_col <- function(rows, field) {
    vapply(rows, function(row) row[[field]],
           if (field %in% c("term", "factor", "level")) character(1) else numeric(1))
}

test_that("an L9 four-factor SN ANOVA has no residual df until a factor is pooled", {
    data <- l9_design()
    r <- make_results()
    expect_no_error(
        out <- .doe_fill_taguchi_analysis(
            r, data, factor_names = c("A", "B", "C", "D"),
            resp_names = c("Y1", "Y2"), sn_type = "larger")
    )
    expect_true(isTRUE(out))
    terms <- anova_col(r$analysisSnAnova$rows, "term")
    dfs <- anova_col(r$analysisSnAnova$rows, "df")
    expect_true("Residuals" %in% terms)
    expect_false(any(grepl("^Pooled error", terms)))
    expect_equal(unname(dfs[terms == "Residuals"]), 0)
    expect_match(r$analysisInfo$content, "no residual degrees of freedom")
    expect_setequal(
        unique(anova_col(r$analysisResponseSN$rows, "factor")),
        c("A", "B", "C", "D"))
})

test_that("two copies of an L9 give residual df without pooling", {
    data <- rbind(l9_design(), l9_design())
    rownames(data) <- NULL
    data$Y1[10:18] <- data$Y1[10:18] + 0.4
    data$Y2[10:18] <- data$Y2[10:18] - 0.3
    r <- make_results()
    expect_silent(
        out <- .doe_fill_taguchi_analysis(
            r, data, factor_names = c("A", "B", "C", "D"),
            resp_names = c("Y1", "Y2"), sn_type = "larger")
    )
    expect_true(isTRUE(out))
    terms <- anova_col(r$analysisSnAnova$rows, "term")
    dfs <- anova_col(r$analysisSnAnova$rows, "df")
    expect_true("Residuals" %in% terms)
    expect_equal(unname(dfs[terms == "Residuals"]), 9)
    tested <- r$analysisSnAnova$rows[terms != "Residuals"]
    expect_true(all(vapply(tested, function(row) is.finite(row$F), logical(1))))
    expect_true(all(vapply(tested, function(row) is.finite(row$p), logical(1))))
    expect_false(grepl("no residual degrees of freedom", r$analysisInfo$content))
})

test_that("pooling one 3-level L9 factor frees 2 residual df and finite F/p", {
    data <- l9_design()
    r <- make_results()
    expect_silent(
        out <- .doe_fill_taguchi_analysis(
            r, data, factor_names = c("A", "B", "C"),
            resp_names = c("Y1", "Y2"), sn_type = "larger",
            pooled_factor = "D")
    )
    expect_true(isTRUE(out))

    terms <- anova_col(r$analysisSnAnova$rows, "term")
    expect_equal(terms[length(terms)], "Pooled error (D)")
    expect_false("D" %in% terms)
    expect_false("Residuals" %in% terms)

    rows <- r$analysisSnAnova$rows
    resid <- rows[[length(rows)]]
    expect_equal(resid$df, 2)
    tested <- rows[seq_len(length(rows) - 1L)]
    expect_true(all(vapply(tested, function(row) is.finite(row$F), logical(1))))
    expect_true(all(vapply(tested, function(row) is.finite(row$p), logical(1))))

    expect_match(r$analysisInfo$content, "pooled into the error term")
    expect_match(r$analysisInfo$content, "2 residual df")
    expect_match(r$analysisInfo$content, "does not make a data-selected test unbiased")

    expect_setequal(
        unique(anova_col(r$analysisResponseSN$rows, "factor")),
        c("A", "B", "C", "D"))
    expect_setequal(
        unique(anova_col(r$analysisResponseMean$rows, "factor")),
        c("A", "B", "C", "D"))
    rec <- anova_col(r$analysisRecommend$rows, "factor")
    expect_false("D" %in% rec)
    expect_setequal(rec, c("A", "B", "C"))
})

test_that("a pooled factor still in Factors is treated as error, not tested", {
    data <- l9_design()
    r <- make_results()
    out <- .doe_fill_taguchi_analysis(
        r, data, factor_names = c("A", "B", "C", "D"),
        resp_names = c("Y1", "Y2"), sn_type = "larger",
        pooled_factor = "D")
    expect_true(isTRUE(out))
    terms <- anova_col(r$analysisSnAnova$rows, "term")
    expect_equal(terms[length(terms)], "Pooled error (D)")
    expect_false("D" %in% terms)
    expect_equal(r$analysisSnAnova$rows[[length(terms)]]$df, 2)
})

test_that("invalid or missing pooled selections leave Taguchi ANOVA unpooled", {
    data <- l9_design()

    r_missing <- make_results()
    .doe_fill_taguchi_analysis(
        r_missing, data, factor_names = c("A", "B", "C", "D"),
        resp_names = c("Y1", "Y2"), sn_type = "larger")
    terms_missing <- anova_col(r_missing$analysisSnAnova$rows, "term")
    expect_true("Residuals" %in% terms_missing)
    expect_equal(
        r_missing$analysisSnAnova$rows[[length(terms_missing)]]$df, 0)

    r_bad <- make_results()
    expect_no_error(
        .doe_fill_taguchi_analysis(
            r_bad, data, factor_names = c("A", "B", "C", "D"),
            resp_names = c("Y1", "Y2"), sn_type = "larger",
            pooled_factor = "NotAColumn")
    )
    terms_bad <- anova_col(r_bad$analysisSnAnova$rows, "term")
    expect_true("Residuals" %in% terms_bad)
    expect_false(any(grepl("^Pooled error", terms_bad)))
    expect_equal(r_bad$analysisSnAnova$rows[[length(terms_bad)]]$df, 0)

    r_resp <- make_results()
    .doe_fill_taguchi_analysis(
        r_resp, data, factor_names = c("A", "B", "C", "D"),
        resp_names = c("Y1", "Y2"), sn_type = "larger",
        pooled_factor = "Y1")
    terms_resp <- anova_col(r_resp$analysisSnAnova$rows, "term")
    expect_true("Residuals" %in% terms_resp)
    expect_equal(r_resp$analysisSnAnova$rows[[length(terms_resp)]]$df, 0)
})

test_that("SN delta ranks the planted L9 factor first", {
    data <- l9_design()
    r <- make_results()
    out <- .doe_fill_taguchi_analysis(
        r, data, factor_names = c("A", "B", "C", "D"),
        resp_names = c("Y1", "Y2"), sn_type = "larger")
    expect_true(isTRUE(out))

    fac <- anova_col(r$analysisSnDelta$rows, "factor")
    rnk <- anova_col(r$analysisSnDelta$rows, "rank")
    expect_setequal(fac, c("A", "B", "C", "D"))
    expect_equal(fac[rnk == 1], "A")
    expect_true(is.data.frame(r$analysisSnDeltaPlot$state))
    expect_true(all(c("factor", "delta") %in% names(r$analysisSnDeltaPlot$state)))
    expect_false(any(vapply(r$analysisSnDeltaPlot$state,
                            function(o) inherits(o, "lm"), logical(1))))
})

test_that("SN ANOVA percent contribution sums to 100", {
    data <- l9_design()
    r <- make_results()
    .doe_fill_taguchi_analysis(
        r, data, factor_names = c("A", "B", "C"),
        resp_names = c("Y1", "Y2"), sn_type = "larger",
        pooled_factor = "D")
    contrib <- vapply(r$analysisSnAnova$rows, function(row) row$contrib, numeric(1))
    expect_true(all(is.finite(contrib)))
    expect_equal(sum(contrib), 100, tolerance = 1e-8)
})

test_that("predicted SN matches the additive main-effect formula", {
    data <- l9_design()
    r <- make_results()
    .doe_fill_taguchi_analysis(
        r, data, factor_names = c("A", "B", "C", "D"),
        resp_names = c("Y1", "Y2"), sn_type = "larger")

    Y <- as.matrix(data[, c("Y1", "Y2"), drop = FALSE])
    sn <- apply(Y, 1, function(row) .doe_sn_ratio(row, type = "larger"))
    rec <- do.call(rbind, lapply(r$analysisRecommend$rows, as.data.frame,
                                 stringsAsFactors = FALSE))
    expected <- .doe_sn_predict_additive(sn, data, rec)

    items <- vapply(r$analysisSnPredict$rows, function(row) row$item, character(1))
    sns <- vapply(r$analysisSnPredict$rows, function(row) row$sn, numeric(1))
    expect_equal(unname(sns[items == "Overall mean SN"]), expected$overall)
    expect_equal(unname(sns[items == "Predicted SN (preferred settings)"]),
                 expected$predicted)
    expect_equal(unname(sns[items == "Change"]), expected$change)
    expect_gt(expected$predicted, expected$overall)
    expect_match(r$analysisInfo$content, "Predicted SN at the preferred settings")
})

test_that("SN delta rank and additive prediction helpers are consistent", {
    level_means <- data.frame(
        factor = c("A", "A", "B", "B"),
        level = c("1", "2", "1", "2"),
        meanSN = c(1, 5, 2, 3),
        stringsAsFactors = FALSE)
    delta <- .doe_sn_delta_rank(level_means)
    expect_equal(delta$factor, c("A", "B"))
    expect_equal(delta$delta, c(4, 1))
    expect_equal(delta$rank, c(1L, 2L))

    sn <- c(1, 2, 3, 4)
    dat <- data.frame(A = c("L", "L", "H", "H"),
                      B = c("L", "H", "L", "H"),
                      stringsAsFactors = FALSE)
    rec <- data.frame(factor = c("A", "B"), level = c("H", "H"),
                      stringsAsFactors = FALSE)
    pred <- .doe_sn_predict_additive(sn, dat, rec)
    expect_equal(pred$overall, 2.5)
    expect_equal(pred$predicted, 4)
    expect_equal(pred$change, 1.5)
})
