# Effect Summary: Type III logworth, FDR, excluding terms and refitting.
# The table is term-level (JMP Source), not coefficient-level.

root <- if (file.exists("R/doe-utils.R")) "." else "../.."
source(file.path(root, "R/doe-utils.R"))
source(file.path(root, "R/doe-analyze.R"))

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
              "analysisEffects", "analysisEffectPlot",
              "analysisHalfNormal", "analysisPareto", "analysisMainEffects",
              "analysisInteraction", "analysisResiduals", "analysisContour")
    stats::setNames(lapply(keys, function(k) make_item()), keys)
}

cont_data <- function() {
    set.seed(11)
    d <- expand.grid(A = c(-1, 1), B = c(-1, 1), C = c(-1, 1))
    d <- d[rep(seq_len(nrow(d)), each = 3), , drop = FALSE]
    d$Y <- 10 + 3 * d$A + 0.2 * d$B + 2.5 * d$A * d$B + stats::rnorm(nrow(d), sd = 0.4)
    rownames(d) <- NULL
    d
}

test_that("term aliases match JMP-style stars and R formula names", {
    expect_equal(.doe_canonicalize_term("A:B"), "A:B")
    expect_equal(.doe_canonicalize_term("A*B"), "A:B")
    expect_equal(.doe_canonicalize_term(" A * B "), "A:B")
    expect_equal(.doe_canonicalize_term("I(A^2)"), "I(A^2)")
    expect_equal(.doe_canonicalize_term("A^2"), "I(A^2)")
    expect_equal(.doe_canonicalize_term("A*A"), "I(A^2)")
    expect_true(.doe_terms_equal("A:B", "B*A"))
    expect_true(.doe_terms_equal("I(Temp^2)", "Temp*Temp"))
    expect_false(.doe_terms_equal("A:B", "A"))
    expect_equal(.doe_effect_label("A:B"), "A*B")
    expect_equal(.doe_effect_label("I(A^2)"), "A*A")
})

test_that("logworth is -log10(p), capped when p is 0", {
    expect_equal(.doe_logworth(0.01), 2)
    expect_equal(.doe_logworth(0.001), 3)
    expect_equal(.doe_logworth(0), 16)
    expect_true(is.na(.doe_logworth(NA_real_)))
    expect_true(is.na(.doe_logworth(-0.1)))
})

test_that("exclude expands the model and drops the matching term", {
    dropped <- .doe_rhs_without_terms("(A + B)^2 + I(A^2)", "A*B")
    keep <- strsplit(dropped$rhs, " + ", fixed = TRUE)[[1]]
    expect_true(any(vapply(dropped$dropped, function(t) .doe_terms_equal(t, "A:B"),
                           logical(1))))
    expect_false(any(vapply(keep, function(t) .doe_terms_equal(t, "A:B"),
                            logical(1))))
    expect_true("I(A^2)" %in% keep)
    expect_equal(dropped$missing, character(0))

    miss <- .doe_rhs_without_terms("A + B", "Nope")
    expect_equal(miss$rhs, "A + B")
    expect_equal(miss$missing, "Nope")
})

test_that("Type III p-values match drop-each-term extra SS", {
    d <- cont_data()
    fit <- stats::lm(Y ~ A * B, data = d)
    eff <- .doe_type3_effects(fit, d)
    expect_setequal(eff$term, c("A", "B", "A:B"))
    expect_equal(eff$label[eff$term == "A:B"], "A*B")

    red <- stats::lm(Y ~ A + B, data = d)
    cmp <- stats::anova(red, fit)
    expect_equal(eff$p[eff$term == "A:B"], cmp[2, "Pr(>F)"], tolerance = 1e-10)
    expect_equal(eff$logworth[eff$term == "A:B"],
                 .doe_logworth(cmp[2, "Pr(>F)"]), tolerance = 1e-10)

    # Balanced extra-SS tests the main even while the interaction stays in.
    red_a <- stats::lm(Y ~ B + A:B, data = d)
    cmp_a <- stats::anova(red_a, fit)
    expect_equal(eff$p[eff$term == "A"], cmp_a[2, "Pr(>F)"], tolerance = 1e-10)
    expect_gt(eff$logworth[eff$term == "A"], 2)
})

test_that("FDR raises p-values and lowers logworth", {
    d <- cont_data()
    fit <- stats::lm(Y ~ A * B * C, data = d)
    raw <- .doe_type3_effects(fit, d)
    fdr <- .doe_apply_effect_fdr(raw)
    ok <- is.finite(raw$p)
    expect_true(all(fdr$p[ok] + 1e-12 >= raw$p[ok]))
    expect_true(all(fdr$logworth[ok] <= raw$logworth[ok] + 1e-12))
})

test_that("Effect Summary is sorted by logworth and omits the pooled residual", {
    d <- cont_data()
    r <- make_results()
    out <- .doe_fill_lm_analysis(r, d, dep = "Y", factors = c("A", "B"),
                                 model_preset = "main2fi", resp_info = NULL)
    expect_true(isTRUE(out))
    terms <- vapply(r$analysisEffects$rows, `[[`, "", "term")
    expect_false("Residuals" %in% terms)
    expect_true("A*B" %in% terms)
    lw <- vapply(r$analysisEffects$rows, `[[`, 0, "logworth")
    expect_equal(lw, sort(lw, decreasing = TRUE, na.last = TRUE))
    p <- vapply(r$analysisEffects$rows, `[[`, 0, "p")
    expect_true(all(is.finite(p)))
    expect_equal(r$analysisEffectPlot$state$term[1], r$analysisEffects$rows[[1]]$term)
    expect_true(is.data.frame(r$analysisEffectPlot$state))
    expect_true(all(is.finite(r$analysisEffectPlot$state$logworth)))
})

test_that("excluding an interaction refits without that term", {
    d <- cont_data()
    r <- make_results()
    out <- .doe_fill_lm_analysis(
        r, d, dep = "Y", factors = c("A", "B"),
        model_preset = "main2fi", resp_info = NULL,
        exclude_terms = "A*B")
    expect_true(isTRUE(out))
    src <- vapply(r$analysisEffects$rows, `[[`, "", "term")
    expect_false("A*B" %in% src)
    expect_setequal(src, c("A", "B"))
    anova_terms <- vapply(r$analysisAnova$rows, `[[`, "", "term")
    expect_false("A:B" %in% anova_terms)
    expect_match(r$analysisInfo$content, "Excluded from the model")
    expect_match(r$analysisInfo$content, "A\\*B")
})

test_that("an unknown exclude name is reported and the model is unchanged", {
    d <- cont_data()
    r <- make_results()
    .doe_fill_lm_analysis(r, d, dep = "Y", factors = c("A", "B"),
                          model_preset = "main", resp_info = NULL,
                          exclude_terms = "NotATerm")
    src <- vapply(r$analysisEffects$rows, `[[`, "", "term")
    expect_setequal(src, c("A", "B"))
    expect_match(r$analysisInfo$content, "No model term matched")
    expect_match(r$analysisInfo$content, "NotATerm")
})

test_that("a saturated fit leaves logworth blank instead of spinning a plot", {
    d <- data.frame(A = c(-1, 1, -1, 1), B = c(-1, -1, 1, 1),
                    Y = c(1, 4, 2, 5))
    r <- make_results()
    expect_no_error(
        out <- .doe_fill_lm_analysis(r, d, dep = "Y", factors = c("A", "B"),
                                     model_preset = "main2fi", resp_info = NULL)
    )
    expect_true(isTRUE(out))
    lw <- vapply(r$analysisEffects$rows, function(row) row$logworth, numeric(1))
    expect_true(all(is.na(lw)))
    expect_null(r$analysisEffectPlot$state)
    expect_match(r$analysisInfo$content, "effect summary plot")
})

test_that("FDR in the filled table matches p.adjust of the Type III p-values", {
    d <- cont_data()
    r_raw <- make_results()
    .doe_fill_lm_analysis(r_raw, d, dep = "Y", factors = c("A", "B", "C"),
                          model_preset = "main2fi", resp_info = NULL)
    r_fdr <- make_results()
    .doe_fill_lm_analysis(r_fdr, d, dep = "Y", factors = c("A", "B", "C"),
                          model_preset = "main2fi", resp_info = NULL,
                          effect_fdr = TRUE)
    p_raw <- vapply(r_raw$analysisEffects$rows, `[[`, 0, "p")
    p_fdr <- vapply(r_fdr$analysisEffects$rows, `[[`, 0, "p")
    # Rows are sorted by logworth, so match on the source name.
    raw_by <- stats::setNames(p_raw, vapply(r_raw$analysisEffects$rows, `[[`, "", "term"))
    fdr_by <- stats::setNames(p_fdr, vapply(r_fdr$analysisEffects$rows, `[[`, "", "term"))
    expect_equal(unname(fdr_by[names(raw_by)]),
                 unname(stats::p.adjust(raw_by, method = "BH")[names(raw_by)]),
                 tolerance = 1e-10)
    expect_match(r_fdr$analysisInfo$content, "FDR")
    expect_true(isTRUE(r_fdr$analysisEffectPlot$state$fdr[1]))
})
