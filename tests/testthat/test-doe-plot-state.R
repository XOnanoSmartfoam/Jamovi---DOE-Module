# jamovi reports a visible image that has state but no rendered file as still
# rendering, which the user sees as a spinner that never resolves. So a plot
# must only be given state when it can actually be drawn.

fac_levels <- list(Surface = c("Asphalt", "Carpet"),
                   Shoe    = c("Rubber", "EVA"),
                   XO.SOLE = c("Ground", "None"),
                   User    = c("Jake", "Brandon"))

nominal_design <- function() {
    set.seed(1)
    d <- expand.grid(fac_levels, stringsAsFactors = FALSE)
    d[["Data Quality"]] <- round(runif(nrow(d), 40, 90), 3)
    d
}

facs <- names(fac_levels)

state_of <- function(res, name) {
    it <- tryCatch(res[[name]], error = function(e) NULL)
    if (is.null(it)) return(NULL)
    tryCatch(it$state, error = function(e) NULL)
}

test_that("contour gets no state when no two factors are continuous", {
    skip_if_not_installed("jmvcore")

    res <- do.call(jmvdoe::analyzedesign, list(
        data = nominal_design(),
        responses = "Data Quality",
        factors = facs,
        method = "regression",
        model = "main2fi",
        contour = TRUE
    ))

    expect_null(state_of(res, "analysisContour"))
    # The plots that can be drawn still are.
    expect_false(is.null(state_of(res, "analysisHalfNormal")))
})

test_that("the skipped contour plot is explained in the model summary", {
    skip_if_not_installed("jmvcore")

    res <- do.call(jmvdoe::analyzedesign, list(
        data = nominal_design(),
        responses = "Data Quality",
        factors = facs,
        method = "regression",
        contour = TRUE
    ))

    html <- res[["analysisInfo"]]$content
    expect_match(html, "Not plotted")
    expect_match(html, "contour")
})

test_that("interaction gets no state with a single factor", {
    skip_if_not_installed("jmvcore")

    d <- nominal_design()
    res <- do.call(jmvdoe::analyzedesign, list(
        data = d[, c("Surface", "Data Quality")],
        responses = "Data Quality",
        factors = "Surface",
        method = "regression",
        model = "main",
        interaction = TRUE
    ))

    expect_null(state_of(res, "analysisInteraction"))
})

test_that("an effects plot with nothing estimable renders a note, not nothing", {
    # Returning FALSE here would leave the image without a file, and an image
    # with state and no file is what spins forever.
    empty <- data.frame(term = character(0), effect = numeric(0),
                        abs_effect = numeric(0), stringsAsFactors = FALSE)
    image <- list(state = empty)

    pdf(NULL)
    on.exit(dev.off(), add = TRUE)
    expect_true(jmvdoe:::.doe_plot_half_normal(image))
    expect_true(jmvdoe:::.doe_plot_pareto(image))
    empty_lw <- data.frame(term = character(0), logworth = numeric(0),
                           stringsAsFactors = FALSE)
    expect_true(jmvdoe:::.doe_plot_effect_summary(list(state = empty_lw)))
})

test_that("a plot with no state at all still renders nothing", {
    image <- list(state = NULL)
    expect_false(jmvdoe:::.doe_plot_half_normal(image))
    expect_false(jmvdoe:::.doe_plot_contour(image))
    expect_false(jmvdoe:::.doe_plot_sn(image))
    expect_false(jmvdoe:::.doe_plot_effect_summary(image))
    expect_false(jmvdoe:::.doe_plot_sn_delta(image))
})
