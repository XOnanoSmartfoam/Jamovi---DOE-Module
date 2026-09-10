# A render is a separate pass from the run: jamovi has to serialise each
# Image's state and restore it before the render function is called. A fitted
# model in state keeps its terms' .Environment, which is the frame of
# .doe_fill_lm_analysis and therefore reaches the results object itself -- a
# circular, multi-megabyte blob. That render never completed, so the image
# stayed in ANALYSIS_RENDERING and showed a spinner forever. These tests pin
# the state down to plain data.

l4_data <- function() {
    d <- data.frame(
        Shoe    = c("Rubber", "EVA", "EVA", "Rubber"),
        Surface = c("Asphalt", "Asphalt", "Carpet", "Carpet"),
        XO.SOLE = c("Ground", "None", "Ground", "None"),
        stringsAsFactors = FALSE
    )
    d$Quality.of.Signal <- c(49.181, 46.399, 41.464, 66.167)
    d
}

continuous_data <- function() {
    d <- expand.grid(Temp = c(150, 200), Time = c(10, 30), Rate = c(1, 2))
    set.seed(4)
    d$Yield <- 50 + 0.1 * d$Temp + 0.5 * d$Time + stats::rnorm(8)
    d
}

image_states <- function(res, names) {
    out <- list()
    for (nm in names) {
        it <- tryCatch(res[[nm]], error = function(e) NULL)
        out[[nm]] <- if (is.null(it)) NULL else tryCatch(it$state, error = function(e) NULL)
    }
    out
}

# Anything that reaches an environment or a closure drags an unbounded slice of
# the session into the serialised state.
carries_environment <- function(obj, depth = 0) {
    if (depth > 6) return(FALSE)
    if (is.environment(obj) || is.function(obj)) return(TRUE)
    if (!is.null(attr(obj, ".Environment"))) return(TRUE)
    if (is.list(obj))
        for (el in obj) if (carries_environment(el, depth + 1)) return(TRUE)
    for (a in names(attributes(obj)))
        if (carries_environment(attr(obj, a), depth + 1)) return(TRUE)
    FALSE
}

test_that("no image state contains a fitted model", {
    res <- do.call(jmvdoe::analyzedesign, list(
        data = l4_data(), responses = "Quality.of.Signal",
        factors = c("Shoe", "Surface", "XO.SOLE"),
        method = "regression", model = "main2fi", goal = "maximize",
        halfNormal = TRUE, pareto = TRUE, residuals = TRUE,
        mainEffects = TRUE, interaction = TRUE, contour = TRUE))

    states <- image_states(res, c("analysisHalfNormal", "analysisPareto",
                                  "analysisMainEffects", "analysisInteraction",
                                  "analysisResiduals", "analysisContour"))
    for (nm in names(states)) {
        st <- states[[nm]]
        if (is.null(st)) next
        expect_false(any(vapply(st, function(o) inherits(o, "lm"), logical(1))),
                     info = paste(nm, "holds an lm"))
        expect_false(carries_environment(st),
                     info = paste(nm, "state reaches an environment or closure"))
    }
})

test_that("image state stays far below the size that broke rendering", {
    res <- do.call(jmvdoe::analyzedesign, list(
        data = l4_data(), responses = "Quality.of.Signal",
        factors = c("Shoe", "Surface", "XO.SOLE"),
        method = "regression", model = "main2fi", goal = "maximize",
        halfNormal = TRUE, pareto = TRUE, residuals = TRUE,
        mainEffects = TRUE, interaction = TRUE))

    states <- image_states(res, c("analysisHalfNormal", "analysisPareto",
                                  "analysisMainEffects", "analysisInteraction",
                                  "analysisResiduals"))
    for (nm in names(states)) {
        st <- states[[nm]]
        expect_false(is.null(st), info = paste(nm, "should have state"))
        kb <- length(serialize(st, NULL)) / 1024
        # The regression was 3892.9 KB for four rows of data.
        expect_lt(kb, 50)
    }
})

test_that("every state survives a serialise round trip and still renders", {
    res <- do.call(jmvdoe::analyzedesign, list(
        data = l4_data(), responses = "Quality.of.Signal",
        factors = c("Shoe", "Surface", "XO.SOLE"),
        method = "regression", model = "main2fi", goal = "maximize",
        halfNormal = TRUE, pareto = TRUE, residuals = TRUE,
        mainEffects = TRUE, interaction = TRUE))

    renderers <- list(
        analysisHalfNormal  = jmvdoe:::.doe_plot_half_normal,
        analysisPareto      = jmvdoe:::.doe_plot_pareto,
        analysisMainEffects = jmvdoe:::.doe_plot_main_effects,
        analysisInteraction = jmvdoe:::.doe_plot_interaction,
        analysisResiduals   = jmvdoe:::.doe_plot_residuals)

    states <- image_states(res, names(renderers))
    for (nm in names(renderers)) {
        st <- states[[nm]]
        expect_false(is.null(st), info = nm)
        restored <- unserialize(serialize(st, NULL))

        path <- tempfile(fileext = ".png")
        grDevices::png(path, width = 500, height = 400)
        drawn <- tryCatch(renderers[[nm]](list(state = restored)),
                          finally = grDevices::dev.off())
        expect_true(isTRUE(drawn), info = paste(nm, "did not draw"))
        expect_true(file.exists(path) && file.info(path)$size > 0,
                    info = paste(nm, "wrote no file"))
        unlink(path)
    }
})

test_that("the contour surface is predicted during the run, not at render", {
    res <- do.call(jmvdoe::analyzedesign, list(
        data = continuous_data(), responses = "Yield",
        factors = c("Temp", "Time", "Rate"),
        method = "regression", model = "main", goal = "maximize",
        contour = TRUE))

    st <- res[["analysisContour"]]$state
    expect_false(is.null(st))
    expect_true(all(c("grid", "xname", "yname") %in% names(st)))
    expect_true(is.data.frame(st$grid))
    expect_true(all(c("x", "y", "pred") %in% names(st$grid)))
    expect_false(any(vapply(st, function(o) inherits(o, "lm"), logical(1))))
    expect_false(carries_environment(st))

    path <- tempfile(fileext = ".png")
    grDevices::png(path, width = 500, height = 400)
    drawn <- tryCatch(jmvdoe:::.doe_plot_contour(
        list(state = unserialize(serialize(st, NULL)))),
        finally = grDevices::dev.off())
    expect_true(isTRUE(drawn))
    unlink(path)
})

test_that("a contour grid is refused when there are not two continuous factors", {
    d <- l4_data()
    fit <- stats::lm(Quality.of.Signal ~ Shoe + Surface, data = d)
    expect_null(jmvdoe:::.doe_contour_grid(fit, d, c("Shoe", "Surface")))
})

test_that("a contour grid is refused when a factor never varies", {
    d <- continuous_data()
    d$Time <- 10
    fit <- stats::lm(Yield ~ Temp + Time, data = d)
    expect_null(jmvdoe:::.doe_contour_grid(fit, d, c("Temp", "Time")))
})

test_that("effect summary plot state is plain numbers and still renders", {
    res <- do.call(jmvdoe::analyzedesign, list(
        data = continuous_data(), responses = "Yield",
        factors = c("Temp", "Time", "Rate"),
        method = "regression", model = "main2fi", goal = "maximize",
        effectSummary = TRUE))

    st <- res[["analysisEffectPlot"]]$state
    expect_false(is.null(st))
    expect_true(is.data.frame(st))
    expect_true(all(c("term", "logworth") %in% names(st)))
    expect_false(carries_environment(st))
    expect_false(any(vapply(st, function(o) inherits(o, "lm"), logical(1))))
    kb <- length(serialize(st, NULL)) / 1024
    expect_lt(kb, 50)

    path <- tempfile(fileext = ".png")
    grDevices::png(path, width = 500, height = 400)
    drawn <- tryCatch(jmvdoe:::.doe_plot_effect_summary(
        list(state = unserialize(serialize(st, NULL)))),
        finally = grDevices::dev.off())
    expect_true(isTRUE(drawn))
    expect_true(file.exists(path) && file.info(path)$size > 0)
    unlink(path)
})

test_that("taguchi SN delta plot state is plain numbers and still renders", {
    d <- l4_data()
    res <- do.call(jmvdoe::analyzedesign, list(
        data = d, responses = "Quality.of.Signal",
        factors = c("Shoe", "Surface", "XO.SOLE"),
        method = "taguchi", snType = "larger",
        plotSN = TRUE, plotMeans = TRUE))

    st <- res[["analysisSnDeltaPlot"]]$state
    expect_false(is.null(st))
    expect_true(is.data.frame(st))
    expect_true(all(c("factor", "delta") %in% names(st)))
    expect_false(carries_environment(st))
    expect_false(any(vapply(st, function(o) inherits(o, "lm"), logical(1))))
    kb <- length(serialize(st, NULL)) / 1024
    expect_lt(kb, 50)

    path <- tempfile(fileext = ".png")
    grDevices::png(path, width = 450, height = 400)
    drawn <- tryCatch(jmvdoe:::.doe_plot_sn_delta(
        list(state = unserialize(serialize(st, NULL)))),
        finally = grDevices::dev.off())
    expect_true(isTRUE(drawn))
    expect_true(file.exists(path) && file.info(path)$size > 0)
    unlink(path)
})
