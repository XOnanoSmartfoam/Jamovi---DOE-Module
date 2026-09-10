# When nothing in Responses can be modelled, the message has to tell the two
# causes apart: an experiment that has not been run yet, versus Responses and
# Factors filled in the wrong way round. The old wording assumed the first and
# told people to type in values they had already typed in.

swap_data <- function() {
    d <- data.frame(
        Shoe    = c("Rubber", "EVA", "EVA", "Rubber"),
        Surface = c("Asphalt", "Asphalt", "Carpet", "Carpet"),
        XO.SOLE = c("Ground", "None", "Ground", "None"),
        stringsAsFactors = FALSE
    )
    d$Quality.of.Signal <- c(49.181, 46.399, 41.464, 66.167)
    d
}

test_that("swapped responses and factors are diagnosed as a swap", {
    d <- swap_data()
    msg <- jmvdoe:::.doe_no_response_message(
        resp = c("Shoe", "Surface", "XO.SOLE"),
        facs = "Quality.of.Signal",
        data = d)

    expect_match(msg, "look swapped")
    expect_match(msg, "hold text rather than measurements")
    # It has to name the offending columns, not just gesture at them.
    expect_match(msg, "Shoe")
    expect_match(msg, "Quality.of.Signal")
    # And it must not repeat the misleading advice.
    expect_false(grepl("still empty", msg))
})

test_that("a genuinely empty numeric response keeps the original wording", {
    d <- swap_data()
    d$Quality.of.Signal <- NA_real_
    msg <- jmvdoe:::.doe_no_response_message(
        resp = "Quality.of.Signal",
        facs = c("Shoe", "Surface", "XO.SOLE"),
        data = d)

    expect_match(msg, "still empty")
    expect_match(msg, "Run the experiment")
    expect_false(grepl("swapped", msg))
})

test_that("text responses with no numeric factor are not called a swap", {
    d <- swap_data()
    d$Notes <- c("a", "b", "c", "d")
    msg <- jmvdoe:::.doe_no_response_message(
        resp = "Notes",
        facs = c("Shoe", "Surface", "XO.SOLE"),
        data = d)

    expect_match(msg, "hold text rather than measurements")
    expect_false(grepl("swapped", msg))
    expect_match(msg, "Put the column you measured")
})

test_that("the empty-numeric message survives the whole analysis", {
    d <- swap_data()
    d$Quality.of.Signal <- NA_real_
    res <- do.call(jmvdoe::analyzedesign, list(
        data = d, responses = "Quality.of.Signal",
        factors = c("Shoe", "Surface", "XO.SOLE"), method = "regression"))

    expect_match(res$analysisInfo$content, "still empty")
})

test_that("an early exit never leaves an image with state but nothing drawn", {
    # State with no rendered file is reported as ANALYSIS_RENDERING, which is a
    # spinner that never resolves.
    d <- swap_data()
    d$Quality.of.Signal <- NA_real_
    res <- do.call(jmvdoe::analyzedesign, list(
        data = d, responses = "Quality.of.Signal",
        factors = c("Shoe", "Surface", "XO.SOLE"),
        method = "regression", halfNormal = TRUE, pareto = TRUE,
        residuals = TRUE, mainEffects = TRUE, interaction = TRUE,
        contour = TRUE))

    for (nm in c("analysisHalfNormal", "analysisPareto", "analysisMainEffects",
                 "analysisInteraction", "analysisResiduals", "analysisContour",
                 "analysisEffectPlot", "analysisPlotSN", "analysisPlotMeans",
                 "analysisSnDeltaPlot")) {
        it <- tryCatch(res[[nm]], error = function(e) NULL)
        if (is.null(it)) next
        expect_null(tryCatch(it$state, error = function(e) NULL),
                    info = paste(nm, "has state after an early exit"))
    }
})

test_that("a column of numeric text still counts as measurable", {
    expect_true(jmvdoe:::.doe_looks_numeric(c("1.5", "2.5")))
    expect_true(jmvdoe:::.doe_looks_numeric(c(1.5, NA)))
    expect_true(jmvdoe:::.doe_looks_numeric(as.numeric(c(NA, NA))))
    expect_false(jmvdoe:::.doe_looks_numeric(c("Rubber", "EVA")))
    expect_false(jmvdoe:::.doe_looks_numeric(factor(c("Asphalt", "Carpet"))))
})

test_that("column lists read as English", {
    expect_equal(jmvdoe:::.doe_and_list("a"), "<code>a</code>")
    expect_equal(jmvdoe:::.doe_and_list(c("a", "b")), "<code>a</code> and <code>b</code>")
    expect_equal(jmvdoe:::.doe_and_list(c("a", "b", "c")),
                 "<code>a</code>, <code>b</code> and <code>c</code>")
})
