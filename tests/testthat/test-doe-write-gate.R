# The spreadsheet write is driven by the designOutput Output option. jmvcore
# only emits Output columns while that option is enabled, so writing with the
# box unchecked is wasted work and the results panel must not claim success.

source(file.path("..", "..", "R", "doe-utils.R"))

fake_analysis <- function(designOutput = FALSE) {
    written <- new.env(parent = emptyenv())
    written$touched <- FALSE
    list(
        options = list(
            designOutput = designOutput,
            simulateResponses = FALSE
        ),
        results = list(designOutput = list(
            mark = function() written$touched <- TRUE
        )),
        written = written
    )
}

make_output <- function(enabled = FALSE) {
    opts <- jmvcore::Options$new()
    opts$.addOption(jmvcore::OptionOutput$new("designOutput"))
    if (enabled) {
        # jamovi stores an Output option as a list, and Output$enabled reads
        # isTRUE(option$value$value).
        opt <- opts$option("designOutput")
        opt$value <- list(value = TRUE)
    }
    jmvcore::Output$new(
        options = opts, name = "designOutput",
        title = "Spreadsheet columns", initInRun = TRUE
    )
}

fac <- data.frame(
    Run = c("1.1", "2.1"),
    A = c(-1, 1),
    stringsAsFactors = FALSE
)
resp <- data.frame(Y = c(1, 2))

test_that("an unchecked box reports preview and never touches the Output", {
    self <- fake_analysis(designOutput = FALSE)
    res <- .doe_try_write_design(self, fac, resp, seed = 1)

    expect_false(isTRUE(res$ok))
    expect_true(isTRUE(res$disabled))
    expect_false(self$written$touched)
})

test_that("a real Output that jamovi has not enabled is treated as disabled", {
    skip_if_not_installed("jmvcore")
    self <- list(
        options = list(designOutput = TRUE, simulateResponses = FALSE),
        results = list(designOutput = make_output())
    )

    # jmvcore wraps asProtoBuf in if (self$enabled), so anything written while
    # the option is off is silently dropped before it reaches the spreadsheet.
    expect_false(self$results$designOutput$enabled)
    res <- .doe_try_write_design(self, fac, resp, seed = 1)
    expect_true(isTRUE(res$disabled))
})

test_that("the preview tip explains how to send the design", {
    tip <- .doe_write_tip(list(ok = FALSE, disabled = TRUE), n_y = 1)
    expect_match(tip, "preview")
    expect_match(tip, "Add design to spreadsheet")
    expect_false(grepl("Sent ", tip, fixed = TRUE))
})

test_that("a checked box writes and reports what was sent", {
    skip_if_not_installed("jmvcore")
    self <- list(
        options = list(designOutput = TRUE, simulateResponses = FALSE),
        results = list(designOutput = make_output(enabled = TRUE))
    )

    expect_true(self$results$designOutput$enabled)
    res <- .doe_try_write_design(self, fac, resp, seed = 1)
    expect_true(isTRUE(res$ok))
    expect_equal(res$n_rows, 2)
    expect_match(.doe_write_tip(res, n_y = 1), "Sent ")
})

test_that("a later run re-declares the columns instead of dropping them", {
    skip_if_not_installed("jmvcore")
    out <- make_output(enabled = TRUE)
    expect_true(.doe_write_to_spreadsheet(out, fac, response_df = resp)$ok)

    # jamovi rebuilds the results objects on every run and restores the state.
    # Returning early here would send an empty column list, which makes the
    # server delete the design columns it created earlier.
    again <- make_output(enabled = TRUE)
    again$setState(out$state)
    res <- .doe_write_to_spreadsheet(again, fac, response_df = resp)

    expect_true(res$ok)
    expect_true(isTRUE(res$unchanged))
    expect_false(again$isNotFilled())
})
