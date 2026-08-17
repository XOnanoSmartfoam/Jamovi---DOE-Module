# The spreadsheet write must be opt-in. Writing on every run makes jamovi
# rewrite the analysis options server side, which freezes the options panel.

source(file.path("..", "..", "R", "doe-utils.R"))

fake_analysis <- function(addToSpreadsheet = FALSE, designOutput = FALSE,
                          filled = FALSE) {
    written <- new.env(parent = emptyenv())
    written$touched <- FALSE
    list(
        options = list(
            addToSpreadsheet = addToSpreadsheet,
            designOutput = designOutput,
            simulateResponses = FALSE
        ),
        results = list(designOutput = list(
            mark = function() written$touched <- TRUE,
            isFilled = function() filled
        )),
        written = written
    )
}

fac <- data.frame(
    Run = c("1.1", "2.1"),
    A = c(-1, 1),
    stringsAsFactors = FALSE
)
resp <- data.frame(Y = c(1, 2))

test_that("preview mode never touches the Output", {
    self <- fake_analysis()
    res <- .doe_try_write_design(self, fac, resp, seed = 1)

    expect_false(isTRUE(res$ok))
    expect_true(isTRUE(res$disabled))
    expect_false(self$written$touched)
})

test_that("checking Columns in Data without the button does not write", {
    self <- fake_analysis(designOutput = TRUE, filled = FALSE)
    res <- .doe_try_write_design(self, fac, resp, seed = 1)

    expect_false(isTRUE(res$ok))
    expect_true(isTRUE(res$disabled))
    expect_false(self$written$touched)
})

test_that("the preview tip explains how to send the design", {
    tip <- .doe_write_tip(list(ok = FALSE, disabled = TRUE), n_y = 1)
    expect_match(tip, "preview")
    expect_match(tip, "Add design to spreadsheet")
    expect_false(grepl("Sent ", tip, fixed = TRUE))
})

test_that("the button writes and reports what was sent", {
    skip_if_not_installed("jmvcore")
    opts <- jmvcore::Options$new()
    opts$.addOption(jmvcore::OptionAction$new("addToSpreadsheet", TRUE))
    opts$.addOption(jmvcore::OptionOutput$new("designOutput"))
    out <- jmvcore::Output$new(
        options = opts, name = "designOutput",
        title = "Spreadsheet columns", initInRun = TRUE
    )
    self <- list(
        options = list(
            addToSpreadsheet = TRUE,
            designOutput = TRUE,
            simulateResponses = FALSE
        ),
        results = list(designOutput = out)
    )

    res <- .doe_try_write_design(self, fac, resp, seed = 1)
    expect_true(isTRUE(res$ok))
    expect_equal(res$n_rows, 2)
    expect_match(.doe_write_tip(res, n_y = 1), "Sent ")
})

test_that("a checked Output box without the button does not write", {
    skip_if_not_installed("jmvcore")
    opts <- jmvcore::Options$new()
    opts$.addOption(jmvcore::OptionOutput$new("designOutput"))
    out <- jmvcore::Output$new(
        options = opts, name = "designOutput",
        title = "Spreadsheet columns", initInRun = TRUE
    )
    self <- list(
        options = list(
            addToSpreadsheet = FALSE,
            designOutput = TRUE,
            simulateResponses = FALSE
        ),
        results = list(designOutput = out)
    )

    res <- .doe_try_write_design(self, fac, resp, seed = 1)
    expect_false(isTRUE(res$ok))
    expect_true(isTRUE(res$disabled))
})
