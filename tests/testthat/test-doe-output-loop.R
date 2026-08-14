# Re-sending identical Output data re-triggers the analysis in jamovi, which
# starves later option changes. Guard against that regression.

source(file.path("..", "..", "R", "doe-utils.R"))

make_output <- function() {
    opts <- jmvcore::Options$new()
    opts$.addOption(jmvcore::OptionOutput$new("designOutput"))
    jmvcore::Output$new(
        options = opts, name = "designOutput",
        title = "Spreadsheet columns", initInRun = TRUE
    )
}

fac <- data.frame(
    Run = c("1.1", "2.1", "1.2", "2.2"),
    A = c(-1, 1, -1, 1),
    stringsAsFactors = FALSE
)
resp <- data.frame(Y = c(1, 2, 3, 4))

test_that("first write sends data, identical second run does not", {
    skip_if_not_installed("jmvcore")
    out <- make_output()

    first <- .doe_write_to_spreadsheet(out, fac, response_df = resp)
    expect_true(first$ok)
    expect_null(first$unchanged)
    expect_equal(first$n_rows, 4)

    second <- .doe_write_to_spreadsheet(out, fac, response_df = resp)
    expect_true(second$ok)
    expect_true(isTRUE(second$unchanged))
})

test_that("a changed design is sent again", {
    skip_if_not_installed("jmvcore")
    out <- make_output()
    .doe_write_to_spreadsheet(out, fac, response_df = resp)

    stacked <- .doe_repeat_design(fac[, "A", drop = FALSE], 3)
    resp3 <- data.frame(Y = seq_len(nrow(stacked)))
    third <- .doe_write_to_spreadsheet(out, stacked, response_df = resp3)
    expect_true(third$ok)
    expect_null(third$unchanged)
    expect_equal(third$n_rows, 12)
})

test_that("clearing state (as jamovi does on an option change) forces a write", {
    skip_if_not_installed("jmvcore")
    out <- make_output()
    .doe_write_to_spreadsheet(out, fac, response_df = resp)
    expect_true(isTRUE(.doe_write_to_spreadsheet(out, fac, response_df = resp)$unchanged))

    out$setState(NULL)
    again <- .doe_write_to_spreadsheet(out, fac, response_df = resp)
    expect_true(again$ok)
    expect_null(again$unchanged)
})
