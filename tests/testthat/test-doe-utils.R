# Standalone tests for shared DOE helpers (no jamovi runtime required)

source(file.path("..", "..", "R", "doe-utils.R"))

test_that("parse levels handles commas and whitespace", {
    expect_equal(.doe_parse_levels("-1, 1"), c("-1", "1"))
    expect_equal(.doe_parse_levels("Low, Medium, High"), c("Low", "Medium", "High"))
    expect_equal(.doe_parse_levels(""), character(0))
})

test_that("SN ratio formulas match standard Taguchi definitions", {
    y <- c(10, 12, 11)
    expect_equal(.doe_sn_ratio(y, "smaller"), -10 * log10(mean(y^2)))
    expect_equal(.doe_sn_ratio(y, "larger"), -10 * log10(mean(1 / y^2)))
    expect_equal(.doe_sn_ratio(y, "nominal"), 10 * log10(mean(y)^2 / var(y)))
})

test_that("model formula builders work", {
    expect_true(inherits(.doe_model_formula(c("A", "B"), "main"), "formula"))
    expect_true(inherits(.doe_model_formula(c("A", "B"), "main2fi"), "formula"))
    expect_true(inherits(.doe_model_formula(c("A", "B"), "rsm"), "formula"))
})

test_that("full factorial run count matches product of levels", {
    skip_if_not_installed("DoE.base")
    des <- DoE.base::fac.design(
        factor.names = list(A = c(-1, 1), B = c(-1, 1), C = c("L", "H")),
        randomize = FALSE
    )
    expect_equal(nrow(des), 8)
})

test_that("Taguchi L8 OA has 8 runs", {
    skip_if_not_installed("DoE.base")
    des <- .doe_make_oa("L8.2.7", c("A", "B", "C"), c(2, 2, 2), randomize = FALSE, seed = 1)
    expect_equal(nrow(des), 8)
})

test_that("three 3-level factors switch from L4 to L9", {
    resolved <- .doe_taguchi_resolve_array("L4.2.3", c(3, 3, 3))
    expect_true(resolved$switched)
    expect_equal(resolved$row$id[1], "L9.3.4")
    skip_if_not_installed("DoE.base")
    des <- .doe_make_oa("L4.2.3", c("Shoe", "Sock", "Surface"), c(3, 3, 3),
                       randomize = FALSE, seed = 1)
    expect_equal(nrow(des), 9)
})

test_that("two-level factors keep L4", {
    resolved <- .doe_taguchi_resolve_array("L4.2.3", c(2, 2, 2))
    expect_false(resolved$switched)
    expect_equal(resolved$row$id[1], "L4.2.3")
})

test_that("integer options coerce from jamovi-like values", {
    expect_equal(.doe_int_opt(3), 3L)
    expect_equal(.doe_int_opt("3"), 3L)
    expect_equal(.doe_int_opt("r3"), 3L)
    expect_equal(.doe_int_opt("r10"), 10L)
    expect_equal(.doe_int_opt("1 copy"), 1L)
    expect_equal(.doe_int_opt("3 copies"), 3L)
    expect_equal(.doe_int_opt(list(value = "4")), 4L)
    expect_equal(.doe_int_opt(integer(0)), 1L)
    expect_equal(.doe_int_opt(NA_integer_), 1L)
    expect_equal(.doe_int_opt(NULL, default = 8L, min = 4L), 8L)
    expect_equal(.doe_int_opt(0, min = 0L), 0L)
    expect_equal(.doe_int_opt(-2, min = 1L), 1L)
})

test_that("design replicates use DoE-style run.rep labels", {
    skip_if_not_installed("DoE.base")
    des <- .doe_make_oa(
        "L4.2.3", c("A", "B", "C"), c(2, 2, 2),
        randomize = FALSE, seed = 1, replications = 3
    )
    expect_equal(nrow(des), 12)
    df <- .doe_as_data_frame(des)
    expect_equal(df$Run[c(1, 5, 9)], c("1.1", "1.2", "1.3"))

    base <- data.frame(A = c(-1, 1), B = c(-1, 1))
    repd <- .doe_repeat_design(base, 3)
    expect_equal(nrow(repd), 6)
    expect_equal(repd$Run, c("1.1", "2.1", "1.2", "2.2", "1.3", "2.3"))

    one <- .doe_make_oa(
        "L4.2.3", c("A", "B", "C"), c(2, 2, 2),
        randomize = FALSE, seed = 1, replications = 1
    )
    stacked <- .doe_repeat_design(.doe_as_data_frame(one), 3)
    expect_equal(nrow(stacked), 12)
    expect_equal(stacked$Run[c(1, 5, 9)], c("1.1", "1.2", "1.3"))
})

test_that("response builder creates Y columns without response replicates", {
    fac <- data.frame(A = c(-1, 1, -1, 1), B = c(-1, -1, 1, 1))
    resp <- .doe_build_response_df(fac, n_responses = 2, simulate = TRUE, seed = 1)
    expect_equal(names(resp), c("Y1", "Y2"))
})

test_that("FrF2 2^(4-1) has 8 runs", {
    skip_if_not_installed("FrF2")
    des <- FrF2::FrF2(nruns = 8, nfactors = 4, randomize = FALSE)
    expect_equal(nrow(des), 8)
})

test_that("BBD generates 15 runs for 3 factors with 3 centers", {
    skip_if_not_installed("rsm")
    bbd <- rsm::bbd(3, n0 = 3, randomize = FALSE)
    expect_equal(nrow(bbd), 15)
})

test_that("AlgDesign D-optimal returns requested trials", {
    skip_if_not_installed("AlgDesign")
    cand <- expand.grid(A = c(-1, 0, 1), B = c(-1, 0, 1), C = factor(c("L", "H")))
    opt <- AlgDesign::optFederov(~ A + B + C, data = cand, nTrials = 10, criterion = "D")
    expect_equal(nrow(opt$design), 10)
})

test_that("response goals parse and preferred levels follow max/min/match", {
    source(file.path("..", "..", "R", "doe-analyze.R"))
    opt <- list(
        list(name = "Yield", goal = "maximize"),
        list(name = "Defects", goal = "minimize"),
        list(name = "Thickness", goal = "match", target = "50")
    )
    info <- .doe_parse_responses(opt)
    expect_equal(info$name, c("Yield", "Defects", "Thickness"))
    expect_equal(info$goal, c("maximize", "minimize", "match"))
    expect_equal(info$target[3], 50)
    expect_equal(.doe_goal_to_sn("match"), "nominal")
    expect_match(.doe_goal_label("match", 50), "50")

    dat <- data.frame(
        A = c(-1, -1, 1, 1),
        Y = c(10, 12, 20, 22)
    )
    rec_max <- .doe_recommend_settings(dat, "Y", "A", "maximize")
    rec_min <- .doe_recommend_settings(dat, "Y", "A", "minimize")
    rec_match <- .doe_recommend_settings(dat, "Y", "A", "match", target = 11)
    expect_equal(rec_max$level, "1")
    expect_equal(rec_min$level, "-1")
    expect_equal(rec_match$level, "-1")

    shared <- .doe_parse_responses(
        list(list(name = "Thickness", goal = "match")),
        target = 50
    )
    expect_equal(shared$target, 50)
    ignored <- .doe_parse_responses(
        list(list(name = "Yield", goal = "maximize")),
        target = 50
    )
    expect_true(is.na(ignored$target))
})

test_that("outer array adds Y columns and does not fail without noise factors", {
    skip_if_not_installed("DoE.base")
    fac <- data.frame(Run = c("1", "2", "3", "4"),
                      A = c("-1", "1", "-1", "1"),
                      B = c("-1", "-1", "1", "1"),
                      stringsAsFactors = FALSE)
    empty <- .doe_build_outer_response_df(fac, list(), resp_info = data.frame(
        name = "Y", goal = "maximize", target = NA_real_
    ))
    expect_false(empty$ok)
    expect_null(empty$df)

    noise <- list(list(name = "Humidity", levels = "Low, High"))
    outer <- .doe_build_outer_response_df(fac, noise, resp_info = data.frame(
        name = "Yield", goal = "match", target = 50
    ), simulate = TRUE, seed = 1)
    expect_true(outer$ok)
    expect_equal(nrow(outer$df), 4)
    expect_equal(ncol(outer$df), 2)
    expect_true(all(grepl("^Yield_", names(outer$df))))

    source(file.path("..", "..", "R", "doe-analyze.R"))
    data <- cbind(fac, outer$df)
    expect_equal(.doe_response_names(data, "Yield"), names(outer$df))
})

test_that("evaluation helpers pick Y columns and coerce factors", {
    source(file.path("..", "..", "R", "doe-analyze.R"))
    fac <- data.frame(A = c("-1", "1", "-1", "1"), B = c("-1", "-1", "1", "1"),
                      stringsAsFactors = FALSE)
    ens <- .doe_ensure_eval_responses(fac, NULL, seed = 1)
    expect_true(ens$simulated)
    expect_equal(names(ens$resp), "Y1")
    data <- cbind(fac, ens$resp)
    expect_equal(.doe_response_names(data), "Y1")
    expect_equal(.doe_factor_names_from_design(data), c("A", "B"))
    coerced <- .doe_coerce_for_lm(data, c("A", "B"), "Y1")
    expect_true(is.numeric(coerced$A))
    expect_true(is.numeric(coerced$Y1))
})

test_that("spreadsheet payload includes Run, factors, and responses", {
    fac <- data.frame(
        Run = c("1.1", "2.1", "1.2", "2.2"),
        A = c(-1, 1, -1, 1),
        B = c("Low", "Low", "High", "High"),
        stringsAsFactors = FALSE
    )
    resp <- data.frame(Yield = c(10, 12, 11, 13))
    payload <- .doe_build_output_payload(fac, response_df = resp)
    expect_true(payload$ok)
    expect_equal(payload$n_rows, 4)
    expect_equal(payload$titles, c("Run", "A", "B", "Yield"))
    expect_equal(payload$measureTypes, c("nominal", "continuous", "nominal", "continuous"))
    expect_equal(as.character(payload$values[[1]]), c("1.1", "2.1", "1.2", "2.2"))
    expect_equal(payload$keys, 1:4)
    expect_false(is.factor(payload$values[[1]]))
    expect_equal(payload$values[[4]], c(10, 12, 11, 13))

    empty <- .doe_build_output_payload(data.frame())
    expect_false(empty$ok)

    sig_a <- .doe_output_signature(payload)
    payload2 <- .doe_build_output_payload(fac, response_df = resp)
    expect_identical(sig_a, .doe_output_signature(payload2))
    resp3 <- data.frame(Yield = c(10, 12, 11, 99))
    expect_false(identical(
        sig_a, .doe_output_signature(.doe_build_output_payload(fac, response_df = resp3))
    ))
    tip_on <- .doe_write_tip(list(ok = TRUE, n_rows = 4), 1, simulate = TRUE)
    expect_match(tip_on, "Sent 4 run")
    expect_false(grepl("already exist", tip_on))
    expect_match(.doe_preview_tip(), "Design Table")
})
