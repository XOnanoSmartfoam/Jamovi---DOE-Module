root <- if (file.exists("R/doe-utils.R")) "." else ".."
source(file.path(root, "R/doe-utils.R"))

stopifnot(identical(.doe_parse_levels("-1, 1"), c("-1", "1")))
y <- c(10, 12, 11)
stopifnot(abs(.doe_sn_ratio(y, "smaller") - (-10 * log10(mean(y^2)))) < 1e-9)
stopifnot(abs(.doe_sn_ratio(y, "larger") - (-10 * log10(mean(1 / y^2)))) < 1e-9)
stopifnot(abs(.doe_sn_ratio(y, "nominal") - (10 * log10(mean(y)^2 / var(y)))) < 1e-9)

oa <- .doe_make_oa("L8.2.7", c("A", "B", "C"), c(2, 2, 2), randomize = FALSE, seed = 1)
stopifnot(nrow(oa) == 8)
oa9 <- .doe_make_oa("L9.3.4", c("A", "B", "C"), c(3, 3, 3), randomize = FALSE, seed = 1)
stopifnot(nrow(oa9) == 9)

# Design-level replicates expand run count (not response columns)
oa4x3 <- .doe_make_oa(
    "L4.2.3", c("A", "B", "C"), c(2, 2, 2),
    randomize = FALSE, seed = 1, replications = 3
)
stopifnot(nrow(oa4x3) == 12)

ff <- DoE.base::fac.design(
    factor.names = list(A = c(-1, 1), B = c(-1, 1), C = c("L", "H")),
    replications = 2,
    randomize = FALSE
)
stopifnot(nrow(ff) == 16)

sc <- FrF2::FrF2(nruns = 8, nfactors = 4, replications = 2, randomize = FALSE)
stopifnot(nrow(sc) == 16)

bbd <- rsm::bbd(3, n0 = 3, randomize = FALSE)
bbd_df <- .doe_repeat_design(as.data.frame(bbd), 3)
stopifnot(nrow(bbd) == 15, nrow(bbd_df) == 45)
stopifnot(identical(bbd_df$Run[c(1, 16, 31)], c("1.1", "1.2", "1.3")))

cand <- expand.grid(A = c(-1, 0, 1), B = c(-1, 0, 1), C = factor(c("L", "H")))
opt <- AlgDesign::optFederov(~ A + B + C, data = cand, nTrials = 10, criterion = "D")
stopifnot(nrow(opt$design) == 10)
opt_rep <- .doe_repeat_design(opt$design, 2)
stopifnot(nrow(opt_rep) == 20, identical(opt_rep$Run[c(1, 11)], c("1.1", "1.2")))

# Native DoE packages expose run.no.std.rp as Run
ff_df <- .doe_as_data_frame(ff)
stopifnot("Run" %in% names(ff_df), identical(ff_df$Run[c(1, 9)], c("1.1", "1.2")))
oa_df <- .doe_as_data_frame(oa4x3)
stopifnot(identical(oa_df$Run[c(1, 5, 9)], c("1.1", "1.2", "1.3")))

# Response columns are Y1, Y2, ... (no response replicates)
fac <- data.frame(A = c(-1, 1, -1, 1), B = c(-1, -1, 1, 1))
resp <- .doe_build_response_df(fac, n_responses = 2, simulate = TRUE, seed = 1)
stopifnot(ncol(resp) == 2, identical(names(resp), c("Y1", "Y2")))
stopifnot(all(is.finite(as.matrix(resp))))

message("ALL_SMOKE_OK")
