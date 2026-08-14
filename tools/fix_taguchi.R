root <- if (file.exists("R/doe-utils.R")) "." else ".."
source(file.path(root, "R/doe-utils.R"))

oa4 <- .doe_make_oa("L4.2.3", c("A", "B", "C"), c(2, 2, 2), randomize = FALSE, seed = 1)
stopifnot(nrow(oa4) == 4)

oa8 <- .doe_make_oa("L8.2.7", c("A", "B", "C"), c(2, 2, 2), randomize = FALSE, seed = 1)
stopifnot(nrow(oa8) == 8)

oa9 <- .doe_make_oa("L9.3.4", c("A", "B", "C"), c(3, 3, 3), randomize = FALSE, seed = 1)
stopifnot(nrow(oa9) == 9)

# Mismatch should fail clearly (3-level factors on L4)
err <- tryCatch(
    .doe_make_oa("L4.2.3", c("A", "B"), c(3, 3), randomize = FALSE, seed = 1),
    error = function(e) e
)
stopifnot(inherits(err, "error"))
stopifnot(grepl("does not fit", err$message))

message("TAGUCHI_FIX_OK")
