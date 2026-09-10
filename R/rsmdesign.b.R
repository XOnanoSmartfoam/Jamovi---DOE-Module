rsmdesignClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "rsmdesignClass",
    inherit = rsmdesignBase,
    private = list(
        .run = function() {
            fac <- .doe_parse_continuous_factors(self$options$factors)
            if (nrow(fac) < 2) {
                self$results$info$setContent(
                    "<p>Click <b>Add factor</b> and enter at least two continuous factors with numeric low, high settings. The design appears here once they are added.</p>")
                return()
            }
            fac$name <- .doe_safe_names(fac$name)
            k <- nrow(fac)
            dtype <- self$options$designType
            n0 <- .doe_int_opt(self$options$n0, default = 3L, min = 1L)
            reps <- .doe_int_opt(self$options$replicates)
            rand <- isTRUE(self$options$randomize)
            seed <- .doe_int_opt(self$options$seed, default = 1L, min = NA_integer_)
            if (rand && is.finite(seed))
                set.seed(seed)

            des <- tryCatch({
                if (identical(dtype, "bbd")) {
                    if (k < 3 || k > 7)
                        stop("Box-Behnken designs require 3 to 7 factors.")
                    rsm::bbd(k, n0 = n0, randomize = rand)
                } else {
                    alpha <- if (identical(self$options$alpha, "faces")) 1 else "rotatable"
                    rsm::ccd(k, n0 = n0, alpha = alpha, randomize = rand)
                }
            }, error = function(e) e)

            if (inherits(des, "error")) {
                jmvcore::reject(des$message)
                return()
            }

            coded <- as.data.frame(des)
            xcols <- grep("^x[0-9]+$", names(coded), value = TRUE)
            if (length(xcols) == 0)
                xcols <- setdiff(names(coded), c("run.order", "std.order", "Block"))
            coded_df <- coded[, xcols, drop = FALSE]
            names(coded_df) <- fac$name[seq_along(xcols)]

            actual_df <- coded_df
            for (i in seq_along(xcols)) {
                nm <- fac$name[i]
                lo <- fac$low[i]
                hi <- fac$high[i]
                mid <- (lo + hi) / 2
                half <- (hi - lo) / 2
                actual_df[[nm]] <- mid + as.numeric(coded_df[[nm]]) * half
            }

            # CCD/BBD: stack whole design; Run = std.rep (DoE style)
            actual_df <- .doe_repeat_design(actual_df, reps)
            coded_df <- .doe_repeat_design(coded_df, reps)

            resp_info <- .doe_parse_responses_opt(self)
            resp_df <- .doe_build_response_df(
                actual_df,
                names = resp_info$name,
                simulate = isTRUE(self$options$simulateResponses),
                seed = seed
            )

            actual_out <- actual_df
            if (!is.null(resp_df))
                actual_out <- cbind(actual_out, resp_df, stringsAsFactors = FALSE)
            coded_out <- coded_df

            .doe_present_design(
                self,
                paste0(
                    "<p><b>Response surface design</b> (", toupper(dtype), ") with ",
                    k, " factors. ", .doe_runs_sentence(nrow(actual_out), reps), "</p>"
                ),
                actual_out, actual_df, resp_df, seed
            )
            .doe_fill_table(self$results$coded, coded_out)
        }
    )
)
