screeningClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "screeningClass",
    inherit = screeningBase,
    private = list(
        .run = function() {
            factors <- .doe_parse_factors(self$options$factors)
            if (nrow(factors) < 2) {
                self$results$info$setContent(
                    "<p>Click <b>Add factor</b> and enter at least two 2-level factors. The design appears here once they are added.</p>")
                return()
            }
            factors$name <- .doe_safe_names(factors$name)
            nfac <- nrow(factors)
            nruns <- .doe_int_opt(self$options$nRuns, default = 8L, min = 4L)
            ncenter <- .doe_int_opt(self$options$centerPoints, default = 0L, min = 0L)
            reps <- .doe_int_opt(self$options$replicates)
            rand <- isTRUE(self$options$randomize)
            seed <- .doe_int_opt(self$options$seed, default = 1L, min = NA_integer_)
            if (rand && is.finite(seed))
                set.seed(seed)

            factor.names <- as.list(factors$levels)
            names(factor.names) <- factors$name
            for (i in seq_len(nfac)) {
                if (length(factor.names[[i]]) != 2)
                    factor.names[[i]] <- factor.names[[i]][1:2]
            }

            dtype <- self$options$designType
            des <- tryCatch({
                if (identical(dtype, "pb")) {
                    FrF2::pb(
                        nruns = nruns,
                        nfactors = nfac,
                        factor.names = factor.names,
                        ncenter = ncenter,
                        replications = 1,
                        randomize = rand
                    )
                } else {
                    FrF2::FrF2(
                        nruns = nruns,
                        nfactors = nfac,
                        factor.names = factor.names,
                        ncenter = ncenter,
                        replications = 1,
                        randomize = rand
                    )
                }
            }, error = function(e) e)

            if (inherits(des, "error")) {
                jmvcore::reject(des$message)
                return()
            }

            fac_df <- .doe_repeat_design(.doe_as_data_frame(des), reps)

            resp_info <- .doe_parse_responses_opt(self)
            resp_df <- .doe_build_response_df(
                fac_df,
                names = resp_info$name,
                simulate = isTRUE(self$options$simulateResponses),
                seed = seed
            )
            df <- fac_df
            if (!is.null(resp_df))
                df <- cbind(df, resp_df, stringsAsFactors = FALSE)

            res_txt <- tryCatch({
                di <- attr(des, "design.info")
                if (!is.null(di$catlg.entry)) {
                    paste(di$catlg.entry[[1]]$res, collapse = "")
                } else if (identical(dtype, "pb")) {
                    "Plackett-Burman / nonregular"
                } else {
                    "see alias structure"
                }
            }, error = function(e) "unknown")

            .doe_present_design(
                self,
                paste0(
                    "<p><b>Screening design</b> (", dtype, ") with ", nfac,
                    " factors. ", .doe_runs_sentence(nrow(df), reps),
                    " Requested base size ", nruns, ". Resolution: ", res_txt, ".</p>"
                ),
                df, fac_df, resp_df, seed
            )

            alias_txt <- if (identical(dtype, "frf2")) {
                .doe_alias_text(des)
            } else {
                "Alias structure for Plackett-Burman designs is complex (nonregular)."
            }
            self$results$alias$setContent(alias_txt)
        }
    )
)
