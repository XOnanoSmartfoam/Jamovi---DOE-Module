taguchiarraysClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "taguchiarraysClass",
    inherit = taguchiarraysBase,
    private = list(
        .run = function() {
            factors <- .doe_parse_factors(self$options$factors, default_levels = c("1", "2"))
            if (nrow(factors) < 1) {
                self$results$info$setContent("<p>Click <b>Add factor</b> and enter a name and levels. The design appears here once you add at least one control factor.</p>")
                return()
            }
            factors$name <- .doe_safe_names(factors$name)
            nlevels <- vapply(factors$levels, length, integer(1))
            array_id <- self$options$arrayId
            reps <- .doe_int_opt(self$options$replicates, default = 2L, min = 2L)
            rand <- isTRUE(self$options$randomize)
            seed <- .doe_int_opt(self$options$seed, default = 1L, min = NA_integer_)

            resolved <- tryCatch(
                .doe_taguchi_resolve_array(array_id, nlevels),
                error = function(e) e
            )
            if (inherits(resolved, "error")) {
                self$results$info$setContent(paste0("<p>", resolved$message, "</p>"))
                return()
            }
            array_id <- resolved$row$id[1]
            note_array <- resolved$note

            inner <- tryCatch(
                .doe_make_oa(
                    array_id, factors$name, nlevels,
                    randomize = rand, seed = seed, replications = 1L
                ),
                error = function(e) e
            )
            if (inherits(inner, "error")) {
                self$results$info$setContent(paste0("<p>", inner$message, "</p>"))
                return()
            }

            df <- .doe_repeat_design(.doe_as_data_frame(inner), reps)
            keep <- intersect(c("Run", factors$name), names(df))
            if (length(intersect(factors$name, names(df))) == 0)
                keep <- names(df)
            factor_df <- df[, keep, drop = FALSE]
            factor_df <- .doe_relabel_design(factor_df, factors)

            resp_info <- .doe_parse_responses_opt(self)

            note_outer <- ""
            response_df <- .doe_build_response_df(
                factor_df,
                names = resp_info$name,
                simulate = isTRUE(self$options$simulateResponses),
                seed = seed
            )
            if (isTRUE(self$options$useOuter)) {
                outer_res <- .doe_build_outer_response_df(
                    factor_df,
                    self$options$noiseFactors,
                    resp_info = resp_info,
                    simulate = isTRUE(self$options$simulateResponses),
                    seed = seed
                )
                note_outer <- outer_res$note
                if (isTRUE(outer_res$ok) && !is.null(outer_res$df))
                    response_df <- outer_res$df
            }

            out <- factor_df
            if (!is.null(response_df))
                out <- cbind(out, response_df, stringsAsFactors = FALSE)
            out <- .doe_ensure_run_column(out, reps)

            cat <- .doe_taguchi_catalog()
            lab <- cat$label[match(array_id, cat$id)]
            if (is.na(lab)) lab <- array_id

            .doe_present_design(
                self,
                paste0(
                    note_array,
                    "<p><b>Taguchi array</b>: ", lab, " with ", nrow(factors),
                    " control factor(s). ", .doe_runs_sentence(nrow(out), reps),
                    note_outer, "</p>"
                ),
                out, factor_df, response_df, seed
            )
        }
    )
)
