customdesignClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "customdesignClass",
    inherit = customdesignBase,
    private = list(
        .run = function() {
            factors <- .doe_parse_factors(self$options$factors)
            if (nrow(factors) < 1) {
                self$results$info$setContent("<p>Click <b>Add factor</b> and enter a name and levels. The design appears here once you add at least one factor.</p>")
                return()
            }
            factors$name <- .doe_safe_names(factors$name)
            nruns <- .doe_int_opt(self$options$nRuns, default = 12L, min = 4L)
            reps <- .doe_int_opt(self$options$replicates)
            model <- self$options$model
            criterion <- self$options$criterion
            rand <- isTRUE(self$options$randomize)
            seed <- .doe_int_opt(self$options$seed, default = 1L, min = NA_integer_)
            if (is.finite(seed))
                set.seed(seed)

            cand <- tryCatch(.doe_candidate_set(factors), error = function(e) e)
            if (inherits(cand, "error")) {
                jmvcore::reject(cand$message)
                return()
            }

            for (i in seq_len(nrow(factors))) {
                nm <- factors$name[i]
                if (identical(factors$type[i], "continuous") && nm %in% names(cand))
                    cand[[nm]] <- as.numeric(as.character(cand[[nm]]))
            }

            frml <- .doe_model_formula(factors$name, model)
            opt <- tryCatch(
                AlgDesign::optFederov(
                    frml = frml,
                    data = cand,
                    nTrials = nruns,
                    criterion = criterion,
                    evaluateI = identical(criterion, "I"),
                    nRepeats = 5
                ),
                error = function(e) e
            )
            if (inherits(opt, "error")) {
                jmvcore::reject(opt$message)
                return()
            }

            fac_df <- opt$design
            rownames(fac_df) <- NULL
            if (rand)
                fac_df <- fac_df[sample.int(nrow(fac_df)), , drop = FALSE]
            for (nm in names(fac_df)) {
                if (is.factor(fac_df[[nm]]))
                    fac_df[[nm]] <- as.character(fac_df[[nm]])
            }
            fac_df <- .doe_repeat_design(fac_df, reps)

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

            deff <- tryCatch({
                if (!is.null(opt$D)) sprintf("D-efficiency: %.4f", opt$D) else ""
            }, error = function(e) "")
            ieff <- tryCatch({
                if (!is.null(opt$I)) sprintf("I-criterion: %.4f", opt$I) else ""
            }, error = function(e) "")

            .doe_present_design(
                self,
                paste0(
                    "<p><b>Custom ", criterion, "-optimal design</b> for a ", model,
                    " model with ", nrow(factors), " factors. ",
                    .doe_runs_sentence(nrow(df), reps),
                    " Requested ", nruns, " base runs.</p>"
                ),
                df, fac_df, resp_df, seed
            )
            self$results$efficiency$setContent(paste(c(deff, ieff), collapse = "\n"))
        }
    )
)
