fullfactorialClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "fullfactorialClass",
    inherit = fullfactorialBase,
    private = list(
        .run = function() {
            factors <- .doe_parse_factors(self$options$factors)
            if (nrow(factors) < 1) {
                self$results$info$setContent(
                    "<p>Click <b>Add factor</b> and enter a name and levels. The design appears here once you add at least one factor.</p>")
                return()
            }

            factors$name <- .doe_safe_names(factors$name)
            nlevels <- vapply(factors$levels, length, integer(1))
            if (any(nlevels < 2)) {
                jmvcore::reject("Each factor needs at least two levels.")
                return()
            }

            factor.names <- as.list(factors$levels)
            names(factor.names) <- factors$name

            reps <- .doe_int_opt(self$options$replicates)
            rand <- isTRUE(self$options$randomize)
            seed <- .doe_int_opt(self$options$seed, default = 1L, min = NA_integer_)
            if (rand && is.finite(seed))
                set.seed(seed)

            des <- tryCatch(
                DoE.base::fac.design(
                    factor.names = factor.names,
                    replications = 1,
                    randomize = rand
                ),
                error = function(e) e
            )
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

            .doe_present_design(
                self,
                paste0(
                    "<p><b>Full factorial</b> with ", nrow(factors), " factors. ",
                    .doe_runs_sentence(nrow(df), reps), "</p>"
                ),
                df, fac_df, resp_df, seed
            )
            .doe_maybe_evaluate_lm(
                self, fac_df, resp_df, factors$name, self$options$analysisModel,
                resp_info = resp_info
            )
        },
        .plotHalfNormal = function(image, ...) .doe_plot_half_normal(image),
        .plotPareto = function(image, ...) .doe_plot_pareto(image),
        .plotMainEffects = function(image, ...) .doe_plot_main_effects(image),
        .plotInteraction = function(image, ...) .doe_plot_interaction(image),
        .plotResiduals = function(image, ...) .doe_plot_residuals(image),
        .plotContour = function(image, ...) .doe_plot_contour(image)
    )
)
