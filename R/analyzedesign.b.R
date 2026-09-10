analyzedesignClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "analyzedesignClass",
    inherit = analyzedesignBase,
    private = list(
        .run = function() {
            resp <- .doe_selected_vars(self$options$responses)
            facs <- .doe_selected_vars(self$options$factors)

            if (length(resp) < 1 || length(facs) < 1) {
                self$results$analysisInfo$setContent(paste0(
                    "<p>Move the column you measured into <b>Responses</b>, and the ",
                    "columns you varied into <b>Factors</b>.</p>",
                    "<p>If you built the runs with one of the DOE design analyses, ",
                    "these are the columns it wrote into the spreadsheet.</p>"
                ))
                return()
            }

            data <- self$data
            if (is.null(data) || nrow(data) == 0) {
                self$results$analysisInfo$setContent(
                    "<p>The spreadsheet has no rows to analyze.</p>")
                return()
            }

            # A selection can outlive the column it points at (renamed or deleted
            # in the spreadsheet), and a name that is not a column would reach
            # lm() as an unusable term.
            resp <- resp[resp %in% names(data)]
            facs <- facs[facs %in% names(data)]
            pooled <- .doe_selected_vars(self$options$pooledFactor)
            pooled <- pooled[pooled %in% names(data)]
            if (length(resp) < 1 || (length(facs) < 1 && length(pooled) < 1)) {
                self$results$analysisInfo$setContent(paste0(
                    "<p>The selected columns are no longer in the spreadsheet. ",
                    "Choose the responses and factors again.</p>"
                ))
                return()
            }

            has_values <- vapply(resp, function(nm) {
                any(is.finite(.doe_numeric_col(data[[nm]])))
            }, logical(1))
            if (!any(has_values)) {
                self$results$analysisInfo$setContent(
                    .doe_no_response_message(resp, facs, data))
                return()
            }
            empty_note <- if (all(has_values)) "" else paste0(
                "<p>Skipped ",
                paste0("<code>", resp[!has_values], "</code>", collapse = ", "),
                " — no measured values yet.</p>"
            )
            resp <- resp[has_values]

            orig <- unique(c(resp, facs, pooled))
            conv <- .doe_syntactic_columns(data, orig)
            data <- conv$data
            safe <- stats::setNames(conv$cols, orig)
            resp <- unname(safe[resp])
            facs <- unname(safe[facs])
            pooled <- unname(safe[pooled])
            note <- paste0(.doe_rename_note(conv, unique(c(resp, facs, pooled))), empty_note)

            goal <- self$options$goal
            target <- suppressWarnings(as.numeric(self$options$target))
            if (length(target) != 1)
                target <- NA_real_

            if (identical(self$options$method, "taguchi")) {
                private$.runTaguchi(data, resp, facs, goal, target, note,
                                    pooled = pooled)
                return()
            }

            preset <- self$options$model
            rhs <- NULL
            if (identical(preset, "custom")) {
                terms <- trimws(paste(self$options$customTerms, collapse = " "))
                if (nzchar(terms))
                    rhs <- terms
                else
                    preset <- "main2fi"
            }

            resp_info <- data.frame(
                name = resp,
                goal = rep(goal, length(resp)),
                target = rep(target, length(resp)),
                stringsAsFactors = FALSE
            )

            .doe_fill_lm_analysis(
                self$results, data,
                dep = resp[1],
                factors = facs,
                model_preset = preset,
                plot_opts = .doe_eval_plot_opts(self$options),
                resp_info = resp_info,
                rhs = rhs,
                extra_note = note,
                exclude_terms = self$options$excludeTerms,
                effect_fdr = isTRUE(self$options$effectFDR)
            )
        },
        .runTaguchi = function(data, resp, facs, goal, target, note,
                               pooled = character(0)) {
            sn_type <- self$options$snType
            if (identical(sn_type, "auto")) {
                sn_type <- .doe_goal_to_sn(goal)
                note <- paste0(
                    note, "<p>SN type from the goal (",
                    .doe_goal_label(goal, target), ") &rarr; <code>", sn_type, "</code>.</p>"
                )
            }
            if (length(resp) < 2) {
                note <- paste0(
                    note, "<p>Signal-to-noise needs repeated measurements. Add every ",
                    "repeat of a run as its own response column to get a meaningful SN ratio.</p>"
                )
            }
            .doe_fill_taguchi_analysis(
                self$results, data,
                factor_names = facs,
                resp_names = resp,
                sn_type = sn_type,
                sn_model = isTRUE(self$options$snModel),
                plot_sn = isTRUE(self$options$plotSN),
                plot_means = isTRUE(self$options$plotMeans),
                extra_note = note,
                pooled_factor = pooled
            )
        },
        .plotEffectSummary = function(image, ...) .doe_plot_effect_summary(image),
        .plotHalfNormal = function(image, ...) .doe_plot_half_normal(image),
        .plotPareto = function(image, ...) .doe_plot_pareto(image),
        .plotMainEffects = function(image, ...) .doe_plot_main_effects(image),
        .plotInteraction = function(image, ...) .doe_plot_interaction(image),
        .plotResiduals = function(image, ...) .doe_plot_residuals(image),
        .plotContour = function(image, ...) .doe_plot_contour(image),
        .plotSN = function(image, ...) .doe_plot_sn(image),
        .plotMeans = function(image, ...) .doe_plot_means(image),
        .plotSnDelta = function(image, ...) .doe_plot_sn_delta(image)
    )
)
