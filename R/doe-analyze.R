#' Shared model-fit / Taguchi evaluation used by design generators.

.doe_response_names <- function(df, explicit = NULL) {
    nm <- names(df)
    if (!is.null(explicit) && length(explicit) > 0) {
        hit <- intersect(explicit, nm)
        if (length(hit) > 0)
            return(hit)
        prefixed <- unique(unlist(lapply(explicit, function(p) {
            grep(paste0("^", p, "_"), nm, value = TRUE)
        })))
        if (length(prefixed) > 0)
            return(prefixed)
    }
    y <- grep("^Y[0-9]+$", nm, value = TRUE)
    if (length(y) == 0)
        y <- grep("^Y_", nm, value = TRUE)
    y
}

.doe_factor_names_from_design <- function(df, explicit = NULL) {
    if (!is.null(explicit) && length(explicit) > 0)
        return(intersect(explicit, names(df)))
    drop <- c("Run", "Replicate", "Blocks", "Block")
    setdiff(names(df), c(drop, .doe_response_names(df)))
}

.doe_coerce_for_lm <- function(data, factor_names, dep) {
    data[[dep]] <- as.numeric(data[[dep]])
    for (nm in factor_names) {
        v <- data[[nm]]
        if (is.numeric(v) && !is.factor(v))
            next
        nums <- suppressWarnings(as.numeric(as.character(v)))
        if (length(nums) == length(v) && all(is.finite(nums)))
            data[[nm]] <- nums
        else
            data[[nm]] <- factor(as.character(v))
    }
    data
}

#' If responses are missing, simulate columns so Evaluate can still run.
.doe_ensure_eval_responses <- function(factor_df, resp_df = NULL, seed = NULL,
                                       names = NULL) {
    has_y <- !is.null(resp_df) && ncol(resp_df) >= 1 &&
        any(is.finite(as.numeric(as.matrix(resp_df))))
    if (has_y)
        return(list(resp = resp_df, simulated = FALSE))
    sim <- .doe_build_response_df(
        factor_df,
        n_responses = max(1L, length(names)),
        simulate = TRUE,
        seed = seed,
        names = names
    )
    list(resp = sim, simulated = TRUE)
}

.doe_recommend_settings <- function(data, dep, factors, goal = "maximize",
                                    target = NA_real_) {
    rows <- lapply(factors, function(f) {
        means <- stats::aggregate(
            data[[dep]], by = list(level = data[[f]]),
            FUN = mean, na.rm = TRUE
        )
        if (nrow(means) < 1)
            return(NULL)
        if (identical(goal, "match") && is.finite(target))
            pick <- which.min(abs(means$x - target))
        else if (identical(goal, "minimize"))
            pick <- which.min(means$x)
        else
            pick <- which.max(means$x)
        data.frame(
            response = dep,
            goal = .doe_goal_label(goal, target),
            factor = f,
            level = as.character(means$level[pick]),
            mean = means$x[pick],
            stringsAsFactors = FALSE
        )
    })
    rows <- Filter(Negate(is.null), rows)
    if (length(rows) == 0)
        return(NULL)
    do.call(rbind, rows)
}

.doe_fill_recommend_table <- function(tbl, rec) {
    if (is.null(tbl) || is.null(rec) || nrow(rec) == 0)
        return(invisible(NULL))
    for (i in seq_len(nrow(rec))) {
        tbl$addRow(rowKey = i, values = list(
            response = rec$response[i],
            goal = rec$goal[i],
            factor = rec$factor[i],
            level = rec$level[i],
            mean = rec$mean[i]
        ))
    }
    invisible(NULL)
}

.doe_recommend_html <- function(rec) {
    if (is.null(rec) || nrow(rec) == 0)
        return("")
    bits <- vapply(split(rec, rec$response), function(d) {
        goal <- d$goal[1]
        sets <- paste0("<code>", d$factor, "</code> = <code>", d$level, "</code>",
                       collapse = ", ")
        paste0("<p>To <b>", goal, "</b> <code>", d$response[1],
               "</code>, prefer ", sets, " (best main-effect level).</p>")
    }, character(1))
    paste(bits, collapse = "")
}

.doe_item <- function(results, name) {
    if (is.null(name) || !nzchar(name))
        return(NULL)
    tryCatch(results[[name]], error = function(e) NULL)
}

.doe_fill_anova_table <- function(tbl, fit, response = NULL, start_key = 1L) {
    if (is.null(tbl))
        return(start_key)
    a <- tryCatch(stats::anova(fit), error = function(e) NULL)
    if (is.null(a))
        return(start_key)
    for (i in seq_len(nrow(a))) {
        vals <- list(
            term = rownames(a)[i],
            ss = a[i, "Sum Sq"],
            df = a[i, "Df"],
            ms = a[i, "Mean Sq"],
            F = if ("F value" %in% names(a)) a[i, "F value"] else NA,
            p = if ("Pr(>F)" %in% names(a)) a[i, "Pr(>F)"] else NA
        )
        if (!is.null(response))
            vals$response <- response
        tbl$addRow(rowKey = start_key + i - 1L, values = vals)
    }
    start_key + nrow(a)
}

.doe_fill_coef_table <- function(tbl, sm, response = NULL, start_key = 1L) {
    if (is.null(tbl))
        return(start_key)
    ct <- as.data.frame(sm$coefficients)
    for (i in seq_len(nrow(ct))) {
        vals <- list(
            term = rownames(ct)[i],
            estimate = ct[i, 1],
            se = ct[i, 2],
            t = ct[i, 3],
            p = ct[i, 4]
        )
        if (!is.null(response))
            vals$response <- response
        tbl$addRow(rowKey = start_key + i - 1L, values = vals)
    }
    start_key + nrow(ct)
}

#' Fit lm and populate jamovi analysis result items.
#' @param resp_info data.frame with name, goal for each response to fit
#' @param rhs optional model right-hand side (overrides model_preset)
.doe_fill_lm_analysis <- function(results, data, dep, factors, model_preset,
                                  plot_opts = list(), items = list(),
                                  extra_note = "", resp_info = NULL, rhs = NULL) {
    def_items <- list(
        info = "analysisInfo",
        anova = "analysisAnova",
        coef = "analysisCoef",
        recommend = "analysisRecommend",
        halfNormal = "analysisHalfNormal",
        pareto = "analysisPareto",
        mainEffects = "analysisMainEffects",
        interaction = "analysisInteraction",
        residuals = "analysisResiduals",
        contour = "analysisContour"
    )
    items <- utils::modifyList(def_items, items)

    if (is.null(resp_info) || nrow(resp_info) < 1) {
        goal <- "maximize"
        resp_info <- data.frame(name = dep, goal = goal, stringsAsFactors = FALSE)
    }
    resp_info <- resp_info[resp_info$name %in% names(data), , drop = FALSE]
    if (nrow(resp_info) < 1)
        resp_info <- data.frame(name = dep, goal = "maximize", stringsAsFactors = FALSE)

    info <- .doe_item(results, items$info)
    deps <- unique(resp_info$name)
    if (length(deps) < 1 || length(factors) < 1) {
        if (!is.null(info))
            info$setContent("<p>Evaluation needs at least one response column and one factor.</p>")
        return(invisible(FALSE))
    }

    for (d in deps)
        data <- .doe_coerce_for_lm(data, factors, d)
    keep <- unique(c(deps, factors))
    data <- data[, keep, drop = FALSE]
    data <- data[stats::complete.cases(data), , drop = FALSE]
    if (nrow(data) < 3) {
        if (!is.null(info))
            info$setContent("<p>Not enough complete rows to fit a model.</p>")
        return(invisible(FALSE))
    }

    preset <- if (is.null(model_preset) || !nzchar(model_preset)) "main" else model_preset
    if (identical(preset, "custom") && is.null(rhs))
        preset <- "main"
    if (is.null(rhs)) {
        fml_rhs <- as.character(.doe_model_formula(factors, preset))[2]
    } else {
        fml_rhs <- rhs
    }

    summaries <- character(0)
    rec_all <- list()
    first_fit <- NULL
    first_dep <- resp_info$name[1]
    first_goal <- resp_info$goal[1]
    first_data <- NULL
    anova_key <- 1L
    coef_key <- 1L

    for (i in seq_len(nrow(resp_info))) {
        d <- resp_info$name[i]
        g <- resp_info$goal[i]
        tgt <- if ("target" %in% names(resp_info)) resp_info$target[i] else NA_real_
        fml <- stats::as.formula(paste(d, "~", fml_rhs))
        fit <- tryCatch(stats::lm(fml, data = data), error = function(e) e)
        if (inherits(fit, "error")) {
            summaries <- c(summaries, paste0("<p>Could not fit <code>", d, "</code>: ",
                                             fit$message, "</p>"))
            next
        }
        sm <- summary(fit)
        summaries <- c(summaries, paste0(
            "<p><b>", d, "</b> (<i>", .doe_goal_label(g, tgt), "</i>): <code>",
            paste(deparse(fml), collapse = ""), "</code><br>",
            "R<sup>2</sup> = ", signif(sm$r.squared, 4),
            ", Adjusted R<sup>2</sup> = ", signif(sm$adj.r.squared, 4),
            ", N = ", nrow(data), ".</p>"
        ))
        anova_key <- .doe_fill_anova_table(
            .doe_item(results, items$anova), fit, response = d, start_key = anova_key)
        coef_key <- .doe_fill_coef_table(
            .doe_item(results, items$coef), sm, response = d, start_key = coef_key)
        rec <- .doe_recommend_settings(data, d, factors, g, target = tgt)
        if (!is.null(rec))
            rec_all[[length(rec_all) + 1]] <- rec
        if (identical(g, "match") && !is.finite(tgt))
            summaries <- c(summaries,
                paste0("<p>Match target for <code>", d,
                       "</code> needs a Target value to pick preferred settings.</p>"))
        if (is.null(first_fit)) {
            first_fit <- fit
            first_dep <- d
            first_goal <- g
            first_data <- data
        }
    }

    rec_df <- if (length(rec_all)) do.call(rbind, rec_all) else NULL
    .doe_fill_recommend_table(.doe_item(results, items$recommend), rec_df)

    if (!is.null(info)) {
        info$setContent(paste0(
            paste(summaries, collapse = ""),
            .doe_recommend_html(rec_df),
            extra_note
        ))
    }

    if (is.null(first_fit))
        return(invisible(FALSE))

    effects <- .doe_effects_from_lm(first_fit)
    po <- utils::modifyList(
        list(halfNormal = TRUE, pareto = TRUE, mainEffects = FALSE,
             interaction = FALSE, residuals = TRUE, contour = FALSE),
        plot_opts
    )
    plot_title <- paste0(" (", .doe_goal_label(first_goal), " ", first_dep, ")")

    if (isTRUE(po$halfNormal)) {
        img <- .doe_item(results, items$halfNormal)
        if (!is.null(img)) img$setState(effects)
    }
    if (isTRUE(po$pareto)) {
        img <- .doe_item(results, items$pareto)
        if (!is.null(img)) img$setState(effects)
    }
    if (isTRUE(po$mainEffects)) {
        img <- .doe_item(results, items$mainEffects)
        if (!is.null(img))
            img$setState(list(data = first_data, dep = first_dep, factors = factors,
                              goal = first_goal, subtitle = plot_title))
    }
    if (isTRUE(po$interaction)) {
        img <- .doe_item(results, items$interaction)
        if (!is.null(img))
            img$setState(list(data = first_data, dep = first_dep, factors = factors,
                              goal = first_goal, subtitle = plot_title))
    }
    if (isTRUE(po$residuals)) {
        img <- .doe_item(results, items$residuals)
        if (!is.null(img)) img$setState(list(fit = first_fit))
    }
    if (isTRUE(po$contour)) {
        img <- .doe_item(results, items$contour)
        if (!is.null(img))
            img$setState(list(fit = first_fit, data = first_data, factors = factors,
                              dep = first_dep, goal = first_goal))
    }
    invisible(TRUE)
}

.doe_fill_taguchi_analysis <- function(results, data, factor_names, resp_names,
                                       sn_type = "smaller", sn_model = TRUE,
                                       plot_sn = TRUE, plot_means = TRUE,
                                       items = list(), extra_note = "") {
    def_items <- list(
        info = "analysisInfo",
        perRun = "analysisPerRun",
        responseSN = "analysisResponseSN",
        responseMean = "analysisResponseMean",
        snAnova = "analysisSnAnova",
        plotSN = "analysisPlotSN",
        plotMeans = "analysisPlotMeans"
    )
    items <- utils::modifyList(def_items, items)
    info <- .doe_item(results, items$info)

    if (length(factor_names) < 1 || length(resp_names) < 1) {
        if (!is.null(info))
            info$setContent("<p>Evaluation needs control factors and at least one response column.</p>")
        return(invisible(FALSE))
    }

    for (f in factor_names)
        data[[f]] <- factor(as.character(data[[f]]))
    for (r in resp_names)
        data[[r]] <- as.numeric(data[[r]])
    data <- data[stats::complete.cases(data[, factor_names, drop = FALSE]), , drop = FALSE]
    if (nrow(data) < 1) {
        if (!is.null(info))
            info$setContent("<p>No complete rows for control factors.</p>")
        return(invisible(FALSE))
    }

    Y <- as.matrix(data[, resp_names, drop = FALSE])
    sn <- apply(Y, 1, function(row) .doe_sn_ratio(row, type = sn_type))
    mu <- apply(Y, 1, function(row) {
        row <- as.numeric(row)
        row <- row[is.finite(row)]
        if (length(row) == 0) NA_real_ else mean(row)
    })

    per <- .doe_item(results, items$perRun)
    if (!is.null(per)) {
        for (i in seq_len(nrow(data))) {
            per$addRow(rowKey = i, values = list(
                run = i,
                mean = mu[i],
                sn = sn[i]
            ))
        }
    }

    snTbl <- .doe_item(results, items$responseSN)
    meanTbl <- .doe_item(results, items$responseMean)
    rowKey <- 1
    snPlotData <- list()
    meanPlotData <- list()
    for (f in factor_names) {
        snMeans <- stats::aggregate(sn, by = list(level = data[[f]]), FUN = mean, na.rm = TRUE)
        yMeans <- stats::aggregate(mu, by = list(level = data[[f]]), FUN = mean, na.rm = TRUE)
        for (j in seq_len(nrow(snMeans))) {
            if (!is.null(snTbl)) {
                snTbl$addRow(rowKey = rowKey, values = list(
                    factor = f,
                    level = as.character(snMeans$level[j]),
                    meanSN = snMeans$x[j]
                ))
            }
            if (!is.null(meanTbl)) {
                meanTbl$addRow(rowKey = rowKey, values = list(
                    factor = f,
                    level = as.character(yMeans$level[j]),
                    meanY = yMeans$x[j]
                ))
            }
            snPlotData[[length(snPlotData) + 1]] <- data.frame(
                factor = f, level = as.character(snMeans$level[j]),
                value = snMeans$x[j], stringsAsFactors = FALSE)
            meanPlotData[[length(meanPlotData) + 1]] <- data.frame(
                factor = f, level = as.character(yMeans$level[j]),
                value = yMeans$x[j], stringsAsFactors = FALSE)
            rowKey <- rowKey + 1
        }
    }

    rec_sn <- .doe_recommend_settings(
        cbind(SN = sn, data[, factor_names, drop = FALSE]),
        "SN", factor_names, "maximize"
    )
    if (!is.null(rec_sn)) {
        rec_sn$response <- paste0("SN (", paste(resp_names, collapse = ", "), ")")
        rec_sn$goal <- paste0("maximize SN / ", sn_type)
    }
    .doe_fill_recommend_table(.doe_item(results, "analysisRecommend"), rec_sn)

    if (!is.null(info)) {
        info$setContent(paste0(
            "<p><b>Taguchi analysis</b> using <code>", sn_type,
            "</code> SN ratios across ", length(resp_names),
            " response column(s) and ", nrow(data), " inner runs.</p>",
            extra_note, .doe_recommend_html(rec_sn)
        ))
    }

    if (isTRUE(sn_model) && length(factor_names) >= 1) {
        dfm <- data[, factor_names, drop = FALSE]
        dfm$SN <- sn
        fml <- stats::as.formula(paste("SN ~", paste(factor_names, collapse = " + ")))
        fit <- tryCatch(stats::lm(fml, data = dfm), error = function(e) e)
        if (!inherits(fit, "error"))
            .doe_fill_anova_table(.doe_item(results, items$snAnova), fit)
    }

    if (isTRUE(plot_sn)) {
        img <- .doe_item(results, items$plotSN)
        if (!is.null(img) && length(snPlotData) > 0)
            img$setState(do.call(rbind, snPlotData))
    }
    if (isTRUE(plot_means)) {
        img <- .doe_item(results, items$plotMeans)
        if (!is.null(img) && length(meanPlotData) > 0)
            img$setState(do.call(rbind, meanPlotData))
    }
    invisible(TRUE)
}

.doe_eval_plot_opts <- function(options) {
    list(
        halfNormal = isTRUE(options$halfNormal),
        pareto = isTRUE(options$pareto),
        mainEffects = isTRUE(options$mainEffects),
        interaction = isTRUE(options$interaction),
        residuals = isTRUE(options$residuals),
        contour = isTRUE(options$contour)
    )
}

.doe_maybe_evaluate_lm <- function(self, factor_df, resp_df, factor_names, model_preset,
                                   resp_info = NULL) {
    if (!isTRUE(self$options$evaluateDesign))
        return(invisible(FALSE))
    if (is.null(resp_info))
        resp_info <- tryCatch(.doe_parse_responses_opt(self),
                              error = function(e) NULL)
    ens <- .doe_ensure_eval_responses(
        factor_df, resp_df, self$options$seed,
        names = if (!is.null(resp_info) && nrow(resp_info) > 0) resp_info$name else NULL
    )
    extra <- if (isTRUE(ens$simulated)) {
        "<p>Responses were simulated for this evaluation (enable <b>Fill responses with random data</b> or enter measured Y values for a real analysis).</p>"
    } else {
        ""
    }
    data <- factor_df
    if (!is.null(ens$resp))
        data <- cbind(data, ens$resp, stringsAsFactors = FALSE)
    dep <- .doe_response_names(
        data, if (!is.null(resp_info)) resp_info$name else NULL)[1]
    facs <- .doe_factor_names_from_design(data, factor_names)
    .doe_fill_lm_analysis(
        self$results, data, dep, facs, model_preset,
        plot_opts = .doe_eval_plot_opts(self$options),
        extra_note = extra,
        resp_info = resp_info
    )
}

.doe_maybe_evaluate_taguchi <- function(self, factor_df, resp_df, factor_names,
                                        resp_info = NULL) {
    if (!isTRUE(self$options$evaluateDesign))
        return(invisible(FALSE))
    if (is.null(resp_info))
        resp_info <- tryCatch(.doe_parse_responses_opt(self),
                              error = function(e) NULL)
    ens <- .doe_ensure_eval_responses(
        factor_df, resp_df, self$options$seed,
        names = if (!is.null(resp_info) && nrow(resp_info) > 0) resp_info$name else NULL
    )
    extra <- if (isTRUE(ens$simulated)) {
        "<p>Responses were simulated for this evaluation.</p>"
    } else {
        ""
    }
    data <- factor_df
    if (!is.null(ens$resp))
        data <- cbind(data, ens$resp, stringsAsFactors = FALSE)
    sn_type <- self$options$snType
    if (!is.null(resp_info) && nrow(resp_info) >= 1) {
        sn_type <- .doe_goal_to_sn(resp_info$goal[1])
        extra <- paste0(
            extra, "<p>SN type from response goal (<code>",
            resp_info$name[1], "</code>: ",
            .doe_goal_label(resp_info$goal[1], resp_info$target[1]),
            ") &rarr; <code>", sn_type, "</code>.</p>"
        )
    }
    resp_names <- .doe_response_names(
        data, if (!is.null(resp_info)) resp_info$name else NULL)
    .doe_fill_taguchi_analysis(
        self$results, data,
        factor_names = .doe_factor_names_from_design(data, factor_names),
        resp_names = resp_names,
        sn_type = sn_type,
        sn_model = isTRUE(self$options$snModel),
        plot_sn = isTRUE(self$options$plotSN),
        plot_means = isTRUE(self$options$plotMeans),
        extra_note = extra
    )
}

.doe_plot_half_normal <- function(image, ...) {
    eff <- image$state
    if (is.null(eff) || nrow(eff) == 0) return(FALSE)
    ae <- sort(eff$abs_effect)
    n <- length(ae)
    if (n < 1) return(FALSE)
    p <- (seq_len(n) - 0.5) / n
    theoretical <- stats::qnorm(0.5 + p / 2)
    df <- data.frame(theoretical = theoretical, effect = ae)
    pobj <- ggplot2::ggplot(df, ggplot2::aes(x = theoretical, y = effect)) +
        ggplot2::geom_point(size = 2) +
        ggplot2::geom_text(
            data = utils::tail(cbind(df, term = eff$term[order(eff$abs_effect)]), min(5, n)),
            ggplot2::aes(label = term), hjust = -0.1, size = 3
        ) +
        ggplot2::labs(x = "Half-normal quantile", y = "|Effect|", title = "Half-Normal Plot") +
        ggplot2::theme_bw()
    print(pobj)
    TRUE
}

.doe_plot_pareto <- function(image, ...) {
    eff <- image$state
    if (is.null(eff) || nrow(eff) == 0) return(FALSE)
    df <- eff[order(eff$abs_effect, decreasing = TRUE), , drop = FALSE]
    df$term <- factor(df$term, levels = rev(df$term))
    pobj <- ggplot2::ggplot(df, ggplot2::aes(x = term, y = abs_effect)) +
        ggplot2::geom_col(fill = "#3B6EA5") +
        ggplot2::coord_flip() +
        ggplot2::labs(x = NULL, y = "|Effect|", title = "Pareto Plot of Effects") +
        ggplot2::theme_bw()
    print(pobj)
    TRUE
}

.doe_plot_main_effects <- function(image, ...) {
    st <- image$state
    if (is.null(st)) return(FALSE)
    data <- st$data; dep <- st$dep; factors <- st$factors
    rows <- lapply(factors, function(f) {
        means <- stats::aggregate(data[[dep]], by = list(level = data[[f]]), FUN = mean, na.rm = TRUE)
        data.frame(factor = f, level = as.character(means$level), mean = means$x, stringsAsFactors = FALSE)
    })
    df <- do.call(rbind, rows)
    pobj <- ggplot2::ggplot(df, ggplot2::aes(x = level, y = mean, group = 1)) +
        ggplot2::geom_line() + ggplot2::geom_point() +
        ggplot2::facet_wrap(~ factor, scales = "free_x") +
        ggplot2::labs(x = "Level", y = paste("Mean", dep),
                      title = paste0("Main Effects", if (!is.null(st$subtitle)) st$subtitle else "")) +
        ggplot2::theme_bw()
    print(pobj)
    TRUE
}

.doe_plot_interaction <- function(image, ...) {
    st <- image$state
    if (is.null(st) || length(st$factors) < 2) return(FALSE)
    f1 <- st$factors[[1]]; f2 <- st$factors[[2]]; dep <- st$dep; data <- st$data
    means <- stats::aggregate(data[[dep]], by = list(a = data[[f1]], b = data[[f2]]), FUN = mean, na.rm = TRUE)
    names(means)[3] <- "mean"
    pobj <- ggplot2::ggplot(means, ggplot2::aes(x = a, y = mean, color = b, group = b)) +
        ggplot2::geom_line() + ggplot2::geom_point() +
        ggplot2::labs(x = f1, y = paste("Mean", dep), color = f2, title = "Interaction Plot") +
        ggplot2::theme_bw()
    print(pobj)
    TRUE
}

.doe_plot_residuals <- function(image, ...) {
    st <- image$state
    if (is.null(st) || is.null(st$fit)) return(FALSE)
    fit <- st$fit
    df <- data.frame(
        fitted = stats::fitted(fit),
        resid = stats::resid(fit),
        theoretical = stats::qqnorm(stats::resid(fit), plot.it = FALSE)$x
    )
    p1 <- ggplot2::ggplot(df, ggplot2::aes(x = fitted, y = resid)) +
        ggplot2::geom_point() + ggplot2::geom_hline(yintercept = 0, linetype = 2) +
        ggplot2::labs(x = "Fitted", y = "Residual", title = "Residuals vs Fitted") +
        ggplot2::theme_bw()
    p2 <- ggplot2::ggplot(df, ggplot2::aes(x = theoretical, y = sort(resid))) +
        ggplot2::geom_point() +
        ggplot2::labs(x = "Theoretical", y = "Residual", title = "Normal Q-Q") +
        ggplot2::theme_bw()
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 2)))
    print(p1, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
    print(p2, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
    TRUE
}

.doe_plot_contour <- function(image, ...) {
    st <- image$state
    if (is.null(st)) return(FALSE)
    data <- st$data; factors <- st$factors; dep <- st$dep; fit <- st$fit
    numFacs <- factors[vapply(factors, function(f) is.numeric(data[[f]]), logical(1))]
    if (length(numFacs) < 2) return(FALSE)
    xname <- numFacs[[1]]; yname <- numFacs[[2]]
    xr <- range(data[[xname]], na.rm = TRUE)
    yr <- range(data[[yname]], na.rm = TRUE)
    griddf <- expand.grid(
        x = seq(xr[1], xr[2], length.out = 40),
        y = seq(yr[1], yr[2], length.out = 40)
    )
    names(griddf) <- c(xname, yname)
    for (f in setdiff(factors, c(xname, yname))) {
        if (is.numeric(data[[f]]))
            griddf[[f]] <- mean(data[[f]], na.rm = TRUE)
        else
            griddf[[f]] <- names(sort(table(data[[f]]), decreasing = TRUE))[1]
    }
    griddf$pred <- tryCatch(stats::predict(fit, newdata = griddf), error = function(e) NA)
    if (all(is.na(griddf$pred))) return(FALSE)
    pobj <- ggplot2::ggplot(griddf, ggplot2::aes(x = .data[[xname]], y = .data[[yname]], z = pred)) +
        ggplot2::geom_contour_filled(bins = 12) +
        ggplot2::labs(x = xname, y = yname, fill = dep, title = "Contour Plot") +
        ggplot2::theme_bw()
    print(pobj)
    TRUE
}

.doe_plot_sn <- function(image, ...) {
    df <- image$state
    if (is.null(df) || nrow(df) == 0) return(FALSE)
    pobj <- ggplot2::ggplot(df, ggplot2::aes(x = level, y = value, group = 1)) +
        ggplot2::geom_line() + ggplot2::geom_point() +
        ggplot2::facet_wrap(~ factor, scales = "free_x") +
        ggplot2::labs(x = "Level", y = "Mean SN", title = "SN Main Effects") +
        ggplot2::theme_bw()
    print(pobj)
    TRUE
}

.doe_plot_means <- function(image, ...) {
    df <- image$state
    if (is.null(df) || nrow(df) == 0) return(FALSE)
    pobj <- ggplot2::ggplot(df, ggplot2::aes(x = level, y = value, group = 1)) +
        ggplot2::geom_line() + ggplot2::geom_point() +
        ggplot2::facet_wrap(~ factor, scales = "free_x") +
        ggplot2::labs(x = "Level", y = "Mean Response", title = "Means Main Effects") +
        ggplot2::theme_bw()
    print(pobj)
    TRUE
}
