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

#' Column names chosen in a jamovi variable box, as a plain character vector.
.doe_selected_vars <- function(opt) {
    if (is.null(opt))
        return(character(0))
    v <- unlist(opt, use.names = FALSE)
    v <- as.character(v)
    unique(v[!is.na(v) & nzchar(v)])
}

#' Rename columns so model formulas can be built from them.
#'
#' Formulas are assembled by pasting column names together, so a spreadsheet
#' column called "Cure Time" would produce an unparseable formula. Renaming up
#' front keeps the fit working; the caller reports any change to the user.
.doe_syntactic_columns <- function(data, cols) {
    orig <- names(data)
    safe <- make.names(orig, unique = TRUE)
    names(data) <- safe
    map <- stats::setNames(safe, orig)
    # Unknown columns keep their own name rather than becoming NA: callers split
    # `cols` back into responses and factors by position, and an NA name would
    # otherwise be carried into a model formula.
    hit <- match(cols, orig)
    list(
        data = data,
        cols = ifelse(is.na(hit), cols, safe[hit]),
        renamed = map[map != names(map)]
    )
}

#' Measured values as numbers.
#'
#' A column typed as nominal in jamovi arrives as a factor whose integer codes
#' are not the measured values, so the labels are read back first.
.doe_numeric_col <- function(x) {
    if (is.null(x))
        return(numeric(0))
    if (is.factor(x))
        x <- as.character(x)
    suppressWarnings(as.numeric(x))
}

#' Could this column hold measurements at all?
#'
#' Distinguishes a response column nobody has filled in yet (numeric, all
#' missing -- the user just has not run the experiment) from a column of factor
#' levels dropped into Responses by mistake. An empty column counts as
#' measurable, because there is nothing yet to say otherwise.
.doe_looks_numeric <- function(x) {
    if (is.null(x))
        return(FALSE)
    if (is.numeric(x) && !is.factor(x))
        return(TRUE)
    v <- as.character(if (is.factor(x)) levels(x) else x)
    v <- v[!is.na(v) & nzchar(trimws(v))]
    if (length(v) == 0)
        return(TRUE)
    all(!is.na(suppressWarnings(as.numeric(v))))
}

#' "a", "a and b", "a, b and c" -- each wrapped in <code>.
.doe_and_list <- function(x) {
    x <- paste0("<code>", x, "</code>")
    n <- length(x)
    if (n == 0) return("")
    if (n == 1) return(x)
    paste0(paste(x[-n], collapse = ", "), " and ", x[n])
}

#' Explain why no response could be modelled.
#'
#' Two very different causes land here: the experiment has not been run yet, or
#' factor columns were dropped into Responses by mistake. Telling someone to go
#' and type in values they already typed in is worse than useless, so the text
#' columns are named and a numeric column sitting in Factors is called out as
#' the likely swap.
.doe_no_response_message <- function(resp, facs, data) {
    measurable <- vapply(resp, function(nm) .doe_looks_numeric(data[[nm]]), logical(1))
    if (any(measurable)) {
        return(paste0(
            "<p>The response column is still empty. Run the experiment, type the ",
            "measured value next to each run, and the model appears here.</p>"
        ))
    }

    numeric_facs <- facs[vapply(facs, function(nm) .doe_looks_numeric(data[[nm]]), logical(1))]
    msg <- paste0(
        "<p>", .doe_and_list(resp), " hold text rather than measurements, ",
        "so there is nothing to model.</p>"
    )
    if (length(numeric_facs) > 0) {
        paste0(msg,
            "<p>", .doe_and_list(numeric_facs), " in <b>Factors</b> ",
            if (length(numeric_facs) == 1) "is" else "are",
            " numeric, so <b>Responses</b> and <b>Factors</b> look swapped. ",
            "<b>Responses</b> takes the value you measured; <b>Factors</b> takes ",
            "the settings you varied.</p>")
    } else {
        paste0(msg,
            "<p>Put the column you measured in <b>Responses</b>, and the columns ",
            "you varied in <b>Factors</b>.</p>")
    }
}

#' Note describing which of the selected columns had to be renamed.
.doe_rename_note <- function(conv, used) {
    changed <- conv$renamed[conv$renamed %in% used]
    if (length(changed) == 0)
        return("")
    paste0(
        "<p>Renamed for modelling: ",
        paste0("<code>", names(changed), "</code> &rarr; <code>", changed, "</code>",
               collapse = ", "),
        ".</p>"
    )
}

.doe_coerce_for_lm <- function(data, factor_names, dep) {
    data[[dep]] <- .doe_numeric_col(data[[dep]])
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

#' SN range (best minus worst level mean) and rank for each factor.
.doe_sn_delta_rank <- function(level_means) {
    empty <- data.frame(factor = character(0), delta = numeric(0),
                        rank = integer(0), stringsAsFactors = FALSE)
    if (is.null(level_means) || nrow(level_means) < 1)
        return(empty)
    split_f <- split(level_means, level_means$factor)
    rows <- lapply(names(split_f), function(f) {
        v <- split_f[[f]]$meanSN
        v <- v[is.finite(v)]
        if (length(v) < 2)
            return(NULL)
        data.frame(factor = f, delta = max(v) - min(v), stringsAsFactors = FALSE)
    })
    rows <- Filter(Negate(is.null), rows)
    if (length(rows) == 0)
        return(empty)
    out <- do.call(rbind, rows)
    out <- out[order(-out$delta, out$factor), , drop = FALSE]
    out$rank <- as.integer(rank(-out$delta, ties.method = "min"))
    rownames(out) <- NULL
    out
}

#' Additive SN prediction at the preferred main-effect levels.
#'
#' Predicted SN = overall mean + sum of (level mean - overall mean) for each
#' recommended factor. Interactions are ignored, matching the usual Taguchi
#' confirmation formula.
.doe_sn_predict_additive <- function(sn, data, rec) {
    overall <- mean(sn, na.rm = TRUE)
    if (!is.finite(overall))
        return(NULL)
    if (is.null(rec) || nrow(rec) < 1)
        return(list(overall = overall, predicted = overall, change = 0))
    adj <- 0
    for (i in seq_len(nrow(rec))) {
        f <- rec$factor[i]
        if (!f %in% names(data))
            next
        hit <- as.character(data[[f]]) == as.character(rec$level[i])
        m <- mean(sn[hit], na.rm = TRUE)
        if (is.finite(m))
            adj <- adj + (m - overall)
    }
    pred <- overall + adj
    list(overall = overall, predicted = pred, change = pred - overall)
}

#' Blank rather than NaN in a results table.
#'
#' A saturated fit has no error term, so summary() yields NaN for SE, t, p and
#' the F tests. jamovi prints those as the literal text "NaN", which reads like
#' a fault; an NA renders as an empty cell instead.
.doe_finite_or_na <- function(x) {
    if (length(x) != 1)
        return(NA_real_)
    x <- suppressWarnings(as.numeric(x))
    if (!is.finite(x))
        return(NA_real_)
    x
}

.doe_item <- function(results, name) {
    if (is.null(name) || !nzchar(name))
        return(NULL)
    tryCatch(results[[name]], error = function(e) NULL)
}

.doe_fill_anova_table <- function(tbl, fit, response = NULL, start_key = 1L,
                                  residual_label = NULL, contrib = FALSE) {
    if (is.null(tbl))
        return(start_key)
    a <- tryCatch({
        if (identical(fit$df.residual, 0L) || isTRUE(fit$df.residual == 0))
            suppressWarnings(stats::anova(fit))
        else
            stats::anova(fit)
    }, error = function(e) NULL)
    if (is.null(a))
        return(start_key)
    ss_all <- suppressWarnings(as.numeric(a[, "Sum Sq"]))
    ss_total <- sum(ss_all[is.finite(ss_all)])
    for (i in seq_len(nrow(a))) {
        term <- rownames(a)[i]
        if (!is.null(residual_label) && identical(term, "Residuals"))
            term <- residual_label
        vals <- list(
            term = term,
            ss = .doe_finite_or_na(a[i, "Sum Sq"]),
            df = a[i, "Df"],
            ms = .doe_finite_or_na(a[i, "Mean Sq"]),
            F = if ("F value" %in% names(a)) .doe_finite_or_na(a[i, "F value"]) else NA_real_,
            p = if ("Pr(>F)" %in% names(a)) .doe_finite_or_na(a[i, "Pr(>F)"]) else NA_real_
        )
        if (isTRUE(contrib)) {
            vals$contrib <- if (is.finite(ss_total) && ss_total > 0 && is.finite(ss_all[i]))
                100 * ss_all[i] / ss_total else NA_real_
        }
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
            estimate = .doe_finite_or_na(ct[i, 1]),
            se = .doe_finite_or_na(ct[i, 2]),
            t = .doe_finite_or_na(ct[i, 3]),
            p = .doe_finite_or_na(ct[i, 4])
        )
        if (!is.null(response))
            vals$response <- response
        tbl$addRow(rowKey = start_key + i - 1L, values = vals)
    }
    start_key + nrow(ct)
}

#' Split an Exclude terms string into individual term names.
.doe_parse_exclude_terms <- function(text) {
    if (is.null(text))
        return(character(0))
    raw <- paste(unlist(text, use.names = FALSE), collapse = " ")
    if (!nzchar(trimws(raw)))
        return(character(0))
    parts <- unlist(strsplit(raw, "[,;\n]+"), use.names = FALSE)
    parts <- trimws(parts)
    unique(parts[nzchar(parts)])
}

#' Map a user-typed or ANOVA term onto the names lm uses in term.labels.
#'
#' JMP writes interactions as X1*X2 and quadratics as X1*X1. R uses X1:X2 and
#' I(X1^2). Accepting both keeps the exclude box usable next to Effect Summary.
.doe_canonicalize_term <- function(term) {
    x <- gsub("\\s+", "", as.character(term)[1])
    if (!nzchar(x) || is.na(x))
        return("")
    if (grepl("^I\\(.+\\^2\\)$", x))
        return(x)
    if (grepl("^[A-Za-z.][A-Za-z0-9._]*\\^2$", x))
        return(sprintf("I(%s^2)", sub("\\^2$", "", x)))
    parts <- strsplit(x, "[*:]")[[1]]
    parts <- parts[nzchar(parts)]
    if (length(parts) == 2L && identical(parts[[1]], parts[[2]]))
        return(sprintf("I(%s^2)", parts[[1]]))
    gsub("*", ":", x, fixed = TRUE)
}

.doe_terms_equal <- function(a, b) {
    ca <- .doe_canonicalize_term(a)
    cb <- .doe_canonicalize_term(b)
    if (!nzchar(ca) || !nzchar(cb))
        return(FALSE)
    if (identical(ca, cb))
        return(TRUE)
    pa <- strsplit(ca, ":", fixed = TRUE)[[1]]
    pb <- strsplit(cb, ":", fixed = TRUE)[[1]]
    length(pa) == length(pb) && setequal(pa, pb)
}

#' JMP-style source label: A:B -> A*B, I(A^2) -> A*A.
.doe_effect_label <- function(term) {
    x <- as.character(term)[1]
    m <- regexec("^I\\((.+)\\^2\\)$", x)
    hit <- regmatches(x, m)[[1]]
    if (length(hit) == 2L)
        return(paste0(hit[[2]], "*", hit[[2]]))
    gsub(":", "*", x, fixed = TRUE)
}

#' Drop matching terms from a model RHS, expanding (A+B)^2 first.
.doe_rhs_without_terms <- function(rhs, exclude) {
    exclude <- .doe_parse_exclude_terms(exclude)
    rhs <- trimws(as.character(rhs)[1])
    empty <- list(rhs = rhs, dropped = character(0), missing = character(0))
    if (!nzchar(rhs) || length(exclude) < 1)
        return(empty)
    fml <- tryCatch(stats::as.formula(paste("~", rhs)), error = function(e) NULL)
    if (is.null(fml))
        return(list(rhs = rhs, dropped = character(0), missing = exclude))
    labs <- attr(stats::terms(fml), "term.labels")
    if (length(labs) < 1)
        return(list(rhs = "1", dropped = character(0), missing = exclude))
    drop <- vapply(labs, function(t) {
        any(vapply(exclude, function(e) .doe_terms_equal(t, e), logical(1)))
    }, logical(1))
    missing <- exclude[!vapply(exclude, function(e) {
        any(vapply(labs, function(t) .doe_terms_equal(t, e), logical(1)))
    }, logical(1))]
    keep <- labs[!drop]
    new_rhs <- if (length(keep) < 1) "1" else paste(keep, collapse = " + ")
    list(rhs = new_rhs, dropped = labs[drop], missing = missing)
}

.doe_logworth <- function(p) {
    p <- suppressWarnings(as.numeric(p))
    if (length(p) != 1 || is.na(p) || p < 0)
        return(NA_real_)
    if (p == 0)
        return(16)
    lw <- -log10(p)
    if (!is.finite(lw))
        return(NA_real_)
    min(lw, 16)
}

#' Type III extra-SS tests: each term last, including mains beside interactions.
.doe_type3_effects <- function(fit, data) {
    empty <- data.frame(
        term = character(0), label = character(0), df = integer(0),
        F = numeric(0), p = numeric(0), logworth = numeric(0),
        stringsAsFactors = FALSE)
    if (is.null(fit) || inherits(fit, "error"))
        return(empty)
    labs <- attr(stats::terms(fit), "term.labels")
    if (length(labs) < 1)
        return(empty)
    dep <- as.character(stats::formula(fit)[[2]])
    rows <- lapply(labs, function(term) {
        keep <- setdiff(labs, term)
        rhs <- if (length(keep) < 1) "1" else paste(keep, collapse = " + ")
        red <- tryCatch(
            stats::lm(stats::as.formula(paste(dep, "~", rhs)), data = data),
            error = function(e) e)
        if (inherits(red, "error")) {
            return(data.frame(term = term, label = .doe_effect_label(term),
                              df = NA_integer_, F = NA_real_, p = NA_real_,
                              logworth = NA_real_, stringsAsFactors = FALSE))
        }
        cmp <- tryCatch({
            if (identical(fit$df.residual, 0L) || isTRUE(fit$df.residual == 0))
                suppressWarnings(stats::anova(red, fit))
            else
                stats::anova(red, fit)
        }, error = function(e) NULL)
        p <- if (is.null(cmp) || nrow(cmp) < 2)
            NA_real_ else .doe_finite_or_na(cmp[2, "Pr(>F)"])
        fval <- if (is.null(cmp) || nrow(cmp) < 2)
            NA_real_ else .doe_finite_or_na(cmp[2, "F"])
        dfd <- if (is.null(cmp) || nrow(cmp) < 2)
            NA_integer_ else as.integer(cmp[2, "Df"])
        data.frame(
            term = term,
            label = .doe_effect_label(term),
            df = dfd,
            F = fval,
            p = p,
            logworth = .doe_logworth(p),
            stringsAsFactors = FALSE)
    })
    do.call(rbind, rows)
}

.doe_apply_effect_fdr <- function(effects) {
    if (is.null(effects) || nrow(effects) < 1)
        return(effects)
    ok <- is.finite(effects$p)
    adj <- rep(NA_real_, nrow(effects))
    if (any(ok))
        adj[ok] <- stats::p.adjust(effects$p[ok], method = "BH")
    effects$p <- adj
    effects$logworth <- vapply(adj, .doe_logworth, numeric(1))
    effects
}

.doe_fill_effect_table <- function(tbl, effects, response = NULL, start_key = 1L) {
    if (is.null(tbl) || is.null(effects) || nrow(effects) < 1)
        return(start_key)
    lw <- effects$logworth
    ord <- order(lw, decreasing = TRUE, na.last = TRUE)
    effects <- effects[ord, , drop = FALSE]
    for (i in seq_len(nrow(effects))) {
        vals <- list(
            term = effects$label[i],
            logworth = if (is.finite(effects$logworth[i])) effects$logworth[i] else NA_real_,
            p = .doe_finite_or_na(effects$p[i])
        )
        if (!is.null(response))
            vals$response <- response
        tbl$addRow(rowKey = start_key + i - 1L, values = vals)
    }
    start_key + nrow(effects)
}

#' Fit lm and populate jamovi analysis result items.
#' @param resp_info data.frame with name, goal for each response to fit
#' @param rhs optional model right-hand side (overrides model_preset)
.doe_fill_lm_analysis <- function(results, data, dep, factors, model_preset,
                                  plot_opts = list(), items = list(),
                                  extra_note = "", resp_info = NULL, rhs = NULL,
                                  exclude_terms = "", effect_fdr = FALSE) {
    def_items <- list(
        info = "analysisInfo",
        anova = "analysisAnova",
        coef = "analysisCoef",
        recommend = "analysisRecommend",
        effects = "analysisEffects",
        effectSummary = "analysisEffectPlot",
        halfNormal = "analysisHalfNormal",
        pareto = "analysisPareto",
        mainEffects = "analysisMainEffects",
        interaction = "analysisInteraction",
        residuals = "analysisResiduals",
        contour = "analysisContour"
    )
    items <- utils::modifyList(def_items, items)

    if (is.null(resp_info) || nrow(resp_info) < 1)
        resp_info <- data.frame(name = dep, goal = "maximize", stringsAsFactors = FALSE)

    info <- .doe_item(results, items$info)

    # Only model columns that are really present. Falling back to `dep` here
    # would let an unmatched name (or NA) reach lm(), which throws out of
    # .run() and leaves jamovi with a half-applied analysis.
    resp_info <- resp_info[!is.na(resp_info$name) &
                           resp_info$name %in% names(data), , drop = FALSE]
    factors <- factors[!is.na(factors) & factors %in% names(data)]
    deps <- unique(resp_info$name)
    if (length(deps) < 1 || length(factors) < 1) {
        if (!is.null(info))
            info$setContent(paste0(
                "<p>This needs at least one response column and one factor column ",
                "that are still in the spreadsheet.</p>"
            ))
        return(invisible(FALSE))
    }

    for (d in deps)
        data <- .doe_coerce_for_lm(data, factors, d)
    data <- data[, unique(c(deps, factors)), drop = FALSE]

    preset <- if (is.null(model_preset) || !nzchar(model_preset)) "main" else model_preset
    if (identical(preset, "custom") && is.null(rhs))
        preset <- "main"
    if (is.null(rhs)) {
        fml_rhs <- as.character(.doe_model_formula(factors, preset))[2]
    } else {
        fml_rhs <- rhs
    }

    dropped <- .doe_rhs_without_terms(fml_rhs, exclude_terms)
    if (length(dropped$dropped) > 0) {
        fml_rhs <- dropped$rhs
        extra_note <- paste0(
            extra_note,
            "<p>Excluded from the model: <code>",
            paste(.doe_effect_label(dropped$dropped), collapse = "</code>, <code>"),
            "</code>.</p>")
    }
    if (length(dropped$missing) > 0) {
        extra_note <- paste0(
            extra_note,
            "<p>No model term matched ",
            paste0("<code>", dropped$missing, "</code>", collapse = ", "),
            " — use the Source names in Effect Summary.</p>")
    }
    if (isTRUE(effect_fdr)) {
        extra_note <- paste0(
            extra_note,
            "<p>Logworth and PValue use BH FDR-adjusted tests. The blue line is ",
            "still at 2 (adjusted p = 0.01).</p>")
    }

    summaries <- character(0)
    rec_all <- list()
    first_fit <- NULL
    first_dep <- resp_info$name[1]
    first_goal <- resp_info$goal[1]
    first_data <- NULL
    first_effects <- NULL
    anova_key <- 1L
    coef_key <- 1L
    effect_key <- 1L

    for (i in seq_len(nrow(resp_info))) {
        d <- resp_info$name[i]
        g <- resp_info$goal[i]
        tgt <- if ("target" %in% names(resp_info)) resp_info$target[i] else NA_real_

        # Complete cases per response, so a response nobody has measured yet
        # cannot empty out the runs available to the others.
        d_data <- data[, c(d, factors), drop = FALSE]
        d_data <- d_data[stats::complete.cases(d_data), , drop = FALSE]
        if (nrow(d_data) < 3) {
            summaries <- c(summaries, paste0(
                "<p><code>", d, "</code> has ", nrow(d_data),
                " run(s) with a measured value. Enter at least 3 to fit a model.</p>"))
            next
        }

        fml <- stats::as.formula(paste(d, "~", fml_rhs))
        fit <- tryCatch(stats::lm(fml, data = d_data), error = function(e) e)
        if (inherits(fit, "error")) {
            summaries <- c(summaries, paste0("<p>Could not fit <code>", d, "</code>: ",
                                             fit$message, "</p>"))
            next
        }
        sm <- summary(fit)
        cf <- stats::coef(fit)
        n_aliased <- sum(is.na(cf))
        adj <- .doe_finite_or_na(sm$adj.r.squared)
        summaries <- c(summaries, paste0(
            "<p><b>", d, "</b> (<i>", .doe_goal_label(g, tgt), "</i>): <code>",
            paste(deparse(fml), collapse = ""), "</code><br>",
            "R<sup>2</sup> = ", signif(sm$r.squared, 4),
            if (is.na(adj)) "" else paste0(", Adjusted R<sup>2</sup> = ", signif(adj, 4)),
            ", N = ", nrow(d_data), ".</p>"
        ))

        # A design with no spare runs cannot estimate error, so every F, p and
        # SE comes back empty. Say so, rather than leaving blank cells.
        if (identical(fit$df.residual, 0L) || isTRUE(fit$df.residual == 0)) {
            summaries <- c(summaries, paste0(
                "<p>This model uses all ", nrow(d_data), " run(s), leaving no runs ",
                "over to estimate error, so F, p, SE and adjusted R<sup>2</sup> ",
                "cannot be computed. Add replicates or centre points, or model ",
                "fewer terms, to get a test of significance. The sums of squares ",
                "and the preferred settings below are still usable.</p>"
            ))
        }
        if (n_aliased > 0) {
            summaries <- c(summaries, paste0(
                "<p>", n_aliased, " term(s) could not be estimated separately from ",
                "the others in this design and were dropped: <code>",
                paste(names(cf)[is.na(cf)], collapse = "</code>, <code>"),
                "</code>. A design this size cannot separate them.</p>"
            ))
        }
        anova_key <- .doe_fill_anova_table(
            .doe_item(results, items$anova), fit, response = d, start_key = anova_key)
        coef_key <- .doe_fill_coef_table(
            .doe_item(results, items$coef), sm, response = d, start_key = coef_key)
        effects <- .doe_type3_effects(fit, d_data)
        if (isTRUE(effect_fdr))
            effects <- .doe_apply_effect_fdr(effects)
        effect_key <- .doe_fill_effect_table(
            .doe_item(results, items$effects), effects, response = d,
            start_key = effect_key)
        rec <- .doe_recommend_settings(d_data, d, factors, g, target = tgt)
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
            first_data <- d_data
            first_effects <- effects
        }
    }

    rec_df <- if (length(rec_all)) do.call(rbind, rec_all) else NULL
    .doe_fill_recommend_table(.doe_item(results, items$recommend), rec_df)

    info_html <- paste0(
        paste(summaries, collapse = ""),
        .doe_recommend_html(rec_df),
        extra_note
    )

    if (is.null(first_fit)) {
        if (!is.null(info))
            info$setContent(info_html)
        return(invisible(FALSE))
    }

    effects <- .doe_effects_from_lm(first_fit)
    po <- utils::modifyList(
        list(halfNormal = TRUE, pareto = TRUE, mainEffects = FALSE,
             interaction = FALSE, residuals = TRUE, contour = FALSE,
             effectSummary = TRUE),
        plot_opts
    )
    plot_title <- paste0(" (", .doe_goal_label(first_goal), " ", first_dep, ")")

    n_cont <- sum(vapply(factors, function(f) is.numeric(first_data[[f]]), logical(1)))
    have_effects <- !is.null(effects) && nrow(effects) > 0
    skipped <- character(0)

    # jamovi reports a visible image that has state but no rendered file as still
    # rendering, which shows as a spinner that never resolves. So only give an
    # image state when the plot can actually be drawn, and say why when it cannot.
    fill_plot <- function(key, drawable, value, reason) {
        if (!isTRUE(po[[key]]))
            return(invisible(NULL))
        img <- .doe_item(results, items[[key]])
        if (is.null(img))
            return(invisible(NULL))
        if (isTRUE(drawable))
            img$setState(value)
        else
            skipped <<- c(skipped, reason)
        invisible(NULL)
    }

    by_factor <- list(data = first_data, dep = first_dep, factors = factors,
                      goal = first_goal, subtitle = plot_title)

    fill_plot("halfNormal", have_effects, effects,
              "half-normal plot (the fit left no estimable effects)")
    fill_plot("pareto", have_effects, effects,
              "Pareto plot (the fit left no estimable effects)")
    have_logworth <- !is.null(first_effects) && nrow(first_effects) > 0 &&
        any(is.finite(first_effects$logworth))
    plot_effects <- NULL
    if (have_logworth) {
        plot_effects <- first_effects[, c("label", "logworth"), drop = FALSE]
        names(plot_effects)[1] <- "term"
        plot_effects$fdr <- isTRUE(effect_fdr)
    }
    fill_plot("effectSummary", have_logworth, plot_effects,
              "effect summary plot (no residual df, so p and logworth are blank)")
    fill_plot("mainEffects", length(factors) >= 1, by_factor,
              "main effects plot (needs at least one factor)")
    fill_plot("interaction", length(factors) >= 2, by_factor,
              "interaction plot (needs two or more factors)")
    # Only plain numbers go into state; see .doe_contour_grid for why a fitted
    # model must never be stored there.
    fill_plot("residuals", TRUE,
              list(fitted = as.numeric(stats::fitted(first_fit)),
                   resid = as.numeric(stats::resid(first_fit))),
              "")

    contour_state <- if (isTRUE(po$contour) && n_cont >= 2)
        .doe_contour_grid(first_fit, first_data, factors) else NULL
    if (!is.null(contour_state))
        contour_state$dep <- first_dep
    fill_plot("contour", !is.null(contour_state), contour_state,
              if (n_cont >= 2)
                  "contour plot (the model could not predict across these factors)"
              else
                  "contour plot (needs two continuous factors; these are nominal)")

    if (length(skipped) > 0)
        info_html <- paste0(info_html, "<p>Not plotted: ",
                            paste(skipped, collapse = "; "), ".</p>")
    if (!is.null(info))
        info$setContent(info_html)

    invisible(TRUE)
}

.doe_fill_taguchi_analysis <- function(results, data, factor_names, resp_names,
                                       sn_type = "smaller", sn_model = TRUE,
                                       plot_sn = TRUE, plot_means = TRUE,
                                       items = list(), extra_note = "",
                                       pooled_factor = NULL) {
    def_items <- list(
        info = "analysisInfo",
        perRun = "analysisPerRun",
        responseSN = "analysisResponseSN",
        responseMean = "analysisResponseMean",
        snAnova = "analysisSnAnova",
        snDelta = "analysisSnDelta",
        snPredict = "analysisSnPredict",
        plotSN = "analysisPlotSN",
        plotMeans = "analysisPlotMeans",
        plotDelta = "analysisSnDeltaPlot"
    )
    items <- utils::modifyList(def_items, items)
    info <- .doe_item(results, items$info)

    # Same reason as the lm path: a name that is not a column of `data` would
    # error out of .run() instead of reporting a problem.
    factor_names <- factor_names[!is.na(factor_names) & factor_names %in% names(data)]
    resp_names <- resp_names[!is.na(resp_names) & resp_names %in% names(data)]
    pooled <- .doe_selected_vars(pooled_factor)
    pooled <- pooled[!is.na(pooled) & pooled %in% names(data)]
    pooled <- setdiff(pooled, resp_names)
    if (length(pooled) > 1L)
        pooled <- pooled[1L]
    all_factors <- unique(c(factor_names, pooled))
    active <- setdiff(all_factors, pooled)
    if (length(all_factors) < 1 || length(resp_names) < 1) {
        if (!is.null(info))
            info$setContent(paste0(
                "<p>This needs control factor columns and at least one response ",
                "column that are still in the spreadsheet.</p>"
            ))
        return(invisible(FALSE))
    }

    for (f in all_factors)
        data[[f]] <- factor(as.character(data[[f]]))
    for (r in resp_names)
        data[[r]] <- .doe_numeric_col(data[[r]])
    data <- data[stats::complete.cases(data[, all_factors, drop = FALSE]), , drop = FALSE]
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
    sn_levels <- list()
    for (f in all_factors) {
        snMeans <- stats::aggregate(sn, by = list(level = data[[f]]), FUN = mean, na.rm = TRUE)
        yMeans <- stats::aggregate(mu, by = list(level = data[[f]]), FUN = mean, na.rm = TRUE)
        sn_levels[[length(sn_levels) + 1L]] <- data.frame(
            factor = f,
            level = as.character(snMeans$level),
            meanSN = snMeans$x,
            stringsAsFactors = FALSE)
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
    sn_level_df <- if (length(sn_levels) > 0) do.call(rbind, sn_levels) else NULL
    delta <- .doe_sn_delta_rank(sn_level_df)
    deltaTbl <- .doe_item(results, items$snDelta)
    if (!is.null(deltaTbl) && nrow(delta) > 0) {
        for (i in seq_len(nrow(delta))) {
            deltaTbl$addRow(rowKey = i, values = list(
                factor = delta$factor[i],
                delta = delta$delta[i],
                rank = delta$rank[i]
            ))
        }
    }

    rec_factors <- if (length(active) > 0) active else all_factors
    rec_sn <- .doe_recommend_settings(
        cbind(SN = sn, data[, rec_factors, drop = FALSE]),
        "SN", rec_factors, "maximize"
    )
    if (!is.null(rec_sn)) {
        rec_sn$response <- paste0("SN (", paste(resp_names, collapse = ", "), ")")
        rec_sn$goal <- paste0("maximize SN / ", sn_type)
    }
    .doe_fill_recommend_table(.doe_item(results, "analysisRecommend"), rec_sn)

    pred <- .doe_sn_predict_additive(sn, data, rec_sn)
    predTbl <- .doe_item(results, items$snPredict)
    if (!is.null(predTbl) && !is.null(pred)) {
        predTbl$addRow(rowKey = 1L, values = list(
            item = "Overall mean SN", sn = pred$overall))
        predTbl$addRow(rowKey = 2L, values = list(
            item = "Predicted SN (preferred settings)", sn = pred$predicted))
        predTbl$addRow(rowKey = 3L, values = list(
            item = "Change", sn = pred$change))
    }
    if (!is.null(pred) && is.finite(pred$predicted)) {
        extra_note <- paste0(
            extra_note,
            "<p>Predicted SN at the preferred settings is ",
            signif(pred$predicted, 4), " (overall mean ",
            signif(pred$overall, 4), "). That additive prediction ignores ",
            "interactions; confirm it with a follow-up run.</p>")
    }

    residual_label <- NULL
    if (isTRUE(sn_model) && length(active) >= 1) {
        dfm <- data[, active, drop = FALSE]
        dfm$SN <- sn
        fml <- stats::as.formula(paste("SN ~", paste(active, collapse = " + ")))
        fit <- tryCatch(stats::lm(fml, data = dfm), error = function(e) e)
        if (!inherits(fit, "error")) {
            residual_label <- if (length(pooled) == 1L)
                paste0("Pooled error (", pooled, ")") else NULL
            .doe_fill_anova_table(
                .doe_item(results, items$snAnova), fit,
                residual_label = residual_label, contrib = TRUE)
            if (identical(fit$df.residual, 0L) || isTRUE(fit$df.residual == 0)) {
                extra_note <- paste0(
                    extra_note,
                    "<p>The S/N ANOVA has no residual degrees of freedom, so F ",
                    "and p cannot be computed. Move a negligible factor into ",
                    "<b>Pooled into error</b> to free residual df.</p>")
            } else if (length(pooled) == 1L) {
                extra_note <- paste0(
                    extra_note,
                    "<p><code>", pooled, "</code> was pooled into the error term (",
                    fit$df.residual, " residual df). That assumes this factor is ",
                    "negligible; it does not make a data-selected test unbiased.</p>")
            }
        }
    } else if (isTRUE(sn_model) && length(pooled) >= 1 && length(active) < 1) {
        extra_note <- paste0(
            extra_note,
            "<p>ANOVA on SN needs at least one factor that is not pooled into error.</p>")
    }

    if (!is.null(info)) {
        info$setContent(paste0(
            "<p><b>Taguchi analysis</b> using <code>", sn_type,
            "</code> SN ratios across ", length(resp_names),
            " response column(s) and ", nrow(data), " inner runs.</p>",
            extra_note, .doe_recommend_html(rec_sn)
        ))
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
    img_d <- .doe_item(results, items$plotDelta)
    if (!is.null(img_d) && nrow(delta) > 0)
        img_d$setState(delta[, c("factor", "delta"), drop = FALSE])
    invisible(TRUE)
}

#' Predicted surface over the first two continuous factors.
#'
#' Evaluated during the run, so the fitted model never has to be carried in an
#' image's state. An lm keeps its terms' .Environment, which here is the frame
#' of .doe_fill_lm_analysis and therefore reaches the results object itself; a
#' state like that is circular and megabytes wide, and the render pass -- which
#' has to serialise and restore state -- never completes, leaving the image
#' spinning. Returns NULL when no surface can be drawn.
.doe_contour_grid <- function(fit, data, factors, n = 40L) {
    num <- factors[vapply(factors, function(f) is.numeric(data[[f]]), logical(1))]
    if (length(num) < 2)
        return(NULL)
    xname <- num[[1]]
    yname <- num[[2]]
    xr <- range(data[[xname]], na.rm = TRUE)
    yr <- range(data[[yname]], na.rm = TRUE)
    if (!all(is.finite(c(xr, yr))) || xr[1] == xr[2] || yr[1] == yr[2])
        return(NULL)

    grid <- expand.grid(
        seq(xr[1], xr[2], length.out = n),
        seq(yr[1], yr[2], length.out = n)
    )
    names(grid) <- c(xname, yname)
    for (f in setdiff(factors, c(xname, yname))) {
        grid[[f]] <- if (is.numeric(data[[f]]))
            mean(data[[f]], na.rm = TRUE)
        else
            names(sort(table(data[[f]]), decreasing = TRUE))[1]
    }

    pred <- tryCatch(as.numeric(stats::predict(fit, newdata = grid)),
                     error = function(e) NA_real_)
    if (length(pred) != nrow(grid) || all(is.na(pred)))
        return(NULL)

    # Only the plotted columns, as plain vectors, so nothing carries an
    # environment into the serialised state.
    list(
        grid = data.frame(x = as.numeric(grid[[xname]]),
                          y = as.numeric(grid[[yname]]),
                          pred = pred),
        xname = xname,
        yname = yname
    )
}

.doe_eval_plot_opts <- function(options) {
    list(
        halfNormal = isTRUE(options$halfNormal),
        pareto = isTRUE(options$pareto),
        mainEffects = isTRUE(options$mainEffects),
        interaction = isTRUE(options$interaction),
        residuals = isTRUE(options$residuals),
        contour = isTRUE(options$contour),
        effectSummary = !identical(options$effectSummary, FALSE)
    )
}

#' Draw an explanatory placeholder instead of nothing.
#'
#' A render function that returns FALSE produces no file, and jamovi keeps an
#' image that has state but no file in the "rendering" state forever. Whenever
#' state was set but the plot cannot be drawn, draw this instead.
.doe_plot_note <- function(msg) {
    pobj <- ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0, y = 0, label = msg, size = 4) +
        ggplot2::theme_void()
    print(pobj)
    TRUE
}

.doe_plot_half_normal <- function(image, ...) {
    eff <- image$state
    if (is.null(eff)) return(FALSE)
    if (nrow(eff) == 0)
        return(.doe_plot_note("No estimable effects to plot."))
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
    if (is.null(eff)) return(FALSE)
    if (nrow(eff) == 0)
        return(.doe_plot_note("No estimable effects to plot."))
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

.doe_plot_effect_summary <- function(image, ...) {
    st <- image$state
    if (is.null(st)) return(FALSE)
    df <- if (is.data.frame(st)) st else NULL
    if (is.null(df) || nrow(df) == 0 || !("logworth" %in% names(df)))
        return(.doe_plot_note("No logworth values to plot."))
    df <- df[is.finite(df$logworth), , drop = FALSE]
    if (nrow(df) == 0)
        return(.doe_plot_note("No logworth values to plot."))
    df <- df[order(df$logworth, decreasing = TRUE), , drop = FALSE]
    df$term <- factor(df$term, levels = rev(as.character(df$term)))
    fdr <- isTRUE(df$fdr[1])
    ylab <- if (fdr) "Logworth (FDR)" else "Logworth"
    pobj <- ggplot2::ggplot(df, ggplot2::aes(x = term, y = logworth)) +
        ggplot2::geom_col(fill = "#9AA5B1") +
        ggplot2::geom_hline(yintercept = 2, colour = "#1F4E79", linewidth = 0.6) +
        ggplot2::coord_flip() +
        ggplot2::labs(x = NULL, y = ylab, title = "Effect Summary") +
        ggplot2::theme_bw()
    print(pobj)
    TRUE
}

.doe_plot_main_effects <- function(image, ...) {
    st <- image$state
    if (is.null(st)) return(FALSE)
    data <- st$data; dep <- st$dep; factors <- st$factors
    if (length(factors) < 1)
        return(.doe_plot_note("No factors to plot."))
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
    if (is.null(st)) return(FALSE)
    if (length(st$factors) < 2)
        return(.doe_plot_note("An interaction plot needs two or more factors."))
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
    if (is.null(st)) return(FALSE)
    if (is.null(st$fitted) || is.null(st$resid))
        return(.doe_plot_note("No fitted values to take residuals from."))
    df <- data.frame(
        fitted = st$fitted,
        resid = st$resid,
        theoretical = stats::qqnorm(st$resid, plot.it = FALSE)$x
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
    if (is.null(st$grid) || nrow(st$grid) == 0)
        return(.doe_plot_note("No predicted surface to plot."))
    pobj <- ggplot2::ggplot(st$grid, ggplot2::aes(x = x, y = y, z = pred)) +
        ggplot2::geom_contour_filled(bins = 12) +
        ggplot2::labs(x = st$xname, y = st$yname, fill = st$dep,
                      title = "Contour Plot") +
        ggplot2::theme_bw()
    print(pobj)
    TRUE
}

.doe_plot_sn <- function(image, ...) {
    df <- image$state
    if (is.null(df)) return(FALSE)
    if (nrow(df) == 0)
        return(.doe_plot_note("No signal-to-noise values to plot."))
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
    if (is.null(df)) return(FALSE)
    if (nrow(df) == 0)
        return(.doe_plot_note("No response means to plot."))
    pobj <- ggplot2::ggplot(df, ggplot2::aes(x = level, y = value, group = 1)) +
        ggplot2::geom_line() + ggplot2::geom_point() +
        ggplot2::facet_wrap(~ factor, scales = "free_x") +
        ggplot2::labs(x = "Level", y = "Mean Response", title = "Means Main Effects") +
        ggplot2::theme_bw()
    print(pobj)
    TRUE
}

.doe_plot_sn_delta <- function(image, ...) {
    df <- image$state
    if (is.null(df)) return(FALSE)
    if (nrow(df) == 0 || !("delta" %in% names(df)))
        return(.doe_plot_note("No SN deltas to plot."))
    df <- df[is.finite(df$delta), , drop = FALSE]
    if (nrow(df) == 0)
        return(.doe_plot_note("No SN deltas to plot."))
    df <- df[order(df$delta, decreasing = TRUE), , drop = FALSE]
    df$factor <- factor(df$factor, levels = rev(as.character(df$factor)))
    pobj <- ggplot2::ggplot(df, ggplot2::aes(x = factor, y = delta)) +
        ggplot2::geom_col(fill = "#9AA5B1") +
        ggplot2::coord_flip() +
        ggplot2::labs(x = NULL, y = "SN Delta", title = "SN Factor Delta") +
        ggplot2::theme_bw()
    print(pobj)
    TRUE
}
