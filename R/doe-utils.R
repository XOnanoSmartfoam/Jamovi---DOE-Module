#' Parse a comma-separated levels string into a character vector.
.doe_parse_levels <- function(levels_text) {
    if (is.null(levels_text) || is.na(levels_text) || !nzchar(trimws(levels_text)))
        return(character(0))
    parts <- strsplit(as.character(levels_text), ",", fixed = TRUE)[[1]]
    trimws(parts[nzchar(trimws(parts))])
}

#' Normalize the factor Array option into a data.frame.
#' Expected elements: name, levels (comma-separated). Optional: type.
.doe_parse_factors <- function(factors_opt, default_levels = c("-1", "1")) {
    if (is.null(factors_opt) || length(factors_opt) == 0)
        return(data.frame(name = character(0), levels = I(list()),
                          type = character(0), stringsAsFactors = FALSE))

    rows <- lapply(seq_along(factors_opt), function(i) {
        item <- factors_opt[[i]]
        nm <- if (!is.null(item$name) && nzchar(trimws(item$name)))
            trimws(as.character(item$name)) else paste0("X", i)
        lv <- .doe_parse_levels(item$levels)
        if (length(lv) < 2)
            lv <- default_levels
        typ <- if (!is.null(item$type) && nzchar(as.character(item$type)))
            as.character(item$type) else "categorical"
        list(name = nm, levels = list(lv), type = typ)
    })

    data.frame(
        name = vapply(rows, `[[`, character(1), "name"),
        levels = I(lapply(rows, function(x) x$levels[[1]])),
        type = vapply(rows, `[[`, character(1), "type"),
        stringsAsFactors = FALSE
    )
}

#' Parse continuous factors with low/high from Array option.
.doe_parse_continuous_factors <- function(factors_opt) {
    if (is.null(factors_opt) || length(factors_opt) == 0)
        return(data.frame(name = character(0), low = numeric(0),
                          high = numeric(0), stringsAsFactors = FALSE))

    rows <- lapply(seq_along(factors_opt), function(i) {
        item <- factors_opt[[i]]
        nm <- if (!is.null(item$name) && nzchar(trimws(item$name)))
            trimws(as.character(item$name)) else paste0("X", i)
        lv <- .doe_parse_levels(item$levels)
        if (length(lv) >= 2) {
            lo <- suppressWarnings(as.numeric(lv[1]))
            hi <- suppressWarnings(as.numeric(lv[length(lv)]))
        } else {
            lo <- NA_real_
            hi <- NA_real_
        }
        if (is.na(lo) || is.na(hi)) {
            lo <- -1
            hi <- 1
        }
        if (lo == hi)
            hi <- lo + 1
        list(name = nm, low = min(lo, hi), high = max(lo, hi))
    })

    data.frame(
        name = vapply(rows, `[[`, character(1), "name"),
        low = vapply(rows, `[[`, numeric(1), "low"),
        high = vapply(rows, `[[`, numeric(1), "high"),
        stringsAsFactors = FALSE
    )
}

#' Make unique, R-safe factor names.
.doe_safe_names <- function(names) {
    names <- make.names(names, unique = TRUE)
    names
}

#' Coerce a jamovi Integer/List/Number option to a safe integer.
#' List names like r3 are used because jamovi ComboBoxes coerce "3" to a
#' number and then fail to match the string option, leaving the default.
.doe_int_opt <- function(x, default = 1L, min = 1L) {
    if (is.list(x) && !is.null(x$value))
        x <- x$value
    if (is.character(x) && length(x) >= 1 && !is.na(x[1])) {
        digits <- gsub("[^0-9]+", "", x[1])
        if (nzchar(digits))
            x <- digits
    }
    r <- suppressWarnings(as.integer(x)[1])
    if (!length(r) || is.na(r) || !is.finite(r))
        r <- as.integer(default)[1]
    if (is.finite(min) && r < min)
        r <- as.integer(min)[1]
    as.integer(r)
}

#' Parse the responses Array option: name + goal.
#' `target` is the analysis-level Target value (used when goal is match).
.doe_parse_responses <- function(responses_opt, target = NA_real_) {
    empty <- data.frame(name = character(0), goal = character(0),
                        target = numeric(0), stringsAsFactors = FALSE)
    if (is.null(responses_opt) || length(responses_opt) == 0)
        return(empty)

    shared_tgt <- suppressWarnings(as.numeric(target))
    if (length(shared_tgt) < 1 || !is.finite(shared_tgt[1]))
        shared_tgt <- NA_real_
    else
        shared_tgt <- shared_tgt[1]

    rows <- lapply(seq_along(responses_opt), function(i) {
        item <- responses_opt[[i]]
        nm <- if (!is.null(item$name) && nzchar(trimws(as.character(item$name))))
            trimws(as.character(item$name)) else ""
        goal <- if (!is.null(item$goal) && nzchar(as.character(item$goal)))
            as.character(item$goal) else "maximize"
        if (!goal %in% c("maximize", "minimize", "match"))
            goal <- "maximize"
        tgt <- NA_real_
        if (!is.null(item$target) && !identical(item$target, ""))
            tgt <- suppressWarnings(as.numeric(item$target))
        if (!is.finite(tgt))
            tgt <- shared_tgt
        if (!is.finite(tgt) || !identical(goal, "match"))
            tgt <- NA_real_
        list(name = nm, goal = goal, target = tgt)
    })
    names_raw <- vapply(rows, `[[`, character(1), "name")
    keep <- nzchar(names_raw)
    if (!any(keep))
        return(data.frame(name = "Y", goal = "maximize", target = NA_real_,
                          stringsAsFactors = FALSE))
    rows <- rows[keep]
    data.frame(
        name = .doe_safe_names(vapply(rows, `[[`, character(1), "name")),
        goal = vapply(rows, `[[`, character(1), "goal"),
        target = vapply(rows, function(x) {
            t <- x$target
            if (is.null(t) || length(t) < 1) NA_real_ else as.numeric(t)
        }, numeric(1)),
        stringsAsFactors = FALSE
    )
}

.doe_parse_responses_opt <- function(self) {
    tgt <- tryCatch(self$options$responseTarget, error = function(e) NA_real_)
    .doe_parse_responses(self$options$responses, target = tgt)
}

.doe_goal_label <- function(goal, target = NA_real_) {
    if (identical(goal, "minimize"))
        return("minimize")
    if (identical(goal, "match")) {
        if (is.finite(target))
            return(paste0("match target (", target, ")"))
        return("match target")
    }
    "maximize"
}

.doe_goal_to_sn <- function(goal) {
    if (identical(goal, "maximize"))
        return("larger")
    if (identical(goal, "match"))
        return("nominal")
    "smaller"
}

#' Convert a design object / data.frame into a plain data.frame of factor columns.
#' DoE.base / FrF2 style run labels: `1`, `2`, ... or `1.1`, `2.1`, ..., `1.2`, ...
.doe_std_run_labels <- function(n_base, n_reps = 1L) {
    n_base <- max(0L, as.integer(n_base))
    n_reps <- max(1L, as.integer(n_reps))
    if (n_base < 1L)
        return(character(0))
    if (n_reps == 1L)
        return(as.character(seq_len(n_base)))
    paste(
        rep(seq_len(n_base), times = n_reps),
        rep(seq_len(n_reps), each = n_base),
        sep = "."
    )
}

.doe_as_data_frame <- function(design) {
    df <- as.data.frame(design)

    # Prefer DoE run.no.std.rp (e.g. 1.1, 2.1, ..., 1.2) when replications exist
    run_lab <- NULL
    ro <- tryCatch({
        if (requireNamespace("DoE.base", quietly = TRUE))
            DoE.base::run.order(design)
        else
            attr(design, "run.order")
    }, error = function(e) NULL)
    if (is.data.frame(ro) && "run.no.std.rp" %in% names(ro))
        run_lab <- as.character(ro$run.no.std.rp)

    # Drop meta / replication-block columns (Run carries std.rep instead)
    drop <- intersect(names(df), c(
        "run.no", "run.no.in.std.order", "run.no.std.rp",
        "std.order", "run.order", "Block", "Blocks", "Replicate"
    ))
    if (length(drop))
        df <- df[, setdiff(names(df), drop), drop = FALSE]

    for (nm in names(df)) {
        if (is.factor(df[[nm]]))
            df[[nm]] <- as.character(df[[nm]])
    }

    if (!is.null(run_lab) && length(run_lab) == nrow(df))
        df <- cbind(Run = run_lab, df, stringsAsFactors = FALSE)

    df
}

.doe_table_cell <- function(x) {
    if (is.null(x) || length(x) < 1)
        return("")
    x <- x[[1]]
    if (is.factor(x))
        x <- as.character(x)
    if (length(x) != 1 || (is.atomic(x) && is.na(x)))
        return("")
    if (is.numeric(x) || is.logical(x) || is.character(x))
        return(x)
    as.character(x)
}

#' Populate a jamovi Table from a data.frame (dynamic columns).
#' Always clears previous rows first: addRow only appends, so leftover
#' rows from a 1-rep run would hide or duplicate a later replicate stack.
.doe_fill_table <- function(table, df) {
    if (is.null(table))
        return(invisible(NULL))
    tryCatch(table$deleteRows(), error = function(e) invisible(NULL))
    if (is.null(df) || nrow(df) == 0)
        return(invisible(NULL))

    if (!("Run" %in% names(df)))
        df <- cbind(Run = .doe_std_run_labels(nrow(df), 1L), df, stringsAsFactors = FALSE)

    for (col in names(df)) {
        typ <- if (identical(col, "Run")) {
            if (is.character(df[[col]]) || any(grepl("\\.", as.character(df[[col]]), fixed = FALSE)))
                "text"
            else
                "integer"
        } else if (is.numeric(df[[col]])) {
            "number"
        } else {
            "text"
        }
        # Run may already exist from .r.yaml; ignore duplicate-column errors
        tryCatch(
            table$addColumn(name = col, title = col, type = typ),
            error = function(e) invisible(NULL)
        )
        tryCatch(table$getColumn(col)$setVisible(TRUE), error = function(e) invisible(NULL))
    }
    # Hide leftover columns from a previous factor list (jamovi cannot delete them)
    old_cols <- tryCatch(names(table$columns), error = function(e) character(0))
    for (col in setdiff(old_cols, names(df))) {
        tryCatch(table$getColumn(col)$setVisible(FALSE), error = function(e) invisible(NULL))
    }

    for (i in seq_len(nrow(df))) {
        vals <- lapply(names(df), function(col) .doe_table_cell(df[[col]][i]))
        names(vals) <- names(df)
        row_key <- if ("Run" %in% names(df)) as.character(df$Run[i]) else as.character(i)
        table$addRow(rowKey = row_key, values = vals)
    }
    invisible(NULL)
}

#' Stack a design data.frame for additional experimental replicates.
#' Labels runs as DoE-style `std.rep` (e.g. 1.1, 2.1, ..., 1.2).
.doe_repeat_design <- function(df, n_reps = 1L) {
    n_reps <- .doe_int_opt(n_reps, default = 1L, min = 1L)
    if (is.null(df) || nrow(df) == 0)
        return(df)

    # Drop prior run/rep bookkeeping before restacking
    drop <- intersect(names(df), c("Run", "Replicate", "Blocks", "Block"))
    if (length(drop))
        df <- df[, setdiff(names(df), drop), drop = FALSE]

    n_base <- nrow(df)
    if (n_reps == 1L) {
        return(cbind(Run = .doe_std_run_labels(n_base, 1L), df, stringsAsFactors = FALSE))
    }

    pieces <- lapply(seq_len(n_reps), function(r) df)
    out <- do.call(rbind, pieces)
    rownames(out) <- NULL
    cbind(Run = .doe_std_run_labels(n_base, n_reps), out, stringsAsFactors = FALSE)
}

#' Ensure a Run column exists (DoE std.rep labels when n_reps > 1).
.doe_ensure_run_column <- function(df, n_reps = 1L) {
    if (is.null(df) || nrow(df) == 0)
        return(df)
    if ("Run" %in% names(df))
        return(df)
    n_reps <- max(1L, as.integer(n_reps))
    n_base <- if (n_reps > 1L) as.integer(round(nrow(df) / n_reps)) else nrow(df)
    if (n_reps > 1L && n_base * n_reps == nrow(df))
        run <- .doe_std_run_labels(n_base, n_reps)
    else
        run <- .doe_std_run_labels(nrow(df), 1L)
    cbind(Run = run, df, stringsAsFactors = FALSE)
}

#' Build response columns (empty or simulated) for spreadsheet / results.
#' @param names optional column names (overrides Y1, Y2, ... numbering)
#' @param simulate if TRUE, fill with random data plus small planted factor effects
.doe_build_response_df <- function(factor_df, n_responses = 1L,
                                   simulate = FALSE, seed = NULL,
                                   names = NULL) {
    n <- nrow(factor_df)
    if (!is.null(names) && length(names) > 0) {
        resp_names <- .doe_safe_names(as.character(names))
        n_responses <- length(resp_names)
    } else {
        n_responses <- max(0L, as.integer(n_responses))
        resp_names <- paste0("Y", seq_len(n_responses))
    }
    if (n_responses < 1 || n < 1)
        return(NULL)

    coded <- list()
    for (nm in names(factor_df)) {
        if (nm %in% c("Run", "Replicate", "Blocks", "Block"))
            next
        v <- factor_df[[nm]]
        if (is.numeric(v) && !is.factor(v)) {
            s <- stats::sd(v, na.rm = TRUE)
            coded[[nm]] <- if (!is.finite(s) || s == 0) rep(0, n) else as.numeric(scale(v))
        } else {
            f <- factor(v)
            if (nlevels(f) == 2) {
                coded[[nm]] <- ifelse(as.integer(f) == 1L, -1, 1)
            } else {
                coded[[nm]] <- as.numeric(f) - mean(as.numeric(f))
            }
        }
    }

    if (isTRUE(simulate) && !is.null(seed) && is.finite(seed))
        set.seed(as.integer(seed) + 101L)

    cols <- list()
    fn <- names(coded)
    for (r in seq_len(n_responses)) {
        nm <- resp_names[[r]]
        if (isTRUE(simulate)) {
            y <- stats::rnorm(n, mean = 50 + 2 * (r - 1), sd = 2)
            if (length(fn) >= 1)
                y <- y + (6 - 0.5 * r) * coded[[fn[1]]]
            if (length(fn) >= 2)
                y <- y + (3.5 - 0.3 * r) * coded[[fn[2]]]
            if (length(fn) >= 2)
                y <- y + 2 * coded[[fn[1]]] * coded[[fn[2]]]
            if (length(fn) >= 3)
                y <- y + 1.5 * coded[[fn[3]]]
            cols[[nm]] <- as.numeric(y)
        } else {
            cols[[nm]] <- rep(NA_real_, n)
        }
    }
    as.data.frame(cols, check.names = FALSE, stringsAsFactors = FALSE)
}

#' Cross an inner design with a noise (outer) array.
#' Returns list(df, note, ok). On failure df is NULL and the inner design should still be shown.
.doe_build_outer_response_df <- function(factor_df, noise_opt, resp_info = NULL,
                                         simulate = FALSE, seed = NULL) {
    noise <- .doe_parse_factors(noise_opt, default_levels = c("Low", "High"))
    if (nrow(noise) < 1)
        return(list(df = NULL, ok = FALSE,
                    note = " Add at least one noise factor (name and two levels) to build the outer array."))

    noise$name <- .doe_safe_names(noise$name)
    fnames <- list()
    for (i in seq_len(nrow(noise)))
        fnames[[noise$name[i]]] <- as.character(noise$levels[[i]])

    # fac.design() requires 2+ factors; a single noise factor is a valid outer array.
    outer_df <- tryCatch(
        as.data.frame(expand.grid(fnames, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE),
                      stringsAsFactors = FALSE),
        error = function(e) e
    )
    if (inherits(outer_df, "error"))
        return(list(df = NULL, ok = FALSE, note = paste0(" Outer array could not be built: ", outer_df$message)))
    if (ncol(outer_df) < 1 || nrow(outer_df) < 1)
        return(list(df = NULL, ok = FALSE, note = " Outer array had no noise combinations."))

    prefix <- if (!is.null(resp_info) && nrow(resp_info) >= 1)
        as.character(resp_info$name[1]) else "Y"
    labels <- vapply(seq_len(nrow(outer_df)), function(i) {
        paste(paste0(names(outer_df), as.character(unlist(outer_df[i, , drop = TRUE]))),
              collapse = "_")
    }, character(1))
    labels <- make.names(paste0(prefix, "_", labels), unique = TRUE)

    n_inner <- nrow(factor_df)
    response_df <- as.data.frame(
        matrix(NA_real_, nrow = n_inner, ncol = length(labels)),
        stringsAsFactors = FALSE
    )
    names(response_df) <- labels
    if (isTRUE(simulate)) {
        sim <- .doe_build_response_df(
            factor_df,
            n_responses = ncol(response_df),
            simulate = TRUE,
            seed = seed
        )
        if (!is.null(sim)) {
            for (i in seq_len(ncol(response_df)))
                response_df[[i]] <- sim[[i]]
        }
    }
    list(
        df = response_df,
        ok = TRUE,
        note = paste0(" Outer array crossed (", nrow(outer_df),
                      " noise combinations) for <code>", prefix, "</code>.")
    )
}

#' Treat jamovi Action / Output / Bool values as a simple on/off flag.
.doe_opt_on <- function(x) {
    if (isTRUE(x))
        return(TRUE)
    if (is.list(x) && !is.null(x$value) && isTRUE(x$value))
        return(TRUE)
    FALSE
}

#' Send the design to the spreadsheet, but only when the user asked for it.
#'
#' Touching the Output makes jamovi rewrite the analysis options server side,
#' which desynchronises the options panel and freezes every later edit. Writing
#' only when the Add design to spreadsheet button is clicked keeps design
#' iteration free of that round trip.
.doe_try_write_design <- function(self, factor_df, response_df, seed) {
    action <- .doe_opt_on(tryCatch(self$options$addToSpreadsheet, error = function(e) FALSE))
    if (!action)
        return(list(ok = FALSE, disabled = TRUE))

    tryCatch(
        .doe_write_to_spreadsheet(
            self$results$designOutput,
            factor_df,
            n_responses = if (is.null(response_df)) 0L else ncol(response_df),
            simulate = isTRUE(self$options$simulateResponses),
            seed = seed,
            response_df = response_df
        ),
        error = function(e) list(ok = FALSE, error = conditionMessage(e))
    )
}

#' Show the design in results and optionally write it to the spreadsheet.
.doe_present_design <- function(self, header_html, df, fac_df, resp_df, seed) {
    n_y <- if (is.null(resp_df)) 0L else ncol(resp_df)
    write_res <- .doe_try_write_design(self, fac_df, resp_df, seed)
    self$results$info$setContent(paste0(
        header_html,
        .doe_write_tip(write_res, n_y, isTRUE(self$options$simulateResponses)),
        .doe_html_design(df)
    ))
    .doe_fill_table(self$results$design, df)
    invisible(write_res)
}

#' Columns to send through jamovi's Output API (Run, factors, responses).
.doe_build_output_payload <- function(factor_df, n_responses = 1L,
                                      simulate = FALSE, seed = NULL,
                                      response_df = NULL) {
    if (is.null(factor_df) || nrow(factor_df) == 0)
        return(list(ok = FALSE, error = "Design has no rows to write."))

    n <- nrow(factor_df)
    titles <- character(0)
    descriptions <- character(0)
    measureTypes <- character(0)
    values_list <- list()

    add_col <- function(title, description, measureType, values) {
        if (length(values) != n)
            values <- rep(values, length.out = n)
        titles <<- c(titles, title)
        descriptions <<- c(descriptions, description)
        measureTypes <<- c(measureTypes, measureType)
        values_list[[length(values_list) + 1]] <<- values
    }

    run_vals <- if ("Run" %in% names(factor_df)) factor_df$Run else .doe_std_run_labels(n, 1L)
    add_col("Run", "Standard run.replicate (DoE style)", "nominal",
            as.character(run_vals))

    for (nm in names(factor_df)) {
        if (identical(nm, "Run") || identical(nm, "Replicate") ||
            identical(nm, "Blocks") || identical(nm, "Block"))
            next
        v <- factor_df[[nm]]
        if (is.numeric(v) && !is.factor(v)) {
            add_col(nm, paste("DOE factor", nm), "continuous", as.numeric(v))
        } else {
            add_col(nm, paste("DOE factor", nm), "nominal", as.character(v))
        }
    }

    if (is.null(response_df) || ncol(response_df) == 0) {
        response_df <- .doe_build_response_df(
            factor_df,
            n_responses = n_responses,
            simulate = simulate,
            seed = seed
        )
    }

    if (!is.null(response_df) && ncol(response_df) > 0) {
        for (nm in names(response_df)) {
            desc <- if (isTRUE(simulate)) {
                paste("Simulated response", nm, "(replace with measured values when ready)")
            } else {
                paste("Response", nm, "- enter measured values")
            }
            add_col(nm, desc, "continuous", as.numeric(response_df[[nm]]))
        }
    }

    if (length(titles) < 1)
        return(list(ok = FALSE, error = "No columns to write."))

    list(
        ok = TRUE,
        keys = seq_along(titles),
        titles = titles,
        descriptions = descriptions,
        measureTypes = measureTypes,
        values = values_list,
        n_rows = n,
        n_cols = length(titles),
        response_df = response_df
    )
}

#' Fingerprint of the columns we are about to send, stored as the Output's
#' state so an unchanged design is not sent twice.
.doe_output_signature <- function(payload) {
    cols <- vapply(payload$values, function(v) {
        paste0(as.character(v), collapse = "\u001f")
    }, character(1))
    paste(
        payload$n_rows,
        paste(payload$titles, collapse = "\u001e"),
        paste(payload$measureTypes, collapse = "\u001e"),
        paste(cols, collapse = "\u001d"),
        sep = "|"
    )
}

#' Write design columns into the jamovi spreadsheet via the Output API.
#'
#' jamovi Output is built for residuals/scores on existing rows. For a generated
#' design we must (1) not skip on isNotFilled, (2) not send setRowNums — that
#' makes the server map onto rows that do not exist on a blank sheet, (3) send
#' each column with setValues(index=i, vec) like jmv PCA, using character for
#' names/levels and double for responses. Errors in the Python apply step are
#' swallowed by jamovi, so a failed write looks like a no-op.
.doe_write_to_spreadsheet <- function(output, factor_df, n_responses = 1L,
                                      simulate = FALSE,
                                      seed = NULL, response_df = NULL) {
    if (is.null(output))
        return(list(ok = FALSE, error = "No spreadsheet output object."))

    payload <- .doe_build_output_payload(
        factor_df,
        n_responses = n_responses,
        simulate = simulate,
        seed = seed,
        response_df = response_df
    )
    if (!isTRUE(payload$ok))
        return(payload)

    # Re-sending identical data makes jamovi write back the designOutput option
    # and notify analyses that the data changed, which re-triggers this analysis.
    # That loop keeps the engine busy re-running with the previous options, so
    # later option changes (e.g. replicates) never take effect. Only send when
    # the design differs, or when jamovi reports the columns as not filled.
    sig <- .doe_output_signature(payload)
    prev <- tryCatch(output$state, error = function(e) NULL)
    filled <- isTRUE(tryCatch(output$isFilled(), error = function(e) FALSE))
    if (filled && is.character(prev) && length(prev) == 1L && identical(prev, sig)) {
        return(list(
            ok = TRUE,
            unchanged = TRUE,
            n_rows = payload$n_rows,
            n_cols = payload$n_cols,
            response_df = payload$response_df
        ))
    }

    output$set(
        keys = payload$keys,
        titles = payload$titles,
        descriptions = payload$descriptions,
        measureTypes = payload$measureTypes
    )
    for (i in seq_along(payload$values)) {
        v <- payload$values[[i]]
        if (identical(payload$measureTypes[i], "continuous"))
            output$setValues(index = i, as.numeric(v))
        else
            output$setValues(index = i, as.character(v))
    }
    tryCatch(output$setState(sig), error = function(e) invisible(NULL))

    list(
        ok = TRUE,
        n_rows = payload$n_rows,
        n_cols = payload$n_cols,
        response_df = payload$response_df
    )
}

.doe_preview_tip <- function() {
    paste0(
        "<p>The Design Table below updates as you change factors, responses and ",
        "replicates. Click <b>Add design to spreadsheet</b> when you want the runs in Data.</p>"
    )
}

.doe_write_tip <- function(write_res, n_y, simulate = FALSE) {
    if (isTRUE(write_res$disabled)) {
        return(paste0(
            "<p>This is a <b>preview</b>. The design below updates as you change ",
            "factors, responses and replicates, and nothing has been written to the ",
            "<b>Data</b> spreadsheet yet.</p>",
            "<p>When the design is final, click <b>Add design to spreadsheet</b>.</p>"
        ))
    }
    if (isTRUE(write_res$ok)) {
        n_rows <- write_res$n_rows
        row_txt <- if (!is.null(n_rows) && is.finite(n_rows))
            paste0(n_rows, " run(s) and ")
        else
            ""
        sim_note <- if (isTRUE(simulate))
            " Response columns were filled with random demo data."
        else
            " Enter measured values in the Y columns."
        return(paste0(
            "<p>Sent ", row_txt, n_y,
            " response column(s) to the <b>Data</b> spreadsheet.",
            sim_note,
            " If you see <code>A (2)</code> instead of <code>A</code>, delete jamovi's empty starter columns ",
            "(right-click A, B, C → Delete) and send the design again.</p>",
            "<p>Click <b>Add design to spreadsheet</b> again after you change the design. ",
            "Uncheck <b>Columns in Data</b> to remove the generated columns. ",
            "Check <b>Evaluate this design</b> to analyze these runs here.</p>"
        ))
    }
    if (!is.null(write_res$error) && nzchar(write_res$error)) {
        return(paste0(
            "<p>Could not add the design to the spreadsheet: ", write_res$error,
            "</p><p>", .doe_copy_instructions(), "</p>"
        ))
    }
    paste0("<p>", .doe_copy_instructions(), "</p>")
}

.doe_html_esc <- function(x) {
    x <- as.character(x)
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x
}

.doe_runs_sentence <- function(n_total, n_reps) {
    n_reps <- .doe_int_opt(n_reps, default = 1L, min = 1L)
    n_total <- .doe_int_opt(n_total, default = 0L, min = 0L)
    n_base <- if (n_reps > 1L && n_total >= n_reps)
        as.integer(round(n_total / n_reps)) else n_total
    paste0(n_base, " base run(s) × ", n_reps, " replicate(s) = <b>",
           n_total, " total runs</b>.")
}

#' Visible HTML copy of the design so stacked replicates always appear
#' even if the jamovi Table widget keeps a stale row set.
.doe_html_design <- function(df, max_rows = 120L) {
    if (is.null(df) || nrow(df) == 0)
        return("")
    show <- min(nrow(df), as.integer(max_rows))
    header <- paste0("<th>", vapply(names(df), .doe_html_esc, character(1)),
                     "</th>", collapse = "")
    body <- vapply(seq_len(show), function(i) {
        cells <- vapply(names(df), function(col) {
            paste0("<td>", .doe_html_esc(.doe_table_cell(df[[col]][i])), "</td>")
        }, character(1))
        paste0("<tr>", paste(cells, collapse = ""), "</tr>")
    }, character(1))
    extra <- if (nrow(df) > show)
        paste0("<p>Showing first ", show, " of ", nrow(df), " runs.</p>") else ""
    paste0(
        "<div style='overflow:auto'><table border='1' cellpadding='4' cellspacing='0'>",
        "<thead><tr>", header, "</tr></thead><tbody>",
        paste(body, collapse = ""),
        "</tbody></table></div>", extra
    )
}

#' Instructions note for jamovi design generators.
.doe_copy_instructions <- function() {
    paste(
        "Click Add design to spreadsheet to write these runs to Data.",
        "Delete jamovi's empty A, B, C columns first if you want those names reused.",
        "Check Evaluate this design to analyze these runs in this same results panel."
    )
}

#' Signal-to-noise ratio for a numeric vector of replicates.
#' @param y numeric vector
#' @param type one of 'smaller', 'larger', 'nominal'
.doe_sn_ratio <- function(y, type = c("smaller", "larger", "nominal")) {
    type <- match.arg(type)
    y <- as.numeric(y)
    y <- y[is.finite(y)]
    if (length(y) == 0)
        return(NA_real_)
    if (type == "smaller")
        return(-10 * log10(mean(y^2)))
    if (type == "larger") {
        if (any(y == 0))
            return(NA_real_)
        return(-10 * log10(mean(1 / y^2)))
    }
    # nominal-the-best (Type II): 10 log10(mean^2 / variance)
    if (length(y) < 2)
        return(NA_real_)
    m <- mean(y)
    v <- stats::var(y)
    if (!is.finite(v) || v <= 0)
        return(NA_real_)
    10 * log10((m^2) / v)
}

#' Build Alias text from a model / FrF2 design.
.doe_alias_text <- function(design) {
    txt <- tryCatch({
        al <- FrF2::aliases(lm(y ~ .^2, data = cbind(as.data.frame(design), y = 1)))
        paste(capture.output(print(al)), collapse = "\n")
    }, error = function(e) {
        tryCatch({
            paste(capture.output(print(summary(design))), collapse = "\n")
        }, error = function(e2) e$message)
    })
    txt
}

#' Terms list from jamovi Terms option -> formula right-hand side.
.doe_terms_to_formula <- function(terms, factors) {
    if (is.null(terms) || length(terms) == 0) {
        if (length(factors) == 0)
            return("1")
        return(paste(factors, collapse = " + "))
    }
    parts <- vapply(terms, function(t) {
        if (length(t) == 1) as.character(t) else paste(t, collapse = ":")
    }, character(1))
    paste(parts, collapse = " + ")
}

#' Effect estimates for half-normal / Pareto from a linear model with coded factors.
.doe_effects_from_lm <- function(fit) {
    cf <- stats::coef(fit)
    cf <- cf[!is.na(cf) & names(cf) != "(Intercept)"]
    if (length(cf) == 0)
        return(data.frame(term = character(0), effect = numeric(0),
                          abs_effect = numeric(0), stringsAsFactors = FALSE))
    # For +/-1 coded 2-level factors, effect = 2 * coefficient
    data.frame(
        term = names(cf),
        effect = as.numeric(cf) * 2,
        abs_effect = abs(as.numeric(cf) * 2),
        stringsAsFactors = FALSE
    )
}

#' Taguchi OA catalog metadata for UI notes / capacity checks.
.doe_taguchi_catalog <- function() {
    data.frame(
        id = c("L4.2.3", "L8.2.7", "L9.3.4", "L12.2.11", "L16.2.15",
               "L16.4.5", "L18.2.1.3.7", "L27.3.13", "L32.2.31"),
        label = c("L4 (3 factors @ 2 levels)",
                  "L8 (7 factors @ 2 levels)",
                  "L9 (4 factors @ 3 levels)",
                  "L12 (11 factors @ 2 levels)",
                  "L16 (15 factors @ 2 levels)",
                  "L16 (5 factors @ 4 levels)",
                  "L18 (1@2 + 7@3 levels)",
                  "L27 (13 factors @ 3 levels)",
                  "L32 (31 factors @ 2 levels)"),
        nruns = c(4, 8, 9, 12, 16, 16, 18, 27, 32),
        # max columns available at each level size (best-effort capacity guide)
        max2 = c(3, 7, 0, 11, 15, 0, 1, 0, 31),
        max3 = c(0, 0, 4, 0, 0, 0, 7, 13, 0),
        max4 = c(0, 0, 0, 0, 0, 5, 0, 0, 0),
        stringsAsFactors = FALSE
    )
}

.doe_taguchi_need <- function(nlevels) {
    tab <- table(as.integer(nlevels))
    list(
        n2 = if ("2" %in% names(tab)) as.integer(tab[["2"]]) else 0L,
        n3 = if ("3" %in% names(tab)) as.integer(tab[["3"]]) else 0L,
        n4 = if ("4" %in% names(tab)) as.integer(tab[["4"]]) else 0L,
        other = setdiff(names(tab), c("2", "3", "4"))
    )
}

.doe_taguchi_row_fits <- function(row, need) {
    isTRUE(need$n2 <= row$max2[1] && need$n3 <= row$max3[1] && need$n4 <= row$max4[1])
}

#' Smallest catalog array that can hold this mix of factor levels.
.doe_taguchi_pick_array <- function(nlevels) {
    need <- .doe_taguchi_need(nlevels)
    if (length(need$other) > 0)
        return(NULL)
    catg <- .doe_taguchi_catalog()
    ok <- catg[need$n2 <= catg$max2 & need$n3 <= catg$max3 & need$n4 <= catg$max4, , drop = FALSE]
    if (nrow(ok) == 0)
        return(NULL)
    ok <- ok[order(ok$nruns, ok$id), , drop = FALSE]
    ok[1, , drop = FALSE]
}

#' Use the chosen array when it fits; otherwise switch to the smallest that does.
.doe_taguchi_resolve_array <- function(array_id, nlevels) {
    id_name <- if (identical(array_id, "L18")) "L18.2.1.3.7" else as.character(array_id)
    catg <- .doe_taguchi_catalog()
    row <- catg[match(id_name, catg$id), , drop = FALSE]
    if (nrow(row) == 0 || is.na(row$id[1]))
        stop("Unknown orthogonal array: ", id_name)

    need <- .doe_taguchi_need(nlevels)
    if (length(need$other) > 0)
        stop("This array catalog only supports 2-, 3-, or 4-level factors. Found levels: ",
             paste(need$other, collapse = ", "))

    if (.doe_taguchi_row_fits(row, need))
        return(list(row = row, switched = FALSE, note = ""))

    alt <- .doe_taguchi_pick_array(nlevels)
    if (is.null(alt)) {
        stop(
            "No orthogonal array in this catalog holds ",
            need$n2, " two-level, ", need$n3, " three-level, and ",
            need$n4, " four-level factor(s). Reduce factors or level counts."
        )
    }
    list(
        row = alt,
        switched = TRUE,
        note = paste0(
            "<p><b>", row$label[1], "</b> cannot hold this factor mix (",
            need$n2, " two-level, ", need$n3, " three-level, ",
            need$n4, " four-level). Using <b>", alt$label[1], "</b> instead.</p>"
        )
    )
}

#' Validate that factor level mix can fit the chosen Taguchi array.
#' If it does not, the smallest catalog array that does fit is returned instead.
.doe_taguchi_validate <- function(array_id, nlevels) {
    resolved <- .doe_taguchi_resolve_array(array_id, nlevels)
    invisible(resolved$row)
}

#' Map UI array id to DoE.base oa.design call.
.doe_make_oa <- function(array_id, factor_names, nlevels, randomize = TRUE,
                         seed = NULL, replications = 1L) {
    nfac <- length(factor_names)
    if (nfac < 1)
        stop("Add at least one factor.")

    row <- .doe_taguchi_validate(array_id, nlevels)
    nruns <- as.integer(row$nruns[1])
    replications <- max(1L, as.integer(replications))

    fnames <- vector("list", nfac)
    names(fnames) <- factor_names
    for (i in seq_len(nfac))
        fnames[[i]] <- as.character(seq_len(nlevels[i]))

    seed_arg <- if (is.null(seed) || !is.finite(seed)) NULL else as.integer(seed)

    # Taguchi arrays are often saturated; DoE.base defaults min.residual.df=1
    # which wrongly rejects L4 (3@2) as needing 5>4 runs.
    DoE.base::oa.design(
        nfactors = nfac,
        nlevels = as.integer(nlevels),
        nruns = nruns,
        factor.names = fnames,
        replications = replications,
        randomize = randomize,
        seed = seed_arg,
        min.residual.df = 0
    )
}

#' Apply character level labels to OA columns.
.doe_relabel_design <- function(df, factor_info) {
    for (i in seq_len(nrow(factor_info))) {
        nm <- factor_info$name[i]
        if (!(nm %in% names(df)))
            next
        labs <- factor_info$levels[[i]]
        cur <- sort(unique(as.character(df[[nm]])))
        if (length(labs) >= length(cur)) {
            map <- setNames(labs[seq_along(cur)], cur)
            df[[nm]] <- unname(map[as.character(df[[nm]])])
        }
    }
    df
}

#' Candidate set for AlgDesign from mixed factors.
.doe_candidate_set <- function(factor_info) {
    grids <- lapply(seq_len(nrow(factor_info)), function(i) {
        typ <- factor_info$type[i]
        lv <- factor_info$levels[[i]]
        if (typ == "continuous") {
            nums <- suppressWarnings(as.numeric(lv))
            if (all(is.finite(nums)) && length(nums) >= 2) {
                # 3-level coded for continuous: low, mid, high
                lo <- min(nums); hi <- max(nums)
                return(c(lo, (lo + hi) / 2, hi))
            }
        }
        lv
    })
    names(grids) <- factor_info$name
    expand.grid(grids, stringsAsFactors = TRUE)
}

#' Build model formula for custom / analyze designs.
.doe_model_formula <- function(factor_names, model = c("main", "main2fi", "rsm")) {
    model <- match.arg(model)
    if (length(factor_names) == 0)
        return(~ 1)
    if (model == "main")
        return(stats::as.formula(paste("~", paste(factor_names, collapse = " + "))))
    if (model == "main2fi")
        return(stats::as.formula(paste("~ (", paste(factor_names, collapse = " + "), ")^2")))
    # RSM: main + FO interactions + pure quadratic for numeric-looking names
    main <- paste(factor_names, collapse = " + ")
    quads <- paste(sprintf("I(%s^2)", factor_names), collapse = " + ")
    stats::as.formula(paste("~ (", main, ")^2 +", quads))
}
