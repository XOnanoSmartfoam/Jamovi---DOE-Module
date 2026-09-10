m <- file.path(Sys.getenv("APPDATA"), "jamovi", "modules", "jmvdoe")
cat("module dir:", m, "\n")
cat("declared version:",
    as.character(utils::packageVersion(
        "jmvdoe", lib.loc = file.path(getwd(), "build", "R4.4.1-x64-win64"))), "\n")

y <- readLines(file.path(m, "jamovi.yaml"), warn = FALSE)
cat("installed jamovi.yaml version:", grep("^version", y, value = TRUE), "\n\n")

e <- new.env()
lazyLoad(file.path(m, "R", "jmvdoe"), envir = e)
for (f in c(".doe_contour_grid", ".doe_no_response_message",
            ".doe_looks_numeric", ".doe_and_list"))
    cat(sprintf("  %-28s present: %s\n", f, exists(f, envir = e)))

src <- paste(deparse(get(".doe_plot_residuals", envir = e)), collapse = " ")
cat("\n  residuals renderer uses st$fitted:", grepl("st\\$fitted", src), "\n")
cat("  residuals renderer still uses st$fit:", grepl("st\\$fit[^t]", src), "\n")

src <- paste(deparse(get(".doe_plot_contour", envir = e)), collapse = " ")
cat("  contour renderer uses st$grid:", grepl("st\\$grid", src), "\n")
cat("  contour renderer still calls predict:", grepl("predict", src), "\n")
