# Headless harness: exercises a generator's .run() without the jamovi engine.
# Usage: Rscript tools/diag_analysis.R
setTimeLimit(elapsed = 60, transient = TRUE)

.libPaths(c(
    file.path(Sys.getenv("APPDATA"), "jamovi", "modules", "jmvdoe", "R"),
    "C:/Program Files/jamovi 2.6.44.0/Resources/modules/base/R",
    "C:/Program Files/jamovi 2.6.44.0/Resources/modules/jmv/R",
    .libPaths()
))
suppressMessages(library(jmvdoe))
cat("installed jmvdoe:", as.character(utils::packageVersion("jmvdoe")), "\n\n")

opts <- jmvdoe:::fullfactorialOptions$new(replicates = "1 copy")
an <- jmvdoe:::fullfactorialClass$new(options = opts)
priv <- an$.__enclos_env__$private

report <- function(label) {
    rows <- nrow(as.data.frame(an$results$design))
    state <- an$results$designOutput$state
    cat(sprintf("%-26s table rows=%-3s outputFilled=%-5s stateSet=%s\n",
                label, rows, an$results$designOutput$isFilled(),
                !is.null(state)))
}

cat("== 1 copy ==\n")
priv$.run(); report("first run")
priv$.run(); report("re-run (no change)")

cat("\n== switch to 3 copies ==\n")
repOpt <- opts$.__enclos_env__$private$..replicates
repOpt$value <- "3 copies"
cat("  options$replicates =", opts$replicates, "\n")
priv$.run(); report("after option change")
priv$.run(); report("re-run (no change)")

cat("\nsummary line:\n")
info <- an$results$info$content
cat(sub("^.*?<p>", "", regmatches(info, regexpr("<p>.*?</p>", info))), "\n")
