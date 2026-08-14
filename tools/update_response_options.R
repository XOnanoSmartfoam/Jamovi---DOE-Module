# Batch-add responseReplicates + simulateResponses to design .a.yaml files
files <- c(
    "fullfactorial", "screening", "rsmdesign", "customdesign", "taguchiarrays"
)
root <- if (dir.exists("jamovi")) "jamovi" else file.path("..", "jamovi")

snippet <- "
    - name: responseReplicates
      title: Response replicates
      type: Integer
      min: 1
      default: 1

    - name: simulateResponses
      title: Fill responses with random data
      type: Bool
      default: true
"

for (nm in files) {
    path <- file.path(root, paste0(nm, ".a.yaml"))
    txt <- readLines(path, warn = FALSE)
    if (any(grepl("^\\s*- name: responseReplicates\\s*$", txt))) {
        message("skip a.yaml ", nm)
        next
    }
    # insert before designOutput
    i <- grep("^\\s*- name: designOutput\\s*$", txt)[1]
    if (is.na(i)) stop("no designOutput in ", nm)
    insert <- strsplit(trimws(snippet, which = "left"), "\n", fixed = TRUE)[[1]]
    # keep YAML indentation of 4 spaces for options
    insert <- insert[nzchar(insert)]
    txt <- c(txt[1:(i - 1)], insert, txt[i:length(txt)])
    writeLines(txt, path)
    message("updated a.yaml ", nm)
}
message("done")
