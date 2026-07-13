#!/usr/bin/env Rscript
# Export ground-truth metadata for every packaged .rda so the Python conversion
# can be validated non-circularly (against values R itself computed).
#
# Run from the repo root:
#   conda run -n M3T Rscript python/_data_raw/export_rda_reference.R
#
# Writes:
#   python/tests/golden/rda_reference.json   -- shapes, colnames, numeric-col
#                                               sums/NA-counts, rownames
#   python/_data_raw/ghgi_lists/<name>.json  -- full content of the two GHGI
#                                               list datasets (pyreadr can't read
#                                               these), preserving row names.

suppressMessages(library(jsonlite))

data_dir <- "data"
ref_out <- "python/tests/golden/rda_reference.json"
ghgi_out_dir <- "python/_data_raw/ghgi_lists"
dir.create(dirname(ref_out), recursive = TRUE, showWarnings = FALSE)
dir.create(ghgi_out_dir, recursive = TRUE, showWarnings = FALSE)

# GHGI list datasets exported in full (nested list of data frames w/ rownames).
ghgi_lists <- c("GHGI_NG_distribution", "GHGI_NG_transmission")

df_summary <- function(df) {
  num <- vapply(df, is.numeric, logical(1))
  sums <- lapply(names(df)[num], function(cn) sum(df[[cn]], na.rm = TRUE))
  names(sums) <- names(df)[num]
  na_counts <- lapply(names(df), function(cn) sum(is.na(df[[cn]])))
  names(na_counts) <- names(df)
  list(
    class = paste(class(df), collapse = "/"),
    nrow = nrow(df), ncol = ncol(df),
    colnames = names(df),
    rownames = if (!identical(rownames(df), as.character(seq_len(nrow(df)))))
      rownames(df) else NULL,
    numeric_col_sums = sums,
    na_counts = na_counts
  )
}

ref <- list()
files <- list.files(data_dir, pattern = "[.]rda$", full.names = TRUE)
for (f in files) {
  e <- new.env(); load(f, envir = e); nm <- ls(e)[1]; obj <- e[[nm]]

  if (is.matrix(obj)) {
    # e.g. Neighboring_states: square adjacency matrix with state dimnames.
    ref[[nm]] <- list(
      class = paste(class(obj), collapse = "/"),
      nrow = nrow(obj), ncol = ncol(obj),
      rownames = rownames(obj), colnames = colnames(obj),
      total = sum(obj, na.rm = TRUE)
    )
    df <- as.data.frame(obj)
    df <- cbind(`.rowname` = rownames(obj), df, stringsAsFactors = FALSE)
    write_json(df, file.path(ghgi_out_dir, paste0(nm, ".json")),
               dataframe = "columns", na = "null", digits = 15, auto_unbox = TRUE)
  } else if (is.data.frame(obj)) {
    ref[[nm]] <- df_summary(obj)
  } else if (is.list(obj)) {
    # named list of data frames (the two GHGI datasets)
    ref[[nm]] <- list(
      class = paste(class(obj), collapse = "/"),
      length = length(obj),
      names = names(obj),
      elements = lapply(obj, df_summary)
    )
    # full content for the Python builder, rownames preserved as a column
    full <- lapply(obj, function(df) {
      df2 <- cbind(`.rowname` = rownames(df), df, stringsAsFactors = FALSE)
      df2
    })
    write_json(full, file.path(ghgi_out_dir, paste0(nm, ".json")),
               dataframe = "columns", na = "null", digits = 15, auto_unbox = TRUE)
  } else {
    ref[[nm]] <- list(class = paste(class(obj), collapse = "/"),
                      length = length(obj))
  }
}

write_json(ref, ref_out, auto_unbox = TRUE, digits = 12, pretty = TRUE, na = "null")
cat("wrote", ref_out, "and", length(ghgi_lists), "GHGI list JSONs\n")
