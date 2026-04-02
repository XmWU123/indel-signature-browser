library(lsa)

#' Find the best-matching spectrum for each signature from a catalog
#'
#' @param signatures Matrix with mutation types as rows and signatures as columns
#' @param catalog_path Path to a TSV catalog file (mutation types as rows, samples as columns)
#' @param min_mutations Minimum total mutation count for a sample to be considered
#' @return Subset of the catalog containing only the unique best-match columns (raw counts)
find_best_match_spectra <- function(signatures, catalog_path, min_mutations = 0) {
  catalog <- read.table(
    catalog_path,
    header = TRUE,
    sep = "\t",
    row.names = 1,
    check.names = FALSE
  )
  cat_mat <- as.matrix(catalog)
  cat_mat <- cat_mat[, colSums(cat_mat) >= min_mutations, drop = FALSE]
  best_samples <- character(0)
  for (i in seq_len(ncol(signatures))) {
    sig_vec <- signatures[, i]
    cos_sims <- apply(cat_mat, 2, function(x) lsa::cosine(sig_vec, x))
    best_samples <- c(best_samples, names(which.max(cos_sims)))
  }
  unique_samples <- unique(best_samples)
  catalog[, unique_samples, drop = FALSE]
}

#' Find the names of the top N best-matching samples for each signature
#'
#' @param signatures Matrix with mutation types as rows and signatures as columns
#' @param catalog_path Path to a TSV catalog file (mutation types as rows, samples as columns)
#' @param n Number of top matches to return per signature
#' @param min_mutations Minimum total mutation count for a sample to be considered
#' @return List with two elements: `names` (character matrix with one row per
#'   signature and n columns, named BestMatch_1, ...) and `cosines` (numeric
#'   matrix of same dimensions with the corresponding cosine similarities).
#'   Row names are the signature column names.
find_top_n_match_names <- function(signatures, catalog_path, n = 3,
                                   min_mutations = 0) {
  catalog <- read.table(
    catalog_path,
    header = TRUE,
    sep = "\t",
    row.names = 1,
    check.names = FALSE
  )
  cat_mat <- as.matrix(catalog)
  cat_mat <- cat_mat[, colSums(cat_mat) >= min_mutations, drop = FALSE]
  name_mat <- matrix(
    NA_character_,
    nrow = ncol(signatures),
    ncol = n,
    dimnames = list(colnames(signatures), paste0("BestMatch_", seq_len(n)))
  )
  cos_mat <- matrix(
    NA_real_,
    nrow = ncol(signatures),
    ncol = n,
    dimnames = list(colnames(signatures), paste0("CosSim_", seq_len(n)))
  )
  for (i in seq_len(ncol(signatures))) {
    sig_vec <- signatures[, i]
    cos_sims <- apply(cat_mat, 2, function(x) lsa::cosine(sig_vec, x)[1, 1])
    sorted <- sort(cos_sims, decreasing = TRUE)
    top_idx <- seq_len(min(n, length(sorted)))
    name_mat[i, top_idx] <- names(sorted[top_idx])
    cos_mat[i, top_idx] <- unname(sorted[top_idx])
  }
  list(names = name_mat, cosines = cos_mat)
}
