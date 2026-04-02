source(here::here("code", "collapse_476_to_89.R"))

collapse_for_dendro <- function() {
  t476 <- read.delim(
    here::here("Manuscript_data", "Mo_CAP9_analysis",
               "cluster_cap9_results",
               "CAP9_Koh476_clustered_medoid_signatures.tsv"),
    row.names = 1, check.names = FALSE
  )

  t89 <- t476_to_89(t476)

  # t476_to_89() appends "_converted" to column names; strip it
  colnames(t89) <- sub("_converted$", "", colnames(t89))

  # Replace leading "C." prefix with "CV476."
  colnames(t89) <- sub("^C\\.", "CV476.", colnames(t89))

  write.table(
    t89,
    here::here("Manuscript_data", "Mo_CAP9_analysis",
               "cluster_cap9_results",
               "CAP9_476_converted_to_89.tsv"),
    sep = "\t", quote = FALSE
  )

  invisible(t89)
}
