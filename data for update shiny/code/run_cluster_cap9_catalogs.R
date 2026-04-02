source("code/cluster_catalogs.R")

sig_dir <- "Manuscript_data/Mo_CAP9_analysis/Signatures"
plot_output <- "plot_output/clip_v_non_clip"

rename_sigs <- function(file_path) {
  sigs <- read.table(file_path, header = TRUE, sep = "\t")
  fname <- basename(file_path)

  prefix <- ""
  # First: nocap -> N, cap9 -> 9
  if (grepl("nocap", fname)) {
    prefix <- "N"
  }
  if (grepl("cap9", fname)) {
    prefix <- "C"
  }
  # Then: PCAWG -> P, Hartwig -> H (prepended before the N/9)
  if (grepl("PCAWG", fname)) {
    prefix <- paste0("P", prefix)
  }
  if (grepl("Hartwig", fname)) {
    prefix <- paste0("H", prefix)
  }

  # Remove "hdp." from column names and add prefix
  clean_names <- gsub("hdp\\.", "", colnames(sigs))
  colnames(sigs) <- paste0(prefix, clean_names)

  # Determine source group for coloring
  if (grepl("PCAWG", fname)) {
    source_group <- "PCAWG"
  } else if (grepl("cap9", fname)) {
    source_group <- "Hartwig_cap9"
  } else {
    source_group <- "Hartwig_nocap"
  }
  attr(sigs, "source_group") <- source_group
  sigs
}

# Write one temp file per source group, returns named list of temp paths
split_by_source <- function(file_list) {
  all_sigs <- lapply(file_list, rename_sigs)

  # Check for row count mismatches
  nrows <- sapply(all_sigs, nrow)
  expected <- max(nrows)
  if (any(nrows != expected)) {
    bad <- file_list[nrows != expected]
    warning(
      "Skipping files with wrong row count: ",
      paste(bad, collapse = ", ")
    )
    all_sigs <- all_sigs[nrows == expected]
  }

  # Group by source
  groups <- sapply(all_sigs, function(x) attr(x, "source_group"))
  tmp_files <- list()
  for (grp in unique(groups)) {
    combined <- do.call(cbind, all_sigs[groups == grp])
    tmp <- tempfile(fileext = ".tsv")
    write.table(combined, tmp, sep = "\t", quote = FALSE)
    tmp_files[[grp]] <- tmp
  }
  tmp_files
}

# High-discrimination colors: Liu (InsDel) is brightest
cap9_colors <- c(
  Liu = "#FF0000",
  PCAWG = "#8B008B",
  Hartwig_cap9 = "#006400",
  Hartwig_nocap = "#4169E1"
)

# --- Koh476 clustering ---
koh476_tmp <- split_by_source(Sys.glob(file.path(sig_dir, "*Koh476*")))
koh476_args <- c(
  list(
    file.path(plot_output, "dendrogram_cap9_Koh476_signatures.pdf"),
    min_similarity_to_display = 0,
    pdf_width = 44,
    colors = cap9_colors,
    bold_sources = "Liu"
  ),
  koh476_tmp,
  list(Liu = "Manuscript_data/Liu_et_al_final_476_type_signatures.tsv")
)
do.call(cluster_catalogs, koh476_args)

# --- Koh89 clustering ---
koh89_tmp <- split_by_source(Sys.glob(file.path(sig_dir, "*Koh89*")))
koh89_args <- c(
  list(
    file.path(plot_output, "dendrogram_cap9_Koh89_signatures.pdf"),
    min_similarity_to_display = 0,
    pdf_width = 44,
    colors = cap9_colors,
    bold_sources = "Liu"
  ),
  koh89_tmp,
  list(Liu = "Manuscript_data/Liu_et_al_final_89_type_signatures.tsv")
)
do.call(cluster_catalogs, koh89_args)
