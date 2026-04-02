# Helper functions for interactive dendrogram v2
# Loads ALL signature files from Koh89/Koh476 subdirectories

library(ggdendro)
library(lsa)
library(plotly)

# Colors for source groups
dendro2_colors <- c(
  Liu  = "#FF0000",
  C.H  = "#006400",
  C.P  = "#8B008B",
  C.PH = "#FF8C00",
  N.H  = "#4169E1"
)

#' Parse new-format signature file and rename columns
#'
#' Filename scheme: {CAP9,NoCAP}.mSigHdp.{Hartwig,PCAWG,PH}.{Koh89,Koh476}.{cancertype}.txt
#' Returns data.frame with `source_group` attribute
rename_sigs2 <- function(file_path) {
  sigs <- read.table(file_path, header = TRUE, sep = "\t")
  fname <- tools::file_path_sans_ext(basename(file_path))

  # Split on ".mSigHdp." to get cap prefix and remainder
  parts <- strsplit(fname, "\\.mSigHdp\\.")[[1]]
  cap_raw <- parts[1]  # "CAP9" or "NoCAP"
  remainder <- parts[2]  # e.g. "PH.Koh89.Bone.SoftTissue"

  cap <- if (cap_raw == "CAP9") "C" else "N"

  # Find Koh89 or Koh476 to split dataset from cancertype
  koh_match <- regexpr("Koh(89|476)", remainder)
  koh_str <- regmatches(remainder, koh_match)  # "Koh89" or "Koh476"
  koh_pos <- koh_match[1]

  # Dataset is everything before the Koh token (minus trailing dot)
  dataset_raw <- substr(remainder, 1, koh_pos - 2)  # e.g. "PH", "Hartwig", "PCAWG"
  dataset <- switch(dataset_raw,
    Hartwig = "H",
    PCAWG   = "P",
    PH      = "PH"
  )

  # Cancertype is everything after "Koh{89,476}."
  cancertype <- sub(paste0(".*", koh_str, "\\."), "", remainder)

  # Build column names
  clean_names <- gsub("hdp\\.", "", colnames(sigs))
  colnames(sigs) <- paste0(cap, ".", dataset, ".", cancertype, ".", clean_names)

  source_group <- paste0(cap, ".", dataset)
  attr(sigs, "source_group") <- source_group
  sigs
}

#' Load all signatures for a dataset type + Liu reference
#'
#' @param dataset_type "Koh89" or "Koh476"
#' @param sig_dir Base directory containing Koh89/ and Koh476/ subdirs
#' @param data_dir Directory containing Liu reference signatures
#' @return List with: combined (features-as-rows data.frame), source_vec (named character vector)
load_all_signatures <- function(
    dataset_type,
    sig_dir = "../Manuscript_data/Mo_CAP9_analysis/Signatures",
    catalog_dir = "../Manuscript_data/Mo_CAP9_analysis/Catalogs",
    data_dir = "../Manuscript_data") {

  sig_files <- Sys.glob(file.path(sig_dir, dataset_type, "*.txt"))

  all_sigs <- lapply(sig_files, rename_sigs2)

  # Filter to matching row counts (expected number of mutation types)
  nrows <- sapply(all_sigs, nrow)
  expected <- max(nrows)
  all_sigs <- all_sigs[nrows == expected]

  # Get mutation type row names from a catalog file (extraction files lack them)
  cat_files <- Sys.glob(file.path(catalog_dir, paste0("*", dataset_type, "*.txt")))
  cat_rownames <- rownames(read.table(cat_files[1], header = TRUE, sep = "\t",
                                       row.names = 1, check.names = FALSE))

  # Assign row names to extraction sigs
  # For Koh476, CAP9 extraction files have rows in ICAMS::catalog.row.order$ID476
  # order, which differs from cat_rownames; set ICAMS order first, then reorder.
  for (i in seq_along(all_sigs)) {
    if (dataset_type == "Koh476" && grepl("^C\\.", attr(all_sigs[[i]], "source_group"))) {
      rownames(all_sigs[[i]]) <- ICAMS::catalog.row.order$ID476
      all_sigs[[i]] <- all_sigs[[i]][cat_rownames, , drop = FALSE]
    } else {
      rownames(all_sigs[[i]]) <- cat_rownames
    }
  }

  # Build source vector
  source_vec <- character(0)
  for (s in all_sigs) {
    grp <- attr(s, "source_group")
    source_vec <- c(source_vec, setNames(rep(grp, ncol(s)), colnames(s)))
  }

  # Combine extraction sigs
  cap9_combined <- do.call(cbind, all_sigs)

  # Read Liu reference
  liu_suffix <- if (dataset_type == "Koh89") "89" else "476"
  liu_file <- file.path(data_dir, paste0("Liu_et_al_final_", liu_suffix, "_type_signatures.tsv"))
  liu_sigs <- read.table(liu_file, header = TRUE, sep = "\t", row.names = 1, check.names = FALSE)

  # Reorder extraction sigs to match Liu row order
  cap9_combined <- cap9_combined[rownames(liu_sigs), ]

  source_vec <- c(source_vec, setNames(rep("Liu", ncol(liu_sigs)), colnames(liu_sigs)))

  combined <- cbind(cap9_combined, liu_sigs)

  list(combined = combined, source_vec = source_vec)
}

#' Filter signatures by cosine similarity to a seed signature
#'
#' @param combined Features-as-rows data.frame
#' @param seed_name Column name of the seed signature
#' @param min_cos_sim Minimum cosine similarity threshold
#' @return Subset of combined keeping only columns >= threshold (always includes seed)
filter_by_seed <- function(combined, seed_name, min_cos_sim) {
  seed_vec <- combined[, seed_name]
  cos_sims <- sapply(colnames(combined), function(s) {
    lsa::cosine(seed_vec, combined[, s])[1, 1]
  })
  keep <- names(cos_sims)[cos_sims >= min_cos_sim]
  if (!(seed_name %in% keep)) keep <- c(seed_name, keep)
  combined[, keep, drop = FALSE]
}

#' Compute dendrogram from combined signature matrix
#'
#' @param combined Features-as-rows matrix
#' @return List with: hc, cosine_sim, dend_data
compute_dendrogram <- function(combined) {
  combined_t <- t(combined)
  combined_norm <- combined_t / sqrt(rowSums(combined_t^2))
  cosine_sim <- combined_norm %*% t(combined_norm)
  cosine_dist <- as.dist(1 - cosine_sim)

  hc <- hclust(cosine_dist, method = "average")
  dend_data <- dendro_data(hc)

  list(hc = hc, cosine_sim = cosine_sim, dend_data = dend_data)
}

#' Build an interactive plotly dendrogram
#'
#' @param dend_data Output from dendro_data()
#' @param source_vec Named vector mapping sig name -> source group
#' @param colors Named color vector
#' @return plotly object with source="dendrogram"
build_plotly_dendrogram <- function(dend_data, source_vec,
                                    colors = dendro2_colors) {
  seg_df <- dend_data$segments
  label_df <- dend_data$labels
  label_df$source <- source_vec[as.character(label_df$label)]
  label_df$color <- colors[label_df$source]

  seg_x <- as.vector(rbind(seg_df$x, seg_df$xend, NA))
  seg_y <- as.vector(rbind(seg_df$y, seg_df$yend, NA))

  p <- plot_ly(source = "dendrogram") |>
    add_trace(
      x = seg_x, y = seg_y,
      type = "scatter", mode = "lines",
      line = list(color = "gray40", width = 1),
      hoverinfo = "none",
      showlegend = FALSE
    )

  for (grp in names(colors)) {
    grp_labels <- label_df[label_df$source == grp, ]
    if (nrow(grp_labels) == 0) next

    p <- p |> add_trace(
      x = grp_labels$x,
      y = rep(0, nrow(grp_labels)),
      type = "scatter",
      mode = "markers",
      marker = list(size = 7, color = colors[grp], symbol = "circle"),
      text = as.character(grp_labels$label),
      customdata = as.character(grp_labels$label),
      hovertemplate = "%{text}<extra></extra>",
      name = grp,
      legendgroup = grp
    )
  }

  annotations <- lapply(seq_len(nrow(label_df)), function(i) {
    list(
      x = label_df$x[i],
      y = -0.01,
      text = as.character(label_df$label[i]),
      textangle = -90,
      xref = "x", yref = "y",
      showarrow = FALSE,
      font = list(size = 9, color = label_df$color[i]),
      xanchor = "center",
      yanchor = "top"
    )
  })

  p |> layout(
    title = "Hierarchical Clustering (Cosine Distance)",
    xaxis = list(
      title = "", showticklabels = FALSE, showgrid = FALSE,
      zeroline = FALSE
    ),
    yaxis = list(
      title = "Cosine Distance", zeroline = FALSE,
      range = c(-0.12, max(seg_df$y) * 1.05)
    ),
    annotations = annotations,
    margin = list(b = 100, t = 30, l = 50, r = 20),
    shapes = list(
      list(
        type = "line", x0 = 0, x1 = max(label_df$x) + 1,
        y0 = 0.1, y1 = 0.1,
        xref = "x", yref = "y",
        line = list(color = "gray50", dash = "dash", width = 1)
      )
    ),
    hovermode = "closest",
    dragmode = "zoom"
  ) |>
    config(displayModeBar = TRUE, displaylogo = FALSE) |>
    event_register("plotly_click")
}
