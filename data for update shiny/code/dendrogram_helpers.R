# Helper functions for interactive dendrogram visualization
# Adapted from run_cluster_cap9_catalogs.R and cluster_catalogs.R

library(ggdendro)
library(lsa)
library(plotly)

# Colors for source groups
cap9_colors <- c(
  Liu        = "#FF0000",
  PCAWG      = "#8B008B",
  Hartwig_cap9   = "#006400",
  Hartwig_nocap  = "#4169E1"
)

#' Rename signatures in a CAP9 file: add source prefix, strip "hdp."
#' Returns data.frame with source_group attribute
rename_sigs <- function(file_path) {
  sigs <- read.table(file_path, header = TRUE, sep = "\t")
  fname <- basename(file_path)

  prefix <- ""
  if (grepl("nocap", fname)) prefix <- "N"
  if (grepl("cap9", fname))  prefix <- "C"
  if (grepl("PCAWG", fname))   prefix <- paste0("P", prefix)
  if (grepl("Hartwig", fname)) prefix <- paste0("H", prefix)

  clean_names <- gsub("hdp\\.", "", colnames(sigs))
  colnames(sigs) <- paste0(prefix, clean_names)

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

#' Load CAP9 signatures + Liu reference for a given dataset type
#'
#' @param dataset_type "Koh89" or "Koh476"
#' @param sig_dir Directory containing CAP9 signature files
#' @param data_dir Directory containing Liu reference signatures
#' @return List with: combined (features-as-rows matrix), source_vec (named vector)
load_cap9_signatures <- function(
    dataset_type,
    sig_dir = "../Manuscript_data/Mo_CAP9_analysis/Signatures",
    data_dir = "../Manuscript_data") {

  # Find matching CAP9 files
  cap9_files <- Sys.glob(file.path(sig_dir, paste0("*", dataset_type, "*")))

  # Read and rename
  all_sigs <- lapply(cap9_files, rename_sigs)

  # Filter to matching row counts
  nrows <- sapply(all_sigs, nrow)
  expected <- max(nrows)
  all_sigs <- all_sigs[nrows == expected]

  # Build source vector for CAP9 sigs
  source_vec <- character(0)
  for (s in all_sigs) {
    grp <- attr(s, "source_group")
    source_vec <- c(source_vec, setNames(rep(grp, ncol(s)), colnames(s)))
  }

  # Combine CAP9 sigs (no row names in these files)
  cap9_combined <- do.call(cbind, all_sigs)

  # Read Liu reference
  liu_suffix <- if (dataset_type == "Koh89") "89" else "476"
  liu_file <- file.path(data_dir, paste0("Liu_et_al_final_", liu_suffix, "_type_signatures.tsv"))
  liu_sigs <- read.table(liu_file, header = TRUE, sep = "\t", row.names = 1, check.names = FALSE)

  # Add Liu to source vector
  source_vec <- c(source_vec, setNames(rep("Liu", ncol(liu_sigs)), colnames(liu_sigs)))

  # Combine: cap9 files have no row names, liu has row names
  # They should have same number of rows (features)
  combined <- cbind(cap9_combined, liu_sigs)

  list(combined = combined, source_vec = source_vec)
}

#' Compute dendrogram from combined signature matrix
#'
#' @param combined Features-as-rows matrix
#' @return List with: hc, cosine_sim, dend_data, combined_t
compute_dendrogram <- function(combined) {
  combined_t <- t(combined)

  # Normalize for cosine distance
  combined_norm <- combined_t / sqrt(rowSums(combined_t^2))
  cosine_sim <- combined_norm %*% t(combined_norm)
  cosine_dist <- as.dist(1 - cosine_sim)

  hc <- hclust(cosine_dist, method = "average")
  dend_data <- dendro_data(hc)

  list(
    hc = hc,
    cosine_sim = cosine_sim,
    dend_data = dend_data,
    combined_t = combined_t
  )
}

#' Build an interactive plotly dendrogram
#'
#' @param dend_data Output from dendro_data()
#' @param source_vec Named vector mapping sig name -> source group
#' @param colors Named color vector
#' @return plotly object with source="dendrogram"
build_plotly_dendrogram <- function(dend_data, source_vec,
                                    colors = cap9_colors) {
  seg_df <- dend_data$segments
  label_df <- dend_data$labels
  label_df$source <- source_vec[as.character(label_df$label)]
  label_df$color <- colors[label_df$source]

  # Segments as connected lines with NA breaks
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

  # Add leaf markers by source group (for legend + click handling)
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

  # Rotated text annotations for leaf labels
  annotations <- lapply(seq_len(nrow(label_df)), function(i) {
    list(
      x = label_df$x[i],
      y = -0.01,
      text = as.character(label_df$label[i]),
      textangle = -90,
      xref = "x", yref = "y",
      showarrow = FALSE,
      font = list(size = 9, color = label_df$color[i]),
      xanchor = "right",
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
