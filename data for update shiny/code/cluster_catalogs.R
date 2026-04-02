library(ggplot2)
library(ggdendro)

#' Hierarchical clustering of signature catalogs using cosine distance
#'
#' @param output_file Path to the output PDF file
#' @param min_similarity_to_display Minimum cosine similarity to any other signature
#'   required to display a label. Signatures with max similarity below this threshold
#'   will have their labels suppressed.
#' @param pdf_width,pdf_height PDF dimensions in inches
#' @param colors Named character vector of colors keyed by source name
#' @param bold_sources Character vector of source names whose labels should be bold
#' @param n_panels Number of panels to split the dendrogram into
#' @param ... Named arguments where names are source labels and values are file paths
#'   to TSV files containing signature data (features as rows, samples as columns)
#'
#' @examples
#' # See code/run_cluster_catalogs.R for complete examples
cluster_catalogs <- function(output_file, min_similarity_to_display = 0.85,
                             pdf_width = 11, pdf_height = 8.5,
                             colors = NULL, bold_sources = NULL,
                             n_panels = 1, split_markers = NULL, ...) {
  file_args <- list(...)

  if (length(file_args) == 0) {
    stop("At least one file path must be provided")
  }

  # Check that all arguments are named

  arg_names <- names(file_args)
  if (is.null(arg_names) || any(arg_names == "")) {
    stop("All file arguments must be named (e.g., Source1 = 'path/to/file.tsv')")
  }

  # Read all files and track sources
  sig_list <- list()
  source_vec <- character(0)

  for (source_name in arg_names) {
    file_path <- file_args[[source_name]]
    sigs <- read.table(
      file_path,
      header = TRUE,
      sep = "\t",
      row.names = 1
    )
    sig_list[[source_name]] <- sigs
    source_vec <- c(source_vec, rep(source_name, ncol(sigs)))
  }

  # Combine all signatures and transpose (samples as rows, features as columns)
  # Unname to prevent cbind from adding source prefixes to column names
  combined <- do.call(cbind, unname(sig_list))
  combined_t <- t(combined)
  names(source_vec) <- rownames(combined_t)

  # Normalize rows to unit vectors for cosine distance
  combined_norm <- combined_t / sqrt(rowSums(combined_t^2))

  # Compute cosine distance matrix (1 - cosine similarity)
  cosine_sim <- combined_norm %*% t(combined_norm)
  cosine_dist <- as.dist(1 - cosine_sim)

  # Hierarchical clustering (average linkage works well with cosine distance)
  hc <- hclust(cosine_dist, method = "average")

  # Extract dendrogram data
  dend_data <- dendro_data(hc)

  # Add source info to labels
  label_df <- dend_data$labels
  label_df$source <- source_vec[label_df$label]

  # Suppress labels for signatures with max similarity to any other < threshold
  # Set diagonal to 0 so we only consider other signatures
  cosine_sim_other <- cosine_sim
  diag(cosine_sim_other) <- 0
  max_sim_to_other <- apply(cosine_sim_other, 1, max)
  sigs_to_hide <- names(max_sim_to_other[max_sim_to_other < min_similarity_to_display])
  label_df$label <- ifelse(
    label_df$label %in% sigs_to_hide,
    "",
    as.character(label_df$label)
  )

  # Bold face for specified sources
  label_df$fontface <- ifelse(
    label_df$source %in% bold_sources, "bold", "plain"
  )

  # Build color palette
  if (!is.null(colors)) {
    cb_palette <- colors
  } else {
    # Color-blind friendly palette (Okabe-Ito extended)
    cb_colors <- c(
      "#0072B2", "#D55E00", "#009E73", "#F0E442",
      "#CC79A7", "#56B4E9", "#E69F00", "#000000"
    )
    n_sources <- length(arg_names)
    if (n_sources > length(cb_colors)) {
      stop("Too many sources (max ", length(cb_colors), ")")
    }
    cb_palette <- setNames(cb_colors[seq_len(n_sources)], arg_names)
  }

  # Build plot(s)
  n_leaves <- nrow(label_df)
  seg_df <- dend_data$segments

  # Determine number of panels: use split_markers to find minimum k that
  # separates all marker signatures into different subtrees
  if (!is.null(split_markers)) {
    missing <- split_markers[!split_markers %in% hc$labels]
    if (length(missing) > 0) {
      warning("split_markers not found in data: ", paste(missing, collapse = ", "))
      split_markers <- split_markers[split_markers %in% hc$labels]
    }
    if (length(split_markers) >= 2) {
      for (k in 2:n_leaves) {
        cl <- cutree(hc, k = k)
        marker_cl <- cl[split_markers]
        if (length(unique(marker_cl)) == length(split_markers)) {
          n_panels <- k
          message("split_markers separated at k=", k)
          break
        }
      }
    }
  }

  # Split by cutting the tree into n_panels subtrees
  if (n_panels > 1) {
    clusters <- cutree(hc, k = n_panels)
    # Map cluster IDs to contiguous x-position ranges using dendrogram order
    ordered_labels <- hc$labels[hc$order]
    ordered_clusters <- clusters[ordered_labels]
    # Find boundaries where cluster assignment changes
    panel_ranges <- list()
    current_cluster <- ordered_clusters[1]
    start_x <- 1
    panel_idx <- 1
    for (j in seq_along(ordered_clusters)) {
      if (ordered_clusters[j] != current_cluster || j == length(ordered_clusters)) {
        end_x <- if (ordered_clusters[j] != current_cluster) j - 1 else j
        panel_ranges[[panel_idx]] <- c(start_x - 0.5, end_x + 0.5)
        panel_idx <- panel_idx + 1
        if (ordered_clusters[j] != current_cluster && j == length(ordered_clusters)) {
          panel_ranges[[panel_idx]] <- c(j - 0.5, j + 0.5)
        }
        current_cluster <- ordered_clusters[j]
        start_x <- j
      }
    }
  } else {
    panel_ranges <- list(c(0.5, n_leaves + 0.5))
  }

  plots <- list()
  for (i in seq_along(panel_ranges)) {
    xmin <- panel_ranges[[i]][1]
    xmax <- panel_ranges[[i]][2]

    # Subset labels in this panel
    panel_labels <- label_df[label_df$x > xmin & label_df$x <= xmax, ]

    # Subset segments: include if both x and xend fall within range
    panel_segs <- seg_df[
      pmin(seg_df$x, seg_df$xend) >= xmin &
      pmax(seg_df$x, seg_df$xend) <= xmax, ]

    p <- ggplot() +
      geom_segment(
        data = panel_segs,
        aes(x = x, y = y, xend = xend, yend = yend)
      ) +
      geom_text(
        data = panel_labels,
        aes(x = x, y = y - 0.01, label = label, color = source,
            fontface = fontface),
        hjust = 1,
        angle = 90,
        size = 3
      ) +
      geom_hline(yintercept = 0.1, linetype = "dashed", color = "gray50") +
      scale_color_manual(values = cb_palette, drop = FALSE) +
      coord_cartesian(ylim = c(-0.15, NA), clip = "off") +
      labs(
        title = if (n_panels == 1) {
          "Hierarchical Clustering of Signatures (Cosine Distance)"
        } else {
          paste0("Hierarchical Clustering of Signatures (Cosine Distance) — Part ", i)
        },
        x = "",
        y = "Cosine Distance",
        color = "Source"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.margin = margin(t = 10, r = 10, b = 60, l = 10)
      )

    plots[[i]] <- p
  }

  # Full dendrogram plot (always included as page 1)
  p_full <- ggplot() +
    geom_segment(
      data = seg_df,
      aes(x = x, y = y, xend = xend, yend = yend)
    ) +
    geom_text(
      data = label_df,
      aes(x = x, y = y - 0.01, label = label, color = source,
          fontface = fontface),
      hjust = 1,
      angle = 90,
      size = 3
    ) +
    geom_hline(yintercept = 0.1, linetype = "dashed", color = "gray50") +
    scale_color_manual(values = cb_palette, drop = FALSE) +
    coord_cartesian(ylim = c(-0.15, NA), clip = "off") +
    labs(
      title = "Hierarchical Clustering of Signatures (Cosine Distance)",
      x = "",
      y = "Cosine Distance",
      color = "Source"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.margin = margin(t = 10, r = 10, b = 60, l = 10)
    )

  # Save to PDF — full plot on page 1, then subtree panels if multiple
  cairo_pdf(output_file, width = pdf_width, height = pdf_height)
  print(p_full)
  if (n_panels > 1) {
    for (p in plots) print(p)
  }
  dev.off()

  message("Created ", output_file)

  invisible(list(
    hclust = hc,
    distance = cosine_dist,
    plots = plots
  ))
}
