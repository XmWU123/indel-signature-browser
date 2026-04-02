library(ggplot2)
library(philentropy)
library(reshape2)
library(gridExtra)

plot_output = "plot_output"

#' Compute cosine similarity between two vectors
#' @param a A numeric vector
#' @param b A numeric vector
#' @return Cosine similarity (0 to 1)
cosine_sim_vec <- function(a, b) {
  a_norm <- a / sum(a)
  b_norm <- b / sum(b)
  dist_mat <- rbind(a_norm, b_norm)
  suppressMessages(
    philentropy::distance(dist_mat, method = "cosine", test.na = FALSE)[1]
  )
}

#' Create heatmap and dendrogram of pairwise cosine similarities
#'
#' Creates a multi-page PDF with:
#' - Page 1: Heatmap of pairwise cosine similarities (lower triangle)
#' - Page 2: Dendrogram from hierarchical clustering
#' - Additional pages: Side-by-side plots for each pair with cosine > 0.89
#'
#' @param sig_path Path to .tsv file with signatures
#' @param out_pdf Path to output PDF file (default: based on input filename)
#' @param cosine_cutoff Cosine similarity threshold for dashed line on dendrogram
#' @return Invisibly returns the cosine similarity matrix
#' @export
all_pairwise_cosine <- function(
  sig_path,
  out_pdf = NULL,
  cosine_cutoff = 0.9
) {
  # Read signatures
  sigs <- read.delim(sig_path, sep = "\t", row.names = 1, check.names = FALSE)

  n_sigs <- ncol(sigs)
  sig_names <- colnames(sigs)

  # Compute pairwise cosine similarities
  cos_mat <- matrix(NA, nrow = n_sigs, ncol = n_sigs)
  rownames(cos_mat) <- sig_names
  colnames(cos_mat) <- sig_names

  for (i in seq_len(n_sigs)) {
    for (j in seq_len(n_sigs)) {
      if (i != j) {
        cos_mat[i, j] <- cosine_sim_vec(
          as.numeric(sigs[, i]),
          as.numeric(sigs[, j])
        )
      }
    }
  }

  # Create lower triangle matrix for heatmap (NA for diagonal and upper triangle)
  cos_mat_lower <- cos_mat
  cos_mat_lower[upper.tri(cos_mat_lower, diag = TRUE)] <- NA

  # Melt for ggplot
  melted <- melt(cos_mat_lower, na.rm = TRUE)
  colnames(melted) <- c("Sig1", "Sig2", "Cosine")

  # Order factor levels to maintain original order
  melted$Sig1 <- factor(melted$Sig1, levels = sig_names)
  melted$Sig2 <- factor(melted$Sig2, levels = sig_names)

  # Create label column: only show if cosine >= cutoff
  melted$Label <- ifelse(
    melted$Cosine >= cosine_cutoff,
    sprintf("%.2f", melted$Cosine),
    ""
  )

  # Filter cells with high similarity for vertical lines
  high_sim <- melted[melted$Cosine >= cosine_cutoff, ]

  # Create data for vertical line segments
  # Lines go from the cell to the x-axis
  if (nrow(high_sim) > 0) {
    line_data <- data.frame(
      x = as.numeric(high_sim$Sig2),
      xend = as.numeric(high_sim$Sig2),
      y = as.numeric(high_sim$Sig1) - 0.5,
      yend = 0.5
    )
  } else {
    line_data <- data.frame(
      x = numeric(),
      xend = numeric(),
      y = numeric(),
      yend = numeric()
    )
  }

  # Create heatmap
  p_heat <- ggplot(melted, aes(x = Sig2, y = Sig1, fill = Cosine)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Label), size = 2.5) +
    scale_fill_gradient(
      low = "white",
      high = "red",
      limits = c(0, 1),
      name = "Cosine\nSimilarity"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(size = 8),
      axis.title = element_blank(),
      panel.grid = element_blank()
    ) +
    coord_fixed(clip = "off") +
    ggtitle("Pairwise Cosine Similarities (Lower Triangle)")

  # Add vertical lines for high-similarity cells
  if (nrow(line_data) > 0) {
    p_heat <- p_heat +
      geom_segment(
        data = line_data,
        aes(x = x, xend = xend, y = y, yend = yend),
        inherit.aes = FALSE,
        color = "darkred",
        linetype = "solid",
        linewidth = 0.5
      )
  }

  # Create distance matrix for clustering (1 - cosine)
  # Fill diagonal with 1 for distance calculation
  cos_mat_full <- cos_mat
  diag(cos_mat_full) <- 1
  # Fill lower triangle
  for (i in 2:n_sigs) {
    for (j in 1:(i - 1)) {
      cos_mat_full[i, j] <- cos_mat_full[j, i]
    }
  }

  dist_mat <- as.dist(1 - cos_mat_full)

  # Hierarchical clustering
  hc <- hclust(dist_mat, method = "average")

  # Convert to dendrogram
  dend <- as.dendrogram(hc)

  # Set output PDF path
  if (is.null(out_pdf)) {
    base_name <- tools::file_path_sans_ext(basename(sig_path))
    out_pdf <- paste0(base_name, "_pairwise_cosine.pdf")
  }

  # Save to PDF in landscape mode
  cairo_pdf(
    out_pdf,
    width = 11,
    height = 8.5,
    onefile = TRUE
  )

  # Page 1: Heatmap
  print(p_heat)

  # Page 2: Dendrogram
  par(mar = c(5, 4, 4, 2))
  plot(
    hc,
    main = "Hierarchical Clustering of Signatures",
    xlab = "Signature",
    ylab = "Distance (1 - Cosine Similarity)",
    hang = -1
  )
  # Draw dashed line at cosine = 0.9 (distance = 0.1)
  abline(h = 1 - cosine_cutoff, lty = 2, col = "red", lwd = 2)
  text(
    x = 1,
    y = 1 - cosine_cutoff + 0.02,
    labels = paste("Cosine =", cosine_cutoff),
    col = "red",
    adj = 0
  )

  # Additional pages: plot pairs with cosine similarity > 0.89
  high_sim_threshold <- 0.89

  # Find all pairs in lower triangle with cosine > threshold
  high_pairs <- which(cos_mat_lower > high_sim_threshold, arr.ind = TRUE)

  # Signatures to skip when plotting pairs
  skip_sigs <- c("ID_N", "ID_J", "InsDel_N", "InsDel_J")

  if (nrow(high_pairs) > 0) {
    for (k in seq_len(nrow(high_pairs))) {
      i <- high_pairs[k, 1]
      j <- high_pairs[k, 2]
      sig1_name <- sig_names[i]
      sig2_name <- sig_names[j]
      cos_val <- cos_mat_lower[i, j]

      # Skip if either signature is in the skip list
      if (sig1_name %in% skip_sigs || sig2_name %in% skip_sigs) {
        next
      }

      message(sprintf(
        "Plotting %s vs %s (cosine = %.3f)",
        sig1_name,
        sig2_name,
        cos_val
      ))

      # Get signature vectors
      sig1_vec <- as.numeric(sigs[, sig1_name])
      sig2_vec <- as.numeric(sigs[, sig2_name])

      # Create plots using mSigPlot
      title1 <- sprintf("%s", sig1_name)
      title2 <- sprintf("%s (cosine = %.3f)", sig2_name, cos_val)

      p1 <- mSigPlot::plot_guess(catalog = sig1_vec, plot_title = title1)
      p2 <- mSigPlot::plot_guess(catalog = sig2_vec, plot_title = title2)

      # Arrange plots one over the other
      grid.arrange(p1, p2, nrow = 2)
    }
  }

  dev.off()

  message("Saved PDF to ", out_pdf)

  invisible(cos_mat_full)
}

# Example usage:
all_pairwise_cosine(
  "Manuscript_data/Liu_et_al_final_89_type_signatures.tsv",
  file.path(plot_output, "heatmap_89_type_signatures.pdf")
)

all_pairwise_cosine(
  "Manuscript_data/Liu_et_al_final_476_type_signatures.tsv",
  file.path(plot_output, "heatmap_476_type_signatures.pdf")
)

all_pairwise_cosine(
  "Manuscript_data/Liu_et_al_final_83_type_signatures.tsv",
  file.path(plot_output, "heatmap_83_type_signatures.pdf")
)
