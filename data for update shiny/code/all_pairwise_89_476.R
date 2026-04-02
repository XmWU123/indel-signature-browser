library(ggplot2)
library(philentropy)
library(reshape2)
library(gridExtra)

plot_output <- "plot_output"

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

#' Create heatmap of pairwise cosine similarities
#' @param cos_mat Cosine similarity matrix
#' @param sig_names Signature names
#' @param title Plot title
#' @param cosine_cutoff Threshold for labeling cells and vertical lines
#' @return ggplot object
create_heatmap <- function(cos_mat, sig_names, title, cosine_cutoff = 0.9) {
  # Create lower triangle matrix for heatmap
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
    ggtitle(title)

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

  p_heat
}

#' Create dendrogram plot for signatures
#' @param sigs Signature matrix (features x signatures)
#' @param title Plot title
#' @param cosine_cutoff Threshold for dashed line
#' @return hclust object
plot_dendrogram <- function(sigs, title, cosine_cutoff = 0.9) {
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

  # Fill diagonal and make symmetric
  cos_mat_full <- cos_mat
  diag(cos_mat_full) <- 1
  for (i in 2:n_sigs) {
    for (j in 1:(i - 1)) {
      cos_mat_full[i, j] <- cos_mat_full[j, i]
    }
  }

  dist_mat <- as.dist(1 - cos_mat_full)
  hc <- hclust(dist_mat, method = "average")

  par(mar = c(5, 4, 4, 2))
  plot(
    hc,
    main = title,
    xlab = "Signature",
    ylab = "Distance (1 - Cosine Similarity)",
    hang = -1
  )
  abline(h = 1 - cosine_cutoff, lty = 2, col = "red", lwd = 2)
  text(
    x = 1,
    y = 1 - cosine_cutoff + 0.02,
    labels = paste("Cosine =", cosine_cutoff),
    col = "red",
    adj = 0
  )

  invisible(list(hclust = hc, cos_mat = cos_mat_full))
}

#' Compare 89-type and 476-type signatures with combined visualization
#'
#' Creates a multi-page PDF with:
#' - Page 1: Heatmap of pairwise cosine similarities (89-type)
#' - Page 2: Heatmap of pairwise cosine similarities (476-type)
#' - Page 3: Dendrogram for 89-type signatures
#' - Page 4: Dendrogram for 476-type signatures
#' - Additional pages: For each pair with cosine > 0.89, shows both 89-type
#'   and both 476-type signatures on one page (4 plots total)
#'
#' @param out_pdf Path to output PDF file
#' @param cosine_cutoff Cosine similarity threshold for dashed line on dendrogram
#' @return Invisibly returns list with cosine matrices for both types
#' @export
all_pairwise_89_476 <- function(
    out_pdf = file.path(plot_output, "pairwise_89_476_signatures.pdf"),
    cosine_cutoff = 0.9) {
  # Hard-coded paths to signature files
  sig_path_89 <- "Manuscript_data/Liu_et_al_final_89_type_signatures.tsv"
  sig_path_476 <- "Manuscript_data/Liu_et_al_final_476_type_signatures.tsv"

  # Read signatures
  sigs_89 <- read.delim(sig_path_89, sep = "\t", row.names = 1, check.names = FALSE)
  sigs_476 <- read.delim(sig_path_476, sep = "\t", row.names = 1, check.names = FALSE)

  # Verify signature names match
  if (!identical(colnames(sigs_89), colnames(sigs_476))) {
    in_89_not_476 <- setdiff(colnames(sigs_89), colnames(sigs_476))
    in_476_not_89 <- setdiff(colnames(sigs_476), colnames(sigs_89))
    msg <- "Signature names do not match between 89-type and 476-type files."
    if (length(in_89_not_476) > 0) {
      msg <- paste0(msg, " In 89-type only: ", paste(in_89_not_476, collapse = ", "), ".")
    }
    if (length(in_476_not_89) > 0) {
      msg <- paste0(msg, " In 476-type only: ", paste(in_476_not_89, collapse = ", "), ".")
    }
    warning(msg)
  }

  sig_names <- colnames(sigs_89)
  n_sigs <- length(sig_names)

  # Compute pairwise cosine similarities for 89-type (use for finding pairs)
  cos_mat <- matrix(NA, nrow = n_sigs, ncol = n_sigs)
  rownames(cos_mat) <- sig_names
  colnames(cos_mat) <- sig_names

  for (i in seq_len(n_sigs)) {
    for (j in seq_len(n_sigs)) {
      if (i != j) {
        cos_mat[i, j] <- cosine_sim_vec(
          as.numeric(sigs_89[, i]),
          as.numeric(sigs_89[, j])
        )
      }
    }
  }

  # Create lower triangle for finding pairs
  cos_mat_lower <- cos_mat
  cos_mat_lower[upper.tri(cos_mat_lower, diag = TRUE)] <- NA

  # Fill diagonal for complete matrix
  cos_mat_full <- cos_mat
  diag(cos_mat_full) <- 1
  for (i in 2:n_sigs) {
    for (j in 1:(i - 1)) {
      cos_mat_full[i, j] <- cos_mat_full[j, i]
    }
  }

  # Save to PDF in landscape mode
  cairo_pdf(
    out_pdf,
    width = 11,
    height = 8.5,
    onefile = TRUE
  )

  # Page 1: Heatmap for 89-type signatures
  p_heat_89 <- create_heatmap(
    cos_mat_full,
    sig_names,
    "Pairwise Cosine Similarities (89-type)",
    cosine_cutoff
  )
  print(p_heat_89)

  # Compute cosine similarity matrix for 476-type signatures
  sig_names_476 <- colnames(sigs_476)
  n_sigs_476 <- length(sig_names_476)
  cos_mat_476 <- matrix(NA, nrow = n_sigs_476, ncol = n_sigs_476)
  rownames(cos_mat_476) <- sig_names_476
  colnames(cos_mat_476) <- sig_names_476

  for (i in seq_len(n_sigs_476)) {
    for (j in seq_len(n_sigs_476)) {
      if (i != j) {
        cos_mat_476[i, j] <- cosine_sim_vec(
          as.numeric(sigs_476[, i]),
          as.numeric(sigs_476[, j])
        )
      }
    }
  }
  diag(cos_mat_476) <- 1
  for (i in 2:n_sigs_476) {
    for (j in 1:(i - 1)) {
      cos_mat_476[i, j] <- cos_mat_476[j, i]
    }
  }

  # Page 2: Heatmap for 476-type signatures
  p_heat_476 <- create_heatmap(
    cos_mat_476,
    sig_names_476,
    "Pairwise Cosine Similarities (476-type)",
    cosine_cutoff
  )
  print(p_heat_476)

  # Page 3: Dendrogram for 89-type
  result_89 <- plot_dendrogram(
    sigs_89,
    "Hierarchical Clustering of 89-type Signatures",
    cosine_cutoff
  )

  # Page 4: Dendrogram for 476-type
  result_476 <- plot_dendrogram(
    sigs_476,
    "Hierarchical Clustering of 476-type Signatures",
    cosine_cutoff
  )

  # Additional pages: plot pairs with cosine similarity > 0.89
  high_sim_threshold <- 0.89
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

      # Get 89-type signature vectors
      sig1_89 <- as.numeric(sigs_89[, sig1_name])
      sig2_89 <- as.numeric(sigs_89[, sig2_name])

      # Create plots using mSigPlot
      p1_89 <- mSigPlot::plot_89(
        catalog = sig1_89,
        plot_title = sprintf("%s (89-type)", sig1_name)
      )
      p2_89 <- mSigPlot::plot_89(
        catalog = sig2_89,
        plot_title = sprintf("%s (89-type, cosine = %.3f)", sig2_name, cos_val)
      )

      # InsDel_L is not in 476-type file, so skip 476-type plots if either sig is InsDel_L
      if (sig1_name == "InsDel_L" || sig2_name == "InsDel_L") {
        # Only plot 89-type signatures
        grid.arrange(p1_89, p2_89, nrow = 2)
      } else {
        # Get 476-type signature vectors
        sig1_476 <- as.numeric(sigs_476[, sig1_name])
        sig2_476 <- as.numeric(sigs_476[, sig2_name])

        p1_476 <- mSigPlot::plot_476(
          catalog = sig1_476,
          plot_title = sprintf("%s (476-type)", sig1_name)
        )
        p2_476 <- mSigPlot::plot_476(
          catalog = sig2_476,
          plot_title = sprintf("%s (476-type)", sig2_name)
        )

        # Arrange all 4 plots on one page (2x2 grid)
        grid.arrange(p1_89, p2_89, p1_476, p2_476, nrow = 2, ncol = 2)
      }
    }
  }

  dev.off()

  message("Saved PDF to ", out_pdf)

  invisible(list(
    cos_mat_89 = result_89$cos_mat,
    cos_mat_476 = result_476$cos_mat
  ))
}

# Run the analysis
all_pairwise_89_476()
