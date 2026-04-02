# Generate per-signature assignment hamburger plots to PDF
# Output: per_signature_assignments.pdf
#
# This script generates a "hamburger plot" for each signature showing the
# distribution of signature mutations across cancer types. Each dot represents
# a sample; the red dashed line shows the median for each cancer type.
#
# Usage: Rscript plot_signature_assignments.R [--min-fraction 0.0]

library(ggplot2)
library(scales)
library(argparser)

p <- arg_parser("Generate per-signature assignment hamburger plots to PDF")
p <- add_argument(p, "--min-fraction", type = "double", default = 0.0,
  help = "Only plot a sample's point for a signature if that signature accounts for >= this fraction of the sample's total mutations")
args <- parse_args(p)

# Configuration
data_dir <- "Manuscript_data/"
plot_dir <- "plot_output/assignment"

# Load sample info for MSI status coloring
sample_info <- read.delim(
  file.path(data_dir, "sample_info.tsv"),
  check.names = FALSE
)

in_out_pairs = list(
  list(
    input_file = "assignment_from_172_type/Liu_et_al_83_plus_89_as_83_type_signature_assignments.tsv",
    output_file = "83_plus_89_as_83_assignments.pdf"
  ),
  list(
    input_file = "assignment_from_172_type/Liu_et_al_83_plus_89_as_89_type_signature_assignments.tsv",
    output_file = "83_plus_89_as_89_assignments.pdf"
  ),
  list(
    input_file = "Liu_et_al_648_type_signature_assignments.tsv",
    output_file = "648_assignments.pdf"
  ),

  list(
    input_file = "Liu_et_al_476_type_signature_assignments.tsv",
    output_file = "476_assignments.pdf"
  )
)


# Plotting parameters
params <- list(
  width = 12,
  height = 6,
  base_size = 12,
  point_size = 1.2,
  point_alpha = 0.6,
  min_samples = 3,
  genome_size_mb = NULL
)

#' Create hamburger/snake plot showing signature mutations across cancer types
#'
#' Creates a plot similar to maftools::tcgaCompare showing mutation burden
#' for a specific signature across different cancer types. Each dot represents
#' a sample, with a red line showing the median for each cancer type.
#'
#' @param signature_values Named numeric vector of mutation counts per sample.
#'   Names should be in format "CancerType::SampleID".
#' @param signature_name Character: name of the signature for the plot title.
#' @param genome_size_mb Numeric: genome size in megabases for normalization.
#'   Use 3000 for WGS, 35.8 for WXS/exome. If NULL, plots raw counts.
#' @param min_samples Integer: minimum number of samples required to include
#'   a cancer type in the plot (applied after zero filtering if exclude_zero=TRUE).
#' @param log_scale Logical: whether to use log10 scale for y-axis.
#' @param exclude_zero Logical: whether to exclude samples with zero mutations
#'   from the plot (but they are still counted in sample_counts).
#' @param order_by Character: how to order cancer types. One of "median"
#'   (ascending median), "name" (alphabetical), or "count" (sample count).
#' @param bg_colors Character vector of length 2 for alternating background.
#' @param point_color Character: default color for sample points (used when
#'   sample_info is NULL or sample not found).
#' @param median_color Character: color for median lines.
#' @param point_size Numeric: size of sample points.
#' @param point_alpha Numeric: transparency of points (0-1).
#' @param base_size Numeric: base font size for the plot.
#' @param sample_info Data frame with sample info including MSI status. Must have
#'   columns 'Patient' and 'MSIseq_MSI-H'. If provided, MSI-H samples are colored red.
#' @return A list with components:
#'   \item{plot}{A ggplot2 object (or NULL if no data after filtering)}
#'   \item{sample_counts}{Data frame with columns: cancer_type, total_samples,
#'     nonzero_samples (number of samples with non-zero signature mutations)}
plot_signature_by_cancer_type <- function(
  signature_values,
  signature_name = "Signature",
  genome_size_mb = NULL,
  min_samples = 1,
  log_scale = TRUE,
  exclude_zero = TRUE,
  order_by = c("median", "name", "count"),
  bg_colors = c("#EDF8B1", "#2C7FB8"),
  point_color = "black",
  median_color = "red",
  point_size = 1.5,
  point_alpha = 0.7,
  base_size = 14,
  sample_info = NULL
) {
  order_by <- match.arg(order_by)

  # Parse cancer type from sample names (format: CancerType::SampleID)
  sample_names <- names(signature_values)
  cancer_types <- sub("::.*", "", sample_names)

  # Create data frame with all samples (before filtering)
  df_all <- data.frame(
    sample = sample_names,
    cancer_type = cancer_types,
    mutations = as.numeric(signature_values),
    stringsAsFactors = FALSE
  )

  # Add MSI status coloring and shape if sample_info provided
  if (!is.null(sample_info)) {
    # Extract patient ID by removing "CancerType::" prefix
    df_all$patient_id <- sub("^.*::", "", df_all$sample)
    # Look up MSI status
    df_all$msi_status <- sample_info$`MSIseq_MSI-H`[
      match(df_all$patient_id, sample_info$Patient)
    ]
    # Set color: red for MSI-H (TRUE), default point_color for others
    df_all$dot_color <- ifelse(
      !is.na(df_all$msi_status) & df_all$msi_status == TRUE,
      "red",
      point_color
    )
    # Set shape: solid triangle (17) for MSI-H, circle (16) for others
    df_all$dot_shape <- ifelse(
      !is.na(df_all$msi_status) & df_all$msi_status == TRUE,
      17,  # solid triangle
      16   # solid circle
    )
  } else {
    df_all$dot_color <- point_color
    df_all$dot_shape <- 16  # solid circle
  }

  # Calculate sample counts per cancer type (before any filtering)
  total_by_type <- as.data.frame(table(df_all$cancer_type))
  colnames(total_by_type) <- c("cancer_type", "total_samples")

  nonzero_by_type <- as.data.frame(table(df_all$cancer_type[
    df_all$mutations > 0
  ]))
  colnames(nonzero_by_type) <- c("cancer_type", "nonzero_samples")

  sample_counts <- merge(
    total_by_type,
    nonzero_by_type,
    by = "cancer_type",
    all.x = TRUE
  )
  sample_counts$nonzero_samples[is.na(sample_counts$nonzero_samples)] <- 0

  # Create working data frame for plotting

  df <- df_all

  # Normalize to per megabase if genome size provided
  if (!is.null(genome_size_mb)) {
    df$mutations <- df$mutations / genome_size_mb
    y_label <- "Number of Mutations per Megabase"
  } else {
    y_label <- "Number of Mutations"
  }

  # Exclude zeros for plotting (but we already captured counts above)
  if (exclude_zero) {
    df <- df[df$mutations > 0, ]
  }

  # Filter cancer types with insufficient samples
  type_counts <- table(df$cancer_type)
  valid_types <- names(type_counts)[type_counts >= min_samples]
  df <- df[df$cancer_type %in% valid_types, ]

  if (nrow(df) == 0) {
    warning("No data remaining after filtering")
    return(list(plot = NULL, sample_counts = sample_counts))
  }

  # Calculate medians for each cancer type
  medians <- aggregate(mutations ~ cancer_type, data = df, FUN = median)
  colnames(medians) <- c("cancer_type", "median_mutations")

  # Calculate sample counts for plotting (after filtering)
  counts <- as.data.frame(table(df$cancer_type))
  colnames(counts) <- c("cancer_type", "n_samples")

  # Merge medians and counts
  summary_df <- merge(medians, counts, by = "cancer_type")

  # Order cancer types
  if (order_by == "median") {
    summary_df <- summary_df[order(summary_df$median_mutations), ]
  } else if (order_by == "name") {
    summary_df <- summary_df[order(summary_df$cancer_type), ]
  } else if (order_by == "count") {
    summary_df <- summary_df[order(-summary_df$n_samples), ]
  }

  # Create ordered factor for cancer types
  type_order <- summary_df$cancer_type
  df$cancer_type <- factor(df$cancer_type, levels = type_order)
  summary_df$cancer_type <- factor(summary_df$cancer_type, levels = type_order)

  # Create numeric x positions for better control
  df$x_pos <- as.numeric(df$cancer_type)
  summary_df$x_pos <- as.numeric(summary_df$cancer_type)

  # Sort points within each cancer type by mutation value (ascending)
  df <- df[order(df$cancer_type, df$mutations), ]

  # Calculate within-group rank and spread across x-axis
  df$within_rank <- ave(
    seq_len(nrow(df)),
    df$cancer_type,
    FUN = function(x) seq_along(x)
  )
  df$group_size <- ave(
    seq_len(nrow(df)),
    df$cancer_type,
    FUN = length
  )

  # Spread points from x_pos - 0.4 to x_pos + 0.4
  df$x_plot <- df$x_pos +
    0.8 * (df$within_rank - 1) / pmax(df$group_size - 1, 1) -
    0.4
  # Handle single-point groups (center them)
  df$x_plot[df$group_size == 1] <- df$x_pos[df$group_size == 1]

  # Create alternating background rectangles data
  n_types <- length(type_order)
  bg_df <- data.frame(
    xmin = seq(0.5, n_types - 0.5, by = 1),
    xmax = seq(1.5, n_types + 0.5, by = 1),
    fill = rep(bg_colors, length.out = n_types)
  )

  # Create annotation data for sample counts at bottom
  counts_for_plot <- sample_counts[sample_counts$cancer_type %in% type_order, ]
  counts_for_plot$cancer_type <- factor(
    counts_for_plot$cancer_type,
    levels = type_order
  )
  counts_for_plot$x_pos <- as.numeric(counts_for_plot$cancer_type)
  # Get y position for annotations (below minimum value on log scale)
  y_min <- min(df$mutations)
  # Position labels below the plot area (factor of 10 below minimum on log scale)
  y_label_pos <- y_min / 10

  # Build the plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x_plot, y = mutations)) +
    # Alternating background
    ggplot2::geom_rect(
      data = bg_df,
      ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        ymin = -Inf,
        ymax = Inf,
        fill = fill
      ),
      inherit.aes = FALSE,
      alpha = 0.2
    ) +
    ggplot2::scale_fill_identity() +
    # Points (sorted within cancer type, no jitter needed)
    # Color and shape by MSI status: red triangles for MSI-H, black circles for others
    ggplot2::geom_point(
      ggplot2::aes(color = dot_color, shape = dot_shape),
      size = point_size,
      alpha = point_alpha
    ) +
    ggplot2::scale_color_identity() +
    ggplot2::scale_shape_identity() +
    # Median lines
    ggplot2::geom_segment(
      data = summary_df,
      ggplot2::aes(
        x = x_pos - 0.4,
        xend = x_pos + 0.4,
        y = median_mutations,
        yend = median_mutations
      ),
      color = median_color,
      linewidth = 1,
      linetype = "dashed",
      inherit.aes = FALSE
    ) +
    # Sample counts at bottom (nonzero / total)
    ggplot2::geom_text(
      data = counts_for_plot,
      ggplot2::aes(
        x = x_pos,
        y = y_label_pos,
        label = paste0(nonzero_samples, "\n", total_samples)
      ),
      vjust = 2.0,
      size = 3.5,
      lineheight = 0.9,
      inherit.aes = FALSE
    ) +
    # X-axis labels (cancer types) at top
    ggplot2::scale_x_continuous(
      breaks = seq_along(type_order),
      labels = type_order,
      position = "top"
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(
      title = paste0(signature_name, " mutations"),
      x = NULL,
      y = y_label
    ) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 60,
        hjust = 0.0,
        vjust = -0.5
      ),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      legend.position = "none",
      plot.margin = ggplot2::margin(t = 5, r = 5, b = 80, l = 5, unit = "pt")
    )

  # Apply log scale if requested
  if (log_scale) {
    p <- p +
      ggplot2::scale_y_log10(
        labels = scales::label_number(drop0trailing = TRUE)
      )
  }

  return(list(plot = p, sample_counts = sample_counts))
}


#' Generate hamburger plot for a signature from assignment matrix
#'
#' Convenience wrapper that extracts a signature's values from the
#' assignment matrix and creates the hamburger plot.
#'
#' @param signature_name Character: name of the signature (row name in matrix).
#' @param assignment_matrix Data frame with signatures as rows, samples as columns.
#'   Column names should be in format "CancerType::SampleID".
#' @param sample_info Data frame with sample info including MSI status. If provided,
#'   MSI-H samples are colored red.
#' @param min_fraction Numeric: only plot a sample's point if this signature
#'   accounts for >= \code{min_fraction} of the sample's total mutations.
#' @param ... Additional arguments passed to plot_signature_by_cancer_type.
#' @return A list with components:
#'   \item{plot}{A ggplot2 object (or NULL if no data after filtering)}
#'   \item{sample_counts}{Data frame with columns: cancer_type, total_samples,
#'     nonzero_samples (number of samples with non-zero signature mutations)}
plot_signature_hamburger <- function(
  signature_name,
  assignment_matrix,
  sample_info = NULL,
  min_fraction = 0.0,
  ...
) {
  if (!signature_name %in% rownames(assignment_matrix)) {
    stop("Signature '", signature_name, "' not found in assignment matrix")
  }

  sig_values <- as.numeric(assignment_matrix[signature_name, ])
  names(sig_values) <- colnames(assignment_matrix)

  # Zero out samples where this signature's fraction is below min_fraction
  if (min_fraction > 0) {
    sample_totals <- colSums(assignment_matrix)
    fractions <- sig_values / sample_totals
    fractions[is.na(fractions)] <- 0
    sig_values[fractions < min_fraction] <- 0
  }

  plot_signature_by_cancer_type(
    signature_values = sig_values,
    signature_name = signature_name,
    sample_info = sample_info,
    ...
  )
}


#' Read assignment file and generate hamburger plots to PDF
#'
#' Reads an assignment matrix file and generates a PDF with hamburger plots
#' for each signature.
#'
#' @param input_file Character: filename of the assignment matrix (relative to data_dir).
#' @param output_file Character: filename for the output PDF (relative to plot_dir).
#' @param data_dir Character: directory containing the input file.
#' @param plot_dir Character: directory for the output PDF.
#' @param params List of plotting parameters (width, height, base_size,
#'   point_size, point_alpha, min_samples, genome_size_mb).
#' @param sample_info Data frame with sample info including MSI status. If provided,
#'   MSI-H samples are colored red.
#' @param min_fraction Numeric: only plot a sample's point if the signature
#'   accounts for >= this fraction of the sample's total mutations.
read_assignment_and_plot_hamburger <- function(
  input_file,
  output_file,
  data_dir,
  plot_dir,
  params,
  sample_info = NULL,
  min_fraction = 0.0
) {
  input_path <- file.path(data_dir, input_file)
  output_path <- file.path(plot_dir, output_file)

  message("Loading assignment matrix from: ", input_path)
  assignment_matrix <- read.delim(
    input_path,
    row.names = 1,
    check.names = FALSE
  )

  signature_names <- rownames(assignment_matrix)
  message("Found ", length(signature_names), " signatures in assignment matrix")

  message("Generating PDF: ", output_path)
  pdf(output_path, width = params$width, height = params$height)

  for (sig_name in signature_names) {
    message("  Plotting: ", sig_name)
    result <- tryCatch(
      {
        plot_signature_hamburger(
          signature_name = sig_name,
          assignment_matrix = assignment_matrix,
          sample_info = sample_info,
          min_fraction = min_fraction,
          genome_size_mb = params$genome_size_mb,
          min_samples = params$min_samples,
          log_scale = TRUE,
          exclude_zero = TRUE,
          order_by = "median",
          point_size = params$point_size,
          point_alpha = params$point_alpha,
          base_size = params$base_size
        )
      },
      error = function(e) {
        warning("Failed to plot ", sig_name, ": ", e$message)
        list(plot = NULL)
      }
    )

    if (!is.null(result$plot)) {
      # Suppress log scale transformation warnings
      suppressWarnings(print(result$plot))
    }
  }

  dev.off()
  message("Generated: ", output_path)
}


# =============================================================================
# Main execution
# =============================================================================

dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

for (pair in in_out_pairs) {
  read_assignment_and_plot_hamburger(
    input_file = pair$input_file,
    output_file = sub("\\.pdf$", paste0("_mf", args$min_fraction, ".pdf"), pair$output_file),
    data_dir = data_dir,
    plot_dir = plot_dir,
    params = params,
    sample_info = sample_info,
    min_fraction = args$min_fraction
  )
}
