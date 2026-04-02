# Helper functions for interactive hamburger plots
# These functions prepare data for plotly-based interactive visualization

#' Get hamburger plot data for a signature
#'
#' Extracts and prepares data for creating an interactive hamburger plot
#' showing mutation distribution across cancer types.
#'
#' @param signature_name Character: name of the signature (row name in matrix).
#' @param assignment_matrix Data frame with signatures as rows, samples as columns.
#'   Column names should be in format "CancerType::SampleID".
#' @param min_samples Integer: minimum samples per cancer type to include.
#' @param exclude_zero Logical: whether to exclude zero-mutation samples.
#' @return Data frame with columns: sample_id, cancer_type, mutations, x_pos,
#'   x_plot, median_mutations, and ordering information.
get_hamburger_data <- function(
  signature_name,
  assignment_matrix,
  min_samples = 3,
  exclude_zero = TRUE
) {
  if (!signature_name %in% rownames(assignment_matrix)) {
    stop("Signature '", signature_name, "' not found in assignment matrix")
  }

  sig_values <- as.numeric(assignment_matrix[signature_name, ])
  sample_names <- colnames(assignment_matrix)
  cancer_types <- sub("::.*", "", sample_names)

  # Create data frame with all samples

  df <- data.frame(
    sample_id = sample_names,
    cancer_type = cancer_types,
    mutations = sig_values,
    stringsAsFactors = FALSE
  )

  # Calculate sample counts before filtering
  total_by_type <- as.data.frame(table(df$cancer_type))
  colnames(total_by_type) <- c("cancer_type", "total_samples")

  nonzero_by_type <- as.data.frame(table(df$cancer_type[df$mutations > 0]))
  colnames(nonzero_by_type) <- c("cancer_type", "nonzero_samples")

  sample_counts <- merge(
    total_by_type,
    nonzero_by_type,
    by = "cancer_type",
    all.x = TRUE
  )
  sample_counts$nonzero_samples[is.na(sample_counts$nonzero_samples)] <- 0

  # Filter zeros if requested
  if (exclude_zero) {
    df <- df[df$mutations > 0, ]
  }

  # Filter cancer types with insufficient samples
  type_counts <- table(df$cancer_type)
  valid_types <- names(type_counts)[type_counts >= min_samples]
  df <- df[df$cancer_type %in% valid_types, ]

  if (nrow(df) == 0) {
    return(NULL)
  }

  # Calculate medians for each cancer type
  medians <- aggregate(mutations ~ cancer_type, data = df, FUN = median)
  colnames(medians) <- c("cancer_type", "median_mutations")

  # Order by median
  medians <- medians[order(medians$median_mutations), ]
  type_order <- medians$cancer_type

  # Create ordered factor
  df$cancer_type <- factor(df$cancer_type, levels = type_order)
  df$x_pos <- as.numeric(df$cancer_type)

  # Sort points within each cancer type by mutation value
  df <- df[order(df$cancer_type, df$mutations), ]

  # Calculate within-group positions for spreading points
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

  # Spread points within each cancer type column
  df$x_plot <- df$x_pos +
    0.8 * (df$within_rank - 1) / pmax(df$group_size - 1, 1) -
    0.4
  df$x_plot[df$group_size == 1] <- df$x_pos[df$group_size == 1]

  # Add median info
  df <- merge(df, medians, by = "cancer_type")

  # Add sample counts
  sample_counts <- sample_counts[sample_counts$cancer_type %in% type_order, ]
  sample_counts$cancer_type <- factor(
    sample_counts$cancer_type,
    levels = type_order
  )
  df <- merge(df, sample_counts, by = "cancer_type")

  # Restore order
  df <- df[order(df$x_pos, df$mutations), ]

  attr(df, "type_order") <- type_order
  attr(df, "medians") <- medians

  return(df)
}


#' Create interactive hamburger plot with plotly
#'
#' Creates a plotly figure with click events for sample selection.
#'
#' @param df Data frame from get_hamburger_data().
#' @param signature_name Character: signature name for title.
#' @param bg_colors Character vector of length 2 for alternating backgrounds.
#' @return A plotly object.
create_interactive_hamburger <- function(
  df,
  signature_name,
  bg_colors = c("#EDF8B1", "#2C7FB8")
) {
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }

  type_order <- attr(df, "type_order")
  medians <- attr(df, "medians")
  n_types <- length(type_order)

  # Create alternating background shapes
  bg_shapes <- lapply(seq_len(n_types), function(i) {
    list(
      type = "rect",
      x0 = i - 0.5,
      x1 = i + 0.5,
      y0 = 0,
      y1 = 1,
      xref = "x",
      yref = "paper",
      fillcolor = bg_colors[(i - 1) %% 2 + 1],
      opacity = 0.2,
      line = list(width = 0),
      layer = "below"
    )
  })

  # Create median line shapes
  median_shapes <- lapply(seq_len(nrow(medians)), function(i) {
    x_pos <- which(type_order == medians$cancer_type[i])
    list(
      type = "line",
      x0 = x_pos - 0.4,
      x1 = x_pos + 0.4,
      y0 = medians$median_mutations[i],
      y1 = medians$median_mutations[i],
      xref = "x",
      yref = "y",
      line = list(color = "red", width = 2, dash = "dash"),
      layer = "above"
    )
  })

  # Combine shapes
  all_shapes <- c(bg_shapes, median_shapes)

  # Create hover text
  df$hover_text <- paste0(
    "<b>Sample:</b> ",
    df$sample_id,
    "<br>",
    "<b>Cancer Type:</b> ",
    df$cancer_type,
    "<br>",
    "<b>Mutations:</b> ",
    round(df$mutations, 1)
  )

  # Create the plotly figure
  p <- plotly::plot_ly(
    data = df,
    x = ~x_plot,
    y = ~mutations,
    type = "scatter",
    mode = "markers",
    marker = list(
      size = 6,
      color = "black",
      opacity = 0.6
    ),
    text = ~hover_text,
    hoverinfo = "text",
    customdata = ~sample_id,
    source = "hamburger_plot"
  ) |>
    plotly::layout(
      title = list(
        text = paste0(signature_name, " mutations"),
        x = 0.5
      ),
      xaxis = list(
        title = "",
        tickmode = "array",
        tickvals = seq_len(n_types),
        ticktext = as.character(type_order),
        tickangle = 60,
        side = "top",
        showgrid = FALSE
      ),
      yaxis = list(
        title = "Number of Mutations",
        type = "log",
        showgrid = TRUE
      ),
      shapes = all_shapes,
      hovermode = "closest",
      margin = list(t = 120, b = 50)
    ) |>
    plotly::config(
      displayModeBar = TRUE,
      displaylogo = FALSE
    )

  return(p)
}


#' Get spectrum data for a sample across all ID types
#'
#' Extracts spectrum data for a given sample from 83, 89, and 476 type catalogs.
#'
#' @param sample_id Character: the sample identifier.
#' @param spectra_83 Data frame of ID83 spectra (mutation types as rows).
#' @param spectra_89 Data frame of ID89 spectra.
#' @param spectra_476 Data frame of ID476 spectra.
#' @return List with spectrum data for each ID type.
get_sample_spectra <- function(sample_id, spectra_83, spectra_89, spectra_476) {
  result <- list()

  if (sample_id %in% colnames(spectra_83)) {
    result$id83 <- list(
      mutation_types = rownames(spectra_83),
      counts = as.numeric(spectra_83[, sample_id])
    )
  }

  if (sample_id %in% colnames(spectra_89)) {
    result$id89 <- list(
      mutation_types = rownames(spectra_89),
      counts = as.numeric(spectra_89[, sample_id])
    )
  }

  if (sample_id %in% colnames(spectra_476)) {
    result$id476 <- list(
      mutation_types = rownames(spectra_476),
      counts = as.numeric(spectra_476[, sample_id])
    )
  }

  return(result)
}


#' Define mutation class colors for spectrum plots
#'
#' Returns color mapping consistent with mSigPlot style.
#'
#' @param id_type Character: one of "ID83", "ID89", or "ID476".
#' @return Named list with mutation class colors.
get_mutation_colors <- function(id_type = "ID83") {
  # Colors based on ICAMS/mSigPlot conventions for indel signatures
  # Main categories: DEL (deletions) and INS (insertions)
  # Subcategories by context (C, T, repeat length, etc.)

  colors <- list(
    # 1bp C deletions
    "DEL:C" = "#FDBE6F",
    # 1bp T deletions
    "DEL:T" = "#FC8D59",
    # 2+ bp deletions at repeats
    "DEL:repeats" = "#E34A33",
    # Microhomology deletions
    "DEL:MH" = "#B30000",
    # 1bp C insertions
    "INS:C" = "#78C679",
    # 1bp T insertions
    "INS:T" = "#31A354",
    # 2+ bp insertions at repeats
    "INS:repeats" = "#006837",
    # Complex insertions
    "INS:other" = "#00441B"
  )

  return(colors)
}


#' Classify mutation type into color category
#'
#' @param mut_type Character: mutation type string (e.g., "DEL:C:1:0").
#' @return Character: color category key.
classify_mutation <- function(mut_type) {
  if (grepl("^DEL:C:1:", mut_type)) {
    return("DEL:C")
  }
  if (grepl("^DEL:T:1:", mut_type)) {
    return("DEL:T")
  }
  if (grepl("^DEL:[CT]:[2-9]|^DEL:[CT]:[0-9]{2}", mut_type)) {
    return("DEL:repeats")
  }
  if (grepl("^DEL:.*:M:", mut_type) || grepl("^DEL:MH", mut_type)) {
    return("DEL:MH")
  }
  if (grepl("^INS:C:1:", mut_type)) {
    return("INS:C")
  }
  if (grepl("^INS:T:1:", mut_type)) {
    return("INS:T")
  }
  if (grepl("^INS:[CT]:[2-9]|^INS:[CT]:[0-9]{2}", mut_type)) {
    return("INS:repeats")
  }
  if (grepl("^INS:", mut_type)) {
    return("INS:other")
  }
  if (grepl("^DEL:", mut_type)) {
    return("DEL:repeats")
  }
  return("DEL:C") # fallback
}


#' Convert spectra to JSON for JavaScript consumption
#'
#' Converts all sample spectra to a JSON object for embedding in HTML.
#'
#' @param spectra_83 Data frame of ID83 spectra.
#' @param spectra_89 Data frame of ID89 spectra.
#' @param spectra_476 Data frame of ID476 spectra.
#' @return JSON string with all spectra data.
spectra_to_json <- function(spectra_83, spectra_89, spectra_476) {
  samples <- unique(c(
    colnames(spectra_83),
    colnames(spectra_89),
    colnames(spectra_476)
  ))

  # Get mutation colors for classification
  colors <- get_mutation_colors()

  # Create nested structure
  all_data <- list(
    id83 = list(
      mutation_types = rownames(spectra_83),
      colors = sapply(rownames(spectra_83), function(mt) {
        cat_key <- classify_mutation(mt)
        colors[[cat_key]]
      }),
      samples = as.list(as.data.frame(spectra_83))
    ),
    id89 = list(
      mutation_types = rownames(spectra_89),
      colors = sapply(rownames(spectra_89), function(mt) {
        cat_key <- classify_mutation(mt)
        colors[[cat_key]]
      }),
      samples = as.list(as.data.frame(spectra_89))
    ),
    id476 = list(
      mutation_types = rownames(spectra_476),
      colors = sapply(rownames(spectra_476), function(mt) {
        cat_key <- classify_mutation(mt)
        colors[[cat_key]]
      }),
      samples = as.list(as.data.frame(spectra_476))
    )
  )

  return(jsonlite::toJSON(all_data, auto_unbox = TRUE))
}


#' Plot signature decomposition for a sample
#'
#' Creates a combined plot showing a donut chart of signature contributions,
#' the sample spectrum, a reconstructed spectrum, and the individual signature plots.
#'
#' @param sample_id Character: the sample identifier (column name in assignments).
#' @param assignments Data frame with samples as columns, signatures as rows.
#' @param signatures Data frame with mutation types as rows, signatures as columns.
#' @param spectra Data frame with mutation types as rows, samples as columns.
#' @param plot_fn Function to plot a single signature (e.g., mSigPlot::plot_83).
#' @param title_prefix Character: prefix for the plot title.
#' @return A grid of plots arranged vertically, or NULL if no non-zero assignments.
plot_decomposition <- function(
  sample_id,
  assignments,
  signatures,
  spectra,
  plot_fn,
  title_prefix = ""
) {
  # Get assignments for this sample
  if (!sample_id %in% colnames(assignments)) {
    return(NULL)
  }

  sample_assignments <- assignments[, sample_id]
  names(sample_assignments) <- rownames(assignments)

  # Filter to non-zero and sort by contribution (descending)
  nonzero <- sample_assignments[sample_assignments > 0]
  if (length(nonzero) == 0) {
    return(NULL)
  }
  nonzero <- sort(nonzero, decreasing = TRUE)

  # Create donut plot of signature assignments
  df <- data.frame(
    signature = factor(names(nonzero), levels = rev(names(nonzero))),
    count = as.numeric(nonzero)
  )
  df$fraction <- df$count / sum(df$count)
  df$ymax <- cumsum(df$fraction)
  df$ymin <- c(0, head(df$ymax, -1))
  df$y_mid <- (df$ymax + df$ymin) / 2 # Middle of each slice in y (theta) space

  df$label <- paste0(
    df$signature,
    ": ",
    format(round(df$count), big.mark = ",")
  )

  # Generate colors for the donut segments
  n_sigs <- nrow(df)
  donut_colors <- scales::hue_pal()(n_sigs)
  names(donut_colors) <- levels(df$signature)

  total_mutations <- sum(df$count)

  p_donut <- ggplot2::ggplot(df) +
    # Donut segments
    ggplot2::geom_rect(
      ggplot2::aes(
        ymax = ymax,
        ymin = ymin,
        xmax = 8,
        xmin = 4,
        fill = signature
      )
    ) +
    # Labels with repulsion to avoid overlap
    ggrepel::geom_label_repel(
      ggplot2::aes(x = 8, y = y_mid, label = label, fill = signature),
      size = 3.5,
      color = "black",
      fontface = "bold",
      label.padding = ggplot2::unit(0.25, "lines"),
      # Remove xlim and ylim constraints that force labels into a narrow band:
      # xlim = c(9, 14),
      # ylim = c(-0.1, 1.1),

      # Change direction to "both" to allow radial movement:
      direction = "both", # Allow repulsion in all directions (radially)

      segment.color = "gray40",
      segment.size = 0.5,
      box.padding = 0.5,
      point.padding = 0.3,
      force = 2,
      max.overlaps = Inf,
      min.segment.length = 0
    ) +
    # Set scales with limits and expansion in one call to avoid warnings
    ggplot2::scale_x_continuous(
      limits = c(0, 18),
      expand = ggplot2::expansion(mult = c(0, 0.1))
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::scale_fill_manual(values = donut_colors) +
    ggplot2::labs(
      title = paste0("Signature Decomposition: ", sample_id),
      subtitle = paste0(
        "Total Assigned: ",
        format(round(total_mutations), big.mark = ","),
        " mutations"
      )
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 11, hjust = 0.5),
      legend.position = "none"
    )

  # Create spectrum plot for this sample
  p_spectrum <- NULL
  if (sample_id %in% colnames(spectra)) {
    spectrum_data <- spectra[, sample_id, drop = FALSE]
    total_in_spectrum <- sum(spectrum_data[, 1])
    spectrum_title <- paste0(
      "Observed Spectrum: ",
      sample_id,
      " (",
      format(round(total_in_spectrum), big.mark = ","),
      " total mutations)"
    )
    p_spectrum <- plot_fn(spectrum_data, plot_title = spectrum_title)
  }

  # Create reconstructed spectrum using mSigAct::ReconstructSpectrum
  p_reconstructed <- NULL
  sig_names_ordered <- names(nonzero)
  # Get signatures that exist in the signatures matrix
  valid_sigs <- sig_names_ordered[sig_names_ordered %in% colnames(signatures)]
  if (length(valid_sigs) > 0 && sample_id %in% colnames(spectra)) {
    # Extract the signatures matrix for valid signatures
    sigs_matrix <- as.matrix(signatures[, valid_sigs, drop = FALSE])
    # Extract exposures for valid signatures
    exposures <- nonzero[valid_sigs]

    # Reconstruct the spectrum
    reconstructed <- mSigAct::ReconstructSpectrum(sigs_matrix, exposures, TRUE)
    colnames(reconstructed) <- "Reconstructed"

    # Calculate cosine similarity between original and reconstructed
    original_vec <- as.numeric(spectra[, sample_id])
    reconstructed_vec <- as.numeric(reconstructed[, 1])
    cos_sim <- sum(original_vec * reconstructed_vec) /
      (sqrt(sum(original_vec^2)) * sqrt(sum(reconstructed_vec^2)))

    total_reconstructed <- sum(reconstructed)
    recon_title <- paste0(
      "Reconstructed Spectrum (",
      format(round(total_reconstructed), big.mark = ","),
      " mutations, cosine similarity: ",
      round(cos_sim, 4),
      ")"
    )
    p_reconstructed <- plot_fn(reconstructed, plot_title = recon_title)
  }

  # Create signature plots for each non-zero signature (in descending order)
  sig_plots <- lapply(sig_names_ordered, function(sig_name) {
    if (sig_name %in% colnames(signatures)) {
      count_val <- nonzero[sig_name]
      title_with_count <- paste0(
        sig_name,
        ": ",
        format(round(count_val), big.mark = ","),
        " mutations"
      )
      plot_fn(
        signatures[, sig_name, drop = FALSE],
        plot_title = title_with_count
      )
    } else {
      NULL
    }
  })
  sig_plots <- Filter(Negate(is.null), sig_plots)

  # Combine: donut first, then spectrum, then reconstructed, then signature plots
  all_plots <- list(p_donut)
  heights <- c(4) # Donut height

  if (!is.null(p_spectrum)) {
    all_plots <- c(all_plots, list(p_spectrum))
    heights <- c(heights, 3)
  }

  if (!is.null(p_reconstructed)) {
    all_plots <- c(all_plots, list(p_reconstructed))
    heights <- c(heights, 3)
  }

  if (length(sig_plots) > 0) {
    all_plots <- c(all_plots, sig_plots)
    heights <- c(heights, rep(3, length(sig_plots)))
  }

  do.call(
    gridExtra::grid.arrange,
    c(all_plots, list(ncol = 1, heights = heights))
  )
}
