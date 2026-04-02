library(gridExtra)
library(ggplot2)
library(philentropy)
library(mSigPlot)
library(glue)

#' Find exemplar spectra for a given signature
#' @param sig_path Path to .tsv file with signatures
#' @param sig_col Column name of the signature to compare
#' @param spectra_path Path to .tsv file with mutational spectra
#' @param cosine_cutoff Minimum cosine similarity to include in results
#' @param out_pdf Path to output PDF file (default: sig_col_exemplars.pdf)
#' @param num_exemplars Number of top exemplars to plot (default: 10)
#' @param min_mutations Minimum total mutations to include spectrum (default: 0)
#' @return A data frame with cosine similarities for all spectra above cutoff
#' @export
find_many_similar <- function(
  sig_path,
  sig_col,
  spectra_path,
  cosine_cutoff = 0.8,
  out_pdf = NULL,
  num_exemplars = 10,
  min_mutations = 0,
  do_plot = TRUE
) {
  message("find_many_similar, sig_col = ", sig_col)
  # Read signature and spectra files
  sigs <- read.table(sig_path, sep = "\t", header = TRUE, row.names = 1)
  spectra <- read.table(spectra_path, sep = "\t", header = TRUE, row.names = 1)

  # Check that sig_col exists
  if (!sig_col %in% colnames(sigs)) {
    warning(paste("Column", sig_col, "not found in signature file", sig_path))
    return()
  }

  # Check row counts match
  if (nrow(sigs) != nrow(spectra)) {
    stop(paste(
      "Row counts don't match: signatures have",
      nrow(sigs),
      "rows, spectra have",
      nrow(spectra),
      "rows"
    ))
  }

  # Select plotting function based on number of rows
  nrows <- nrow(sigs)
  if (nrows == 83) {
    plotit <- function(vec, title) plot_83(vec, plot_title = title)
  } else if (nrows == 89) {
    plotit <- function(vec, title) plot_89(vec, plot_title = title)
  } else if (nrows == 476) {
    plotit <- function(vec, title) plot_476(vec, plot_title = title)
  } else {
    stop(paste(
      "Unsupported number of rows:",
      nrows,
      "(expected 83, 89, or 476)"
    ))
  }

  # Get signature vector and normalize
  sig_vec <- as.numeric(sigs[, sig_col])
  sig_norm <- sig_vec / sum(sig_vec)

  # Compute cosine similarity to each spectrum
  cosine_results <- data.frame(
    spectrum = character(),
    cosine = numeric(),
    total_mutations = numeric(),
    stringsAsFactors = FALSE
  )

  for (j in seq_len(ncol(spectra))) {
    spec_vec <- as.numeric(spectra[, j])
    total_muts <- sum(spec_vec)

    # Skip spectra with fewer than min_mutations
    if (total_muts < min_mutations) {
      next
    }

    spec_norm <- spec_vec / total_muts

    dist_mat <- rbind(sig_norm, spec_norm)

    cosine_val <- tryCatch(
      {
        suppressMessages(philentropy::distance(
          dist_mat,
          method = "cosine",
          test.na = FALSE
        )[1])
      },
      error = function(e) NA
    )

    if (!is.na(cosine_val)) {
      cosine_results <- rbind(
        cosine_results,
        data.frame(
          spectrum = colnames(spectra)[j],
          cosine = cosine_val,
          total_mutations = total_muts,
          stringsAsFactors = FALSE
        )
      )
    }
  }

  # Sort by cosine similarity (descending)
  cosine_results <- cosine_results[order(-cosine_results$cosine), ]
  message("initial num cosine results: ", nrow(cosine_results))
  # cosine_results is a data.frame with columns spectrum, cosine, total_mutations

  # Filter by cutoff
  above_cutoff <- cosine_results[cosine_results$cosine >= cosine_cutoff, ]
  if (nrow(above_cutoff) == 0) {
    message("No results above ", cosine_cutoff)
    return(list(
      all_results = c(),
      above_cutoff = c(),
      top_exemplars = c(),
      plots = c()
    ))
  }

  # Select top exemplars for plotting
  top_exemplars <- head(above_cutoff, num_exemplars)
  # top_exemplars is a data.frame with columns spectrum, cosine, total_mutations

  # Set output PDF path
  if (is.null(out_pdf)) {
    out_pdf <- paste0(sig_col, "_exemplars.pdf")
  }

  # Create plots
  plots <- list()

  # Plot signature first
  sig_df <- sigs[, sig_col, drop = FALSE]

  message("About to plot signature ", sig_col)
  plots[[1]] <- plotit(sig_df, paste("Signature:", sig_col))

  # Plot top exemplars
  for (i in seq_len(nrow(top_exemplars))) {
    spec_name <- top_exemplars$spectrum[i]
    cosine_val <- top_exemplars$cosine[i]
    total_muts <- top_exemplars$total_mutations[i]

    spec_df <- spectra[, spec_name, drop = FALSE]
    title <- sprintf(
      "%s (cosine: %.4f, n=%d)",
      spec_name,
      cosine_val,
      total_muts
    )

    plots[[length(plots) + 1]] <- plotit(spec_df, title)
  }

  if (do_plot) {
    # Save to PDF
    pdf_height <- 3 * length(plots)
    pdf_width <- 12

    cairo_pdf(out_pdf, width = pdf_width, height = pdf_height)
    combined_plot <- gridExtra::grid.arrange(grobs = plots, ncol = 1)
    dev.off()

    message(paste("Saved plot to", out_pdf))
  }
  message(paste(
    "Found",
    nrow(above_cutoff),
    "spectra above cosine cutoff of",
    cosine_cutoff
  ))

  # Return results
  list(
    all_results = cosine_results,
    above_cutoff = above_cutoff,
    top_exemplars = top_exemplars,
    plots = plots
  )
}

find_samples_similar_to_sig = function(
  sig_id,
  max_num_similar = 30,
  min_mutations = 50,
  cosine_cutoff = 0.9,
  do_plot = TRUE
) {
  out_dir = "plot_output/plots_of_similar_spectra"
  res = list()
  for (type in c("89", "476")) {
    (res0 = find_many_similar(
      sig_path = glue(
        "Manuscript_data/Liu_et_al_final_{type}_type_signatures.tsv"
      ),
      sig_id,
      glue("Manuscript_data/Liu_et_al_{type}_type_spectra.tsv"),
      cosine_cutoff = cosine_cutoff,
      num_exemplars = max_num_similar,
      out_pdf = glue("{out_dir}/spectra_like_{sig_id}_{type}.pdf"),
      min_mutations = min_mutations,
      do_plot = do_plot
    ))
    res = c(res, list(type = res0))
  }
  res
}
