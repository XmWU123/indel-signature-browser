#' Plot ID83 catalog with optional suppression of high polyT rows
#'
#' Wraps mSigPlot::plot_83 to optionally generate a second plot with polyT 5+
#' rows set to -3 when mutation counts exceed a threshold.
#'
#' @param catalog An ID83 catalog (matrix or data frame with row names).
#' @param plot_title Title for the plot(s).
#' @param text_size Text size passed to plot_83.
#' @param base_size Base size passed to plot_83.
#' @param min_ts_to_trigger If >= 1.1, treated as an absolute count threshold.
#'   If < 1.1, treated as a proportion of total mutations.
#' @param ablate_both If TRUE, the if either the intertion or
#' deletion over the min_ts_to_trigger then suppress both
#' insertions and deletions.
#'
#' @return If no polyT rows exceed threshold, returns a
#' a list with a single element `plots``.
#'   If one or both polyT rows exceed threshold, returns a list
#' with 2 elements: `plots`, a vector of two plots:
#'   the original and one with offending rows set to a negative value
#' and `ablated_catalog`, which is the original catalog with the
#' offending insertion and/deletion of T in long poly-Ts set to 0
#'
#' @export
plot_83_w_wout_t <- function(
  catalog,
  plot_title = NULL,
  text_size = NULL,
  base_size = NULL,
  min_ts_to_trigger = .1,
  ablate_both = TRUE
) {
  # Row names for polyT 5+ rows
  del_t_row <- "DEL:T:1:5+"
  ins_t_row <- "INS:T:1:5+"

  # Get counts from catalog
  del_t_count <- catalog[del_t_row, 1]
  ins_t_count <- catalog[ins_t_row, 1]

  # Determine if we're using proportions or absolute counts
  use_proportion <- min_ts_to_trigger < 1.1

  if (use_proportion) {
    total_count <- sum(catalog[, 1])
    del_t_val <- del_t_count / total_count
    ins_t_val <- ins_t_count / total_count
  } else {
    del_t_val <- del_t_count
    ins_t_val <- ins_t_count
  }

  # Check which rows are offending
  del_t_offending <- del_t_val >= min_ts_to_trigger
  ins_t_offending <- ins_t_val >= min_ts_to_trigger
  if (ablate_both) {
    del_t_offending <- del_t_offending || ins_t_offending
    ins_t_offending <- del_t_offending || ins_t_offending
  }

  # Build argument list for plot_83, excluding NULL values
  plot_args <- list(catalog = catalog)
  if (!is.null(plot_title)) {
    plot_args$plot_title <- plot_title
  }
  if (!is.null(text_size)) {
    plot_args$text_size <- text_size
  }
  if (!is.null(base_size)) {
    plot_args$base_size <- base_size
  }

  # Create the original plot
  p_with_ts <- do.call(mSigPlot::plot_83, plot_args)

  # If no offending rows, return single plot
  if (!del_t_offending && !ins_t_offending) {
    return(list(plots = p_with_ts))
  }

  # Create modified catalog
  catalog_modified <- catalog
  if (del_t_offending) {
    catalog_modified[del_t_row, 1] <- 0
  }
  if (ins_t_offending) {
    catalog_modified[ins_t_row, 1] <- 0
  }

  # Decide how far down to plot the ablated mutation count
  ablation_amount = -0.2 * max(catalog_modified, 1)
  catalog_modified2 <- catalog
  if (del_t_offending) {
    catalog_modified2[del_t_row, 1] <- ablation_amount
  }
  if (ins_t_offending) {
    catalog_modified2[ins_t_row, 1] <- ablation_amount
  }

  # Build title suffix
  if (del_t_offending && ins_t_offending) {
    pref <- "ins T and del T"
  } else if (ins_t_offending) {
    pref <- "ins T"
  } else {
    pref <- "del T"
  }
  suffix <- paste(
    pref,
    "in long poly-T suppressed (indicated by negative)"
  )

  # Build modified title
  if (is.null(plot_title) || plot_title == "") {
    modified_title <- suffix
  } else {
    modified_title <- paste(plot_title, suffix, sep = ", ")
  }

  # Build argument list for modified plot
  plot_args_modified <- list(
    catalog = catalog_modified2,
    plot_title = modified_title
  )
  if (!is.null(text_size)) {
    plot_args_modified$text_size <- text_size
  }
  if (!is.null(base_size)) {
    plot_args_modified$base_size <- base_size
  }

  # Create the modified plot
  p_wout_ts <- do.call(mSigPlot::plot_83, plot_args_modified)

  # Return both plots and the catalog with
  # the offendening peaks 0
  return(list(
    plots = c(p_with_ts, p_wout_ts),
    ablated_catalog = catalog_modified
  ))
}
