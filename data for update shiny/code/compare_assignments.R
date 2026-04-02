# Compare signature assignments between two files
# Creates scatter plots with regression lines for each matched signature

library(ggplot2)
library(MASS) # for robust regression (rlm)

# Load both assignment files
file1 <- "Manuscript_data/recompressed_assignments.tsv"
file2 <- "Manuscript_data/Liu_et_al_83_type_signature_assignments.tsv"

df1 <- read.delim(file1, row.names = 1, check.names = FALSE)
df2 <- read.delim(file2, row.names = 1, check.names = FALSE)

cat("File 1:", nrow(df1), "signatures x", ncol(df1), "samples\n")
cat("File 2:", nrow(df2), "signatures x", ncol(df2), "samples\n")

# Find common signatures
common_sigs <- intersect(rownames(df1), rownames(df2))
only_in_file1 <- setdiff(rownames(df1), rownames(df2))
only_in_file2 <- setdiff(rownames(df2), rownames(df1))

cat("\nCommon signatures:", length(common_sigs), "\n")
if (length(only_in_file1) > 0) {
  cat("Only in file 1:", paste(only_in_file1, collapse = ", "), "\n")
}
if (length(only_in_file2) > 0) {
  cat("Only in file 2:", paste(only_in_file2, collapse = ", "), "\n")
}

# Also match by sample names (columns)
common_samples <- intersect(colnames(df1), colnames(df2))
cat("Common samples:", length(common_samples), "\n")

# Subset to common signatures and samples
df1_common <- df1[common_sigs, common_samples]
df2_common <- df2[common_sigs, common_samples]

# Create PDF with one plot per page
pdf("code/assignment_regression.pdf", width = 7, height = 7)

for (sig in common_sigs) {
  x_vals <- as.numeric(df1_common[sig, ]) + 1
  y_vals <- as.numeric(df2_common[sig, ]) + 1

  # Filter: keep points where at least one value >= 10, and both are > 0 (for log)
  keep <- ((x_vals >= 10) | (y_vals >= 10)) & (x_vals > 0) & (y_vals > 0)
  x_filt <- x_vals[keep]
  y_filt <- y_vals[keep]

  cat(sig, ": kept", sum(keep), "of", length(x_vals), "points\n")

  if (sum(keep) < 3) {
    cat("  Skipping - too few points\n")
    next
  }

  # Robust regression on log-transformed values
  log_x <- log10(x_filt)
  log_y <- log10(y_filt)
  fit <- rlm(log_y ~ log_x)

  # Calculate pseudo R-squared for robust regression
  ss_res <- sum((log_y - predict(fit))^2)
  ss_tot <- sum((log_y - mean(log_y))^2)
  r_squared <- 1 - ss_res / ss_tot

  plot_df <- data.frame(x = x_filt, y = y_filt)

  # Create prediction line for robust regression
  x_range <- range(x_filt)
  pred_df <- data.frame(
    log_x = seq(log10(x_range[1]), log10(x_range[2]), length.out = 100)
  )
  pred_df$log_y <- predict(fit, newdata = pred_df)
  pred_df$x <- 10^pred_df$log_x
  pred_df$y <- 10^pred_df$log_y

  p <- ggplot(plot_df, aes(x = x, y = y)) +
    geom_point(alpha = 0.3, size = 0.5) +
    geom_line(
      data = pred_df,
      aes(x = x, y = y),
      color = "blue",
      linewidth = 1
    ) +
    scale_x_log10() +
    scale_y_log10() +
    coord_fixed(ratio = 1) +
    labs(
      title = sprintf("%s (R² = %.4f, n = %d)", sig, r_squared, sum(keep)),
      x = "Recompressed assignments",
      y = "Original assignments"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      aspect.ratio = 1
    )

  print(p)
}

dev.off()

cat(
  "\nCreated: code/assignment_regression.pdf with",
  length(common_sigs),
  "pages\n"
)
