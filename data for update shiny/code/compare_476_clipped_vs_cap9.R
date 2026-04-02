#!/usr/bin/env Rscript
# Compare the first 100 columns of 476_clipped_spectra.tsv to the
# corresponding columns in the CAP9 Hartwig and PCAWG Koh476 catalogs.
# Prints a cosine similarity table and generates paired plots as PDF.

library(lsa)
library(mSigPlot)
library(ggplot2)

# --- Read data ---
clipped <- read.delim("../Manuscript_data/476_clipped_spectra.tsv",
                       row.names = 1, check.names = FALSE)
hartwig <- read.delim(
  "../Manuscript_data/Mo_CAP9_analysis/Catalogs/CAP9.Hartwig.Koh476.catalog.txt",
  row.names = 1, check.names = FALSE)
pcawg <- read.delim(
  "../Manuscript_data/Mo_CAP9_analysis/Catalogs/CAP9.PCAWG.Koh476.catalog.txt",
  row.names = 1, check.names = FALSE)

# Validate row order against ICAMS canonical order
icams_order <- ICAMS::catalog.row.order$ID476
stopifnot(
  "clipped spectra rownames do not match ICAMS::catalog.row.order$ID476" =
    identical(rownames(clipped), icams_order),
  "Hartwig catalog rownames do not match ICAMS::catalog.row.order$ID476" =
    identical(rownames(hartwig), icams_order),
  "PCAWG catalog rownames do not match ICAMS::catalog.row.order$ID476" =
    identical(rownames(pcawg), icams_order)
)

# First 100 columns of clipped spectra
clipped_100 <- clipped[, 1:100]

# Find matching sample IDs in the two catalog files
samples <- colnames(clipped_100)
in_hartwig <- samples[samples %in% colnames(hartwig)]
in_pcawg   <- samples[samples %in% colnames(pcawg)]
matched <- c(in_hartwig, in_pcawg)

if (length(matched) == 0) stop("No matching sample IDs found")

cat(sprintf("Matched %d samples (%d Hartwig, %d PCAWG) out of first 100\n",
            length(matched), length(in_hartwig), length(in_pcawg)))

# Build the catalog reference for each matched sample
cap9_parts <- list()
if (length(in_hartwig) > 0) cap9_parts <- c(cap9_parts, list(hartwig[, in_hartwig, drop = FALSE]))
if (length(in_pcawg) > 0)   cap9_parts <- c(cap9_parts, list(pcawg[, in_pcawg, drop = FALSE]))
cap9_matched <- do.call(cbind, cap9_parts)

# --- Compute cosine similarities ---
cos_sims <- sapply(matched, function(s) {
  lsa::cosine(clipped_100[, s], cap9_matched[, s])[1, 1]
})

results <- data.frame(
  sample    = matched,
  source    = ifelse(matched %in% in_hartwig, "Hartwig", "PCAWG"),
  cos_sim   = round(cos_sims, 6),
  row.names = NULL
)
results <- results[order(results$cos_sim), ]

cat("\n")
print(results, row.names = FALSE)
cat(sprintf("\nMin: %.6f  Median: %.6f  Max: %.6f\n",
            min(results$cos_sim), median(results$cos_sim), max(results$cos_sim)))

# --- Paired PDF plots ---
pdf_file <- "/tmp/compare_476_clipped_vs_cap9.pdf"
pdf(pdf_file, width = 14, height = 10)

for (s in matched) {
  p1 <- mSigPlot::plot_476(clipped_100[, s, drop = FALSE],
                            plot_title = paste0(s, " — clipped spectra"))
  p2 <- mSigPlot::plot_476(cap9_matched[, s, drop = FALSE],
                            plot_title = paste0(s, " — CAP9 catalog (cos=",
                                                round(cos_sims[s], 4), ")"))
  gridExtra::grid.arrange(p1, p2, nrow = 2)
}

dev.off()
cat(sprintf("\nPDF saved to %s\n", pdf_file))
