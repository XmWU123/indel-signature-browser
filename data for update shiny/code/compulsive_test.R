source("code/read_annotated_vcf.R")

vcf <- read_annotated_vcf("X::DRUP01050028T")

# Way #1: Use clip_le_9 = TRUE (ICAMS handles the clipping internally)
cat1 <- ICAMS::annot_vcf_to_476_catalog(
  vcf,
  sample_id = "clip_le_9_TRUE",
  clip_le_9 = TRUE
)

# Way #2: Pre-filter VCF to R <= 9, then use clip_le_9 = FALSE
vcf_filtered <- vcf[R <= 9, ]
cat2 <- ICAMS::annot_vcf_to_476_catalog(
  vcf_filtered,
  sample_id = "prefiltered_R_le_9",
  clip_le_9 = FALSE,
  FILTER_PASS = TRUE
)

# Extract as numeric vectors
v1 <- cat1[, 1]
v2 <- cat2[, 1]

# Cosine similarity
cos_sim <- lsa::cosine(v1, v2)
cat("Cosine similarity:", cos_sim, "\n")

# Sum of absolute differences
abs_diffs <- abs(v1 - v2)
cat("Sum of absolute differences:", sum(abs_diffs), "\n")

# Highlight elements with differences
diff_idx <- which(abs_diffs > 0)
if (length(diff_idx) > 0) {
  cat("\nElements with differences:\n")
  diff_df <- data.frame(
    mutation_type = rownames(cat1)[diff_idx],
    clip_le_9_TRUE = v1[diff_idx],
    prefiltered_R_le_9 = v2[diff_idx],
    difference = abs_diffs[diff_idx]
  )
  print(diff_df)
} else {
  cat("\nNo differences found — the two catalogs are identical.\n")
}

# Compare to the published spectra file
cat("\n--- Comparison to 476_clipped_spectra.tsv ---\n")
spectra <- read.delim(
  "Manuscript_data/476_clipped_spectra.tsv",
  check.names = FALSE
)
v_pub <- spectra[, "DRUP01050028T"]

cos_pub <- lsa::cosine(v1, v_pub)
cat("Cosine similarity vs published:", cos_pub, "\n")

abs_diffs_pub <- abs(v1 - v_pub)
cat("Sum of absolute differences vs published:", sum(abs_diffs_pub), "\n")

diff_idx_pub <- which(abs_diffs_pub > 0)
if (length(diff_idx_pub) > 0) {
  cat("\nElements with differences vs published:\n")
  diff_df_pub <- data.frame(
    mutation_type = rownames(cat1)[diff_idx_pub],
    computed = v1[diff_idx_pub],
    published = v_pub[diff_idx_pub],
    difference = abs_diffs_pub[diff_idx_pub]
  )
  print(diff_df_pub)
} else {
  cat("\nNo differences found — computed catalog matches published.\n")
}
