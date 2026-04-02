#!/usr/bin/env Rscript

# Directories to search
dirs <- c(
  "/home/steve/MEGA/important_mut_sig_data/fmh-unfiltered_vcfs",
  "/home/steve/MEGA/important_mut_sig_data/pcawg_indel_vcfs"
)

# Find all annotated files
all_files <- unlist(lapply(dirs, function(d) {
  list.files(d, pattern = "\\.annotated\\.indel\\.vcf\\.gz$", full.names = TRUE)
}))

message("Found ", length(all_files), " annotated files")

# Initialize empty data frame for accumulating results
result <- data.frame(
  COSMIC_83 = character(),
  Koh_476 = character(),
  stringsAsFactors = FALSE
)

# Process each file
for (f in all_files) {
  message("Processing: ", basename(f))

  # Read file
  df <- read.delim(gzfile(f), stringsAsFactors = FALSE)

  # Extract columns and get distinct pairs
  pairs <- unique(df[, c("COSMIC_83", "Koh_476")])

  # Accumulate and keep distinct
  result <- unique(rbind(result, pairs))
}

message("Total distinct pairs: ", nrow(result))

# Write result to file
output_path <- "/home/steve/github/Code_Liu_2025/Manuscript_data/unique_pairs.tsv"
write.table(result, output_path, sep = "\t", row.names = FALSE, quote = FALSE)
message("Written to: ", output_path)
