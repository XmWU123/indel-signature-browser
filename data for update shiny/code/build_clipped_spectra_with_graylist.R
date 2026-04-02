#!/usr/bin/env Rscript
#
# Build clipped (R <= 9) spectra catalogs for all annotated VCF files
# from PCAWG, FMH, and PCAWG graylist datasets, for each of the three
# classification types (83, 89, 476). Processes files sequentially,
# writing each column directly to a pre-allocated HDF5 dataset to
# minimize memory usage.
# Converts the HDF5 datasets to TSVs at the end.

# to do column lookup in the hdf5 file, do something like

#  h5 <- H5File$new("clipped_spectra.h5", mode = "r")
#  sids <- h5[["sample_ids"]][]
#  idx <- match("SP12345", sids)
#  col <- h5[["catalog_89"]][, idx]

# the hdf5 file acts like a list with several elements:
#  "sample_ids" , "catalog_89", "catalog_83", "catalog_476", rownames_83, rownames_89, and rownames_476

library(data.table)
library(ICAMS)
library(stringr)
library(hdf5r)

vcf_files <- sort(c(
  Sys.glob(path.expand(
    "~/MEGA/important_mut_sig_data/pcawg_indel_vcfs/*.annotated.indel.vcf.gz"
  )),
  Sys.glob(path.expand(
    "~/MEGA/important_mut_sig_data/fmh-unfiltered_vcfs/*.annotated.indel.vcf.gz"
  )),
  Sys.glob(path.expand(
    "~/MEGA/important_mut_sig_data/ICGC-Pan-Can-PCAWG7-2016-08-12-rawdata/final_consensus_12aug_passonly/graylist/indel/*.annotated.indel.vcf.gz"
  ))
))

stopifnot(length(vcf_files) > 0)
n_samples <- length(vcf_files)
cat("Found", n_samples, "VCF files\n")

sample_ids <- str_extract(basename(vcf_files), "^[^.]+")

# Check for duplicate sample IDs across the three sources
if (anyDuplicated(sample_ids)) {
  dup <- sample_ids[duplicated(sample_ids)]
  stop("Duplicate sample IDs found: ", paste(dup, collapse = ", "))
}

out_dir <- here::here("Manuscript_data")
h5_path <- file.path(out_dir, "clipped_spectra.h5")

# Process the first non-empty file to get row dimensions and names
cat("Initializing from first non-empty VCF...\n")
first_result <- NULL
first_idx <- NA
for (i in seq_along(vcf_files)) {
  vcf <- fread(vcf_files[i])
  if (nrow(vcf) > 0) {
    first_result <- list(
      "83" = ICAMS::annot_vcf_to_83_catalog(
        vcf,
        sample_id = sample_ids[i],
        clip_le_9 = TRUE
      ),
      "89" = ICAMS::annot_vcf_to_89_catalog(
        vcf,
        sample_id = sample_ids[i],
        clip_le_9 = TRUE
      ),
      "476" = ICAMS::annot_vcf_to_476_catalog(
        vcf,
        sample_id = sample_ids[i],
        clip_le_9 = TRUE
      )
    )
    first_idx <- i
    break
  }
}
stopifnot(!is.null(first_result))
rm(vcf)

# Create HDF5 file with pre-allocated datasets (chunked by column)
h5 <- H5File$new(h5_path, mode = "w")

types <- c("83", "89", "476")
for (type in types) {
  cat_mat <- first_result[[type]]
  n_rows <- nrow(cat_mat)
  ds <- h5$create_dataset(
    paste0("catalog_", type),
    dtype = h5types$H5T_NATIVE_DOUBLE,
    space = H5S$new(dims = c(n_rows, n_samples)),
    chunk_dims = c(n_rows, 1)
  )
  ds[, first_idx] <- as.numeric(cat_mat[, 1])
  h5[[paste0("rownames_", type)]] <- rownames(cat_mat)
}
h5[["sample_ids"]] <- sample_ids
rm(first_result)
cat("HDF5 file created:", h5_path, "\n")

# Process remaining files, writing each column directly to HDF5
for (i in seq_along(vcf_files)) {
  if (i == first_idx) {
    next
  }
  vcf <- fread(vcf_files[i])
  if (nrow(vcf) == 0) {
    message("  Skipping ", sample_ids[i], " (empty VCF)")
    for (type in types) {
      n_rows <- h5[[paste0("catalog_", type)]]$dims[1]
      h5[[paste0("catalog_", type)]][, i] <- rep(0, n_rows)
    }
  } else {
    result <- list(
      "83" = ICAMS::annot_vcf_to_83_catalog(
        vcf,
        sample_id = sample_ids[i],
        clip_le_9 = TRUE
      ),
      "89" = ICAMS::annot_vcf_to_89_catalog(
        vcf,
        sample_id = sample_ids[i],
        clip_le_9 = TRUE
      ),
      "476" = ICAMS::annot_vcf_to_476_catalog(
        vcf,
        sample_id = sample_ids[i],
        clip_le_9 = TRUE
      )
    )
    for (type in types) {
      h5[[paste0("catalog_", type)]][, i] <- as.numeric(result[[type]][, 1])
    }
    rm(result)
  }
  rm(vcf)
  if (i %% 100 == 0) {
    cat("Processed", i, "of", n_samples, "\n")
    h5$flush()
  }
}

h5$flush()
h5$close_all()
cat("All samples written to HDF5\n")

# Convert HDF5 datasets to TSVs
h5 <- H5File$new(h5_path, mode = "r")
sids <- h5[["sample_ids"]][]

for (type in types) {
  mat <- h5[[paste0("catalog_", type)]][,]
  rownames(mat) <- h5[[paste0("rownames_", type)]][]
  colnames(mat) <- sids
  out_path <- file.path(out_dir, paste0(type, "_clipped_spectra.tsv"))
  write.table(mat, out_path, sep = "\t", quote = TRUE)
  cat("Wrote", out_path, "\n")
}

h5$close_all()
cat("Done\n")
