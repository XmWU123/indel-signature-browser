#!/usr/bin/env Rscript

# Rename PCAWG VCF files from aliquot IDs to SP IDs.
#
# Finds files matching:
#   <prefix>.consensus.indel.vcf
#   <prefix>.consensus.indel.vcf.tbi
#   <prefix>.annotated.indel.vcf.gz
# where <prefix> is a string of digits, dashes, and lowercase a-f
# (i.e. a UUID-like aliquot ID).
#
# Replaces <prefix> with the corresponding SP_ID from
# PCAWG7::map_aliquot_ID_to_SP_ID.

dir <- normalizePath("~/MEGA/important_mut_sig_data/pcawg_indel_vcfs")

# Only works for the annotated.indel.vcf

pattern <- "^([0-9a-f-]+)\\.(consensus\\.indel\\.vcf(\\.tbi)?|annotated\\.indel\\.vcf\\.gz)$"
pattern <- "^([0-9a-f-]+)\\.(consensus\\.indel\\.vcf(\\.tbi)?|annotated\\.indel\\.vcf\\.gz)$"

files <- list.files(dir, pattern = pattern)

if (length(files) == 0) {
  message("No matching files found in ", dir)
  quit(status = 0)
}

matches <- regmatches(files, regexec(pattern, files))
prefixes <- vapply(matches, `[`, character(1), 2)

unique_prefixes <- unique(prefixes)
sp_ids <- PCAWG7::map_aliquot_ID_to_SP_ID(unique_prefixes)
names(sp_ids) <- unique_prefixes
stopifnot(!anyDuplicated(sp_ids[!is.na(sp_ids)]))

done <- 0
for (i in seq_along(files)) {
  if (FALSE && done > 100) {
    return()
  }

  prefix <- prefixes[i]
  sp_id <- sp_ids[[prefix]]
  if (is.na(sp_id)) {
    warning("Could not map '", prefix, "' to an SP ID; skipping ", files[i])
    next
  }
  old_path <- file.path(dir, files[i])
  new_name <- sub(paste0("^", prefix), sp_id, files[i], fixed = FALSE)
  new_path <- file.path(dir, new_name)
  if (file.exists(new_path)) {
    stop("Destination already exists: ", new_path)
  }
  message(files[i], " -> ", new_name)
  file.rename(old_path, new_path)
  done <- done + 1
}
