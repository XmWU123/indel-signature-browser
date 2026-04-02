#!/usr/bin/env Rscript

# Batch annotate VCF files using ICAMS::AnnotateIDVCF
#
# Usage: Rscript annotate_vcfs.R <n>
#   n: number of files to process
#
# Finds files matching *.consensus.indel.vcf.gz in current directory,
# skips files where *.annotated.indel.vcf.gz already exists,
# and processes up to n files.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop("Usage: Rscript annotate_vcfs.R <n>\n  n = number of files to process")
}
n <- as.integer(args[1])

library(ICAMS)

# Find all .consensus.indel.vcf.gz files
consensus_files <- list.files(pattern = "\\.consensus\\.indel\\.vcf\\.gz$")

if (length(consensus_files) == 0) {
  message("No .consensus.indel.vcf.gz files found in current directory")
  quit(status = 0)
}

message("Found ", length(consensus_files), " consensus VCF files")

# Filter to only files without existing annotated output
files_to_process <- Filter(function(f) {
  prefix <- sub("\\.consensus\\.indel\\.vcf\\.gz$", "", f)
  annotated_file <- paste0(prefix, ".annotated.indel.vcf.gz")
  !file.exists(annotated_file)
}, consensus_files)

message(length(files_to_process), " files need annotation (no existing output)")

# Limit to n files
files_to_process <- head(files_to_process, n)

if (length(files_to_process) == 0) {
  message("No files to process")
  quit(status = 0)
}

message("Processing ", length(files_to_process), " files")

# Process each file
for (f in files_to_process) {
  prefix <- sub("\\.consensus\\.indel\\.vcf\\.gz$", "", f)
  output_file <- paste0(prefix, ".annotated.indel.vcf.gz")

  message("Processing: ", f)

  # Read VCF (filter.status = NULL accepts all variants)
  vcf <- ICAMS:::ReadVCF(f, filter.status = NULL)

  # Discard rows where REF and ALT have same length (not indels)
  is_indel <- nchar(vcf$REF) != nchar(vcf$ALT)
  vcf <- vcf[is_indel, ]

  # Annotate
  result <- ICAMS::AnnotateIDVCF(ID.vcf = vcf, ref.genome = "hg19")

  # Write annotated VCF
  write.table(result$annotated.vcf, gzfile(output_file),
              sep = "\t", row.names = FALSE, quote = FALSE)

  message("  -> Written: ", output_file)
}

message("Done")
