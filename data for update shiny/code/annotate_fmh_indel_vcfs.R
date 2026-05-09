#!/usr/bin/env Rscript

# Batch annotate VCF files using ICAMS::AnnotateIDVCF
#
# Usage: Rscript annotate_fmh_indel_vcfs.R <n>
#   n: number of files to process
#
# Finds files matching *.purple.somatic.vcf.gz in current directory,
# skips files where *.annotated.indel.vcf.gz already exists,
# and processes up to n files.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop(
    "Usage: Rscript annotate_fmh_indel_vcfs.R <n>\n  n = number of files to process"
  )
}
n <- as.integer(args[1])

#library(mSigSpectra) library(mSigPlot)
library(foreach)
library(doFuture)
library(progressr)

# Find all .purple.somatic.vcf.gz files
input_files <- list.files(pattern = "\\.purple\\.somatic\\.vcf\\.gz$")

if (length(input_files) == 0) {
  message("No .purple.somatic.vcf.gz files found in current directory")
  quit(status = 0)
}

message("Found ", length(input_files), " purple somatic VCF files")

# Filter to only files without existing annotated output
files_to_process <- Filter(
  function(f) {
    prefix <- sub("\\.purple\\.somatic\\.vcf\\.gz$", "", f)
    annotated_file <- paste0(prefix, ".annotated.indel.vcf.gz")
    !file.exists(annotated_file)
  },
  input_files
)

message(length(files_to_process), " files need annotation (no existing output)")

# Limit to n files
files_to_process <- head(files_to_process, n)

if (length(files_to_process) == 0) {
  message("No files to process")
  quit(status = 0)
}

message("Processing ", length(files_to_process), " files")

plan(multisession, workers = 5)

with_progress({
  p <- progressor(along = files_to_process)

  foreach(
    f = files_to_process,
    .options.future = list(packages = c("ICAMS"))
  ) %dofuture%
    {
      prefix <- sub("\\.purple\\.somatic\\.vcf\\.gz$", "", f)
      output_file <- paste0(prefix, ".annotated.indel.vcf.gz")

      # Read VCF (filter.status = NULL accepts all variants)
      vcf <- ICAMS:::ReadVCF(f, filter.status = NULL)

      # Discard rows where REF and ALT have same length (not indels)
      is_indel <- nchar(vcf$REF) != nchar(vcf$ALT)
      vcf <- vcf[is_indel, ]

      # Annotate
      result <- ICAMS::AnnotateIDVCF(ID.vcf = vcf, ref.genome = "hg19")

      # Write annotated VCF
      write.table(
        result$annotated.vcf,
        gzfile(output_file),
        sep = "\t",
        row.names = FALSE,
        quote = FALSE
      )

      p(message = sprintf("Written: %s", output_file))
    }
})

message("Done")
