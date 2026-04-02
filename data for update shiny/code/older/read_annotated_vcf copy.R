#' Read an annotated indel VCF file based on a tumor identifier.
#'
#' @param tumor_id A tumor identifier string. Two patterns are supported:
#'   \itemize{
#'     \item SP pattern (e.g. \code{"Liver-HCC::SP116800"}): extracts the
#'           \code{SP\\d+} substring, maps it to an aliquot ID via
#'           \code{PCAWG7::map_SP_ID_to_aliquot_ID()}, and searches for a
#'           matching VCF in the global \code{pcawg_dir}.
#'     \item \code{::} pattern (e.g. \code{"CancerType::SampleID"}): uses the
#'           substring after \code{::} as the identifier and searches for a
#'           matching VCF in the global \code{h_dir}.
#'   }
#'
#' @return A \code{data.table} with the VCF contents, or \code{NULL} if the
#'   identifier matches neither pattern.
read_annotated_vcf <- function(tumor_id) {
  pcawg_dir <- path.expand("~/MEGA/important_mut_sig_data/pcawg_indel_vcfs")
  h_dir <- path.expand("~/MEGA/important_mut_sig_data/fmh-unfiltered_vcfs/")

  sp_match <- regmatches(tumor_id, regexpr("SP\\d+$", tumor_id))

  if (length(sp_match) == 1 && nchar(sp_match) > 0) {
    aliquot_id <- PCAWG7::map_SP_ID_to_aliquot_ID(sp_match)
    pattern <- paste0("*", aliquot_id, "*annotated.indel.vcf.gz")
    hits <- Sys.glob(file.path(pcawg_dir, pattern))
  } else if (grepl("::", tumor_id, fixed = TRUE)) {
    id <- sub("^.*::", "", tumor_id)
    pattern <- paste0("*", id, "*annotated.indel.vcf.gz")
    hits <- Sys.glob(file.path(h_dir, pattern))
  } else {
    warning("No files found for ", tumor_id)
    return(NULL)
  }

  if (length(hits) > 1) {
    warning(
      "Multiple files found for aliquot ID ",
      aliquot_id,
      " (from SP ID ",
      sp_match,
      ") in ",
      pcawg_dir,
      ":\n",
      paste(hits, collapse = "\n")
    )
    return(NULL)
  }

  return(data.table::fread(hits))
}
