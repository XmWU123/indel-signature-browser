#' Filter an annotated indel VCF to a single 476-type mutation type.
#'
#' @param sample_id A tumor identifier string accepted by
#'   \code{\link{read_annotated_vcf}}.
#'
#' @param mutation_type A character string for a mutation type in the
#'   476 indel classification (must be an element of
#'   \code{ICAMS::catalog.row.order$ID476}).
#'
#' @return A \code{data.table} containing only the VCF rows whose
#'   \code{Koh_476} column matches \code{mutation_type}.
filter_vcf_by_mutation_type <- function(sample_id, mutation_type) {
  valid_types <- ICAMS::catalog.row.order$ID476
  if (!mutation_type %in% valid_types) {
    stop(
      "mutation_type \"", mutation_type,
      "\" is not a valid ID476 classification type"
    )
  }

  vcf <- read_annotated_vcf(sample_id)
  vcf[Koh_476 == mutation_type]
}
