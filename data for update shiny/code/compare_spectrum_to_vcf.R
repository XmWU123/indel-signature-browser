source(here::here("code/annot_vcf_to_476_catalog.R"))
source(here::here("code/read_annotated_vcf.R"))
#' Compare a sample's 476-type spectrum from file to one computed from its VCF.
#'
#' Looks up the sample in the 476-type spectra file, reads the annotated
#' VCF via \code{read_annotated_vcf()}, computes a 476-type spectrum from the
#' VCF, and compares the two.
#'
#' @param sample_id A sample identifier (e.g. \code{"Colon::CPCT02050279T"}).
#'   Must be a column name in the spectra file and a valid argument to
#'   \code{read_annotated_vcf()}.
#'
#' @return A list with components:
#'   \item{sample_id}{The input sample ID.}
#'   \item{file_total}{Total mutations in the file spectrum.}
#'   \item{computed_total}{Total mutations in the VCF-computed spectrum.}
#'   \item{abs_diff}{Sum of absolute differences between the two spectra.}
#'   \item{cosine}{Cosine similarity between the two spectra.}
#'   \item{file_spectrum}{The spectrum from the file (numeric vector).}
#'   \item{computed_spectrum}{The spectrum computed from the VCF (numeric vector).}
compare_spectrum_to_vcf <- function(sample_id) {
  spectra <- read.delim(
    here::here("Manuscript_data/Liu_et_al_476_type_spectra.tsv"),
    row.names = 1,
    check.names = FALSE
  )

  if (!sample_id %in% colnames(spectra)) {
    stop("Sample ID \"", sample_id, "\" not found in spectra file")
  }

  file_spectrum <- spectra[, sample_id]

  FILTER_PASS <- !grepl("::SP", sample_id)
  vcf <- read_annotated_vcf(sample_id)
  computed_spectrum <- annot_vcf_to_476_catalog(
    vcf,
    "computed_spectrum",
    FILTER_PASS = FILTER_PASS
  )$computed_spectrum

  file_total <- sum(file_spectrum)
  computed_total <- sum(computed_spectrum)
  abs_diff <- sum(abs(computed_spectrum - file_spectrum))

  cos_sim <- lsa::cosine(computed_spectrum, file_spectrum)[1, 1]

  message(sprintf(
    "%s: file=%d, computed=%d, abs_diff=%.1f, cosine=%.6f",
    sample_id,
    file_total,
    computed_total,
    abs_diff,
    cos_sim
  ))

  list(
    sample_id = sample_id,
    file_total = file_total,
    computed_total = computed_total,
    abs_diff = abs_diff,
    cosine = cos_sim,
    file_spectrum = file_spectrum,
    computed_spectrum = computed_spectrum
  )
}
