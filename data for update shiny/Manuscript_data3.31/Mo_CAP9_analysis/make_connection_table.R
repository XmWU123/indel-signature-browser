library(here)
library(lsa)

source(here::here("code", "find_best_match_spectra.R"))
source(here::here("vignette", "vhelpers.R"))

# Read signature files (mutation types as rows, signatures as columns)
sigs_83 <- as.matrix(read_finalized("83_signatures"))
sigs_89 <- as.matrix(read_finalized("89_signatures"))
sigs_476 <- as.matrix(read_finalized("476_signatures"))

# Spectra file paths
spectra_83_path <- finalized_files[["83_spectra"]]
spectra_89_path <- finalized_files[["89_spectra"]]
spectra_476_path <- finalized_files[["476_spectra"]]

# Map an 89/476-type signature name to the corresponding 83-type name.
# Numeric series: InsDel{N}[a-z]? -> C_ID{N}  (e.g. InsDel1a, InsDel4 -> C_ID1, C_ID4)
# Letter series:  InsDel_{X}       -> ID_{X}   (e.g. InsDel_B -> ID_B)
#                 InsDel_{X}_{greek} -> ID_{X}  (e.g. InsDel_A_alpha -> ID_A)
map_89_to_83 <- function(name89) {
  m <- regmatches(name89, regexpr("^InsDel(\\d+)[a-z]?$", name89, perl = TRUE))
  if (length(m) > 0) {
    return(paste0(
      "C_ID",
      sub("^InsDel(\\d+)[a-z]?$", "\\1", name89, perl = TRUE)
    ))
  }
  m <- regmatches(name89, regexpr("^InsDel_([A-Z])$", name89, perl = TRUE))
  if (length(m) > 0) {
    return(paste0("ID_", sub("^InsDel_([A-Z])$", "\\1", name89, perl = TRUE)))
  }
  m <- regmatches(
    name89,
    regexpr("^InsDel_([A-Z])_(alpha|beta|gamma|delta)$", name89, perl = TRUE)
  )
  if (length(m) > 0) {
    return(paste0(
      "ID_",
      sub(
        "^InsDel_([A-Z])_(alpha|beta|gamma|delta)$",
        "\\1",
        name89,
        perl = TRUE
      )
    ))
  }
  stop(paste("Cannot map 89-type name to 83-type:", name89))
}

sig89_names <- colnames(sigs_89)
sig83_names <- sapply(sig89_names, map_89_to_83, USE.NAMES = FALSE)

stopifnot(
  "Some mapped 83-type names are not present in liu_et_al_83_signatures.tsv" = all(
    sig83_names %in% colnames(sigs_83)
  )
)

# For each 89/476-type signature, find the top 3 best-matching samples in
# each of the three spectra files.  The 83-type lookup uses the mapped 83-type
# signature vector; 89 and 476 use their own signature vectors.
n <- 3
min_mutations <- 30

build_row <- function(idx) {
  name89 <- sig89_names[idx]
  name83 <- sig83_names[idx]

  top83 <- find_top_n_match_names(
    sigs_83[, name83, drop = FALSE],
    spectra_83_path,
    n = n,
    min_mutations = min_mutations
  )
  # browser()
  top89 <- find_top_n_match_names(
    sigs_89[, name89, drop = FALSE],
    spectra_89_path,
    n = n,
    min_mutations = min_mutations
  )
  top476 <- find_top_n_match_names(
    sigs_476[, name89, drop = FALSE],
    spectra_476_path,
    n = n,
    min_mutations = min_mutations
  )

  data.frame(
    InDel89 = name89,
    InDel83 = name83,
    BestMatch83_1 = top83$names[1, 1],
    CosSim83_1 = top83$cosines[1, 1],
    BestMatch83_2 = top83$names[1, 2],
    CosSim83_2 = top83$cosines[1, 2],
    BestMatch83_3 = top83$names[1, 3],
    CosSim83_3 = top83$cosines[1, 3],
    BestMatch89_1 = top89$names[1, 1],
    CosSim89_1 = top89$cosines[1, 1],
    BestMatch89_2 = top89$names[1, 2],
    CosSim89_2 = top89$cosines[1, 2],
    BestMatch89_3 = top89$names[1, 3],
    CosSim89_3 = top89$cosines[1, 3],
    BestMatch476_1 = top476$names[1, 1],
    CosSim476_1 = top476$cosines[1, 1],
    BestMatch476_2 = top476$names[1, 2],
    CosSim476_2 = top476$cosines[1, 2],
    BestMatch476_3 = top476$names[1, 3],
    CosSim476_3 = top476$cosines[1, 3],
    stringsAsFactors = FALSE
  )
}

connection <- do.call(rbind, lapply(seq_along(sig89_names), build_row))

out_path <- finalized_files[["connection_table"]]
old <- options(digits = 12)
write.table(
  connection,
  out_path,
  sep = "\t",
  quote = FALSE,
  row.names = FALSE
)
options(old)
message("Wrote connection table to ", out_path)
