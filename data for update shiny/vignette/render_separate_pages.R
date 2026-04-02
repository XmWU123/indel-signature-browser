#!/usr/bin/env Rscript
# Render separate HTML pages for each signature
#
# Usage: Rscript render_separate_pages.R [options]
#
# Options:
#   --regenerate   Force regeneration of RDS data files even if they exist
#   --sig N        Render only signature index N (1-based)
#
# This script generates individual self-contained HTML pages for each
# signature. It requires pre-computed data from running vignette.qmd first,
# OR it will compute the data itself if not found.

library(magrittr)
library(data.table)
library(quarto)

# Find quarto executable - check common locations if not in PATH
find_quarto <- function() {
  # First try the quarto package's built-in detection
  qpath <- tryCatch(quarto::quarto_path(), error = function(e) NULL)
  if (!is.null(qpath) && file.exists(qpath)) {
    return(qpath)
  }

  # Try system which
  qpath <- Sys.which("quarto")
  if (nchar(qpath) > 0 && file.exists(qpath)) {
    return(qpath)
  }

  # Check common locations
  common_paths <- c(
    "/usr/share/positron/resources/app/quarto/bin/quarto",
    "/usr/local/bin/quarto",
    "/opt/quarto/bin/quarto",
    file.path(Sys.getenv("HOME"), ".local/bin/quarto"),
    file.path(Sys.getenv("HOME"), "bin/quarto")
  )

  for (p in common_paths) {
    if (file.exists(p)) {
      return(p)
    }
  }

  return(NULL)
}

quarto_bin <- find_quarto()
if (is.null(quarto_bin)) {
  stop(
    "Could not find quarto executable. Please install Quarto or set QUARTO_PATH."
  )
}
message("Using quarto: ", quarto_bin)

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
force_regenerate <- "--regenerate" %in% args
single_sig <- NULL
if ("--sig" %in% args) {
  sig_idx <- which(args == "--sig")
  if (sig_idx < length(args)) {
    single_sig <- as.integer(args[sig_idx + 1])
  }
}

# Configuration
data_dir <- "../Manuscript_data/"
rds_dir <- "figure/standalone_data"
output_dir <- "figure/separate_pages"
plot_dir <- "figure/parallel_plots"

# Create directories
dir.create(rds_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Paths to RDS files
all_sig_data_path <- file.path(rds_dir, "all_sig_data.rds")
all_plot_paths_path <- file.path(rds_dir, "all_plot_paths.rds")

# Check if we need to generate RDS files
need_rds <- force_regenerate ||
  !file.exists(all_sig_data_path) ||
  !file.exists(all_plot_paths_path)

if (need_rds) {
  message("Generating RDS data files...")

  # Source helper functions
  source("ppar.R")
  source("vhelpers.R")

  # Load required libraries for computation
  library(lsa)
  library(mSigPlot)
  library(future)
  library(furrr)

  # Minimum cosine similarity thresholds (same as vignette.qmd)
  COSMIC_min_cosine <- 0.9
  Jin_min_cosine <- 0.9
  koh_min_cosine <- 0.9
  min_ts_to_trigger <- 0.15

  # Load connection file
  connect_89_to_83 <- read_finalized("connection_table", row.names = NULL)

  # Load all data files
  message("Loading data files...")

  type83_spectra <- read_finalized("83_spectra")
  type83_spectra.no.polyT <- type83_spectra
  type83_spectra.no.polyT[c("DEL:T:1:5+", "INS:T:1:5+"), ] <- 0

  type83_sigs <- read_finalized("83_signatures")

  cosmic_sigs <- read.delim(
    file.path(data_dir, "COSMIC_v3.5_ID_GRCh37_signatures.tsv"),
    sep = "\t",
    row.names = 1,
    check.names = FALSE
  )

  jin_sigs <- read.delim(
    file.path(data_dir, "jin_2024_sup_tab_1_signatures.tsv"),
    sep = "\t",
    row.names = 1,
    check.names = FALSE
  )

  type89_sigs <- read_finalized("89_signatures")

  type89_spectra <- read_finalized("89_spectra")

  koh_sigs <- read.delim(
    file.path(data_dir, "Koh_signatures.tsv"),
    sep = "\t",
    row.names = 1,
    check.names = FALSE
  )

  ID89.mSigAct.assignment <- read_finalized("89_assignment")

  type476_sigs <- read_finalized("476_signatures")

  to.plot.all.ID476.catalogs <- read_finalized("476_spectra")

  ID89_mapped_from_476 <- read.delim(
    "89_mapped_from_476.tsv",
    sep = "\t",
    row.names = 1
  )
  ID83_mapped_from_476 <- read.delim(
    "83_mapped_from_476.tsv",
    sep = "\t",
    row.names = 1
  )

  # Compute cosine similarity matrices
  message("Computing cosine similarities...")

  # COSMIC matches
  cosmic_cosine_matrix <- matrix(
    NA,
    nrow = ncol(type83_sigs),
    ncol = ncol(cosmic_sigs),
    dimnames = list(colnames(type83_sigs), colnames(cosmic_sigs))
  )
  for (i in seq_len(ncol(type83_sigs))) {
    for (j in seq_len(ncol(cosmic_sigs))) {
      cosmic_cosine_matrix[i, j] <- lsa::cosine(
        as.numeric(type83_sigs[, i]),
        as.numeric(cosmic_sigs[, j])
      )
    }
  }
  cosmic_matches <- lapply(seq_len(nrow(cosmic_cosine_matrix)), function(i) {
    cosines <- cosmic_cosine_matrix[i, ]
    matches <- which(cosines >= COSMIC_min_cosine)
    if (length(matches) == 0) {
      return(NULL)
    }
    data.frame(cosmic_sig = names(matches), cosine = cosines[matches])
  })
  names(cosmic_matches) <- rownames(cosmic_cosine_matrix)

  # Jin matches
  jin_cosine_matrix <- matrix(
    NA,
    nrow = ncol(type83_sigs),
    ncol = ncol(jin_sigs),
    dimnames = list(colnames(type83_sigs), colnames(jin_sigs))
  )
  for (i in seq_len(ncol(type83_sigs))) {
    for (j in seq_len(ncol(jin_sigs))) {
      jin_cosine_matrix[i, j] <- lsa::cosine(
        as.numeric(type83_sigs[, i]),
        as.numeric(jin_sigs[, j])
      )
    }
  }
  jin_matches <- lapply(seq_len(nrow(jin_cosine_matrix)), function(i) {
    cosines <- jin_cosine_matrix[i, ]
    matches <- which(cosines >= Jin_min_cosine)
    if (length(matches) == 0) {
      return(NULL)
    }
    data.frame(jin_sig = names(matches), cosine = cosines[matches])
  })
  names(jin_matches) <- rownames(jin_cosine_matrix)

  # Koh matches
  koh_cosine_matrix <- matrix(
    NA,
    nrow = ncol(type89_sigs),
    ncol = ncol(koh_sigs),
    dimnames = list(colnames(type89_sigs), colnames(koh_sigs))
  )
  for (i in seq_len(ncol(type89_sigs))) {
    for (j in seq_len(ncol(koh_sigs))) {
      koh_cosine_matrix[i, j] <- lsa::cosine(
        as.numeric(type89_sigs[, i]),
        as.numeric(koh_sigs[, j])
      )
    }
  }
  koh_matches <- lapply(seq_len(nrow(koh_cosine_matrix)), function(i) {
    cosines <- koh_cosine_matrix[i, ]
    matches <- which(cosines >= koh_min_cosine)
    if (length(matches) == 0) {
      return(NULL)
    }
    data.frame(koh_sig = names(matches), cosine = cosines[matches])
  })
  names(koh_matches) <- rownames(koh_cosine_matrix)

  # Compute all signature data
  message("Computing signature data...")
  all_sig_data <- lapply(
    seq_len(nrow(connect_89_to_83)),
    function(i) {
      compute_sig_data(
        type89_sig_id = connect_89_to_83$InDel89[i],
        exemplar_89 = connect_89_to_83$BestMatch89_1[i],
        exemplar_83 = connect_89_to_83$BestMatch83_1[i],
        exemplar_476 = connect_89_to_83$BestMatch476_1[i],
        ID83signature = connect_89_to_83$InDel83[i],
        ID89_signatures = type89_sigs,
        ID89_catalogs = type89_spectra,
        ID83_signatures = type83_sigs,
        ID83_catalogs = type83_spectra,
        ID83_catalogs_no_polyT = type83_spectra.no.polyT,
        ID476_signatures = type476_sigs,
        ID476_catalogs = to.plot.all.ID476.catalogs,
        assignment_matrix = ID89.mSigAct.assignment,
        ID89_mapped_signatures = ID89_mapped_from_476,
        ID83_mapped_signatures = ID83_mapped_from_476,
        cosmic_matches = cosmic_matches,
        jin_matches = jin_matches,
        koh_matches = koh_matches
      )
    }
  )
  names(all_sig_data) <- connect_89_to_83$InDel89

  # Get plot paths (assume plots already exist in cache)
  message("Checking plot cache...")
  cache_valid <- check_plot_cache(data_dir, plot_dir)

  if (cache_valid) {
    message("Using cached plots from: ", plot_dir)
    all_plot_paths <- reconstruct_plot_paths(names(all_sig_data), plot_dir)
  } else {
    message("Plot cache invalid. Generating plots...")
    all_plot_paths <- generate_all_plots_parallel(
      all_sig_data = all_sig_data,
      ID89_signatures = type89_sigs,
      ID89_catalogs = type89_spectra,
      ID83_signatures = type83_sigs,
      ID83_catalogs = type83_spectra,
      ID83_catalogs_no_polyT = type83_spectra.no.polyT,
      ID476_signatures = type476_sigs,
      ID476_catalogs = to.plot.all.ID476.catalogs,
      plot_dir = plot_dir,
      ID89_mapped_signatures = ID89_mapped_from_476,
      ID83_mapped_signatures = ID83_mapped_from_476,
      cosmic_signatures = cosmic_sigs,
      jin_signatures = jin_sigs,
      koh_signatures = koh_sigs,
      min_ts_to_trigger = min_ts_to_trigger,
      n_workers = 10
    )
    save_plot_cache(data_dir, plot_dir)
  }

  # Save RDS files
  message("Saving RDS files...")
  saveRDS(all_sig_data, all_sig_data_path)
  saveRDS(all_plot_paths, all_plot_paths_path)
  message("RDS files saved to: ", rds_dir)
} else {
  message("Loading existing RDS files...")
  all_sig_data <- readRDS(all_sig_data_path)
  all_plot_paths <- readRDS(all_plot_paths_path)
}

# Determine which signatures to render
if (!is.null(single_sig)) {
  sig_indices <- single_sig
  message("Rendering single signature index: ", single_sig)
} else {
  sig_indices <- seq_along(all_sig_data)
  message("Rendering all ", length(sig_indices), " signatures...")
}

# Source helper functions for format_signature_name
source("vhelpers.R")

# Render each signature using quarto CLI directly
for (i in sig_indices) {
  sig_name <- names(all_sig_data)[i]
  safe_name <- gsub("[^a-zA-Z0-9_]", "_", sig_name)
  output_file <- file.path(output_dir, paste0(safe_name, ".html"))

  message(
    sprintf("[%d/%d] Rendering %s...", i, length(sig_indices), sig_name)
  )

  tryCatch(
    {
      # Use quarto CLI directly with -P flag for parameters
      temp_output <- basename(output_file)
      cmd <- sprintf(
        '%s render onesig_standalone.qmd -P sig_index:%d --output %s',
        shQuote(quarto_bin),
        i,
        shQuote(temp_output)
      )

      result <- system(cmd, intern = FALSE)

      if (result != 0) {
        message("  ERROR: quarto render failed with exit code ", result)
        next
      }

      # Move the output file to the correct directory
      if (file.exists(temp_output)) {
        file.rename(temp_output, output_file)
      }

      message("  -> ", output_file)
    },
    error = function(e) {
      message("  ERROR: ", e$message)
    }
  )
}

message("\nDone! HTML pages saved to: ", output_dir)
