# ==============================================================================
# Data Loading and Preprocessing
# This module handles all data loading and preprocessing logic
# Note: Packages loaded via R/dependencies.R from server.R
# ==============================================================================

# Validate data files before loading
tryCatch(
  {
    validate_data_files(CONFIG)
  },
  error = function(e) {
    log_error("Data file validation failed: %s", e$message)
    stop(e)
  }
)

# Read Excel file
log_info("Loading data from %s", CONFIG$data_file)
raw_data <- read_excel(CONFIG$data_file, sheet = 1, skip = 1)
log_data_load(CONFIG$data_file, TRUE, nrow(raw_data))

# Clean data
id89_df <- raw_data %>%
  dplyr::select(
    InDel83 = `83-type signature ID`,
    InDel89 = `89-type signature ID (this study)`,
    Aetiology = `Proposed Etiology`
  ) %>%
  fill(InDel83, .direction = "down") %>%
  dplyr::filter(!is.na(InDel89)) %>%
  # Filter out footnote rows - only keep valid signature IDs starting with "InsDel" or "InDel"
  dplyr::filter(grepl("^Ins?Del", InDel89)) %>%
  mutate(
    InDel83 = as.character(InDel83),
    InDel89 = as.character(InDel89),
    Aetiology = as.character(Aetiology)
  )

log_info("Processed %d signature records", nrow(id89_df))

# Read 476 list
log_info("Loading 476 signatures from %s", CONFIG$signatures_csv)
id476_df <- read.csv(
  CONFIG$signatures_csv,
  header = TRUE,
  check.names = FALSE,
  stringsAsFactors = FALSE,
  row.names = NULL
)
ID476_list <- colnames(id476_df)
log_data_load(CONFIG$signatures_csv, TRUE, nrow(id476_df))

# Get all image files
all_pngs <- list.files(
  CONFIG$images_dir,
  pattern = "\\.png$",
  full.names = FALSE
)
log_info("Found %d PNG images in %s", length(all_pngs), CONFIG$images_dir)

# --- Build signature_groups (keyed by ID89) ---
signature_groups <- list()

for (i in seq_len(nrow(id89_df))) {
  ID89 <- id89_df$InDel89[i]
  ID83 <- id89_df$InDel83[i]
  if (is.na(ID83)) {
    ID83 <- "Unknown"
  }

  aetiology <- id89_df$Aetiology[i]
  if (is.na(aetiology)) {
    aetiology <- "Unknown"
  }

  imgs <- paste0(
    ID89,
    c(
      CONFIG$image_patterns$spectrum_89,
      CONFIG$image_patterns$sample_a,
      CONFIG$image_patterns$sample_b,
      CONFIG$image_patterns$sample_c
    )
  )
  id83_imgs <- paste0(
    ID89,
    "_",
    ID83,
    c(
      CONFIG$image_patterns$id83_all,
      CONFIG$image_patterns$id83_filtered
    )
  )
  id476_imgs <- grep(
    paste0("^", ID89, CONFIG$image_patterns$id476, ".*\\.png$"),
    all_pngs,
    value = TRUE,
    ignore.case = TRUE
  )

  signature_groups[[ID89]] <- list(
    imgs = imgs,
    id83 = id83_imgs,
    id83_name = ID83,
    id476 = if (length(id476_imgs) > 0) id476_imgs else character(0),
    thumbnail = paste0(ID89, CONFIG$image_patterns$thumbnail_89),
    aetiology = aetiology
  )
}

log_info("Built %d signature groups", length(signature_groups))

# --- Build id83_groups ---
id83_groups <- list()
existing_thumbnails <- list.files(
  CONFIG$images_dir,
  pattern = CONFIG$image_patterns$thumbnail_pattern,
  full.names = FALSE
)

for (i in seq_len(nrow(id89_df))) {
  raw_id83 <- id89_df$InDel83[i]
  raw_id89 <- id89_df$InDel89[i]

  if (is.na(raw_id83) || raw_id83 == "Unknown") {
    next
  }

  id83_key <- trimws(as.character(raw_id83))

  # Initialize
  if (is.null(id83_groups[[id83_key]])) {
    id83_groups[[id83_key]] <- list(
      members = character(),
      id83_all = character(),
      thumbnail = character()
    )
  }

  # Add member
  if (!raw_id89 %in% id83_groups[[id83_key]]$members) {
    id83_groups[[id83_key]]$members <- c(
      id83_groups[[id83_key]]$members,
      raw_id89
    )
  }

  # Set main image path
  expected_83all <- paste0(
    raw_id89,
    "_",
    raw_id83,
    CONFIG$image_patterns$id83_all
  )
  if (length(id83_groups[[id83_key]]$id83_all) == 0) {
    if (file.exists(file.path(CONFIG$images_dir, expected_83all))) {
      id83_groups[[id83_key]]$id83_all <- expected_83all
    }
  }

  # Set thumbnail (flexible matching)
  if (length(id83_groups[[id83_key]]$thumbnail) == 0) {
    possible_names <- c(
      paste0("ID_", id83_key, "_Thumbnail.png"),
      paste0("C_ID", id83_key, "_Thumbnail.png"),
      paste0(id83_key, "_Thumbnail.png"),
      paste0("C_", id83_key, "_Thumbnail.png"),
      paste0(gsub("ID", "ID_", id83_key), "_Thumbnail.png"),
      paste0(gsub("ID_", "ID", id83_key), "_Thumbnail.png")
    )

    match <- intersect(possible_names, existing_thumbnails)

    if (length(match) > 0) {
      id83_groups[[id83_key]]$thumbnail <- match[1]
    } else {
      # Fuzzy fallback
      clean_key <- gsub("[^A-Za-z0-9]", "", id83_key)
      search_pattern <- paste0(".*", clean_key, ".*_Thumbnail\\.png$")
      fuzzy_match <- grep(
        search_pattern,
        existing_thumbnails,
        value = TRUE,
        ignore.case = TRUE
      )
      if (length(fuzzy_match) > 0) {
        id83_groups[[id83_key]]$thumbnail <- fuzzy_match[1]
      }
    }
  }
}

log_info("Built %d ID83 groups", length(id83_groups))

# Run validation on loaded data
validation_results <- validate_all(
  config = CONFIG,
  signature_groups = signature_groups,
  id83_groups = id83_groups
)
log_validation_results(validation_results)
