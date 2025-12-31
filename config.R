# ==============================================================================
# Configuration Constants
# Centralized configuration for the Indel Signature Browser
# ==============================================================================

# Environment detection
APP_ENV <- Sys.getenv("SHINY_ENV", "development")

# Main configuration list
CONFIG <- list(
  # Data files
  data_file = "data/table_1_2025_12_22.xlsx",
  # signatures_csv = "data/mSigHdp.indel476.final.signatures.csv",
  signatures_csv = "data/type476_liu_et_al_sigs.tsv", # temporary
  type_476_signatures = "data/type476_liu_et_al_sigs.tsv",
  images_dir = "www",

  # Tab names (must match tabPanel names in ui_components/)
  tabs = list(
    HOME = "Home",
    KOH89 = "89-type classification",
    COSMIC83 = "83-type classification",
    SEARCH = "Search",
    ABOUT = "About"
  ),

  # Display labels

  labels = list(
    TYPE_89 = "89-Type",
    TYPE_83 = "83-Type",
    TYPE_476 = "476-Type",
    SIGNATURE = "Signature",
    AETIOLOGY = "Proposed Aetiology"
  ),

  # Colors (matching custom.css)
  colors = list(
    primary = "#2c3e50",
    primary_light = "#34495e",
    success = "#27ae60",
    success_light = "#2ecc71",
    warning = "#ffc107",
    info = "#3498db",
    muted = "#7f8c8d",
    light_bg = "#f8f9fa",
    border = "#e9ecef"
  ),

  # Image file patterns
  image_patterns = list(
    spectrum_89 = "_signature.89spectrum.png",
    sample_a = "_89spectrumA.png",
    sample_b = "_89spectrumB.png",
    sample_c = "_89spectrumC.png",
    id83_all = "_83all.png",
    id83_filtered = "_83filtered.png",
    id476 = "_476all",
    thumbnail_89 = "89Thumbnail.png",
    thumbnail_pattern = "_Thumbnail\\.png$"
  ),

  # UI settings
  ui = list(
    grid_columns = 4,
    card_height = "280px",
    image_max_width = "700px",
    thumbnail_max_width = "200px"
  ),

  # Search settings
  search = list(
    type_labels = list(
      koh89 = "[Type: 89-Type]",
      cosmic83 = "[Type: 83-Type]"
    )
  ),

  # Debug settings (environment-dependent)
  debug = APP_ENV == "development",

  # Logging settings
  logging = list(
    enabled = TRUE,
    level = if (APP_ENV == "development") "DEBUG" else "INFO",
    file = "logs/app.log"
  )
)

# Helper function to get nested config values
get_config <- function(...) {
  keys <- list(...)
  result <- CONFIG
  for (key in keys) {
    result <- result[[key]]
    if (is.null(result)) return(NULL)
  }
  result
}
