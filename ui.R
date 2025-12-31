# UI Definition for Indel Signature Browser
# This file orchestrates the main UI structure by sourcing modular components

# Load all dependencies from central location
source("R/dependencies.R")
load_dependencies()

# Source all UI components
source("ui_components/home_tab.R")
source("ui_components/koh_tab.R")
source("ui_components/cosmic_tab.R")
source("ui_components/search_tab.R")
source("ui_components/about_tab.R")

# Main UI
ui <- navbarPage(
  title = "Indel Signature Explorer",
  theme = NULL,
  id = "navbar",

  # Header: CSS and shinyjs
  header = tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    useShinyjs()
  ),

  # Tab panels
  create_home_tab(),
  create_koh_tab(),
  create_cosmic_tab(),
  create_search_tab(),
  create_about_tab()
)
