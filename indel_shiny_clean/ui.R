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
source("ui_components/476_tab.R")
source("ui_components/analysis_tab.R") 
source("ui_components/repertoire_tab.R")

# Main UI
ui <- navbarPage(
  
  title = tags$a(
    href = "#",
    
    onclick = paste0(
      "Shiny.setInputValue(",
      "'logo_home_click',",
      "Date.now(),",
      "{priority:'event'}",
      ");",
      "return false;"
    ),
    
    title = "Back to Home",
    
    style = paste0(
      "text-decoration:none !important;",
      "display:block !important;",
      "cursor:pointer !important;"
    ),
    
    tags$div(
      style = paste0(
        "height:82px !important;",
        "display:flex !important;",
        "align-items:center !important;",
        "justify-content:flex-start !important;",
        "overflow:visible !important;"
      ),
      
      tags$img(
        src = "parallel_plots/indelsig_logo.png",
        
        style = paste0(
          "width:320px !important;",
          "height:auto !important;",
          "max-width:none !important;",
          "max-height:none !important;",
          "display:block !important;",
          "vertical-align:middle !important;",
          "transform:translate(-60px,-2px) !important;"
        )
      )
    )
  ),
  
  windowTitle = "Indel Signature Browser",
  theme = NULL,
  id = "navbar",
  
  header = tagList(
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "custom.css"
      )
    ),
    useShinyjs()
  ),
  
  create_home_tab(),
  create_analysis_tab(), 
  create_koh_tab(),
  create_476_tab(),
  create_cosmic_tab(),
  create_repertoire_tab(),
  create_search_tab(),
  create_about_tab()
)
