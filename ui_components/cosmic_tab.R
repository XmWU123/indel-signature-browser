# COSMIC ID83 Browser Tab Component
create_cosmic_tab <- function() {
  tabPanel(
    "COSMIC83 Classification",
    icon = icon("layer-group"),
    uiOutput("id83_display")
  )
}
