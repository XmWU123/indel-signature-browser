# COSMIC ID83 Browser Tab Component
create_cosmic_tab <- function() {
  tabPanel(
    "83-type classification",
    icon = icon("layer-group"),
    uiOutput("id83_display")
  )
}
