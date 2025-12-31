# Koh ID89 Browser Tab Component
create_koh_tab <- function() {
  tabPanel(
    "89-type classification",
    icon = icon("dna"),
    # 主内容
    uiOutput("signature_display")
  )
}
