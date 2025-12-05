# Koh ID89 Browser Tab Component
create_koh_tab <- function() {
  tabPanel(
    "Koh89 Classification",
    icon = icon("dna"),
    # 顶部控制面板
    div(
      class = "control-panel",
      checkboxGroupInput(
        inputId = "show_types",
        label = "Select signature types to display:",
        choices = c("Koh89" = "ID89", "COSMIC83" = "ID83", "Koh476" = "ID476"),
        selected = c("ID89", "ID83", "ID476"),
        inline = TRUE
      )
    ),
    # 主内容
    uiOutput("signature_display")
  )
}
