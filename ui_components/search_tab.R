# Search Tab Component
create_search_tab <- function() {
  tabPanel(
    "Search",
    icon = icon("search"),
    div(class = "search-page-container",
        div(class = "search-box-large",
            # 标题
            h1(class = "search-title", "Search Signatures"),
            p(class = "search-subtitle",
              "Enter a signature name to quickly find and view its details"),

            # 搜索框
            div(class = "search-input-container",
                tags$input(
                  id = "search_input",
                  type = "text",
                  class = "search-input-large",
                  placeholder = "Type signature name (e.g., InsDel1a, InsDel2b)..."
                ),
                actionButton("search_btn",
                             "Search",
                             icon = icon("search"),
                             class = "search-btn-large")
            ),

            # 搜索示例
            div(class = "search-examples",
                p(class = "search-examples-title", "Try searching:"),
                div(
                  tags$span(class = "example-tag", onclick = "Shiny.setInputValue('example_click', 'InsDel1a', {priority: 'event'})", "InsDel1a"),
                  tags$span(class = "example-tag", onclick = "Shiny.setInputValue('example_click', 'InsDel2b', {priority: 'event'})", "InsDel2b"),
                  tags$span(class = "example-tag", onclick = "Shiny.setInputValue('example_click', 'InsDel3', {priority: 'event'})", "InsDel3"),
                  tags$span(class = "example-tag", onclick = "Shiny.setInputValue('example_click', 'InsDel10', {priority: 'event'})", "InsDel10"),
                  tags$span(class = "example-tag", onclick = "Shiny.setInputValue('example_click', 'InsDel39', {priority: 'event'})", "InsDel39")
                )
            )
        )
    )
  )
}
