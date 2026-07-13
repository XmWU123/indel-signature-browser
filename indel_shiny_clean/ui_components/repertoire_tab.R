# ui_components/repertoire_tab.R
create_repertoire_tab <- function() {
  tabPanel(
    title = "Overview Table", 
    value = "Overview Table", 
    icon  = icon("book-open"),      
    
    fluidPage(
      style = "padding-top: 20px;", 
      fluidRow(
        column(width = 12,
               # 下面是你之前写好的动态 iframe
               uiOutput("dynamic_repertoire_iframe")
        )
      )
    )
  )
}