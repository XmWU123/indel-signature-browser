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
               # 🌟 新增：常驻顶部的返回按钮
               div(style = "margin-bottom: 15px;",
                   actionButton("btn_back_to_overview", "← Back to Overview Table", 
                                class = "btn-primary", 
                                style = "font-size: 16px; font-weight: bold; padding: 8px 16px; border-radius: 6px;")
               ),
               
               # 下面是你之前写好的动态 iframe
               uiOutput("dynamic_repertoire_iframe")
        )
      )
    )
  )
}