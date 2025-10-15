library(shiny)
library(shinydashboard)
library(shinyjs)
#  数据定义区
signature_groups <- list(
    Indel1A = list(
        ID83thumb = "QQ20250923-224912.png",
        ID89thumb = "QQ20250923-225024.png",
        ID83full  = "QQ20250923-225134.png",
        ID89full  = "QQ20250923-225329.png",
        desc = "Dominated by polymerase slippage events at homopolymer tracts."
   ),
    Indel1B = list(
        ID83thumb = "QQ20250923-234400.png",
        ID89thumb = "QQ20250923-235449.png",
        ID83full  = "QQ20250924-001321.png",
        ID89full  = "QQ20250924-001951.png",
        desc = "Mainly deletion-driven MMRd-like pattern with frequent C/T losses."
    )
)
#  UI
ui <- dashboardPage(
   dashboardHeader(title = "Indel Signiture Browser"),
   
   dashboardSidebar(
       sidebarMenu(id = "tabs",
                   menuItem("Signiture Browser",tabName = "browser",icon = icon("dna") ),
                   menuItem("Signiture Detail",tabName = "detail",icon = icon("chart-bar"))
     )
  ),
   dashboardBody(
       useShinyjs(),
       tabItems(
        # 缩略图浏览页
        tabItem(tabName = "browser",
                h2("Indel Signature Brower"),
                p("Click a group to view its ID83 and ID89 details."),
                hr(),
                
                fluidRow(
                    lapply(names(signature_groups),function(group_name){
                      sig <- signature_groups[[group_name]]
                      column(
                          width = 6,
                          box(width = 12,title = group_name,solidHeader = TRUE,
                              fluidRow(
                                  column(6,img(src = sig$ID83thumb,width = "100%"),p("ID83")),
                                  column(6, img(src = sig$ID89thumb, width = "100%"), p("ID89"))
                              ),
                              br(),
                              actionButton(paste0("show_",group_name),"View Details",class = "btn-primary")
                              )
                          )
                    })
    )
),

# 详细页
tabItem(tabName = "detail",
        actionButton("back_to_browser","← Back",class = "btn btn-secondary mb-3"),
        h2(textOutput("detail_title")),
        p(textOutput("detail_desc")),
        hr(),
        fluidRow(
            column(6,h4("ID83 Signature"),img(src = "", id = "img_ID83", width = "100%")),
            column(6, h4("ID89 Signature"), img(src = "", id = "img_ID89", width = "100%"))
        )
     )
    )
   )
)

# server
server <- function(input,output,session){
    current_group <- reactiveVal(NULL)
    
    # 注册每个组的点击事件
    lapply(names(signature_groups),function(group_name){
        observeEvent(input[[paste0("show_", group_name)]], {
            current_group(group_name)
            updateTabItems(session,"tabs","detail")
        })
    })
    # 更新详细页内容
    output$detail_title <- renderText({
        req(current_group())
        paste("Details for",current_group())
    })
    
    output$detail_desc <- renderText({
        req(current_group())
        signature_groups[[current_group()]]$desc
    })
    
    observe({
        req(current_group())
        sig <- signature_groups[[current_group()]]
        runjs(sprintf("document.getElementById('img_ID83').src='%s';",sig$ID83full))
        runjs(sprintf("document.getElementById('img_ID89').src='%s';", sig$ID89full))
    })
    
    observeEvent(input$back_to_browser,{
        updateTabItems(session,"tabs","browser")
    })
}

shinyApp(ui, server)





