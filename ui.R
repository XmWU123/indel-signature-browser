library(shiny)
library(shinyjs)

ui <- navbarPage(
  title = "Indel Signature Browser",
  theme = NULL,  # 使用自定义样式
  id = "navbar",
  
  # 添加自定义 CSS
  header = tags$head(
    useShinyjs(),
    tags$style(HTML("
      /* 全局样式 - 明亮主题 */
      body { 
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        background: linear-gradient(135deg, #cfe9ff 0%, #ffffff 100%);
        background-attachment: fixed;
        min-height: 100vh;
      }
      
      /* 顶部导航栏样式 */
      .navbar {
        background: linear-gradient(135deg, #2c3e50 0%, #34495e 100%) !important;
        border: none;
        box-shadow: 0 2px 15px rgba(0,0,0,0.2);
        margin-bottom: 0;
      }
      .navbar-default .navbar-nav > li > a {
        color: #ecf0f1 !important;
        font-weight: 600;
        font-size: 15px;
        padding: 15px 20px;
        transition: all 0.3s ease;
      }
      .navbar-default .navbar-nav > li > a:hover {
        background: #3498db !important;
        color: white !important;
      }
      .navbar-default .navbar-nav > .active > a {
        background: #3498db !important;
        color: white !important;
        border-bottom: 3px solid #2ecc71;
      }
      .navbar-brand {
        color: white !important;
        font-weight: 700 !important;
        font-size: 20px !important;
      }
      
      /* 内容容器 */
      .container-fluid {
        padding: 30px;
        max-width: 1400px;
        margin: 0 auto;
      }
      
      /* 选项卡控制面板 */
      .control-panel {
        background: white;
        border-radius: 12px;
        padding: 20px;
        margin-bottom: 25px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.1);
        border: 2px solid rgba(52, 152, 219, 0.2);
      }
      .control-panel label {
        font-weight: 600;
        color: #2c3e50;
        font-size: 16px;
      }
      
      /* 缩略图卡片 */
      .thumbnail-card {
        background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);
        border-radius: 16px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.1);
        padding: 25px;
        margin-bottom: 20px;
        transition: all 0.3s ease;
        text-align: center;
        border: 2px solid rgba(52, 152, 219, 0.1);
      }
      .thumbnail-card:hover {
        transform: translateY(-8px);
        box-shadow: 0 12px 30px rgba(52, 152, 219, 0.3);
        border-color: #3498db;
      }
      .thumbnail-card h4 {
        color: #2c3e50;
        font-weight: 700;
        margin-bottom: 15px;
        font-size: 18px;
      }
      .thumbnail-card img {
        border-radius: 12px;
        border: 3px solid #ecf0f1;
        margin-bottom: 15px;
        transition: all 0.3s ease;
      }
      .thumbnail-card:hover img {
        border-color: #3498db;
        transform: scale(1.05);
      }
      .thumbnail-card .btn {
        width: 100%;
        border-radius: 8px;
        font-weight: 600;
        padding: 12px;
        transition: all 0.3s ease;
        background: linear-gradient(135deg, #3498db 0%, #2980b9 100%);
        border: none;
        color: white;
      }
      .thumbnail-card .btn:hover {
        background: linear-gradient(135deg, #2980b9 0%, #3498db 100%);
        transform: scale(1.05);
      }
      
      /* 图片容器 */
      .img-container {
        background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);
        border-radius: 16px;
        padding: 30px;
        margin-bottom: 25px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.1);
        border: 2px solid rgba(52, 152, 219, 0.1);
      }
      .img-section-title {
        font-size: 24px;
        font-weight: 700;
        color: #2c3e50;
        margin-bottom: 20px;
        padding-bottom: 15px;
        border-bottom: 4px solid #3498db;
        display: inline-block;
      }
      .img-label {
        font-size: 14px;
        font-weight: 600;
        color: #7f8c8d;
        text-align: center;
        margin-bottom: 12px;
        text-transform: uppercase;
        letter-spacing: 1px;
      }
      
      /* 图片样式 */
      .signature-img {
        border-radius: 12px;
        border: 3px solid #ecf0f1;
        cursor: pointer;
        transition: all 0.3s ease;
        display: block;
        margin: 0 auto;
        background: white;
        padding: 5px;
      }
      .signature-img:hover {
        border-color: #3498db;
        box-shadow: 0 8px 25px rgba(52, 152, 219, 0.4);
        transform: scale(1.03);
      }
      
      /* ID83 分组样式 */
      .id83-section {
        background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);
        border-radius: 16px;
        padding: 35px;
        margin-bottom: 30px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.1);
        border: 2px solid rgba(52, 152, 219, 0.1);
      }
      .id83-label {
        font-size: 20px;
        font-weight: 700;
        color: #2c3e50;
        margin-bottom: 25px;
        padding-left: 20px;
        border-left: 5px solid #3498db;
      }
      .member-section {
        background: linear-gradient(135deg, #ecf0f1 0%, #d5d8dc 100%);
        border-radius: 12px;
        padding: 25px;
        margin-bottom: 20px;
        border: 2px solid rgba(52, 152, 219, 0.2);
      }
      .member-name {
        font-weight: 700;
        font-size: 16px;
        color: white;
        margin-bottom: 15px;
        padding: 10px 15px;
        background: linear-gradient(135deg, #3498db 0%, #2ecc71 100%);
        border-radius: 8px;
        display: inline-block;
        box-shadow: 0 2px 8px rgba(52, 152, 219, 0.3);
      }
      
      /* 按钮样式 */
      .btn-back {
        background: linear-gradient(135deg, #e74c3c 0%, #c0392b 100%);
        color: white;
        border: none;
        padding: 14px 28px;
        border-radius: 10px;
        font-weight: 600;
        transition: all 0.3s ease;
        box-shadow: 0 4px 15px rgba(231, 76, 60, 0.4);
      }
      .btn-back:hover {
        transform: translateY(-3px);
        box-shadow: 0 6px 20px rgba(231, 76, 60, 0.5);
        background: linear-gradient(135deg, #c0392b 0%, #e74c3c 100%);
      }
      
      .btn-info {
        background: linear-gradient(135deg, #3498db 0%, #2980b9 100%) !important;
      }
      .btn-success {
        background: linear-gradient(135deg, #2ecc71 0%, #27ae60 100%) !important;
      }
      
      /* Modal 样式 */
      .modal-content {
        border-radius: 16px;
        border: none;
      }
      .modal-header {
        background: linear-gradient(135deg, #3498db 0%, #2ecc71 100%);
        color: white;
        border-radius: 16px 16px 0 0;
      }
      .search-page-container {
        display: flex;
        justify-content: center;   /* 水平居中 */
        align-items: center;       /* 垂直居中 */
        min-height: 80vh;          /* 占满视窗高度，更容易居中 */
      }

     .search-box-large {
       text-align: center;        /* 子元素文字居中 */
       max-width: 600px;          /* 限制宽度，不会太宽 */
       width: 100%;
       margin: 0 auto;
      }
    "))
  ),
  
  # ========== Home Page ==========
  tabPanel(
    "Home",
    icon = icon("house"),
    div(class = "img-container",
        h1("Small Insertions and Deletions (ID) Signatures",
           style = "color:#2c3e50; font-weight:700; margin-bottom:25px;"),
        h3("Small insertions and deletions (ID), also known as indels, are defined as the incorporation or loss of small fragments of DNA (usually between 1 and 50 base pairs) in a specific genomic location.Although there is no single intuitive and naturally constrained set of ID mutation types (as there arguably are for single base substitutions and doublet base substitutions), a compilation of 83 different types considering size, nucleotides affected and presence on repetitive and/or microhomology regions was used to extract mutational signatures. It can be found here.Click on any signature below to learn more about its details..",
           style = "color:#34495e; font-size:18px; line-height:1.8; margin-bottom:20px;"),
        h3("Signature extraction methodsWith a few exceptions, the current set of reference signatures were extracted using SigProfiler (as described in Alexandrov, L.B. et al., 2020) from the 2,780 whole-genome variant calls produced by the ICGC/TCGA Pan Cancer Analysis of Whole Genomes (PCAWG) Network. The stability and reproducibility of the signatures were assessed on somatic mutations from an additional 1,865 whole genomes and 19,184 exomes. All input data and references for original sources are available from synapse.org ID syn11801889.",
           style = "color:#34495e; font-size:18px; line-height:1.8; margin-bottom:20px;"),
        h3("Please select a group from the navigation bar above.",
           style = "color:#3498db; font-size:20px; margin-top:25px; font-weight:600;")
    )
  ),
  
  # ========== Koh ID89 Browser ==========
  tabPanel(
    "Koh ID89 Browser",
    icon = icon("dna"),
    # 顶部控制面板
    div(class = "control-panel",
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
  ),
  
  # ========== COSMIC ID83 Browser ==========
  tabPanel(
    "COSMIC ID83 Browser",
    icon = icon("layer-group"),
    uiOutput("id83_display")
  ),
  
  # ========== Search Page（独立搜索页）==========
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
  ),
    
  
  # ========== About ==========
  tabPanel(
    "About",
    icon = icon("info-circle"),
    fluidRow(
      column(10, offset = 1,
             div(class = "img-container",
                 h2("About", 
                    style = "color:#2c3e50; font-weight:700; margin-bottom:35px; text-align:center; font-size:32px;"),
                 
                 h3("Contact Us", 
                    style = "color:#3498db; font-weight:700; margin-top:25px; margin-bottom:15px; font-size:24px;"),
                 tags$p(style = "font-size:17px; line-height:1.9; color:#555;",
                        "If you experience any issues or have suggestions while visiting this website, don't hesitate to reach out to us."
                 ),
                 
                 hr(style = "margin: 35px 0; border-top: 2px solid #3498db; opacity: 0.3;"),
                 
                 h3("Main Contributors", 
                    style = "color:#2ecc71; font-weight:700; margin-top:25px; margin-bottom:15px; font-size:24px;"),
                 tags$p(style = "font-size:17px; line-height:2.2; color:#555;",
                        "• Xueming Wu", tags$br(),
                        "• Mo Liu", tags$br(),
                        "• Steverozen"
                 ),
                 
                 hr(style = "margin: 35px 0; border-top: 2px solid #2ecc71; opacity: 0.3;"),
                 
                 h3("Email", 
                    style = "color:#e74c3c; font-weight:700; margin-top:25px; margin-bottom:15px; font-size:24px;"),
                 tags$p(style = "font-size:17px; line-height:1.9; color:#555;",
                        icon("envelope", style = "color:#e74c3c; margin-right:10px; font-size:20px;"),
                        tags$a(href = "mailto:wuxm8523@gmail.com", 
                               "wuxm8523@gmail.com",
                               style = "color:#3498db; text-decoration:none; font-weight:600;")
                 ),
                 
                 hr(style = "margin: 45px 0; border-top: 1px solid #bdc3c7;"),
                 
                 tags$p(style = "text-align:center; color:#7f8c8d; margin-top:40px; font-size:15px; font-weight:500;",
                        "© 2025 Indel Signature Browser. All rights reserved."
                 )
             )
      )
    )
  )
)