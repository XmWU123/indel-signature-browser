library(shiny)
library(shinyjs)

h3_style = "color:#34495e; font-size:18px; line-height:1.8; margin-bottom:20px;"

ui <- navbarPage(
  title = "Indel Signature Explorer",
  theme = NULL, # 使用自定义样式
  id = "navbar",

  # 添加自定义 CSS
  header = tags$head(
    useShinyjs(),
    tags$style(HTML(
      "
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
    "
    ))
  ),

  # ========== Home Page ==========

  tabPanel(
    "Home",
    icon = icon("house"),
    div(
      class = "img-container",
      h1(
        "Mutational Signatures of Indels — Small Insertions and Deletions",
        style = "color:#2c3e50; font-weight:700; margin-bottom:25px;"
      ),
      h3(
        paste(
          "Indels are mutations that ",
          "add or delete small sequences of DNA (conventionally < ~50 base pairs long).",
          "There is no single intuitive and naturally",
          "constrained classification of indel mutation types (as there arguably is for single base mutations)",
          "but two classifications are useful and widely used:"
        ),
        style = h3_style
      ),
      tags$ul(
        style = h3_style,
        tags$li(
          tags$strong("COSMIC83:"),
          paste(
            " The most widely used classification, which classifies indels into 83 types.",
            "Described in Alexandrov et al., 2020 and used on the COSMIC mutational signatures web site."
          )
        ),
        tags$li(
          tags$strong("Koh89:"),
          paste(
            " New indel classifiction scheme (Koh et al., 2025), which classifies indels in 89 type.",
            "Provides more informative granularity for indels in homopolymers (e.g. ATTTTTG → ATTTTG).",
            "Koh et al., 2025 also present an even more granular classifiction of 476 mutation types."
          )
        )
      ),
      h3(
        paste(
          "With a few exceptions, the reference signatures on this web site were extracted using both",
          "SigProfiler (as described in Alexandrov et al., 2020) and mSigHdp (Liu et al., 2023) from the",
          "2,780 whole-genome from ICGC/TCGA Pan Cancer Analysis of Whole Genomes (PCAWG) Network and",
          ">4,000 whole-genome from Hartwig Medical Foundation."
        ),
        style = h3_style
      ),
      h3(
        "Please select a group from the navigation bar above.",
        style = "color:#3498db; font-size:20px; margin-top:25px; font-weight:600;"
      )
    )
  ),

  # ========== Koh ID89 Explorer ==========
  tabPanel(
    "Koh ID89 Explorer",
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
  ),

  # ========== COSMIC ID83 Explorer ==========
  tabPanel(
    "COSMIC ID83 Explorer",
    icon = icon("layer-group"),
    uiOutput("id83_display")
  ),

  # ========== About ==========
  tabPanel(
    "About",
    icon = icon("info-circle"),
    fluidRow(
      column(
        10,
        offset = 1,
        div(
          class = "img-container",
          h2(
            "About",
            style = "color:#2c3e50; font-weight:700; margin-bottom:35px; text-align:center; font-size:32px;"
          ),

          h3(
            "Contact Us",
            style = "color:#3498db; font-weight:700; margin-top:25px; margin-bottom:15px; font-size:24px;"
          ),
          tags$p(
            style = "font-size:17px; line-height:1.9; color:#555;",
            "If you experience any issues or have suggestions while visiting this website, don't hesitate to reach out to us."
          ),

          hr(
            style = "margin: 35px 0; border-top: 2px solid #3498db; opacity: 0.3;"
          ),

          h3(
            "Main Contributors",
            style = "color:#2ecc71; font-weight:700; margin-top:25px; margin-bottom:15px; font-size:24px;"
          ),
          tags$p(
            style = "font-size:17px; line-height:2.2; color:#555;",
            "• Xueming Wu",
            tags$br(),
            "• Mo Liu",
            tags$br(),
            "• Steve G. Rozen"
          ),

          hr(
            style = "margin: 35px 0; border-top: 2px solid #2ecc71; opacity: 0.3;"
          ),

          h3(
            "Email",
            style = "color:#e74c3c; font-weight:700; margin-top:25px; margin-bottom:15px; font-size:24px;"
          ),
          tags$p(
            style = "font-size:17px; line-height:1.9; color:#555;",
            icon(
              "envelope",
              style = "color:#e74c3c; margin-right:10px; font-size:20px;"
            ),
            tags$a(
              href = "mailto:wuxm8523@gmail.com",
              "wuxm8523@gmail.com",
              href = "mailto:mo.liu@gzhmu.edu.cn",
              "mo.liu@gzhmu.edu.cn",
              style = "color:#3498db; text-decoration:none; font-weight:600;"
            )
          ),

          hr(style = "margin: 45px 0; border-top: 1px solid #bdc3c7;"),

          tags$p(
            style = "text-align:center; color:#7f8c8d; margin-top:40px; font-size:15px; font-weight:500;",
            "© 2025 Indel Signature Explorer. All rights reserved."
          )
        )
      )
    )
  )
)
