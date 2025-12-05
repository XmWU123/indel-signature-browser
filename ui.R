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
      
.search-page-container {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  min-height: 80vh;
  padding: 50px 20px;
  position: relative;
  overflow: hidden;
}

/* 添加装饰性背景元素 */
.search-page-container::before {
  content: '';
  position: absolute;
  width: 500px;
  height: 500px;
  background: radial-gradient(circle, rgba(52, 152, 219, 0.1) 0%, transparent 70%);
  border-radius: 50%;
  top: -200px;
  left: -200px;
  animation: float 20s ease-in-out infinite;
}

.search-page-container::after {
  content: '';
  position: absolute;
  width: 400px;
  height: 400px;
  background: radial-gradient(circle, rgba(46, 204, 113, 0.1) 0%, transparent 70%);
  border-radius: 50%;
  bottom: -150px;
  right: -150px;
  animation: float 15s ease-in-out infinite reverse;
}

@keyframes float {
  0%, 100% { transform: translate(0, 0) rotate(0deg); }
  33% { transform: translate(30px, -30px) rotate(120deg); }
  66% { transform: translate(-20px, 20px) rotate(240deg); }
}

.search-box-large {
  background: rgba(255, 255, 255, 0.95);
  backdrop-filter: blur(10px);
  border-radius: 30px;
  padding: 60px 50px;
  box-shadow: 0 20px 60px rgba(0,0,0,0.15);
  max-width: 750px;
  width: 100%;
  text-align: center;
  position: relative;
  z-index: 1;
  transition: all 0.3s ease;
  border: 1px solid rgba(255, 255, 255, 0.8);
}

.search-box-large:hover {
  box-shadow: 0 25px 80px rgba(52, 152, 219, 0.2);
  transform: translateY(-5px);
}

/* 搜索图标装饰 */
.search-icon-decoration {
  font-size: 60px;
  margin-bottom: 20px;
  background: linear-gradient(135deg, #3498db 0%, #2ecc71 100%);
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
  animation: pulse 2s ease-in-out infinite;
}

@keyframes pulse {
  0%, 100% { transform: scale(1); opacity: 1; }
  50% { transform: scale(1.1); opacity: 0.8; }
}

.search-title {
  font-size: 42px;
  font-weight: 800;
  color: #2c3e50;
  margin-bottom: 15px;
  background: linear-gradient(135deg, #3498db 0%, #2ecc71 100%);
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
  letter-spacing: -1px;
  text-shadow: 0 2px 10px rgba(52, 152, 219, 0.1);
}

.search-subtitle {
  font-size: 18px;
  color: #7f8c8d;
  margin-bottom: 45px;
  font-weight: 400;
  line-height: 1.6;
}

/* 搜索输入容器 */
.search-input-container {
  display: flex;
  align-items: center;
  background: linear-gradient(135deg, #f8f9fa 0%, #ffffff 100%);
  border-radius: 60px;
  padding: 10px 15px;
  margin-bottom: 30px;
  border: 3px solid transparent;
  background-clip: padding-box;
  box-shadow: 0 8px 25px rgba(0,0,0,0.08);
  transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1);
  position: relative;
}

.search-input-container::before {
  content: '';
  position: absolute;
  top: -3px;
  left: -3px;
  right: -3px;
  bottom: -3px;
  background: linear-gradient(135deg, #3498db 0%, #2ecc71 100%);
  border-radius: 60px;
  opacity: 0;
  transition: opacity 0.4s ease;
  z-index: -1;
}

.search-input-container:focus-within::before {
  opacity: 1;
}

.search-input-container:focus-within {
  transform: scale(1.03);
  box-shadow: 0 15px 40px rgba(52, 152, 219, 0.25);
}

.search-input-large {
  flex: 1;
  background: transparent !important;
  border: none !important;
  padding: 18px 25px !important;
  font-size: 18px !important;
  color: #2c3e50 !important;
  font-weight: 500;
}

.search-input-large:focus {
  outline: none !important;
  box-shadow: none !important;
}

.search-input-large::placeholder {
  color: #95a5a6;
  font-weight: 400;
}

/* 搜索按钮 */
.search-btn-large {
  background: linear-gradient(135deg, #3498db 0%, #2980b9 100%) !important;
  border: none !important;
  color: white !important;
  padding: 18px 40px !important;
  border-radius: 50px !important;
  font-size: 16px !important;
  font-weight: 700 !important;
  transition: all 0.3s ease;
  box-shadow: 0 6px 20px rgba(52, 152, 219, 0.35);
  margin-left: 10px;
  letter-spacing: 0.5px;
  text-transform: uppercase;
}

.search-btn-large:hover {
  background: linear-gradient(135deg, #2980b9 0%, #3498db 100%) !important;
  transform: translateY(-3px) scale(1.05);
  box-shadow: 0 10px 30px rgba(52, 152, 219, 0.5);
}

.search-btn-large:active {
  transform: translateY(-1px) scale(1.02);
}

/* 搜索示例区域 */
.search-examples {
  margin-top: 35px;
  padding-top: 30px;
  border-top: 2px dashed rgba(52, 152, 219, 0.2);
}

.search-examples-title {
  font-size: 15px;
  color: #7f8c8d;
  margin-bottom: 18px;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 1px;
}

/* 示例标签 */
.example-tag {
  display: inline-block;
  background: linear-gradient(135deg, #ecf0f1 0%, #ffffff 100%);
  padding: 12px 24px;
  border-radius: 25px;
  margin: 8px;
  font-size: 14px;
  font-weight: 600;
  color: #2c3e50;
  cursor: pointer;
  transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
  border: 2px solid #ecf0f1;
  box-shadow: 0 4px 12px rgba(0,0,0,0.08);
  position: relative;
  overflow: hidden;
}

.example-tag::before {
  content: '';
  position: absolute;
  top: 0;
  left: -100%;
  width: 100%;
  height: 100%;
  background: linear-gradient(90deg, transparent, rgba(255,255,255,0.5), transparent);
  transition: left 0.5s;
}

.example-tag:hover::before {
  left: 100%;
}

.example-tag:hover {
  background: linear-gradient(135deg, #3498db 0%, #2ecc71 100%);
  color: white;
  transform: translateY(-4px) scale(1.08);
  box-shadow: 0 8px 20px rgba(52, 152, 219, 0.4);
  border-color: #3498db;
}

.example-tag:active {
  transform: translateY(-2px) scale(1.05);
}

/* 快捷键提示 */
.search-hint {
  margin-top: 25px;
  font-size: 13px;
  color: #95a5a6;
  font-style: italic;
}

.search-hint kbd {
  background: linear-gradient(135deg, #34495e 0%, #2c3e50 100%);
  color: white;
  padding: 4px 10px;
  border-radius: 6px;
  font-family: monospace;
  font-size: 12px;
  font-weight: 600;
  box-shadow: 0 2px 6px rgba(0,0,0,0.2);
  border: 1px solid #1a252f;
}

/* 响应式设计 */
@media (max-width: 768px) {
  .search-box-large {
    padding: 40px 30px;
  }
  
  .search-title {
    font-size: 32px;
  }
  
  .search-input-container {
    flex-direction: column;
    gap: 15px;
  }
  
  .search-btn-large {
    width: 100%;
    margin-left: 0;
  }
  
  .example-tag {
    margin: 5px;
    padding: 10px 18px;
    font-size: 13px;
  }
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
  
  # ========== Search Page ==========
  tabPanel(
    "Search",
    icon = icon("search"),
    div(class = "search-page-container",
        div(class = "search-box-large",
            # 搜索图标装饰
            div(class = "search-icon-decoration",
                icon("magnifying-glass")
            ),
            
            # 标题
            h1(class = "search-title", "Find Your Signature"),
            p(class = "search-subtitle", 
              "Quickly search and explore indel signatures from our comprehensive database"),
            
            # 搜索框
            div(class = "search-input-container",
                tags$input(
                  id = "search_input",
                  type = "text",
                  class = "search-input-large",
                  placeholder = "Enter signature name (e.g., InsDel1a, InsDel2b)..."
                ),
                actionButton("search_btn",
                             "Search",
                             icon = icon("search"),
                             class = "search-btn-large")
            ),
            
            # 搜索示例
            div(class = "search-examples",
                p(class = "search-examples-title", 
                  icon("lightbulb"), " Quick Examples"),
                div(
                  tags$span(class = "example-tag", 
                            onclick = "Shiny.setInputValue('example_click', 'InsDel1a', {priority: 'event'})", 
                            icon("dna", style = "margin-right: 5px;"), "InsDel1a"),
                  tags$span(class = "example-tag", 
                            onclick = "Shiny.setInputValue('example_click', 'InsDel2b', {priority: 'event'})", 
                            icon("dna", style = "margin-right: 5px;"), "InsDel2b"),
                  tags$span(class = "example-tag", 
                            onclick = "Shiny.setInputValue('example_click', 'InsDel3', {priority: 'event'})", 
                            icon("dna", style = "margin-right: 5px;"), "InsDel3"),
                  tags$span(class = "example-tag", 
                            onclick = "Shiny.setInputValue('example_click', 'C_ID1', {priority: 'event'})", 
                            icon("dna", style = "margin-right: 5px;"), "C_ID1"),
                  tags$span(class = "example-tag", 
                            onclick = "Shiny.setInputValue('example_click', 'H_ID30', {priority: 'event'})", 
                            icon("dna", style = "margin-right: 5px;"), "H_ID30")
                ),
                
            )
        )
    )
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