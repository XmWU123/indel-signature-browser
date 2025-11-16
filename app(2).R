# app.R 
library(shiny)
library(shinydashboard)
library(shinyjs)

# ---------------- 数据定义 ----------------
df <- read.delim("/home/wuxueming/shinyapp/ID89_ID83_connection_example.txt",
                 header = TRUE, stringsAsFactors = FALSE)

df1 <- read.csv("/home/wuxueming/shinyapp/mSigHdp.indel476.final.signatures.csv",
                header = TRUE, check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL)

ID476_list <- colnames(df1)
all_pngs <- list.files("/home/wuxueming/shinyapp/www", pattern = "\\.png$", full.names = FALSE)
signature_groups <- list()

for (i in seq_len(nrow(df))) {
  ID89 <- df$InDel89[i]
  ID83 <- df$InDel83[i]
  ID476 <- if (i <= length(ID476_list)) ID476_list[i] else NA_character_
  
  imgs <- paste0(ID89, c("_signature.89spectrum.png","_89spectrumA.png","_89spectrumB.png","_89spectrumC.png"))
  id83_imgs <- paste0(ID89, "_", ID83, c("_83all.png", "_83filtered.png"))
  id476_imgs <- grep(paste0("^", ID89, "_476all.*\\.png$"), all_pngs, value = TRUE, ignore.case = TRUE)
  
  if (ID89 == "InsDel39") {
    signature_groups[[ID89]] <- list(
      imgs = imgs, id83 = character(0), id83_name = NA_character_,
      id476 = if (length(id476_imgs) > 0) id476_imgs else character(0),
      thumbnail = paste0(ID89, "Thumbnail.png"),
      desc = "InsDel39 does not have corresponding ID83 signature"
    )
  } else {
    signature_groups[[ID89]] <- list(
      imgs = imgs, id83 = id83_imgs, id83_name = ID83,
      id476 = if (length(id476_imgs) > 0) id476_imgs else character(0),
      thumbnail = paste0(ID89, "Thumbnail.png")
    )
  }
}

# 构建 ID83 分组数据
id83_groups <- list()
for (group_name in names(signature_groups)) {
  sig <- signature_groups[[group_name]]
  if (!is.null(sig$id83_name) && !is.na(sig$id83_name)) {
    id83_key <- sig$id83_name
    if (is.null(id83_groups[[id83_key]])) {
      id83_groups[[id83_key]] <- list(members = character(), id83_all = character())
    }
    id83_groups[[id83_key]]$members <- c(id83_groups[[id83_key]]$members, group_name)
    if (length(id83_groups[[id83_key]]$id83_all) == 0 && length(sig$id83) > 0) {
      id83_groups[[id83_key]]$id83_all <- sig$id83[1]
    }
  }
}

# ---------------- UI ----------------
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Indel Signature Browser"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Signature Browser", tabName = "browser", icon = icon("dna")),
                menuItem("ID83 Browser", tabName = "id83_browser", icon = icon("layer-group"))
    ),
    conditionalPanel(
      condition = "input.tabs == 'browser'",
      checkboxGroupInput(
        inputId = "show_types",
        label = "Select signature types:",
        choices = c("Koh89" = "ID89", "COSMIC83" = "ID83", "Koh476" = "ID476"),
        selected = c("ID89", "ID83", "ID476")
      )
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$style(HTML("
      /* 全局样式 */
      body { font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; }
      .content-wrapper { background-color: #f4f6f9; }
      
      /* 缩略图卡片 */
      .thumbnail-card {
        background: white;
        border-radius: 12px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        padding: 20px;
        margin-bottom: 20px;
        transition: all 0.3s ease;
        text-align: center;
      }
      .thumbnail-card:hover {
        transform: translateY(-5px);
        box-shadow: 0 8px 20px rgba(0,0,0,0.15);
      }
      .thumbnail-card h4 {
        color: #2c3e50;
        font-weight: 600;
        margin-bottom: 15px;
        font-size: 16px;
      }
      .thumbnail-card img {
        border-radius: 8px;
        border: 2px solid #e8eaed;
        margin-bottom: 15px;
      }
      .thumbnail-card .btn {
        width: 100%;
        border-radius: 6px;
        font-weight: 500;
        padding: 10px;
        transition: all 0.3s ease;
      }
      
      /* 图片容器 */
      .img-container {
        background: white;
        border-radius: 12px;
        padding: 25px;
        margin-bottom: 25px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
      }
      .img-section-title {
        font-size: 20px;
        font-weight: 600;
        color: #2c3e50;
        margin-bottom: 20px;
        padding-bottom: 10px;
        border-bottom: 3px solid #3498db;
        display: inline-block;
      }
      .img-label {
        font-size: 13px;
        font-weight: 500;
        color: #7f8c8d;
        text-align: center;
        margin-bottom: 10px;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }
      
      /* 图片样式 */
      .signature-img {
        border-radius: 8px;
        border: 2px solid #e8eaed;
        cursor: pointer;
        transition: all 0.3s ease;
        display: block;
        margin: 0 auto;
      }
      .signature-img:hover {
        border-color: #3498db;
        box-shadow: 0 4px 12px rgba(52, 152, 219, 0.3);
        transform: scale(1.02);
      }
      
      /* ID83 分组样式 */
      .id83-section {
        background: white;
        border-radius: 12px;
        padding: 30px;
        margin-bottom: 30px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
      }
      .id83-label {
        font-size: 18px;
        font-weight: 600;
        color: #2c3e50;
        margin-bottom: 20px;
        padding-left: 15px;
        border-left: 4px solid #3498db;
      }
      .member-section {
        background: #f8f9fa;
        border-radius: 8px;
        padding: 20px;
        margin-bottom: 20px;
      }
      .member-name {
        font-weight: 600;
        font-size: 15px;
        color: #34495e;
        margin-bottom: 15px;
        padding: 8px 12px;
        background: white;
        border-radius: 6px;
        display: inline-block;
        box-shadow: 0 1px 3px rgba(0,0,0,0.1);
      }
      
      /* 按钮样式 */
      .btn-back {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        border: none;
        padding: 12px 24px;
        border-radius: 8px;
        font-weight: 500;
        transition: all 0.3s ease;
        box-shadow: 0 2px 8px rgba(102, 126, 234, 0.3);
      }
      .btn-back:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 12px rgba(102, 126, 234, 0.4);
      }
      
      /* 响应式网格 */
      @media (max-width: 768px) {
        .thumbnail-card { margin-bottom: 15px; }
        .img-container { padding: 15px; }
      }
      
      /* Modal 样式优化 */
      .modal-content {
        border-radius: 12px;
        border: none;
      }
      .modal-header {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        border-radius: 12px 12px 0 0;
      }
    ")),
    tabItems(
      tabItem(tabName = "browser", uiOutput("signature_display")),
      tabItem(tabName = "id83_browser", uiOutput("id83_display"))
    )
  )
)

# ---------------- Server ----------------
server <- function(input, output, session){
  current_group <- reactiveVal(NULL)
  current_id83 <- reactiveVal(NULL)
  
  # ---------------- Signature Browser 页面 ----------------
  output$signature_display <- renderUI({
    if (is.null(current_group())) {
      fluidRow(
        lapply(names(signature_groups), function(group_name){
          if (is.null(signature_groups[[group_name]])) return(NULL)
          
          sig <- signature_groups[[group_name]]
          thumb <- sig$thumbnail
          
          column(3,
                 div(class = "thumbnail-card",
                     h4(group_name),
                     if (!is.null(thumb) && file.exists(file.path("www", thumb))) {
                       tags$img(src = thumb, style = "width:100%; max-width:200px; height:auto;")
                     } else {
                       div(style = "height:120px; line-height:120px; color:#95a5a6;", 
                           icon("image", style = "font-size:48px;"))
                     },
                     actionButton(paste0("show_", group_name), "View Details",
                                  class = "btn btn-primary")
                 )
          )
        })
      )
    } else {
      sig <- signature_groups[[current_group()]]
      show_types <- input$show_types %||% c("ID89", "ID83", "ID476")
      
      id89_imgs <- if ("ID89" %in% show_types) sig$imgs else character(0)
      id83_imgs <- if ("ID83" %in% show_types) sig$id83 else character(0)
      id476_imgs <- if ("ID476" %in% show_types) sig$id476 else character(0)
      
      id89_imgs <- id89_imgs[!is.na(id89_imgs) & nzchar(id89_imgs)]
      id83_imgs <- id83_imgs[!is.na(id83_imgs) & nzchar(id83_imgs)]
      id476_imgs <- id476_imgs[!is.na(id476_imgs) & nzchar(id476_imgs)]
      
      tagList(
        actionButton("back_to_list","← Back to List", class = "btn-back", style = "margin-bottom:20px;"),
        h2(current_group(), style = "color:#2c3e50; font-weight:600; margin-bottom:25px;"),
        
        # Koh89 Spectrum
        if (length(id89_imgs) >= 1 && file.exists(file.path("www", id89_imgs[1]))) {
          div(class = "img-container",
              div(class = "img-section-title", "Koh89 Spectrum"),
              div(class = "img-label", "Signature Spectrum"),
              tags$img(src = id89_imgs[1], class = "signature-img",
                       style = "max-width:700px; width:100%;",
                       onclick = sprintf("Shiny.setInputValue('%s', new Date().getTime());", 
                                         paste0("img_", id89_imgs[1])))
          )
        },
        
        # Koh89 Sample Spectrums A/B/C
        if (length(id89_imgs) > 1) {
          div(class = "img-container",
              div(class = "img-section-title", "Koh89 Sample Spectrums"),
              fluidRow(
                lapply(seq_along(id89_imgs[-1]), function(i){
                  nm <- c("A","B","C")[i]
                  imgnm <- id89_imgs[-1][i]
                  column(4,
                         div(class = "img-label", paste("Sample", nm)),
                         tags$img(src = imgnm, class = "signature-img",
                                  style = "max-width:100%; width:100%;",
                                  onclick = sprintf("Shiny.setInputValue('%s', new Date().getTime());", 
                                                    paste0("img_", imgnm)))
                  )
                })
              )
          )
        },
        
        # COSMIC83
        if (length(id83_imgs) > 0) {
          div(class = "img-container",
              div(class = "img-section-title", "COSMIC83 Signatures"),
              if (length(id83_imgs) >= 1 && file.exists(file.path("www", id83_imgs[1]))) {
                tagList(
                  div(class = "img-label", "All Signatures"),
                  tags$img(src = id83_imgs[1], class = "signature-img",
                           style = "max-width:700px; width:100%; margin-bottom:25px;",
                           onclick = sprintf("Shiny.setInputValue('%s', new Date().getTime());", 
                                             paste0("img_", id83_imgs[1])))
                )
              },
              if (length(id83_imgs) >= 2 && file.exists(file.path("www", id83_imgs[2]))) {
                tagList(
                  div(class = "img-label", "Filtered"),
                  tags$img(src = id83_imgs[2], class = "signature-img",
                           style = "max-width:700px; width:100%;",
                           onclick = sprintf("Shiny.setInputValue('%s', new Date().getTime());", 
                                             paste0("img_", id83_imgs[2])))
                )
              }
          )
        },
        
        # Koh476
        if (length(id476_imgs) >= 1 && file.exists(file.path("www", id476_imgs[1]))) {
          div(class = "img-container",
              div(class = "img-section-title", "Koh476"),
              div(class = "img-label", "Extended Signature Set"),
              tags$img(src = id476_imgs[1], class = "signature-img",
                       style = "max-width:100%; width:100%;",
                       onclick = sprintf("Shiny.setInputValue('%s', new Date().getTime());", 
                                         paste0("img_", id476_imgs[1])))
          )
        },
        
        if (!is.null(sig$desc)) {
          div(style = "background:#fff3cd; border-left:4px solid #ffc107; padding:15px; 
                      border-radius:8px; margin-top:20px;",
              icon("info-circle"), " ", sig$desc)
        }
      )
    }
  })
  
  # ---------------- ID83 Browser 页面 ----------------
  output$id83_display <- renderUI({
    if (is.null(current_id83())) {
      fluidRow(
        lapply(names(id83_groups), function(id83_name){
          id83_info <- id83_groups[[id83_name]]
          members_text <- paste(id83_info$members, collapse = ", ")
          
          column(3,
                 div(class = "thumbnail-card",
                     h4(id83_name, style = "color:#27ae60;"),
                     div(style = "background:#ecf0f1; padding:15px; border-radius:8px; margin-bottom:15px;",
                         div(style = "font-size:11px; color:#7f8c8d; margin-bottom:5px;", "MEMBERS"),
                         div(style = "font-size:12px; color:#34495e; font-weight:500;", members_text)
                     ),
                     actionButton(paste0("show_id83_", id83_name), "View Details",
                                  class = "btn btn-success")
                 )
          )
        })
      )
    } else {
      id83_info <- id83_groups[[current_id83()]]
      members <- id83_info$members
      id83_all_img <- id83_info$id83_all
      
      tagList(
        actionButton("back_to_id83_list","← Back to ID83 List", 
                     class = "btn-back", style = "margin-bottom:20px;"),
        h2(paste("ID83:", current_id83()), 
           style = "color:#27ae60; font-weight:600; margin-bottom:25px;"),
        
        # ID83_all
        div(class = "id83-section",
            div(class = "id83-label", icon("layer-group"), " ID83 All Signatures"),
            if (!is.null(id83_all_img) && length(id83_all_img) > 0 && 
                file.exists(file.path("www", id83_all_img))) {
              tags$img(src = id83_all_img, class = "signature-img",
                       style = "max-width:700px; width:100%;",
                       onclick = sprintf("Shiny.setInputValue('%s', new Date().getTime());", 
                                         paste0("img_", id83_all_img)))
            } else {
              div(style = "color:#95a5a6; text-align:center; padding:40px;",
                  icon("image", style = "font-size:48px;"), br(), "No image available")
            }
        ),
        
        # Koh89
        div(class = "id83-section",
            div(class = "id83-label", icon("dna"), " Koh89 Signatures"),
            lapply(members, function(member_name){
              sig <- signature_groups[[member_name]]
              if (is.null(sig)) return(NULL)
              
              koh89_spectrum <- if (length(sig$imgs) >= 1) sig$imgs[1] else NULL
              koh89_sampleA <- if (length(sig$imgs) >= 2) sig$imgs[2] else NULL
              cosmic83_filtered <- if (length(sig$id83) >= 2) sig$id83[2] else NULL
              
              div(class = "member-section",
                  div(class = "member-name", icon("chevron-right"), " ", member_name),
                  fluidRow(
                    if (!is.null(koh89_spectrum) && file.exists(file.path("www", koh89_spectrum))) {
                      column(4,
                             div(class = "img-label", "Spectrum"),
                             tags$img(src = koh89_spectrum, class = "signature-img",
                                      style = "max-width:100%; width:100%;",
                                      onclick = sprintf("Shiny.setInputValue('%s', new Date().getTime());", 
                                                        paste0("img_", koh89_spectrum)))
                      )
                    },
                    if (!is.null(koh89_sampleA) && file.exists(file.path("www", koh89_sampleA))) {
                      column(4,
                             div(class = "img-label", "Sample A"),
                             tags$img(src = koh89_sampleA, class = "signature-img",
                                      style = "max-width:100%; width:100%;",
                                      onclick = sprintf("Shiny.setInputValue('%s', new Date().getTime());", 
                                                        paste0("img_", koh89_sampleA)))
                      )
                    },
                    if (!is.null(cosmic83_filtered) && file.exists(file.path("www", cosmic83_filtered))) {
                      column(4,
                             div(class = "img-label", "COSMIC83 Filtered"),
                             tags$img(src = cosmic83_filtered, class = "signature-img",
                                      style = "max-width:100%; width:100%;",
                                      onclick = sprintf("Shiny.setInputValue('%s', new Date().getTime());", 
                                                        paste0("img_", cosmic83_filtered)))
                      )
                    }
                  )
              )
            })
        ),
        
        # Koh476
        div(class = "id83-section",
            div(class = "id83-label", icon("microscope"), " Koh476 Extended Set"),
            lapply(members, function(member_name){
              sig <- signature_groups[[member_name]]
              if (is.null(sig) || length(sig$id476) == 0) return(NULL)
              
              koh476_img <- sig$id476[1]
              if (!is.null(koh476_img) && file.exists(file.path("www", koh476_img))) {
                div(class = "member-section",
                    div(class = "member-name", icon("chevron-right"), " ", member_name),
                    tags$img(src = koh476_img, class = "signature-img",
                             style = "max-width:100%; width:100%;",
                             onclick = sprintf("Shiny.setInputValue('%s', new Date().getTime());", 
                                               paste0("img_", koh476_img)))
                )
              }
            })
        )
      )
    }
  })
  
  # ---------------- Event Handlers ----------------
  lapply(names(signature_groups), function(group_name){
    observeEvent(input[[paste0("show_", group_name)]], { current_group(group_name) })
  })
  
  observeEvent(input$back_to_list, { current_group(NULL) })
  
  lapply(names(id83_groups), function(id83_name){
    observeEvent(input[[paste0("show_id83_", id83_name)]], { current_id83(id83_name) })
  })
  
  observeEvent(input$back_to_id83_list, { current_id83(NULL) })
  
  # 图片点击弹窗
  observe({
    all_imgs <- character()
    
    if (!is.null(current_group())) {
      sig <- signature_groups[[current_group()]]
      all_imgs <- unique(c(sig$imgs, sig$id83, sig$id476))
    }
    
    if (!is.null(current_id83())) {
      id83_info <- id83_groups[[current_id83()]]
      all_imgs <- c(all_imgs, id83_info$id83_all)
      for (member in id83_info$members) {
        sig <- signature_groups[[member]]
        if (!is.null(sig)) {
          all_imgs <- c(all_imgs, sig$imgs, sig$id83, sig$id476)
        }
      }
    }
    
    all_imgs <- unique(all_imgs[!is.na(all_imgs) & nzchar(all_imgs)])
    
    lapply(all_imgs, function(img) {
      observeEvent(input[[paste0("img_", img)]], ignoreInit = TRUE, {
        showModal(modalDialog(
          title = img,
          easyClose = TRUE, size = "l", footer = NULL,
          tags$img(src = img, style = "width:100%; height:auto; border-radius:8px;")
        ))
      })
    })
  })
}

shinyApp(ui, server)