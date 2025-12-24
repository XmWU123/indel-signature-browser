library(shiny)
library(shinyjs)
library(shinydashboard)
library(readxl)
library(dplyr)
library(tidyr)
library(data.table)

# ==============================================================================
# 1. 数据加载与预处理 (在 Server 函数外部执行)
# ==============================================================================

# 读取 Excel 文件 (请确保路径正确)
raw_data <- read_excel("/home/wuxueming/shinyapp/table_1_2025_12_14.xlsx", sheet = 1)

# 清洗数据
id89_df <- raw_data %>%
  dplyr::select(
    InDel83 = `83-type signature`,
    InDel89 = `89-type signature`,
    Aetiology = `Proposed Etiology`
  ) %>%
  fill(InDel83, .direction = "down") %>%
  dplyr::filter(!is.na(InDel89)) %>%
  mutate(
    InDel83 = as.character(InDel83),
    InDel89 = as.character(InDel89),
    Aetiology = as.character(Aetiology)
  )

# 读取 476 列表
id476_df <- read.csv(
  "./mSigHdp.indel476.final.signatures.csv",
  header = TRUE,
  check.names = FALSE,
  stringsAsFactors = FALSE,
  row.names = NULL
)
ID476_list <- colnames(id476_df)

# 获取所有图片列表
all_pngs <- list.files("./www", pattern = "\\.png$", full.names = FALSE)

# --- 构建 signature_groups (以 ID89 为核心) ---
signature_groups <- list()

for (i in seq_len(nrow(id89_df))) {
  ID89 <- id89_df$InDel89[i]
  ID83 <- id89_df$InDel83[i]
  if (is.na(ID83)) ID83 <- "Unknown"
  
  aetiology <- id89_df$Aetiology[i]
  if (is.na(aetiology)) aetiology <- "Unknown"
  
  imgs <- paste0(ID89, c("_signature.89spectrum.png", "_89spectrumA.png", "_89spectrumB.png", "_89spectrumC.png"))
  id83_imgs <- paste0(ID89, "_", ID83, c("_83all.png", "_83filtered.png"))
  id476_imgs <- grep(paste0("^", ID89, "_476all.*\\.png$"), all_pngs, value = TRUE, ignore.case = TRUE)
  
  signature_groups[[ID89]] <- list(
    imgs = imgs,
    id83 = id83_imgs,
    id83_name = ID83,
    id476 = if (length(id476_imgs) > 0) id476_imgs else character(0),
    thumbnail = paste0(ID89, "89Thumbnail.png"),
    aetiology = aetiology
  )
}

# --- 构建 id83_groups  ---
id83_groups <- list()
existing_thumbnails <- list.files("www", pattern = "_Thumbnail\\.png$", full.names = FALSE)

for (i in seq_len(nrow(id89_df))) {
  
  raw_id83 <- id89_df$InDel83[i]
  raw_id89 <- id89_df$InDel89[i]
  
  if (is.na(raw_id83) || raw_id83 == "Unknown") next
  
  id83_key <- trimws(as.character(raw_id83))
  
  # 初始化
  if (is.null(id83_groups[[id83_key]])) {
    id83_groups[[id83_key]] <- list(
      members = character(),
      id83_all = character(),
      thumbnail = character()
    )
  }
  
  # 添加成员
  if (!raw_id89 %in% id83_groups[[id83_key]]$members) {
    id83_groups[[id83_key]]$members <- c(id83_groups[[id83_key]]$members, raw_id89)
  }
  
  # 设置大图路径
  expected_83all <- paste0(raw_id89, "_", raw_id83, "_83all.png")
  if (length(id83_groups[[id83_key]]$id83_all) == 0) {
    if (file.exists(file.path("www", expected_83all))) {
      id83_groups[[id83_key]]$id83_all <- expected_83all
    }
  }
  
  # 设置缩略图 (全能匹配)
  if (length(id83_groups[[id83_key]]$thumbnail) == 0) {
    possible_names <- c(
      paste0("ID_", id83_key, "_Thumbnail.png"),
      paste0("C_ID", id83_key, "_Thumbnail.png"),
      paste0(id83_key, "_Thumbnail.png"),
      paste0("C_", id83_key, "_Thumbnail.png"),
      paste0(gsub("ID", "ID_", id83_key), "_Thumbnail.png"),
      paste0(gsub("ID_", "ID", id83_key), "_Thumbnail.png")
    )
    
    match <- intersect(possible_names, existing_thumbnails)
    
    if (length(match) > 0) {
      id83_groups[[id83_key]]$thumbnail <- match[1]
    } else {
      # 模糊兜底
      clean_key <- gsub("[^A-Za-z0-9]", "", id83_key) 
      search_pattern <- paste0(".*", clean_key, ".*_Thumbnail\\.png$")
      fuzzy_match <- grep(search_pattern, existing_thumbnails, value = TRUE, ignore.case = TRUE)
      if (length(fuzzy_match) > 0) {
        id83_groups[[id83_key]]$thumbnail <- fuzzy_match[1]
      }
    }
  }
}

# ==============================================================================
# 2. Server 函数
# ==============================================================================

server <- function(input, output, session) {
  
  observe({
    runjs("$('.sidebar-menu li').removeClass('active');")
  })
  
  current_group <- reactiveVal(NULL)
  current_id83 <- reactiveVal(NULL)
  
  # ---------------- Home page navigation links ----------------
  observeEvent(input$home_goto_koh89, {
    updateNavbarPage(session, "navbar", selected = "Koh89 Classification")
  })
  
  observeEvent(input$home_goto_cosmic83, {
    updateNavbarPage(session, "navbar", selected = "COSMIC83 Classification")
  })
  
  # ---------------- Search 页面逻辑 ----------------
  search_signature <- function(search_term) {
    search_term <- trimws(search_term)
    
    if (search_term == "") {
      showModal(modalDialog(
        title = "Search Error",
        "Please enter a signature name to search.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return()
    }
    
    matched89 <- grep(search_term, names(signature_groups), ignore.case = TRUE, value = TRUE)
    
    matched83_found <- character(0)
    matched83_names <- character(0)
    
    for (g_name in names(id83_groups)) {
      info <- id83_groups[[g_name]]
      if (any(grepl(search_term, info$members, ignore.case = TRUE))) {
        matched83_found <- c(matched83_found, g_name)
      }
      aliases <- c(g_name, paste0("ID", g_name), paste0("ID_", g_name), paste0("C_ID", g_name), paste0("C_ID_", g_name))
      if (any(grepl(search_term, aliases, ignore.case = TRUE))) {
        matched83_names <- c(matched83_names, g_name)
      }
    }
    matched83 <- unique(c(matched83_found, matched83_names))
    
    if (length(matched89) == 0 && length(matched83) == 0) {
      showModal(modalDialog(
        title = "Signature Not Found",
        paste0("'", search_term, "' not found in ID89 or ID83."),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return()
    }
    
    if ((length(matched89) > 0 && length(matched83) > 0) || length(matched89) > 1 || length(matched83) > 1) {
      choices <- c()
      if (length(matched89) > 0) choices <- c(choices, paste0(matched89, " [Type: Koh89]"))
      if (length(matched83) > 0) choices <- c(choices, paste0(matched83, " [Type: COSMIC83]"))
      
      showModal(modalDialog(
        title = "Select Signature",
        tags$p(paste0("Multiple matches found for '", search_term, "':")),
        radioButtons("select_search", label = NULL, choices = choices),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_search", "Go", class = "btn-primary")
        )
      ))
      return()
    }
    
    if (length(matched89) == 1) {
      updateNavbarPage(session, "navbar", selected = "Koh89 Classification")
      current_group(matched89[1])
      updateTextInput(session, "search_input", value = "")
      return()
    }
    if (length(matched83) == 1) {
      updateNavbarPage(session, "navbar", selected = "COSMIC83 Classification")
      current_id83(matched83[1])
      updateTextInput(session, "search_input", value = "")
      return()
    }
  }
  
  observeEvent(input$confirm_search, {
    choice <- input$select_search
    if (is.null(choice)) return()
    
    if (grepl("\\[Type: Koh89\\]$", choice)) {
      sig89 <- sub(" \\[Type: Koh89\\]$", "", choice)
      updateNavbarPage(session, "navbar", selected = "Koh89 Classification")
      current_group(sig89)
    }
    
    if (grepl("\\[Type: COSMIC83\\]$", choice)) {
      sig83 <- sub(" \\[Type: COSMIC83\\]$", "", choice)
      updateNavbarPage(session, "navbar", selected = "COSMIC83 Classification")
      current_id83(sig83)
    }
    updateTextInput(session, "search_input", value = "")
    removeModal()
  })
  
  observeEvent(input$search_btn, {
    search_signature(input$search_input)
  })
  
  # ---------------- Signature Classification 页面 (Koh89) ----------------
  output$signature_display <- renderUI({
    if (is.null(current_group())) {
      fluidRow(
        lapply(names(signature_groups), function(group_name) {
          if (is.null(signature_groups[[group_name]])) return(NULL)
          sig <- signature_groups[[group_name]]
          thumb <- sig$thumbnail
          
          column(3,
                 div(class = "thumbnail-card",
                     actionLink(
                       inputId = paste0("show_", group_name),
                       label = tagList(
                         h4(group_name, style = "color:#2c3e50;font-weight:bold;margin-top:0;"),
                         if (!is.null(thumb) && file.exists(file.path("www", thumb))) {
                           tags$img(src = thumb, style = "width:100%; max-width:200px; height:auto;border-radius:5px; transition: transform 0.2s;")
                         } else {
                           div(style = "height:120px; line-height:120px; color:#95a5a6;", icon("image", style = "font-size:48px;"))
                         }
                       ),
                       style = "text-decoration: none;color: inherit; display: block; cursor:pointer;"
                     )
                 )
          )
        })
      )
    } else {
      sig <- signature_groups[[current_group()]]
      show_types <- input$show_types %||% c("ID89", "ID83", "ID476")
      current_selection <- if (is.null(input$show_types)) c("ID89", "ID83", "ID476") else input$show_types
      
      id89_imgs <- if ("ID89" %in% show_types) sig$imgs else character(0)
      id83_imgs <- if ("ID83" %in% show_types) sig$id83 else character(0)
      id476_imgs <- if ("ID476" %in% show_types) sig$id476 else character(0)
      
      id89_imgs <- id89_imgs[!is.na(id89_imgs) & nzchar(id89_imgs)]
      id83_imgs <- id83_imgs[!is.na(id83_imgs) & nzchar(id83_imgs)]
      id476_imgs <- id476_imgs[!is.na(id476_imgs) & nzchar(id476_imgs)]
      
      tagList(
        actionButton("back_to_list", "← Back to List", class = "btn-back", style = "margin-bottom:20px;"),
        h2(current_group(), style = "color:#2c3e50; font-weight:600; margin-bottom:25px;"),
        
        div(style = "margin-bottom: 20px; padding: 15px; background: #f8f9fa; border-radius:5px; border: 1px solid #e9ecef;",
            checkboxGroupInput("show_types", "Select signature types to display:", 
                               choices = c("Koh89" = "ID89", "COSMIC83" = "ID83", "Koh476" = "ID476"),
                               selected = current_selection, inline = TRUE)
        ),
        
        if (!is.null(sig$aetiology) && !is.na(sig$aetiology) && nchar(sig$aetiology) > 0) {
          div(style = "background: linear-gradient(135deg, #e8f5e9 0%, #c8e6c9 100%); padding:20px; border-radius:12px; margin-bottom:25px; border-left:5px solid #2ecc71; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
              div(style = "display:flex; align-items:center; margin-bottom:10px;",
                  icon("lightbulb", style = "font-size:24px; color:#27ae60; margin-right:12px;"),
                  tags$span(style = "font-size:16px; color:#27ae60; font-weight:700; text-transform:uppercase; letter-spacing:1px;", "Proposed Aetiology")
              ),
              tags$p(style = "font-size:16px; color:#2c3e50; line-height:1.8; margin:0;", sig$aetiology)
          )
        },
        
        # Koh89 Spectrum
        if (length(id89_imgs) >= 1 && file.exists(file.path("www", id89_imgs[1]))) {
          div(class = "img-container",
              div(class = "img-section-title", "Koh89 Signature"),
              div(class = "img-label", "Signature Spectrum"),
              tags$img(src = id89_imgs[1], class = "signature-img", style = "max-width:700px; width:100%;",
                       onclick = sprintf("Shiny.setInputValue('%s', new Date().getTime());", paste0("img_", id89_imgs[1])))
          )
        },
        
        # Koh89 Samples
        if (length(id89_imgs) > 1) {
          div(class = "img-container",
              div(class = "img-section-title", "Koh89 Sample Spectrums"),
              p(class = "text-muted", style = "margin-top: -8px; margin-bottom: 12px; color:#6c757d; font-size: 12px;",
                "Click an image to view details. Samples A is the example tumor spectrum; Sample B is the partial spectrum contributed by all other signatures; Sample A-B is the difference."),
              fluidRow(
                lapply(seq_along(id89_imgs[-1]), function(i) {
                  nm <- c("A", "B", "A-B")[i]
                  imgnm <- id89_imgs[-1][i]
                  column(4,
                         div(class = "img-label", paste("Sample", nm)),
                         tags$img(src = imgnm, class = "signature-img", style = "max-width:100%; width:100%;",
                                  onclick = sprintf("Shiny.setInputValue('%s', new Date().getTime());", paste0("img_", imgnm)))
                  )
                })
              )
          )
        },
        
        # COSMIC83
        if (length(id83_imgs) > 0) {
          div(class = "img-container",
              div(class = "img-section-title", "COSMIC83 Signature"),
              if (length(id83_imgs) >= 1 && file.exists(file.path("www", id83_imgs[1]))) {
                tagList(div(class = "img-label", "Siganture Spectrum"),
                        tags$img(src = id83_imgs[1], class = "signature-img", style = "max-width:700px; width:100%; margin-bottom:25px;",
                                 onclick = sprintf("Shiny.setInputValue('%s', new Date().getTime());", paste0("img_", id83_imgs[1]))))
              },
              if (length(id83_imgs) >= 2 && file.exists(file.path("www", id83_imgs[2]))) {
                tagList(div(class = "img-label", "Sample A in COSMIC83 representation"),
                        tags$img(src = id83_imgs[2], class = "signature-img", style = "max-width:700px; width:100%;",
                                 onclick = sprintf("Shiny.setInputValue('%s', new Date().getTime());", paste0("img_", id83_imgs[2]))))
              }
          )
        },
        
        # Koh476
        if (length(id476_imgs) >= 1 && file.exists(file.path("www", id476_imgs[1]))) {
          div(class = "img-container",
              div(class = "img-section-title", "Koh476 Signature"),
              div(class = "img-label", "Extended Signature Set"),
              p(style = "font-size: 13px; color: #7f8c8d; margin-top: -5px; margin-bottom: 10px;",
                icon("mouse-pointer"), " Right-click and open in new tab for full view"
              ),
              tags$img(src = id476_imgs[1], class = "signature-img", style = "max-width:100%; width:100%;",
                       onclick = sprintf("Shiny.setInputValue('%s', new Date().getTime());", paste0("img_", id476_imgs[1])))
          )
        },
        
        if (!is.null(sig$desc)) {
          div(style = "background:#fff3cd; border-left:4px solid #ffc107; padding:15px; border-radius:8px; margin-top:20px;",
              icon("info-circle"), " ", sig$desc
          )
        }
      )
    }
  })
  
  # ---------------- ID83 Classification 页面----------------
  output$id83_display <- renderUI({
    
    if (is.null(current_id83())) {
      # === 列表视图 (网格对齐) ===
      
      all_names <- names(id83_groups)
      if (length(all_names) == 0) return(NULL)
      
      # 分组逻辑：每行 4 个项目
      chunk_size <- 4
      id_chunks <- split(all_names, ceiling(seq_along(all_names) / chunk_size))
      
      tagList(
        lapply(id_chunks, function(chunk_names) {
          fluidRow(
            style = "margin-bottom: 20px;", # 行间距
            lapply(chunk_names, function(id83_name) {
              
              # 获取组数据
              id83_info <- id83_groups[[id83_name]]
              members_text <- paste(id83_info$members, collapse = ", ")
              thumbnail_path <- id83_info$thumbnail
              
              column(3,
                     div(class = "thumbnail-card",
                         # 卡片样式：固定高度 (280px) 
                         style = "background: #fff; border: 1px solid #ddd; border-radius: 8px; padding: 15px; height: 280px; overflow-y: auto; box-shadow: 0 2px 5px rgba(0,0,0,0.05); transition: box-shadow 0.3s;",
                         onmouseover = "this.style.boxShadow='0 5px 15px rgba(0,0,0,0.2)'",
                         onmouseout = "this.style.boxShadow='0 2px 5px rgba(0,0,0,0.05)'",
                         
                         actionLink(
                           inputId = paste0("show_id83_", id83_name),
                           label = tagList(
                             # 1. 标题
                             h4(id83_name, style = "color:#27ae60; margin-top:0; font-weight:700; text-align: center;"),
                             
                             # 2. 图片容器
                             div(style = "height: 150px; display: flex; align-items: center; justify-content: center; margin-bottom: 10px; background: #f9f9f9; border-radius: 4px;",
                                 if (!is.null(thumbnail_path) && length(thumbnail_path) > 0 && file.exists(file.path("www", thumbnail_path))) {
                                   tags$img(src = thumbnail_path, style = "max-height: 100%; max-width: 100%; border-radius: 4px;")
                                 } else {
                                   div(style = "color:#ccc; text-align: center;", icon("image", style = "font-size:32px; display: block;"), tags$small("No Image"))
                                 }
                             ),
                             
                             # 3. 成员列表
                             div(style = "background:#f4f6f7; padding:8px; border-radius:4px; text-align:left;",
                                 div(style = "font-size:11px; color:#7f8c8d; margin-bottom:5px; font-weight:bold;", "Corresponds to:"),
                                 div(style = "font-size:12px; color:#34495e; line-height:1.4;", members_text)
                             )
                           ),
                           style = "text-decoration: none; color: inherit; display: block;"
                         )
                     )
              )
            })
          )
        })
      )
      
    } else {
      # === 详情视图 ===
      
      id83_info <- id83_groups[[current_id83()]]
      members <- id83_info$members
      id83_all_img <- id83_info$id83_all
      
      tagList(
        actionButton("back_to_id83_list", "← Back to ID83 List", class = "btn-back", style = "margin-bottom:20px;"),
        h2(paste("ID83:", current_id83()), style = "color:#27ae60; font-weight:600; margin-bottom:25px;"),
        
        # ID83 Signature 图片
        div(class = "id83-section",
            div(class = "id83-label", icon("layer-group"), " Signature Spectrum"),
            if (!is.null(id83_all_img) && length(id83_all_img) > 0 && file.exists(file.path("www", id83_all_img))) {
              tags$img(src = id83_all_img, class = "signature-img", style = "max-width:700px; width:100%;",
                       onclick = sprintf("Shiny.setInputValue('%s', new Date().getTime());", paste0("img_", id83_all_img)))
            } else {
              div(style = "color:#95a5a6; text-align:center; padding:40px;", icon("image", style = "font-size:48px;"), br(), "No image available")
            }
        ),
        
        # 成员详细信息
        div(class = "id83-section",
            div(class = "id83-label", icon("dna"), " Member Signatures"),
            lapply(members, function(member_name) {
              sig <- signature_groups[[member_name]]
              if (is.null(sig)) return(NULL)
              
              koh89_spectrum <- if (length(sig$imgs) >= 1) sig$imgs[1] else NULL
              koh89_sampleA <- if (length(sig$imgs) >= 2) sig$imgs[2] else NULL
              cosmic83_filtered <- if (length(sig$id83) >= 2) sig$id83[2] else NULL
              
              div(class = "member-section",
                  div(class = "member-name", icon("chevron-right"), " ", member_name),
                  
                  # 病因推测 (仅在详情视图显示)
                  if (!is.null(sig$aetiology) && !is.na(sig$aetiology) && nchar(sig$aetiology) > 0) {
                    div(style = "background:#fff3cd; padding:12px; border-radius:6px; margin-bottom:15px; border-left:3px solid #ffc107;",
                        tags$strong(style = "color:#856404; font-size:12px;", icon("lightbulb"), " Proposed Aetiology: "),
                        tags$span(style = "color:#856404; font-size:13px;", sig$aetiology)
                    )
                  },
                  
                  # 成员图片
                  fluidRow(
                    if (!is.null(koh89_spectrum) && file.exists(file.path("www", koh89_spectrum))) {
                      column(4, div(class = "img-label", "Koh89 Spectrum"), tags$img(src = koh89_spectrum, class = "signature-img", style = "width:100%;", onclick = sprintf("Shiny.setInputValue('%s', new Date().getTime());", paste0("img_", koh89_spectrum))))
                    },
                    if (!is.null(koh89_sampleA) && file.exists(file.path("www", koh89_sampleA))) {
                      column(4, div(class = "img-label", "Koh89 Sample A"), tags$img(src = koh89_sampleA, class = "signature-img", style = "width:100%;", onclick = sprintf("Shiny.setInputValue('%s', new Date().getTime());", paste0("img_", koh89_sampleA))))
                    },
                    if (!is.null(cosmic83_filtered) && file.exists(file.path("www", cosmic83_filtered))) {
                      column(4, div(class = "img-label", "Sample A (COSMIC83)"), tags$img(src = cosmic83_filtered, class = "signature-img", style = "width:100%;", onclick = sprintf("Shiny.setInputValue('%s', new Date().getTime());", paste0("img_", cosmic83_filtered))))
                    }
                  )
                 )
            })
        )
      )
    }
  })
  
  # ---------------- Event Handlers ----------------
  lapply(names(signature_groups), function(group_name) {
    observeEvent(input[[paste0("show_", group_name)]], {
      current_group(group_name)
    })
  })
  
  observeEvent(input$back_to_list, {
    current_group(NULL)
  })
  
  lapply(names(id83_groups), function(id83_name) {
    observeEvent(input[[paste0("show_id83_", id83_name)]], {
      current_id83(id83_name)
    })
  })
  
  observeEvent(input$back_to_id83_list, {
    current_id83(NULL)
  })
  
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
    
    # 去除空值
    all_imgs <- unique(all_imgs[!is.na(all_imgs) & nzchar(all_imgs)])
    
    # 2. 为每张图片绑定点击事件
    lapply(all_imgs, function(img) {
      observeEvent(input[[paste0("img_", img)]], ignoreInit = TRUE, {
        
        display_title <- "Signature View"
        
        if (grepl("signature\\.89spectrum", img)) {
          display_title <- "Koh89 Signature Spectrum"
        } else if (grepl("_89spectrumA", img)) {
          display_title <- "Koh89 Sample A (Original Spectrum)"
        } else if (grepl("_89spectrumB", img)) {
          display_title <- "Koh89 Sample B (Reconstructed)"
        } else if (grepl("_89spectrumC", img)) {
          display_title <- "Koh89 Sample A-B (Residual)"
        } else if (grepl("_83all", img)) {
          display_title <- "COSMIC83 Signature Spectrum"
        } else if (grepl("_83filtered", img)) {
          display_title <- "Sample A in COSMIC83 Representation"
        } else if (grepl("_476all", img)) {
          display_title <- "Koh476 Extended Signature"
        } else if (grepl("Thumbnail", img)) {
          display_title <- "Group Thumbnail"
        }
        showModal(modalDialog(
          title = display_title,
          easyClose = TRUE,
          size = "l",
          footer = NULL,
          tags$img(src = img, style = "width:100%; height:auto; border-radius:8px;box-shadow: 0 4px 12px rgba(0,0,0,0.15);")
        ))
      })
    })
  })
}