id89_df  <- read.delim("./ID89_ID83_connection_example.txt",
                       header = TRUE,
                       stringsAsFactors = FALSE)

id476_df <- read.csv("./mSigHdp.indel476.final.signatures.csv",
                     header = TRUE, check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL)

ID476_list <- colnames(id476_df)
all_pngs <- list.files("./www", pattern = "\\.png$", full.names = FALSE)
signature_groups <- list()

for (i in seq_len(nrow(id89_df ))) {
  ID89 <- id89_df $InDel89[i]
  ID83 <- id89_df $InDel83[i]
  ID476 <- if (i <= length(ID476_list)) ID476_list[i] else NA_character_
  
  # 读取 Proposed aetiology（假设在第4列）
  # 根据你的实际列名调整，可能是 id89_df $Aetiology 或 id89_df [,4]
  aetiology <- if (ncol(id89_df ) >= 4) id89_df [i, 4] else"Unknown"
  
  imgs <- paste0(ID89, c("_signature.89spectrum.png","_89spectrumA.png","_89spectrumB.png","_89spectrumC.png"))
  id83_imgs <- paste0(ID89, "_", ID83, c("_83all.png", "_83filtered.png"))
  id476_imgs <- grep(paste0("^", ID89, "_476all.*\\.png$"), all_pngs, value = TRUE, ignore.case = TRUE)
  
  if (ID89 == "InsDel39") {
    signature_groups[[ID89]] <- list(
      imgs = imgs, id83 = character(0), id83_name = NA_character_,
      id476 = if (length(id476_imgs) > 0) id476_imgs else character(0),
      thumbnail = paste0(ID89, "Thumbnail.png"),
      aetiology = aetiology,  # 添加 aetiology
      desc = "InsDel39 does not have corresponding COSMIC ID83 signature"
    )
  } else {
    signature_groups[[ID89]] <- list(
      imgs = imgs, id83 = id83_imgs, id83_name = ID83,
      id476 = if (length(id476_imgs) > 0) id476_imgs else character(0),
      thumbnail = paste0(ID89, "Thumbnail.png"),
      aetiology = aetiology  # 添加 aetiology
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
      id83_groups[[id83_key]] <- list(
        members = character(), 
        id83_all = character(),
        thumbnail = character()
      )
    }
    id83_groups[[id83_key]]$members <- c(id83_groups[[id83_key]]$members, group_name)
    
    if (length(id83_groups[[id83_key]]$id83_all) == 0 && length(sig$id83) > 0) {
      id83_groups[[id83_key]]$id83_all <- sig$id83[1]
    }
    
    # 保存第一个成员的缩略图
    if (length(id83_groups[[id83_key]]$thumbnail) == 0) {
      thumbnail_name <- paste0(group_name, "_", id83_key, "_83allthumbnail.png")
      if (file.exists(file.path("www/id83thumbnail", thumbnail_name))) {
        id83_groups[[id83_key]]$thumbnail <- file.path("id83thumbnail", thumbnail_name)
      }
    }
  }
} 

server <- function(input, output, session){
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
  
  # ---------------- Search 页面 ----------------
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
    
    # ---- 在 ID89 Explorer 中匹配 ----
    matched89 <- grep(search_term, names(signature_groups),
                      ignore.case = TRUE, value = TRUE)
    
    # ---- 在 ID83 Explorer 中匹配（查成员 ID89 名）----
    matched83_found <- character(0)
    for (g in names(id83_groups)) {
      members <- id83_groups[[g]]$members
      if (any(grepl(search_term, members, ignore.case = TRUE))) {
        matched83_found <- c(matched83_found, g)
      }
    }
    matched83_names <- grep(search_term, names(id83_groups), 
                            ignore.case = TRUE, value = TRUE)
    
    matched83 <- unique(c(matched83_found, matched83_names))
    # ---- 完全没找到 ----
    if (length(matched89) == 0 && length(matched83) == 0) {
      showModal(modalDialog(
        title = "Signature Not Found",
        paste0("'", search_term, "' not found in ID89 or ID83."),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return()
    }
    
    # ---- 同时匹配 ID89 + ID83 → 弹框让用户选 ----
    if (length(matched89) > 0 && length(matched83) > 0) {
      choices <- c(
        paste0(matched89, "  →  Koh ID89 Explorer"),
        paste0(matched83, "  →  COSMIC ID83 Explorer")
      )
      
      showModal(modalDialog(
        title = "Multiple Matches Found",
        tags$p(paste0("The signature '", search_term, "' appears in both Explorers.")),
        radioButtons("select_search", label = NULL, choices = choices),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_search", "Go", class = "btn-primary")
        )
      ))
      return()
    }
    
    # ---- 只在 ID89 里匹配 ----
    if (length(matched89) > 0) {
      updateNavbarPage(session, "navbar", selected = "Koh ID89 Explorer")
      current_group(matched89[1])
      updateTextInput(session, "search_input", value = "")
      return()
    }
    
    # ---- 只在 ID83 里匹配 ----
    if (length(matched83) > 0) {
      updateNavbarPage(session, "navbar", selected = "COSMIC ID83 Explorer")
      current_id83(matched83[1])
      updateTextInput(session, "search_input", value = "")
      return()
    }
  }
  
  observeEvent(input$confirm_search, {
    choice <- input$select_search
    if (is.null(choice)) return()
    
    # 用户选择跳到 ID89 Explorer
    if (grepl("Koh ID89 Explorer$", choice)) {
      sig89 <- sub("  →  Koh ID89 Explorer$", "", choice)
      updateNavbarPage(session, "navbar", selected = "Koh ID89 Explorer")
      current_group(sig89)
    }
    
    # 用户选择跳到 ID83 Explorer
    if (grepl("COSMIC ID83 Explorer$", choice)) {
      sig83 <- sub("  →  COSMIC ID83 Explorer$", "", choice)
      updateNavbarPage(session, "navbar", selected = "COSMIC ID83 Explorer")
      current_id83(sig83)
    }
    
    updateTextInput(session, "search_input", value = "")
    removeModal()
  })
  observeEvent(input$search_btn, {
    search_signature(input$search_input)
  })
  
  # ---------------- Signature Explorer 页面 (Koh ID89) ----------------
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
                                  class = "btn btn-info")
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
        
        # 显示 Proposed aetiology（新增）
        if (!is.null(sig$aetiology) && !is.na(sig$aetiology) && nchar(sig$aetiology) > 0) {
          div(style = "background: linear-gradient(135deg, #e8f5e9 0%, #c8e6c9 100%); 
                    padding:20px; border-radius:12px; margin-bottom:25px; 
                    border-left:5px solid #2ecc71; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
              div(style = "display:flex; align-items:center; margin-bottom:10px;",
                  icon("lightbulb", style = "font-size:24px; color:#27ae60; margin-right:12px;"),
                  tags$span(style = "font-size:16px; color:#27ae60; font-weight:700; 
                             text-transform:uppercase; letter-spacing:1px;",
                            "Proposed Aetiology"
                  )
              ),
              tags$p(style = "font-size:16px; color:#2c3e50; line-height:1.8; margin:0;",
                     sig$aetiology
              )
          )
        },
        
        # Koh89 Spectrum
        if (length(id89_imgs) >= 1 && file.exists(file.path("www", id89_imgs[1]))) {
          div(class = "img-container",
              div(class = "img-section-title", "Koh89 Signature"),
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
              p(class = "text-muted",
                style = "margin-top: -8px; margin-bottom: 12px; color:#6c757d; font-size: 12px;",
                "Click an image to view details. Samples A is the example tumor spectrum; Sample B is the partial spectrum contributed by all other signatures; Sample A-B is the difference."
              ),
              fluidRow(
                lapply(seq_along(id89_imgs[-1]), function(i){
                  nm <- c("A","B","A-B")[i]
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
              div(class = "img-section-title", "COSMIC83 Signature"),
              if (length(id83_imgs) >= 1 && file.exists(file.path("www", id83_imgs[1]))) {
                tagList(
                  div(class = "img-label", "Siganture Spectrum"),
                  tags$img(src = id83_imgs[1], class = "signature-img",
                           style = "max-width:700px; width:100%; margin-bottom:25px;",
                           onclick = sprintf("Shiny.setInputValue('%s', new Date().getTime());", 
                                             paste0("img_", id83_imgs[1])))
                )
              },
              if (length(id83_imgs) >= 2 && file.exists(file.path("www", id83_imgs[2]))) {
                tagList(
                  div(class = "img-label", "Sample A in COSMIC83 representation"),
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
              div(class = "img-section-title", "Koh476 Signature"),
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
  
  # ---------------- ID83 Explorer 页面 ----------------
  output$id83_display <- renderUI({
    if (is.null(current_id83())) {
      fluidRow(
        lapply(names(id83_groups), function(id83_name){
          id83_info <- id83_groups[[id83_name]]
          members_text <- paste(id83_info$members, collapse = ", ")
          thumbnail_path <- id83_info$thumbnail 
          
          # 彩色标签生成
          member_badges <- lapply(id83_info$members, function(m) {
            tags$span(m, style = "display:inline-block; background-color:#3498db !important; color:#ffffff !important; padding:3px 8px; border-radius:12px; font-size:11px; margin-right:4px; margin-bottom:4px; font-weight:600; box-shadow: 0 2px 4px rgba(52, 152, 219, 0.2);")
          })
          
          column(3,
                 div(class = "thumbnail-card",
                     # 使用 actionLink 包裹
                     actionLink(inputId = paste0("show_id83_", id83_name),
                                label = tagList(
                                  
                                  h4(id83_name, style = "color:#27ae60 !important; margin-top:0; font-weight:700;"),
                                  
                                  if(!is.null(thumbnail_path) && length(thumbnail_path) > 0 && file.exists(file.path("www", thumbnail_path))){
                                    tags$img(src = thumbnail_path, style = "width:100%; max-width:200px; height:auto; margin-bottom:15px; border-radius:8px; border:2px solid #e8eaed; cursor:pointer;")
                                  } else {
                                    div(style = "height:120px; line-height:120px; color:#95a5a6; margin-bottom:15px; cursor:pointer;", 
                                        icon("image", style = "font-size:48px;"), br(), tags$small("No Thumbnail"))
                                  },
                                  
                                  # 2. Members 区块
                                  div(style = "background:#ecf0f1 !important; padding:15px; border-radius:8px; margin-bottom:15px; text-align:left;",
                                      
                                      div(style = "font-size:11px; color:#7f8c8d !important; margin-bottom:5px;", "MEMBERS"),
                                      
                                      # 这里放回 members_text
                                      div(style = "font-size:12px; color:#34495e !important; font-weight:500; line-height:1.4;", members_text)
                                  )
                                ),
                                style = "text-decoration: none; color: inherit; display: block;"
                     )
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
            div(class = "id83-label", icon("layer-group"), " Signature Spectrum"),
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
            div(class = "id83-label", icon("dna"), " Koh89 Signature"),
            lapply(members, function(member_name){
              sig <- signature_groups[[member_name]]
              if (is.null(sig)) return(NULL)
              
              koh89_spectrum <- if (length(sig$imgs) >= 1) sig$imgs[1] else NULL
              koh89_sampleA <- if (length(sig$imgs) >= 2) sig$imgs[2] else NULL
              cosmic83_filtered <- if (length(sig$id83) >= 2) sig$id83[2] else NULL
              
              div(class = "member-section",
                  div(class = "member-name", icon("chevron-right"), " ", member_name),
                  
                  # 显示 aetiology（新增）
                  if (!is.null(sig$aetiology) && !is.na(sig$aetiology) && nchar(sig$aetiology) > 0) {
                    div(style = "background:#fff3cd; padding:12px; border-radius:6px; 
                    margin-bottom:15px; border-left:3px solid #ffc107;",
                    tags$strong(style = "color:#856404; font-size:12px;",
                                icon("lightbulb"), " Proposed Aetiology: "
                    ),
                    tags$span(style = "color:#856404; font-size:13px;",
                              sig$aetiology
                    )
                    )
                  },
                  
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
                             div(class = "img-label", "Sample A in COSMIC83 representation"),
                             tags$img(src = cosmic83_filtered, class = "signature-img",
                                      style = "max-width:100%; width:100%;",
                                      onclick = sprintf("Shiny.setInputValue('%s', new Date().getTime());", 
                                                        paste0("img_", cosmic83_filtered)))
                      )
                    }
                  )
              )
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