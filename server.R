library(shiny)
library(shinyjs)
library(shinydashboard)
library(readxl)
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)

# ==============================================================================
# 1. 数据加载与预处理 (在 Server 函数外部执行)
# ==============================================================================
data_path_prefix <- "Manuscript_data1.17"
img_subdir <- "parallel_plots1.17/" 

# --- 读取统计摘要表 ---
summary_dir <- "vignette1.17"
stats_filename <- "prot_table_1.csv"
target_path <- file.path(summary_dir, stats_filename)

if (file.exists(target_path)) {
  message("正在从目录加载摘要表：", target_path)
  sig_stats_df <- read.csv(target_path, stringsAsFactors = FALSE)
  if ("type89_sig_id" %in% colnames(sig_stats_df)) sig_stats_df$type89_sig_id <- as.character(sig_stats_df$type89_sig_id)
} else if (file.exists(stats_filename)) {
  message("警告：在根目录找到了摘要表。")
  sig_stats_df <- read.csv(stats_filename, stringsAsFactors = FALSE)
  if ("type89_sig_id" %in% colnames(sig_stats_df)) sig_stats_df$type89_sig_id <- as.character(sig_stats_df$type89_sig_id)
} else {
  sig_stats_df <- NULL
  warning("严重警告：无法找到统计表。")
}

# --- 读取关系表 ---
raw_data <- data.table::fread(file.path(data_path_prefix, "89type_to_83type_connection1.tsv"), data.table = FALSE, fill = TRUE)

id89_df <- raw_data %>%
  dplyr::select(InDel83 = InDel83, InDel89 = InDel89, Aetiology = `Proposed.Etiology`) %>%
  fill(InDel83, .direction = "down") %>%
  dplyr::filter(!is.na(InDel89)) %>%
  mutate(across(c(InDel83, InDel89, Aetiology), as.character))

# 修复希腊字母
id89_df$InDel89 <- gsub("InsDel_Aα", "InsDel_A_alpha", id89_df$InDel89)
id89_df$InDel89 <- gsub("InsDel_Aβ", "InsDel_A_beta", id89_df$InDel89)
id89_df$InDel89 <- gsub("InsDel_Kα", "InsDel_K_alpha", id89_df$InDel89)
id89_df$InDel89 <- gsub("InsDel_Kβ", "InsDel_K_beta", id89_df$InDel89)

# ==============================================================================
# [构建 signature_groups]
# ==============================================================================
signature_groups <- list()

img_dir_full_path <- file.path("www", img_subdir)
if (!dir.exists(img_dir_full_path)) {
  warning(paste("警告：找不到图片目录:", img_dir_full_path))
  all_pngs <- character(0)
} else {
  all_pngs <- list.files(img_dir_full_path, pattern = "\\.png$", full.names = FALSE)
}

for (i in seq_len(nrow(id89_df))) {
  ID89 <- id89_df$InDel89[i]
  ID83 <- id89_df$InDel83[i]
  if (is.na(ID83)) ID83 <- "Unknown"
  aetiology <- id89_df$Aetiology[i]
  if (is.na(aetiology)) aetiology <- ""
  
  # --- 读取 Note ---
  md_file_path <- file.path(data_path_prefix, "per_sig_txt", paste0(ID89, ".md"))
  note_content <- NULL
  if (file.exists(md_file_path)) {
    note_lines <- readLines(md_file_path, warn = FALSE)
    if (length(note_lines) > 0) note_content <- paste(note_lines, collapse = "\n")
  }
  
  # --- 读取 ID83 Note ---
  note_content_83 <- NULL
  if (ID83 != "Unknown") {
    md_file_path_83 <- file.path(data_path_prefix, "per_sig_txt", paste0(ID83, ".md"))
    if (file.exists(md_file_path_83)) {
      note_lines_83 <- readLines(md_file_path_83, warn = FALSE)
      if (length(note_lines_83) > 0) note_content_83 <- paste(note_lines_83, collapse = "\n")
    }
  }
  
  safe_name <- gsub("[^a-zA-Z0-9_]", "_", ID89)
  
  # --- 寻找图片 ---
  img_89_top_path <- NULL
  top_filename <- paste0(safe_name, "_89-1.png") 
  if (file.exists(file.path("www", "89", top_filename))) {
    img_89_top_path <- file.path("89", top_filename)
  }
  
  find_one_img <- function(suffix) {
    fname <- paste0(safe_name, suffix)
    if (fname %in% all_pngs) return(paste0(img_subdir, fname)) else return(NULL)
  }
  
  find_matches <- function(type_pattern) {
    pattern <- paste0("^", safe_name, type_pattern, ".*\\.png$")
    matches <- grep(pattern, all_pngs, value = TRUE)
    if (length(matches) > 0) return(sort(paste0(img_subdir, matches))) else return(character(0))
  }
  
  img_89_sig      <- find_one_img("_id89_sig.png")
  img_89_mapped   <- find_one_img("_id89_mapped.png")
  img_89_cat      <- find_one_img("_id89_catalog.png")
  img_89_target   <- find_one_img("_id89_target_sig_partial_spectrum.png")
  img_89_residual <- find_one_img("_id89_residual.png")
  id89_decomp_list <- c(img_89_cat, img_89_target, img_89_residual)
  id89_decomp_list <- id89_decomp_list[!is.null(id89_decomp_list)]
  
  img_koh_matches <- find_matches("_koh_")
  
  # 476 图片
  img_476_sig     <- find_one_img("_id476_sig.png")
  img_476_cat     <- find_one_img("_id476_catalog.png")
  id476_list <- c(img_476_sig, img_476_cat)
  id476_list <- id476_list[!is.null(id476_list)]
  
  # 476 缩略图
  id476_thumb <- find_one_img("_id476_Thumbnail.png")
  
  img_83_sig      <- find_one_img("_id83_sig.png")
  img_83_mapped   <- find_one_img("_id83_mapped.png")
  img_83_cat      <- find_one_img("_id83_catalog.png")
  
  img_83_sig_abl    <- find_one_img("_id83_sig_ablated.png")
  img_83_mapped_abl <- find_one_img("_id83_mapped_ablated.png")
  img_83_cat_abl    <- find_one_img("_id83_catalog_ablated.png")
  
  img_cosmic_matches <- find_matches("_cosmic_")
  img_jin_matches    <- find_matches("_jin_")
  final_thumb <- find_one_img("_Thumbnail.png")
  
  signature_groups[[ID89]] <- list(
    img_89_top = img_89_top_path,
    id89_sig = img_89_sig, id89_mapped = img_89_mapped, id89_decomp = id89_decomp_list, koh_matches = img_koh_matches,
    id476 = id476_list, 
    id476_sig_only = img_476_sig,
    id476_thumb = id476_thumb,
    id83_name = ID83,
    id83_sig = img_83_sig, id83_mapped = img_83_mapped, id83_cat = img_83_cat,
    id83_sig_abl = img_83_sig_abl, id83_mapped_abl = img_83_mapped_abl, id83_cat_abl = img_83_cat_abl,
    cosmic_matches = img_cosmic_matches, jin_matches = img_jin_matches,
    thumbnail = final_thumb, aetiology = aetiology, 
    note = note_content,
    note_id83 = note_content_83 
  )
}

# --- 构建 id83_groups ---
id83_groups <- list()
for (i in seq_len(nrow(id89_df))) {
  raw_id83 <- id89_df$InDel83[i]
  raw_id89 <- id89_df$InDel89[i]
  if (is.na(raw_id83) || raw_id83 == "Unknown") next
  id83_key <- trimws(as.character(raw_id83))
  
  if (is.null(id83_groups[[id83_key]])) {
    md_file_path_83 <- file.path(data_path_prefix, "per_sig_txt", paste0(id83_key, ".md"))
    note_content_83 <- NULL
    if (file.exists(md_file_path_83)) note_content_83 <- paste(readLines(md_file_path_83, warn = FALSE), collapse = "\n")
    id83_groups[[id83_key]] <- list(members = character(), id83_all = NULL, thumbnail = NULL, note = note_content_83)
  }
  
  if (!raw_id89 %in% id83_groups[[id83_key]]$members) id83_groups[[id83_key]]$members <- c(id83_groups[[id83_key]]$members, raw_id89)
  if (is.null(id83_groups[[id83_key]]$id83_all)) id83_groups[[id83_key]]$id83_all <- signature_groups[[raw_id89]]$id83_sig
  
  if (is.null(id83_groups[[id83_key]]$thumbnail)) {
    id83_groups[[id83_key]]$thumbnail <- paste0(img_subdir, id83_key, "_Thumbnail.png")
  }
}

# ==============================================================================
# 2. Server 函数
# ==============================================================================

server <- function(input, output, session) {
  
  observe({ runjs("$('.sidebar-menu li').removeClass('active');") })
  
  
  current_integrated_sig <- reactiveVal(NULL)
  
  # ============================================================================
  # NEW: 纯手工 URL 路由机制 (精简版：只认一个状态变量)
  # ============================================================================
  
  # 1. 监听状态并推送到地址栏
  observe({
    nav <- input$navbar
    id <- current_integrated_sig() # 现在的核心变量只有这一个
    
    qs <- paste0("?nav=", URLencode(nav, reserved = TRUE))
    if (!is.null(id)) {
      qs <- paste0(qs, "&id=", URLencode(id, reserved = TRUE))
    }
    
    current_qs <- session$clientData$url_search
    if (qs != current_qs) {
      updateQueryString(qs, mode = "push")
    }
  })
  
  # 2. 监听地址栏后退按钮并恢复状态
  observeEvent(session$clientData$url_search, {
    query <- getQueryString()
    
    if (length(query) == 0) {
      current_integrated_sig(NULL)
    } else {
      # 恢复 Tab
      if (!is.null(query$nav) && isolate(input$navbar) != query$nav) {
        updateNavbarPage(session, "navbar", selected = query$nav)
      }
      # 恢复详情页状态
      if (!is.null(query$id)) {
        current_integrated_sig(query$id)
      } else {
        current_integrated_sig(NULL)
      }
    }
  })
  
  # ============================================================================
  # 通用跳转逻辑
  # ============================================================================
  jump_to_tab <- function(tab_name, set_group_fn = NULL) {
    updateNavbarPage(session, "navbar", selected = tab_name)
    if (!is.null(set_group_fn)) set_group_fn()
  }
  
  observeEvent(input$home_goto_89, { updateNavbarPage(session, "navbar", selected = "89-type classification") })
  observeEvent(input$home_goto_83, { updateNavbarPage(session, "navbar", selected = "83-type classification") })
  observeEvent(input$home_goto_476, { updateNavbarPage(session, "navbar", selected = "476-type classification") })
  
  # ============================================================================
  # 辅助函数 (移到 server 内部顶层，避免在 renderUI 中重复定义)
  # ============================================================================
  img_block <- function(img_path, width="100%", border=TRUE) {
    if (is.null(img_path) || is.na(img_path)) return(NULL)
    if(length(img_path) > 1) img_path <- img_path[1] 
    div(style = "text-align: center; margin-bottom: 10px;",
        tags$img(src = img_path, class = "signature-img",
                 style = paste0("width: ", width, "; max-width: 900px; ", if(border) "border: 1px solid #ddd; padding: 2px;" else ""),
                 onclick = sprintf("Shiny.setInputValue('%s', new Date().getTime());", paste0("img_", basename(img_path))))
    )
  }
  
  render_83_pair_block <- function(title, std_img, abl_img, caution_text = NULL) {
    tagList(
      h4(title, style = "color: #2c3e50; font-weight: 600; margin-top: 25px; margin-bottom: 15px;"),
      if (!is.null(caution_text)) {
        div(style = "background-color: #fff3cd; border-left: 5px solid #ffc107; padding: 10px 15px; border-radius: 4px; margin-bottom: 15px;",
            div(style = "color: #856404; font-weight: bold; margin-bottom: 5px;", icon("exclamation-triangle"), " Caution"),
            div(style = "color: #333; font-size: 14px; line-height: 1.4;", caution_text)
        )
      },
      if (!is.null(std_img)) img_block(std_img) else div(style="color:#999; font-style:italic;", "Image not available"),
      if (!is.null(abl_img)) {
        tags$details(
          style = "margin-bottom: 20px; border: 1px solid #eee; border-radius: 5px; padding: 10px; background-color: #f9f9f9;",
          tags$summary(
            tags$span("▶ Click here to see with insertions and deletions of T in long poly-T suppressed", 
                      style = "color: #3498db; cursor: pointer; font-weight: bold; user-select: none;")
          ),
          div(style = "margin-top: 15px; border-top: 1px dashed #ddd; padding-top: 10px;", img_block(abl_img))
        )
      }
    )
  }
  
  # ============================================================================
  # 辅助渲染函数：生成带折叠功能的图片对比块
  # ============================================================================
  render_styled_pair_block <- function(title_text, std_img, abl_img, caution_text = NULL) {
    div(class = "id83-section", style = "margin-bottom: 30px; padding: 30px; background: #fff; border: 1px solid #eee; box-shadow: 0 5px 20px rgba(0,0,0,0.03);",
        if(!is.null(caution_text)) caution_text,
        
        # 标题 
        h4(title_text, style = "color: #2c3e50; font-weight: 700; margin-top: 0; margin-bottom: 20px; font-size: 1.2rem;"),
        
        # 1. Standard Scale 图片 (直接显示)
        if (!is.null(std_img)) {
          tags$img(src = std_img, class = "signature-img",
                   onclick = paste0("Shiny.setInputValue('open_modal_image', '", std_img, "', {priority: 'event'})"),
                   style = "width:100%; margin-bottom: 15px;")
        } else { div("Standard scale image not available", style="color:#ccc; padding: 10px;") },
        
        # 2. Abelson Scale 图片 (折叠隐藏)
        if (!is.null(abl_img)) {
          tags$details(
            style = "margin-top: 10px;",
            
            #  箭头图标
            tags$summary(
              style = "cursor: pointer; color: #3498db; font-weight: 500; font-size: 1.35rem; outline: none; user-select: none;",
              # 绿色箭头
              icon("chevron-circle-right", style="color: #27ae60; margin-right: 8px;"), 
              # 蓝色文字
              "Click here to see with insertions and deletions of T in long poly-T suppressed."
            ),
            
            div(style = "margin-top: 15px; border-top: 1px dashed #eee; padding-top: 15px;",
                tags$img(src = abl_img, class = "signature-img",
                         onclick = paste0("Shiny.setInputValue('open_modal_image', '", abl_img, "', {priority: 'event'})"),
                         style = "width:100%;")
            )
          )
        }
    )
  }
  
  # ============================================================================
  # 终极“三合一”综合详情页生成器 (Integrated Page Builder)
  # ============================================================================
  build_integrated_page <- function(sig_name, back_btn_id) {
    sig <- signature_groups[[sig_name]]
    
    current_stats <- if (!is.null(sig_stats_df)) sig_stats_df[sig_stats_df$type89_sig_id == sig_name, ] else NULL
    exemplar_name <- if(!is.null(current_stats)) current_stats$exemplar_id else "Exemplar Sample"
    
    # Poly-T 警告信息
    polyT_sigs <- c("C_ID7", "ID_J", "C_ID10", "ID_N", "ID_O")
    tumor_caution <- if (sig$id83_name %in% polyT_sigs) {
      div(style="font-size: 13px; color: #c0392b; background: #fadbd8; padding: 12px; border-radius: 8px; margin-bottom: 20px; border-left: 5px solid #c0392b; display: flex; align-items: center;",
          icon("triangle-exclamation", style="margin-right: 10px; font-size: 1.2em;"), 
          div("For the supporting tumor plot, mutation counts for insertions and deletions of T in long-poly-T contexts were set to 0. They were also set to 0 when calculating cosine similarity with the signature. The signature was not altered when computing the cosine similarity.")
      )
    } else { NULL }
    
    tagList(
      # 顶部导航 (返回按钮)
      div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 20px;", 
          actionButton(back_btn_id, "← Back to Thumbnails", class = "btn-back"), div()),
      
      # 页面主标题
      h2(paste("Integrated Signature Profile:", sig_name), style = "color:#2c3e50; font-weight:700; margin-top: 0; margin-bottom: 20px; border-bottom: 2px solid #eee; padding-bottom: 10px;"),
      
      # 关联信息框 (合并后不需要跳转链接了，直接展示文本)
      div(style="margin-bottom: 25px; font-size: 15px; background: #f8f9fa; padding: 20px; border-radius: 12px; border-left: 5px solid #34495e; box-shadow: 0 4px 15px rgba(0,0,0,0.05);",
          div(style="margin-bottom: 8px;", tags$span("Base 89-type Unit: ", style="font-weight:bold; color:#7f8c8d; margin-right: 10px;"), tags$span(sig_name, style="color:#e67e22; font-weight:bold;")),
          div(style="margin-bottom: 8px;", tags$span("Associated 476-type: ", style="font-weight:bold; color:#7f8c8d; margin-right: 10px;"), tags$span(sig_name, style="color:#9b59b6; font-weight:bold;")),
          div(tags$span("Associated 83-type Group: ", style="font-weight:bold; color:#7f8c8d; margin-right: 10px;"), tags$span(sig$id83_name, style="color:#27ae60; font-weight:bold;"))
      ),
      
      # Note & Etiology
      if (!is.null(sig$note)) shiny::markdown(sig$note),
      if (nchar(sig$aetiology) > 0) div(style="background:#e8f5e9; padding:15px; border-radius: 8px; margin-bottom: 20px; border-left: 4px solid #2ecc71;", icon("lightbulb"), strong(" Etiology: "), sig$aetiology),
      
      # --- 1. 89-type Classification (橙色) ---
      h3("1. 89-type classification", style = "color: #2c3e50; font-weight: 700; margin-top: 40px; margin-bottom: 20px;"),
      div(class = "id83-section", div(class = "id83-label", style="border-left-color: #e67e22;", icon("dna"), " Signature Profile"), 
          tags$img(src = sig$id89_sig, class = "signature-img", onclick = paste0("Shiny.setInputValue('open_modal_image', '", sig$id89_sig, "', {priority: 'event'})"), style = "width:100%;")),
      
      if (length(sig$id89_decomp) > 0) { 
        tagList(h4("Sample Decomposition Analysis", style = "color: #7f8c8d; margin-top: 20px; font-weight: bold; text-align: center;"), 
                div(class = "id83-section", style="background: #fff;", fluidRow(lapply(seq_along(sig$id89_decomp), function(i) { 
                  lbl <- c("Spectrum (Observed)", "Reconstructed", "Residual")[i]; 
                  column(4, div(class = "img-label", lbl), tags$img(src = sig$id89_decomp[i], class = "signature-img", onclick = paste0("Shiny.setInputValue('open_modal_image', '", sig$id89_decomp[i], "', {priority: 'event'})"), style = "width:100%;"))}))))
      },
      
      # --- 2. 476-type Classification (紫色) ---
      h3("2. 476-type classification", style = "color: #2c3e50; font-weight: 700; margin-top: 40px; margin-bottom: 20px;"),
      if (length(sig$id476) > 0) { 
        div(class = "id83-section", div(class = "id83-label", style="border-left-color: #9b59b6;", icon("microscope"), " 476-type Representations"), 
            lapply(sig$id476, function(p) { label_text <- "476-type Image"; if (grepl("catalog", p, ignore.case = TRUE)) label_text <- "476-type Spectrum" else if (grepl("sig", p, ignore.case = TRUE)) label_text <- "476-type Signature"; 
            div(style = "margin-bottom: 30px;", div(class = "img-label", label_text), tags$img(src = p, class = "signature-img", onclick = paste0("Shiny.setInputValue('open_modal_image', '", p, "', {priority: 'event'})"), style = "width:100%;"))}))
      } else { div(class = "alert alert-warning", "No 476-type representation available.")},
      
      # --- 3. 83-type Classification (绿色) ---
      h3("3. 83-type classification", style = "color: #2c3e50; font-weight: 700; margin-top: 40px; margin-bottom: 30px; border-bottom: 2px solid #eee; padding-bottom: 10px;"),
      if (!is.null(sig$note_id83)) {
        div(style = "background-color: #e3f2fd; padding: 15px; border-radius: 8px; margin-bottom: 20px;", shiny::markdown(sig$note_id83))
      },
      render_styled_pair_block("3.1 83-type signature from de-novo extraction", sig$id83_sig, sig$id83_sig_abl),
      if (!is.null(sig$id83_mapped)) {
        render_styled_pair_block(paste0("3.2 83-type signature converted from 476-type signature corresponding to ", sig_name), sig$id83_mapped, sig$id83_mapped_abl)
      },
      render_styled_pair_block(paste("3.3 83-type spectrum of the supporting tumor", exemplar_name), sig$id83_cat, sig$id83_cat_abl, tumor_caution),
      
      # --- 4. Similarity Summary ---
      if (!is.null(current_stats) && nrow(current_stats) > 0) {
        tagList(
          h3("4. Similarity Summary", style = "color: #2c3e50; font-weight: 700; margin-top: 50px; border-bottom: 2px solid #eee; padding-bottom: 10px;"),
          div(style = "overflow-x: auto; background: white; padding: 25px; border-radius: 12px; box-shadow: 0 4px 15px rgba(0,0,0,0.05);",
              tags$table(class = "table table-hover", style = "width: 100%; margin-top: 10px; font-size: 1.05rem;",
                         tags$thead(tags$tr(style="background:#f8f9fa;", tags$th("Metric"), tags$th("Result / Best Match"), tags$th("Cosine Similarity"))),
                         tags$tbody(
                           tags$tr(tags$td(tags$strong("83-type Representation")), tags$td(sig$id83_name), tags$td(format(current_stats$cosine83, digits=4))),
                           tags$tr(tags$td(tags$strong("476-type Representation")), tags$td("Mapped from 476"), tags$td(format(current_stats$cos_v_collapsed_476, digits=4))),
                           tags$tr(tags$td(tags$strong("Original Sample (89-type)")), tags$td(current_stats$exemplar_id), tags$td(format(current_stats$cosine_89_exemplar, digits=4))),
                           tags$tr(tags$td(tags$strong("Best COSMIC Match")), tags$td(current_stats$best_match_cosmic), tags$td(format(current_stats$cosine_v_cosmic, digits=4))),
                           tags$tr(tags$td(tags$strong("Best Jin Match")), tags$td(current_stats$best_match_jin), tags$td(format(current_stats$cosine_v_jin, digits=4))),
                           tags$tr(tags$td(tags$strong("Best Koh Match")), tags$td(current_stats$best_match_koh), tags$td(format(current_stats$cosine_v_koh, digits=4)))
                         )
              )
          )
        )
      }
    )
  }
  
  # ============================================================================
  # 页面 1: 89-type Classification (列表 / 综合详情页)
  # ============================================================================
  output$signature_display <- renderUI({
    if (is.null(current_integrated_sig())) {
      # 渲染原来的 89 缩略图大厅
      fluidRow(
        lapply(names(signature_groups), function(group_name) {
          sig <- signature_groups[[group_name]]
          thumb <- sig$thumbnail
          column(3,
                 div(class = "thumbnail-card",
                     onclick = paste0("$('#show_", group_name, "').click()"),
                     style = "cursor: pointer; min-height: 220px; display: flex; flex-direction: column; justify-content: space-between;",
                     actionLink(inputId = paste0("show_", group_name), label = NULL, style="display:none;"),
                     h4(group_name, style = "color:#2c3e50; font-weight:700; margin-top:0; margin-bottom: 15px; text-align: center;"),
                     div(style = "flex-grow: 1; display: flex; align-items: center; justify-content: center; background: #fff; border-radius: 8px; overflow: hidden; padding: 5px;",
                         if (!is.null(thumb) && file.exists(file.path("www", thumb))) {
                           tags$img(src = thumb, style = "width:100%; max-height: 150px; object-fit: contain; border-radius: 4px;")
                         } else { div(style = "color:#bdc3c7;", icon("image", class="fa-3x")) }
                     )
                 )
          )
        })
      )
    } else {
      # 调用工厂函数，生成综合页！
      build_integrated_page(current_integrated_sig(), "back_to_list")
    }
  })
  
  # ============================================================================
  # 页面 2: 476-type Classification (列表 / 综合详情页)
  # ============================================================================
  output$id476_display <- renderUI({
    if (is.null(current_integrated_sig())) {
      # 渲染原来的 476 缩略图大厅
      sig_names <- names(signature_groups)
      fluidRow(
        lapply(sig_names, function(name) {
          thumb_path <- signature_groups[[name]]$id476_thumb
          real_thumb_path <- if(!is.null(thumb_path)) file.path("www", thumb_path) else NULL
          column(3,
                 div(class = "thumbnail-card",
                     onclick = sprintf("Shiny.setInputValue('show_476_%s', 1, {priority: 'event'})", name),
                     style = "cursor: pointer; min-height: 200px; display: flex; flex-direction: column; justify-content: space-between;",
                     h4(name, style="color:#2c3e50; font-weight:700; margin-top:0; margin-bottom: 15px; text-align: center;"),
                     div(style = "flex-grow: 1; display: flex; align-items: center; justify-content: center; overflow: hidden; padding: 5px;",
                         if (!is.null(real_thumb_path) && file.exists(real_thumb_path)) {
                           tags$img(src = thumb_path, style = "max-height: 120px; max-width: 100%; border-radius: 4px;")
                         } else { div(style="font-size: 32px; color: #bdc3c7;", icon("border-all")) }
                     ),
                     actionLink(inputId = paste0("btn_show_476_", name), label = NULL, style="display:none;")
                 )
          )
        })
      )
    } else {
      # 调用工厂函数，生成综合页！
      build_integrated_page(current_integrated_sig(), "back_to_476_list")
    }
  })
  
  # ============================================================================
  # 页面 3: 83-type Classification (列表 / 综合详情页)
  # ============================================================================
  output$id83_display <- renderUI({
    if (is.null(current_integrated_sig())) {
      # 渲染原来的 83 缩略图大厅
      all_names <- names(id83_groups)
      if (length(all_names) == 0) return(NULL)
      chunk_size <- 4
      id_chunks <- split(all_names, ceiling(seq_along(all_names) / chunk_size))
      
      tagList(
        lapply(id_chunks, function(chunk_names) {
          fluidRow(
            lapply(chunk_names, function(id83_name) {
              id83_info <- id83_groups[[id83_name]]
              thumb <- id83_info$thumbnail
              column(3,
                     div(class = "thumbnail-card",
                         onclick = paste0("$('#show_id83_", id83_name, "').click()"),
                         style = "cursor: pointer; min-height: 280px; display: flex; flex-direction: column; justify-content: space-between;",
                         actionLink(inputId = paste0("show_id83_", id83_name), label = NULL, style="display:none;"),
                         h4(id83_name, style = "color:#2c3e50; margin-top:0; font-weight:700; text-align: center; margin-bottom: 15px;"),
                         div(style = "flex-grow: 1; display: flex; align-items: center; justify-content: center; overflow: hidden; background: #fff; border-radius: 4px; padding: 5px; margin-bottom: 10px;",
                             if (!is.null(thumb) && file.exists(file.path("www", thumb))) {
                               tags$img(src = thumb, style = "max-height: 120px; max-width: 100%; border-radius: 4px;")
                             } else { div(style = "color:#bdc3c7; text-align: center;", icon("image", class="fa-2x"), br(), tags$small("No Image")) }
                         ),
                         div(style = "background:#f8f9fa; padding:8px; border-radius:4px; text-align:left; border: 1px solid #eee;",
                             div(style = "font-size:11px; color:#95a5a6; margin-bottom:3px; font-weight:bold; text-transform:uppercase;", "Members:"),
                             div(style = "font-size:12px; color:#34495e; line-height:1.4; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;", 
                                 paste(id83_info$members, collapse = ", "))
                         )
                     )
              )
            })
          )
        })
      )
    } else {
      # 调用工厂函数，生成综合页！
      build_integrated_page(current_integrated_sig(), "back_to_id83_list")
    }
  })
  
  
  # ============================================================================
  # 其他逻辑
  # ============================================================================
  
  # ==============================================================================
  # 1. 初始化搜索数据 (Client-side 模式以获得更好的即时反馈)
  # ==============================================================================
  observe({
    # 准备所有可能的 Signature 名字
    # 分组显示，体验更好
    search_choices <- list(
      "89-Type Signatures" = names(signature_groups),
      "83-Type Signatures" = names(id83_groups)
    )
    
    # server = FALSE
    # 这让搜索选项直接加载到浏览器，结合 UI 端的 create=TRUE，
    # 允许用户输入不在列表里的文字（实现模糊搜素的基础）
    updateSelectizeInput(session, "search_input", 
                         choices = search_choices, 
                         selected = character(0),
                         server = FALSE) 
  })
  
  # ==============================================================================
  # 2. 核心搜索逻辑 (支持模糊搜索 + 弹窗选择)
  # ==============================================================================
  # 定义一个处理函数，方便复用
  search_logic <- function(query) {
    req(query)
    query <- trimws(query) # 去掉首尾空格
    
    # --- 步骤 A: 准备全量数据 ---
    names89 <- names(signature_groups)
    names83 <- names(id83_groups)
    
    # --- 步骤 B: 尝试精准匹配 (Exact Match) ---
    # 如果用户输入的名字完全等于某个 Signature，直接跳转
    if (query %in% names89) {
      updateNavbarPage(session, "navbar", selected = "89-type classification")
      current_integrated_sig(query)
      return()
    }
    if (query %in% names83) {
      updateNavbarPage(session, "navbar", selected = "83-type classification")
      current_integrated_sig(query)
      return()
    }
    
    # --- 步骤 C: 模糊匹配 (Fuzzy Match) ---
    # 如果不是精准匹配，我们去库里“捞”一下包含这个词的名字
    matches89 <- grep(query, names89, ignore.case = TRUE, value = TRUE)
    matches83 <- grep(query, names83, ignore.case = TRUE, value = TRUE)
    
    total_matches <- length(matches89) + length(matches83)
    
    # 情况 1: 完全没找到
    if (total_matches == 0) {
      showModal(modalDialog(
        title = "Not Found", 
        paste0("No signatures found matching '", query, "'"), 
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return()
    }
    
    # 情况 2: 瞎猫碰死耗子，刚好只模糊匹配到 1 个 (直接跳转)
    if (total_matches == 1) {
      if (length(matches89) == 1) {
        updateNavbarPage(session, "navbar", selected = "89-type classification")
        current_integrated_sig(matches89)
      } else {
        updateNavbarPage(session, "navbar", selected = "83-type classification")
        current_integrated_sig(matches83)
      }
      return()
    }
    
    # 情况 3: 找到多个 (弹窗让用户选)
    # 这就是你要的“不僵硬”：列出所有可能的选项
    choices_list <- c(
      if(length(matches89)>0) setNames(matches89, paste0(matches89, " (89-type)")),
      if(length(matches83)>0) setNames(matches83, paste0(matches83, " (83-type)"))
    )
    
    showModal(modalDialog(
      title = "Multiple Matches Found",
      "We found several signatures matching your query. Please select one:",
      radioButtons("fuzzy_select", NULL, choices = choices_list),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_fuzzy_search", "Go", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  }
  
  # --- 监听搜索按钮 (Search Button) ---
  observeEvent(input$search_btn, {
    search_logic(input$search_input)
  })
  
  # --- 监听弹窗里的确认按钮 (Go Button) ---
  observeEvent(input$confirm_fuzzy_search, {
    req(input$fuzzy_select)
    removeModal() # 关闭弹窗
    # 拿到用户选中的那个精准名字，再跑一次逻辑 (这次它会进入步骤 B，直接跳转)
    search_logic(input$fuzzy_select)
  })
  
  # ============================================================================
  # NEW: 缩略图点击与跳转逻辑 (核心修改：一对一 vs 一对多)
  # ============================================================================
  
  # 1. 点击 89-type 和 476-type 缩略图 (直接进入三合一综合页)
  lapply(names(signature_groups), function(n) {
    observeEvent(input[[paste0("show_", n)]], { current_integrated_sig(n) })
    observeEvent(input[[paste0("show_476_", n)]], { current_integrated_sig(n) })
    observeEvent(input[[paste0("btn_show_476_", n)]], { current_integrated_sig(n) })
  })
  
  # 2. 点击所有页面的 "Back to List" 按钮
  observeEvent(input$back_to_list, { current_integrated_sig(NULL) })
  observeEvent(input$back_to_476_list, { current_integrated_sig(NULL) })
  observeEvent(input$back_to_id83_list, { current_integrated_sig(NULL) })
  
  # 3. 点击 83-type 缩略图 (弹出选择框)
  lapply(names(id83_groups), function(n) {
    observeEvent(input[[paste0("show_id83_", n)]], {
      members <- id83_groups[[n]]$members
      
      if (length(members) == 1) {
        current_integrated_sig(members[1])
      } else {
        showModal(modalDialog(
          title = paste("Select a signature from", n),
          tags$p("This 83-type signature represents a group containing multiple 89/476-type members. Which specific profile would you like to view?"),
          br(),
          div(style = "display: flex; flex-wrap: wrap; gap: 10px; justify-content: center;",
              lapply(members, function(m) {
                actionButton(paste0("go_to_integrated_", m), m, class = "btn-primary btn-lg")
              })
          ),
          easyClose = TRUE,
          footer = modalButton("Cancel")
        ))
      }
    })
  })
  
  # 4. 监听弹窗按钮点击
  lapply(names(signature_groups), function(m) {
    observeEvent(input[[paste0("go_to_integrated_", m)]], {
      removeModal() 
      current_integrated_sig(m) 
    })
  })
  
  # ==============================================================================
  # 4. 图片查看器逻辑 (Image Viewer - 已适配综合页) 
  # ==============================================================================
  observe({
    all_imgs <- character()
    
    # 因为现在只有综合页，只要 current_integrated_sig() 有值，就把三合一所有的图都收集起来
    if (!is.null(current_integrated_sig())) {
      sig_name <- current_integrated_sig()
      sig <- signature_groups[[sig_name]]
      
      # 收集当前 89 和 476 的图片
      all_imgs <- c(sig$img_89_top, sig$id89_sig, sig$id89_mapped, sig$id89_decomp, sig$koh_matches, sig$id476,
                    sig$id83_sig, sig$id83_mapped, sig$id83_cat,
                    sig$id83_sig_abl, sig$id83_mapped_abl, sig$id83_cat_abl,
                    sig$cosmic_matches, sig$jin_matches)
      
      # 收集它关联的那个 83 的大头图
      if (sig$id83_name != "Unknown") {
        all_imgs <- c(all_imgs, id83_groups[[sig$id83_name]]$id83_all)
      }
    }
    
    # 注册图片点击模态框
    valid_imgs <- unique(all_imgs[!is.null(all_imgs) & !is.na(all_imgs) & all_imgs != ""])
    lapply(valid_imgs, function(p) {
      observeEvent(input[[paste0("img_", basename(p))]], ignoreInit = TRUE, {
        showModal(modalDialog(
          title = "Image View", 
          easyClose = TRUE, 
          size = "l", 
          footer = NULL, 
          tags$img(src = p, style = "width:100%; height:auto;")
        ))
      })
    })
  })
  
  # ============================================================================
  # 监听图片点击并弹出窗口的逻辑
  # ============================================================================
  observeEvent(input$open_modal_image, {
    req(input$open_modal_image) # 确保接收到了图片路径
    
    img_path <- input$open_modal_image
    
    # 打印一下路径，方便调试 (在 RStudio 控制台能看到)
    print(paste("User clicked image:", img_path)) 
    
    showModal(modalDialog(
      title = NULL,
      div(style = "text-align: center;",
          tags$img(src = img_path, 
                   style = "max-width: 100%; max-height: 85vh; border-radius: 8px; box-shadow: 0 5px 15px rgba(0,0,0,0.2);")
      ),
      footer = tagList(
        tags$a(href = img_path, download = basename(img_path), 
               class = "btn btn-primary", icon("download"), "Download", style = "color: white;"),
        modalButton("Close")
      ),
      size = "l",
      easyClose = TRUE,
      fade = TRUE
    ))
  })
}