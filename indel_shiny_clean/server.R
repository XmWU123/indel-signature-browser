library(shiny)
library(shinyjs)
library(shinydashboard)
library(readxl)
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(BSgenome.Hsapiens.1000genomes.hs37d5)
library(BSgenome.Hsapiens.UCSC.hg38)
library(mSigSpectra) # 负责计算和生成矩阵
library(mSigPlot)    # 负责画图
library(R.utils)

# ==============================================================================
# 1. 数据加载与预处理 (路径已适配你现在的 data/ 文件夹)
source("Indel_process.R")
# ==============================================================================

# --- 关键路径定义 ---
data_path_prefix <- "data" 
img_subdir <- "parallel_plots/"

# ==============================================================================
# 网页专用图片路径
#
# signature_groups 中继续保存原始高清图路径：
# parallel_plots/xxx.png
#
# 普通详情页显示时，优先转换为：
# parallel_plots/web/xxx.png
#
# 点击图片放大和下载时，仍然使用原始高清图。
# ==============================================================================

web_img_subdir <- paste0(
  img_subdir,
  "web/"
)

web_img_dir_full_path <- file.path(
  "www",
  "parallel_plots",
  "web"
)

# 应用启动时读取一次web目录中的文件名，
# 避免每次渲染图片时反复扫描整个目录。
if (dir.exists(web_img_dir_full_path)) {
  
  web_png_files <- list.files(
    web_img_dir_full_path,
    pattern = "\\.png$",
    full.names = FALSE,
    ignore.case = TRUE
  )
  
} else {
  
  web_png_files <- character(0)
  
  warning(
    "网页图片目录不存在：",
    web_img_dir_full_path
  )
}

# 记录网页图的修改时间。
# 修改图片并重启Shiny后，URL版本号会变化，
# 防止浏览器继续使用旧缓存。
if (length(web_png_files) > 0) {
  
  web_png_full_paths <- file.path(
    web_img_dir_full_path,
    web_png_files
  )
  
  web_png_versions <- setNames(
    as.integer(
      file.info(web_png_full_paths)$mtime
    ),
    web_png_files
  )
  
} else {
  
  web_png_versions <- integer(0)
}

web_plot_src <- function(original_path) {
  
  if (
    is.null(original_path) ||
    length(original_path) == 0
  ) {
    return("")
  }
  
  original_path <- as.character(
    original_path[1]
  )
  
  if (
    is.na(original_path) ||
    !nzchar(original_path)
  ) {
    return("")
  }
  
  # 只处理parallel_plots下的PNG。
  # 其他类型的路径原样返回。
  if (!startsWith(
    original_path,
    img_subdir
  )) {
    return(original_path)
  }
  
  filename <- basename(
    sub(
      "\\?.*$",
      "",
      original_path
    )
  )
  
  # web目录没有对应文件时，自动退回原始高清图。
  if (!filename %in% web_png_files) {
    return(original_path)
  }
  
  web_path <- paste0(
    web_img_subdir,
    filename
  )
  
  version <- unname(
    web_png_versions[filename]
  )
  
  if (
    length(version) == 1 &&
    !is.na(version)
  ) {
    
    web_path <- paste0(
      web_path,
      "?v=",
      version
    )
  }
  
  web_path
}

# --- 1.1 读取统计摘要表 ---
# 之前在 vignette 文件夹，现在既然在 data 里，我们直接找
stats_filename <- "prot_table_1.csv"
target_path <- file.path(data_path_prefix, stats_filename)

if (file.exists(target_path)) {
  message("正在从目录加载摘要表：", target_path)
  sig_stats_df <- read.csv(target_path, stringsAsFactors = FALSE)
  if ("signature_id" %in% colnames(sig_stats_df)) {
    sig_stats_df$signature_id <- as.character(sig_stats_df$signature_id)
  }
} else {
  sig_stats_df <- NULL
  warning("严重警告：无法找到统计表：", target_path)
}

# --- 1.2 读取关系表 (稳健版) ---
# 注意：因为你把文件直接放到了 data/ 下，所以不需要那一长串子目录了
conn_file_path <- file.path(data_path_prefix, "connection_table.tsv")

if (file.exists(conn_file_path)) {
  raw_data <- data.table::fread(conn_file_path, data.table = FALSE, fill = TRUE)
} else {
  stop("关键错误：找不到关系表 ", conn_file_path)
}

# --- 1.3 逻辑处理 (InDel 列与 Etiology) ---
id89_df <- raw_data %>%
  dplyr::select(any_of(c("InDel83", "InDel89", "Proposed.Etiology", "Etiology", "Aetiology")))

existing_eti_col <- names(id89_df)[grepl("etiology", names(id89_df), ignore.case = TRUE)]
if (length(existing_eti_col) > 0) {
  names(id89_df)[names(id89_df) == existing_eti_col[1]] <- "Aetiology"
}

if (!"Aetiology" %in% colnames(id89_df)) id89_df$Aetiology <- ""

id89_df <- id89_df %>%
  tidyr::fill(InDel83, .direction = "down") %>%
  dplyr::filter(!is.na(InDel89) & InDel89 != "") %>%
  dplyr::mutate(across(c(InDel83, InDel89, Aetiology), as.character))

# ==============================================================================
# 数据表里名字变成希腊字母
# ==============================================================================
id89_df$InDel89 <- gsub("_alpha", "\u03b1", id89_df$InDel89)
id89_df$InDel89 <- gsub("_beta", "\u03b2", id89_df$InDel89)
# ==============================================================================

# ==============================================================================

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
  # 1. 网页显示专用的名字（保留完美的希腊字母 α, β）
  ID89 <- id89_df$InDel89[i]
  
  # 2. 🌟 新增：系统找文件专用的名字（把希腊字母转回安全的纯英文）
  ID89_file <- gsub("\u03b1", "_alpha", ID89)
  ID89_file <- gsub("\u03b2", "_beta", ID89_file)
  
  ID83 <- id89_df$InDel83[i]
  if (is.na(ID83)) ID83 <- "Unknown"
  aetiology <- id89_df$Aetiology[i]
  if (is.na(aetiology)) aetiology <- ""
  
  # --- 读取 Note (注意：这里必须用 ID89_file 找硬盘里的文件！) ---
  md_file_path <- file.path(data_path_prefix, "per_sig_txt", paste0(ID89_file, ".md"))
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
  
  # --- 生成寻找图片的 safe_name (注意：也必须用 ID89_file，防止希腊字母被正则吃掉！) ---
  safe_name <- gsub("[^a-zA-Z0-9_]", "_", ID89_file)
  
  # --- 寻找图片 (全量雷达模式) ---
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
    matches <- grep(pattern, all_pngs, value = TRUE, ignore.case = TRUE)
    if (length(matches) > 0) return(sort(paste0(img_subdir, matches))) else return(character(0))
  }
  
  # === 1. 89-type ===
  img_89_sig      <- find_one_img("_id89_sig.png")
  img_89_mapped   <- find_one_img("_id89_mapped.png")
  img_89_cat      <- find_one_img("_id89_catalog.png")
  img_89_target   <- find_one_img("_id89_target_sig_partial_spectrum.png")
  img_89_residual <- find_one_img("_id89_residual.png")
  id89_decomp_list <- c(img_89_cat, img_89_target, img_89_residual)
  id89_decomp_list <- id89_decomp_list[!is.null(id89_decomp_list)]
  img_koh_matches <- find_matches("_koh_")
  
  # === 2. 476-type ===
  img_476_sig       <- find_one_img("_id476_sig.png")
  img_476_cat_link  <- find_one_img("_id476_catalog.png")
  # 抓取第三张：Best 476 Match 图
  img_476_cat_best  <- find_one_img("_id476_catalog_476match.png")
  id476_thumb       <- find_one_img("_id476_Thumbnail.png")
  
  # === 3. 83-type ===
  img_83_sig       <- find_one_img("_id83_sig.png")
  img_83_sig_abl   <- find_one_img("_id83_sig_ablated.png")
  img_83_mapped    <- find_one_img("_id83_catalog_83match.png")
  if(is.null(img_83_mapped)) img_83_mapped <- find_one_img("_id83_mapped.png")
  img_83_mapped_abl <- find_one_img("_id83_catalog_83match_ablated.png")
  if(is.null(img_83_mapped_abl)) img_83_mapped_abl <- find_one_img("_id83_mapped_ablated.png")
  img_83_cat       <- find_one_img("_id83_catalog.png")
  img_83_cat_abl   <- find_one_img("_id83_catalog_ablated.png")
  
  # === 4. Matches (智能分离标准版和 Ablated 版) ===
  all_cosmic <- find_matches("_cosmic_")
  cosmic_std <- all_cosmic[!grepl("_ablated\\.png$", all_cosmic, ignore.case=TRUE)]
  cosmic_abl <- all_cosmic[grepl("_ablated\\.png$", all_cosmic, ignore.case=TRUE)]
  
  all_jin <- find_matches("_jin_")
  jin_std <- all_jin[!grepl("_ablated\\.png$", all_jin, ignore.case=TRUE)]
  jin_abl <- all_jin[grepl("_ablated\\.png$", all_jin, ignore.case=TRUE)]
  
  final_thumb <- find_one_img("_Thumbnail.png")
  
  signature_groups[[ID89]] <- list(
    img_89_top = img_89_top_path,
    id89_sig = img_89_sig, id89_mapped = img_89_mapped, id89_decomp = id89_decomp_list, koh_matches = img_koh_matches,
    id476_sig = img_476_sig, id476_cat_link = img_476_cat_link, id476_cat_best = img_476_cat_best, id476_thumb = id476_thumb,
    id83_name = ID83,
    id83_sig = img_83_sig, id83_sig_abl = img_83_sig_abl,
    id83_mapped = img_83_mapped, id83_mapped_abl = img_83_mapped_abl,
    id83_cat = img_83_cat, id83_cat_abl = img_83_cat_abl,
    cosmic_std = cosmic_std, cosmic_abl = cosmic_abl,
    jin_std = jin_std, jin_abl = jin_abl,
    thumbnail = final_thumb, aetiology = aetiology, note = note_content, note_id83 = note_content_83 
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
# 静态 HTML 页面地址
#
# 文件没有修改时，URL保持不变，浏览器可以使用缓存；
# 文件发生修改时，文件修改时间改变，URL版本号自动改变。
# ==============================================================================

static_page_url <- function(filename) {
  
  # 浏览器使用的相对URL
  relative_path <- paste0(
    "separate_pages/",
    filename
  )
  
  # R检查文件是否存在时使用的真实路径
  full_path <- file.path(
    "www",
    "separate_pages",
    filename
  )
  
  if (!file.exists(full_path)) {
    return(relative_path)
  }
  
  # 使用文件修改时间作为版本号
  file_version <- as.integer(
    file.info(full_path)$mtime
  )
  
  paste0(
    relative_path,
    "?v=",
    file_version
  )
}

# Overview Table的稳定URL
overview_page_url <- static_page_url(
  "overview_table.html"
)

# ==============================================================================
# 2. Server 函数
# ==============================================================================

server <- function(input, output, session) {
  
  observe({ runjs("$('.sidebar-menu li').removeClass('active');") })
  
  current_integrated_sig <- reactiveVal(NULL)
  
  # 当前详情页实际显示的内容
  # 可选值：89、476、83、similar、summary
  current_detail_section <- reactiveVal("89")
  
  # 统一打开详情页
  open_signature_detail <- function(
    sig_name,
    section = "89"
  ) {
    
    valid_sections <- c(
      "89",
      "476",
      "83",
      "similar",
      "summary"
    )
    
    if (!section %in% valid_sections) {
      section <- "89"
    }
    
    current_detail_section(section)
    current_integrated_sig(sig_name)
    
    shinyjs::runjs(
      "window.scrollTo(0, 0);"
    )
  }
  
  # 默认显示总览表
  current_repertoire_url <- reactiveVal(
    overview_page_url
  )
  
  # 用于强制重新渲染iframe
  iframe_render_version <- reactiveVal(0L)
  
  # 当程序主动打开某个独立报告并切换到 Overview Table 时，
  # 只跳过一次自动重置。
  #
  # 否则，程序刚把独立报告放进 iframe，
  # navbar 进入 Overview Table 时又会立刻把它重置回总表。
  skip_next_overview_reset <- reactiveVal(FALSE)
  
  # 重置iframe为Overview Table
  #
  # 只有当前iframe不是总览表时才修改URL，
  # 防止反复重建iframe和重复下载HTML文件。
  # 强制恢复到Overview Table
  reset_repertoire_to_overview <- function() {
    
    # 必须始终重新指定 Overview URL。
    #
    # 原因：
    # iframe 可能在其内部已经跳转到独立报告，
    # 但 current_repertoire_url() 仍然保存 overview_page_url。
    # 因此不能只依靠 reactiveVal 判断 iframe 当前实际页面。
    current_repertoire_url(
      overview_page_url
    )
    
    # 强制重新创建 iframe。
    #
    # 即使 src 字符串与之前相同，
    # 版本号变化也会让 renderUI() 重新生成 iframe，
    # 从而真正回到 overview_table.html。
    iframe_render_version(
      isolate(
        iframe_render_version()
      ) + 1L
    )
    
    invisible(TRUE)
  }
  
  # 三个缩略图分类页面
  classification_tabs <- c(
    "89-type classification",
    "476-type classification",
    "83-type classification"
  )
  
  # ==============================================================================
  # 点击 Logo 返回 Home
  # ==============================================================================
  
  observeEvent(
    input$logo_home_click,
    {
      # 清除当前 signature 详情
      current_integrated_sig(NULL)
      
      # 重置详情页默认分区
      current_detail_section("89")
      
      # 防止残留 Overview Table 的跳转标记
      skip_next_overview_reset(FALSE)
      
      # 切换到首页
      updateNavbarPage(
        session = session,
        inputId = "navbar",
        selected = "Home"
      )
      
      # 返回页面顶部
      shinyjs::runjs(
        "window.scrollTo(0, 0);"
      )
    },
    ignoreInit = TRUE
  )
  
  # ==============================================================================
  # About page user feedback collection
  #
  # 用户在 About 页提交反馈后，保存到服务器本地 CSV。
  # 注意：
  # 1. 不要保存到 www/ 目录，避免被网页公开访问。
  # 2. 保存到 user_feedback/feedback.csv。
  # ==============================================================================
  
  feedback_status <- reactiveVal(NULL)
  
  output$feedback_status <- renderUI({
    feedback_status()
  })
  
  observeEvent(
    input$feedback_submit,
    {
      # -----------------------------
      # 1. 读取并清洗输入
      # -----------------------------
      feedback_name <- trimws(
        ifelse(
          is.null(input$feedback_name),
          "",
          input$feedback_name
        )
      )
      
      feedback_email <- trimws(
        ifelse(
          is.null(input$feedback_email),
          "",
          input$feedback_email
        )
      )
      
      feedback_type <- trimws(
        ifelse(
          is.null(input$feedback_type),
          "Other",
          input$feedback_type
        )
      )
      
      feedback_message <- trimws(
        ifelse(
          is.null(input$feedback_message),
          "",
          input$feedback_message
        )
      )
      
      # -----------------------------
      # 2. 基础校验
      # -----------------------------
      if (!nzchar(feedback_message)) {
        
        showNotification(
          "Please enter your feedback message before submitting.",
          type = "warning",
          duration = 5
        )
        
        feedback_status(
          div(
            style = paste0(
              "margin-top:15px;",
              "padding:12px 15px;",
              "background:#fff3cd;",
              "border-left:4px solid #f39c12;",
              "border-radius:8px;",
              "color:#856404;",
              "font-weight:600;"
            ),
            icon("triangle-exclamation"),
            " Please enter your feedback message."
          )
        )
        
        return(invisible(NULL))
      }
      
      if (nchar(feedback_message) < 5) {
        
        showNotification(
          "The feedback message is too short.",
          type = "warning",
          duration = 5
        )
        
        return(invisible(NULL))
      }
      
      # 邮箱是可选项；如果填写了，就做一个简单格式检查
      if (
        nzchar(feedback_email) &&
        !grepl(
          "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$",
          feedback_email
        )
      ) {
        
        showNotification(
          "Please enter a valid email address, or leave it blank.",
          type = "warning",
          duration = 5
        )
        
        return(invisible(NULL))
      }
      
      # -----------------------------
      # 3. 反馈保存路径
      # -----------------------------
      feedback_dir <- file.path(
        getwd(),
        "user_feedback"
      )
      
      if (!dir.exists(feedback_dir)) {
        dir.create(
          feedback_dir,
          recursive = TRUE,
          showWarnings = FALSE
        )
      }
      
      feedback_file <- file.path(
        feedback_dir,
        "feedback.csv"
      )
      
      # -----------------------------
      # 4. 组装反馈记录
      # -----------------------------
      feedback_record <- data.frame(
        time = format(
          Sys.time(),
          "%Y-%m-%d %H:%M:%S %Z"
        ),
        
        name = feedback_name,
        email = feedback_email,
        feedback_type = feedback_type,
        message = feedback_message,
        
        current_nav = ifelse(
          is.null(input$navbar),
          "",
          input$navbar
        ),
        
        url_search = ifelse(
          is.null(session$clientData$url_search),
          "",
          session$clientData$url_search
        ),
        
        user_agent = ifelse(
          is.null(session$clientData$http_user_agent),
          "",
          session$clientData$http_user_agent
        ),
        
        stringsAsFactors = FALSE
      )
      
      # -----------------------------
      # 5. 写入 CSV
      # -----------------------------
      tryCatch(
        {
          write.table(
            feedback_record,
            file = feedback_file,
            sep = ",",
            row.names = FALSE,
            col.names = !file.exists(feedback_file),
            append = file.exists(feedback_file),
            quote = TRUE,
            fileEncoding = "UTF-8"
          )
          
          showNotification(
            "Thank you. Your feedback has been submitted successfully.",
            type = "message",
            duration = 5
          )
          
          feedback_status(
            div(
              style = paste0(
                "margin-top:15px;",
                "padding:12px 15px;",
                "background:#e8f5e9;",
                "border-left:4px solid #27ae60;",
                "border-radius:8px;",
                "color:#1e8449;",
                "font-weight:600;"
              ),
              icon("check-circle"),
              " Thank you. Your feedback has been submitted successfully."
            )
          )
          
          # 清空表单
          updateTextInput(
            session,
            "feedback_name",
            value = ""
          )
          
          updateTextInput(
            session,
            "feedback_email",
            value = ""
          )
          
          updateSelectInput(
            session,
            "feedback_type",
            selected = "Bug report"
          )
          
          updateTextAreaInput(
            session,
            "feedback_message",
            value = ""
          )
        },
        
        error = function(e) {
          
          showNotification(
            paste(
              "Failed to save feedback:",
              conditionMessage(e)
            ),
            type = "error",
            duration = 10
          )
          
          feedback_status(
            div(
              style = paste0(
                "margin-top:15px;",
                "padding:12px 15px;",
                "background:#fdecea;",
                "border-left:4px solid #c0392b;",
                "border-radius:8px;",
                "color:#922b21;",
                "font-weight:600;"
              ),
              icon("circle-exclamation"),
              " Failed to save feedback. Please contact us by email."
            )
          )
        }
      )
      
      invisible(NULL)
    },
    
    ignoreInit = TRUE
  )
  
  # ==============================================================================
  # 点击顶部导航栏当前tab时，强制回到该tab的根页面
  #
  # 目的：
  # 1. 点击 Overview Table：恢复 overview_table.html 总表
  # 2. 点击 89-type classification：恢复89缩略图列表
  # 3. 点击 476-type classification：恢复476缩略图列表
  # 4. 点击 83-type classification：恢复83缩略图列表
  #
  # 原因：
  # 当用户已经停留在某个tab内时，
  # 再次点击同一个navbar tab，不会触发 input$navbar 改变。
  # 所以需要额外监听导航栏点击事件。
  # ==============================================================================
  
  session$onFlushed(
    function() {
      
      shinyjs::runjs(
        "
        $(document)
          .off('click.navbarRootReset', 'a[data-toggle=\"tab\"]')
          .on('click.navbarRootReset', 'a[data-toggle=\"tab\"]', function() {
            
            var value = $(this).attr('data-value') || '';
            var text = $(this).text().replace(/\\s+/g, ' ').trim();
            var tabName = value || text;
            
            if (
              tabName === 'Overview Table' ||
              tabName === '89-type classification' ||
              tabName === '476-type classification' ||
              tabName === '83-type classification'
            ) {
              
              Shiny.setInputValue(
                'navbar_root_clicked',
                {
                  tab: tabName,
                  nonce: Date.now()
                },
                {priority: 'event'}
              );
            }
          });
        "
      )
    },
    once = TRUE
  )
  
  
  observeEvent(
    input$navbar_root_clicked,
    {
      
      click_data <- input$navbar_root_clicked
      
      req(
        is.list(click_data),
        !is.null(click_data$tab)
      )
      
      tab_name <- as.character(
        click_data$tab
      )[1]
      
      # --------------------------------------------------------------------------
      # Overview Table：强制恢复总表
      # --------------------------------------------------------------------------
      
      if (identical(
        tab_name,
        "Overview Table"
      )) {
        
        skip_next_overview_reset(FALSE)
        
        reset_repertoire_to_overview()
        
        shinyjs::runjs(
          "window.scrollTo(0, 0);"
        )
        
        return(invisible(NULL))
      }
      
      # --------------------------------------------------------------------------
      # 89 / 476 / 83：强制恢复各自缩略图列表
      # --------------------------------------------------------------------------
      
      if (tab_name %in% classification_tabs) {
        
        current_integrated_sig(NULL)
        
        # 顺便把详情分区状态调回对应分类，保持URL和内部状态更干净
        if (identical(
          tab_name,
          "89-type classification"
        )) {
          
          current_detail_section("89")
          
        } else if (identical(
          tab_name,
          "476-type classification"
        )) {
          
          current_detail_section("476")
          
        } else if (identical(
          tab_name,
          "83-type classification"
        )) {
          
          current_detail_section("83")
        }
        
        shinyjs::runjs(
          "window.scrollTo(0, 0);"
        )
      }
      
      invisible(NULL)
    },
    ignoreInit = TRUE
  )
  
  # 记录切换前所在的页面
  previous_nav <- reactiveVal(NULL)
  
  observeEvent(
    input$navbar,
    {
      
      new_nav <- input$navbar
      old_nav <- previous_nav()
      
      # --------------------------------------------------------------------------
      # 从任意分类页面离开时，清除当前signature详情状态
      # --------------------------------------------------------------------------
      
      if (
        !is.null(old_nav) &&
        old_nav %in% classification_tabs &&
        !identical(new_nav, old_nav)
      ) {
        
        current_integrated_sig(NULL)
        
        shinyjs::runjs(
          "window.scrollTo(0, 0);"
        )
      }
      
      
      # --------------------------------------------------------------------------
      # 用户从其他页面进入 Overview Table
      # --------------------------------------------------------------------------
      
      if (
        !is.null(old_nav) &&
        !identical(old_nav, "Overview Table") &&
        identical(new_nav, "Overview Table")
      ) {
        
        skip_reset <- isolate(
          skip_next_overview_reset()
        )
        
        if (isTRUE(skip_reset)) {
          
          # 本次进入 Overview 是程序为了显示独立报告而触发的，
          # 所以不能重置为总表。
          #
          # 标记只使用一次，用完立即清除。
          skip_next_overview_reset(FALSE)
          
        } else {
          
          # 用户手动重新进入 Overview，
          # 必须强制恢复 overview_table.html。
          reset_repertoire_to_overview()
        }
        
        shinyjs::runjs(
          "window.scrollTo(0, 0);"
        )
      }
      
      
      previous_nav(
        new_nav
      )
      
    },
    ignoreInit = FALSE
  )
  
  # ============================================================================
  # 新增：VCF 分析模块核心逻辑
  # ============================================================================
  
  # 1. 定义结果容器
  vcf_results <- reactiveValues(
    annotated = NULL,
    cat83 = NULL,
    cat89 = NULL,
    cat476 = NULL
  )
  
  # ============================================================================
  # 示例样本与文件上传逻辑管理
  # ============================================================================
  
  # 1. 创建一个响应式变量，用于统一存放当前要分析的 VCF 路径
  current_vcf_path <- reactiveVal(NULL)
  
  # 2. 监听 示例样本 (hg38) 的点击
  observeEvent(input$load_example_hg38, {
    # 存入预设路径
    current_vcf_path("/home/wuxueming/shinyapp/indel-signature-browser/example_data/test_file_1_hg38.vcf")
    
    # 自动切换下拉菜单到 hg38
    updateSelectInput(session, "ref_genome", selected = "hg38")
    
    # 在 UI 上反馈成功信息 (绿色打勾)
    output$current_file_status <- renderUI({
      div(style = "color: #27ae60; font-weight: bold; margin-top: 10px; padding: 10px; background: #e8f5e9; border-radius: 5px; border-left: 4px solid #27ae60;",
          icon("check-circle"), " Example sample (hg38) loaded successfully! Ready.")
    })
  })
  
  # 3. 监听 示例样本 (hg19) 的点击
  observeEvent(input$load_example_hg37, {
    # 存入预设路径
    current_vcf_path("/home/wuxueming/shinyapp/indel-signature-browser/example_data/test_file_2_hg19.vcf")
    
    # 自动切换下拉菜单到 hg19
    updateSelectInput(session, "ref_genome", selected = "hg19")
    
    # 在 UI 上反馈成功信息
    output$current_file_status <- renderUI({
      div(style = "color: #27ae60; font-weight: bold; margin-top: 10px; padding: 10px; background: #e8f5e9; border-radius: 5px; border-left: 4px solid #27ae60;",
          icon("check-circle"), " Example sample (hg19) loaded successfully! Ready.")
    })
  })
  
  # 4. 兼容用户自行上传文件的情况
  observeEvent(input$vcf_file, {
    req(input$vcf_file)
    # 将用户上传的临时路径存入变量
    current_vcf_path(input$vcf_file$datapath)
    
    # 在 UI 上反馈成功信息 (蓝色提示)
    output$current_file_status <- renderUI({
      div(style = "color: #2980b9; font-weight: bold; margin-top: 10px; padding: 10px; background: #ebf5fb; border-radius: 5px; border-left: 4px solid #2980b9;",
          icon("file-upload"), paste(" Custom file loaded"))
    })
  })
  
  # 2. 监听“开始分析”按钮
  observeEvent(input$run_analysis_btn, {
    # 更改 req：要求 current_vcf_path 必须有内容，不论是示例还是上传的
    req(current_vcf_path()) 
    
    withProgress(message = 'Processing VCF...', value = 0, {
      tryCatch({
        # 从响应式变量中提取正确的路径
        vcf_path <- current_vcf_path() 
        
        # ==========================================
        # 🌟 动态获取真实的文件名
        # ==========================================
        # 因为你的 UI 里写的是 fileInput("vcf_file", ...)，所以这里必须用 input$vcf_file
        if (!is.null(input$vcf_file) && vcf_path == input$vcf_file$datapath) {
          # 如果是用户自己上传的，从 name 属性里抓取原始文件名（比如 patient1.vcf 变成 patient1）
          sample_id <- tools::file_path_sans_ext(input$vcf_file$name)
        } else {
          # 如果是点击的示例文件，直接从路径里截取文件名
          sample_id <- tools::file_path_sans_ext(basename(vcf_path))
        }
        
        # ... 保持不变：UI 里的选项必须与官方文档一致 ...
        genome <- input$ref_genome 
        if(genome == "hg19") genome <- "GRCh37"
        if(genome == "hg38") genome <- "GRCh38"
        
        # ==========================================
        # 步骤 1：读取与初步清洗 (结合你的规则)
        # ==========================================
        message("\n[追踪] 1. 读取并清洗 VCF...")
        incProgress(0.1, detail ="Reading VCF data...")
        
        # 1. 使用官方的 read_vcf 读取，它可以自动处理很多 VCF 头文件的边界情况
        # 默认 filter = TRUE 会保留 "PASS", ".", ""
        raw_vcf <- mSigSpectra::read_vcf(vcf_path, filter = TRUE)
        
        # 2. 你的专属清洗逻辑：统一染色体命名并剔除杂牌军
        raw_vcf$CHROM <- gsub("^chr", "", raw_vcf$CHROM, ignore.case = TRUE)
        valid_chroms <- c(as.character(1:22), "X", "Y")
        raw_vcf <- raw_vcf[raw_vcf$CHROM %in% valid_chroms, ]
        
        # 3. 解决多等位基因问题 (对应 Gotcha 2)
        raw_vcf <- raw_vcf %>%
          tidyr::separate_rows(ALT, sep = ",") %>%
          as.data.frame()
        
        # ==========================================
        # 步骤 2：官方防御性管道 (核心纠错，防止崩溃)
        # ==========================================
        message("\n[Trace] 2. Official QC and Indel extraction...")
        incProgress(0.3, detail = "Executing official safety QC...")
        
        # 1. 过滤掉不符合生物学聚合酶足迹规律的伪 Indels、复杂突变和含 N 的行
        clean_data <- mSigSpectra::check_and_remove_discarded_variants(raw_vcf)
        
        # 2. 智能分类，提取出真正安全的 Indel 数据
        parts <- mSigSpectra::split_vcf(clean_data$df)
        indel_df <- parts$ID
        
        if (is.null(indel_df) || nrow(indel_df) == 0) {
          stop("No valid Indel mutations meeting COSMIC standards were found after strict quality control!")
        }
        
        # ==========================================
        # 步骤 3：根据 GitHub 源码确认的稳定版函数
        # ==========================================
        message("[Trace] 3. Comparing reference genome and generating matrices...")
        incProgress(0.6, detail = "Performing Indel annotation...")
        
        # 1. 调用截图 image_cb79a2.png 确认的 annotate_id_vcf 函数
        # 作用：计算 Indel 突变在基因组中的侧翼上下文和重复序列长度
        id_ann_list <- mSigSpectra::annotate_id_vcf(
          vcf = indel_df, 
          ref_genome = genome
        )
        
        # 提取注释后的 VCF 数据框，用于后续矩阵计算
        ann_id <- id_ann_list$annotated.vcf
        vcf_results$annotated <- ann_id
        
        # 2. 生成 ID83/89/476 矩阵 (使用 vcf_to_id_catalog)
        # 注意：这里的 type 参数决定了生成的矩阵分类框架
        incProgress(0.2, detail = "Building classification matrix...")
        
        vcf_results$cat83 <- mSigSpectra::vcf_to_id_catalog(
          ann_id, type = "ID83", ref_genome = genome, region = "genome", sample_name = sample_id
        )
        
        vcf_results$cat89 <- mSigSpectra::vcf_to_id_catalog(
          ann_id, type = "ID89", ref_genome = genome, region = "genome", sample_name = sample_id
        )
        
        vcf_results$cat476 <- mSigSpectra::vcf_to_id_catalog(
          ann_id, type = "ID476", ref_genome = genome, region = "genome", sample_name = sample_id
        )
        
        message("[Tracking] All matrices generated successfully!")
        incProgress(0.19, detail = "Analysis complete!")
        showNotification("Success: ID83/89/476 matrices are ready.", type = "message")
        
      }, error = function(e) {
        # ==========================================
        # 🛡️ 错误拦截护盾：防止程序崩溃并友好提示用户
        # ==========================================
        error_msg <- conditionMessage(e)
        
        # 拦截 1：精准捕捉“基因组越界”错误
        if (grepl("beyond the boundaries", error_msg, ignore.case = TRUE) || 
            grepl("out of bounds", error_msg, ignore.case = TRUE)) {
          
          # 弹出强烈的红色错误提示，持续 10 秒
          showNotification(
            shiny::HTML("<b>⚠️ Reference Genome Mismatch!</b><br>The program attempted to read mutation coordinates that exceed the chromosome length. Please verify that your VCF file matches the selected reference genome (e.g., hg19/hg38)!"),
            type = "error",
            duration = 10
          )
          
        } 
        # 拦截 2：捕获由于没有任何符合标准的突变导致的提前退出 (之前的 stop 报错)
        else if (grepl("未发现符合", error_msg)) {
          showNotification(
            shiny::HTML("<b>⚠️ Data Filtering Notice:</b><br>After strict quality control, no valid Indel mutations meeting the criteria for matrix construction were found."),
            type = "warning",
            duration = 8
          )
        }
        # 拦截 3：捕获其他未知系统崩溃
        else {
          showNotification(
            paste("❌ 分析意外中断:", error_msg),
            type = "error",
            duration = 15
          )
        }
        
        # 打印错误到后台日志，方便你作为开发者排查
        message("\n[Fatal Error Intercepted] ", error_msg)
      })
    })
  })
  
  # ============================================================================
  # 3. 绘图输出 (采用数据与视图分离策略，解决文字重叠)
  # ============================================================================
  
  # 渲染 83 Type 图表
  output$plot_id83 <- renderPlot({
    req(vcf_results$cat83) # 确保矩阵已生成
    
    mSigPlot::plot_ID83(
      vcf_results$cat83, 
      plot_title = colnames(vcf_results$cat83)[1],
      # 👇 放大参数：
      base_size = 12,             # 放大基础字号（坐标轴、标题、刻度）
      class_label_cex = 0.75,      # 放大顶部彩条里的文字
      count_label_cex = 0.9      # 放大柱子上悬浮的统计数字
    )
  })
  
  # 同理，渲染 89 Type 图表
  output$plot_id89 <- renderPlot({
    req(vcf_results$cat89)
    mSigPlot::plot_ID89(
      vcf_results$cat89, 
      plot_title = colnames(vcf_results$cat89)[1],
      # 👇 放大参数：
      base_size = 14.5,
      class_label_cex = 0.9,
      count_label_cex = 1.0
    )
  })
  
  # 同理，渲染 476 Type 图表
  output$plot_id476 <- renderPlot({
    req(vcf_results$cat476)
    sample_name <- colnames(vcf_results$cat476)[1]
    
    p <- mSigPlot::plot_ID476(
      vcf_results$cat476, 
      plot_title = "",
      # 👇 放大参数：
      base_size = 14.5,
      class_label_cex = 0.9,
      count_label_cex = 1.0,
      num_peak_labels = 5         # 标注最高的5个突变柱子
    )
    
    # 细节微调：476的统计数字容易和底下的柱子重叠，我们稍微把它往上提一点
    if (!is.null(p)) {
      for (i in seq_along(p$layers)) {
        if (inherits(p$layers[[i]]$geom, "GeomText")) {
          layer_data <- p$layers[[i]]$data
          if (!is.null(layer_data) && nrow(layer_data) > 0 && 
              "y" %in% colnames(layer_data) && length(unique(layer_data$y)) == 1) {
            p$layers[[i]]$data$y <- layer_data$y[1] * 1.02
          }
        }
      }
    }
    # 加上拥有完美上下呼吸感的自定义标题
    p + ggplot2::ggtitle(sample_name) +
      ggplot2::theme(
        # t = 10 (离彩条空出10px), b = 15 (离数字空出15px)
        plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5, 
                                           margin = ggplot2::margin(t = 10, b = 15)),
        plot.margin = ggplot2::margin(t = 15, r = 10, b = 10, l = 10)
      )
  })
  
  # ============================================================================
  # 4. 下载逻辑 (矩阵与 VCF 分开下载)
  # ============================================================================
  
  # 1. 动态渲染两个下载按钮
  output$download_ui <- renderUI({
    req(vcf_results$cat83, vcf_results$cat89, vcf_results$cat476, vcf_results$annotated)
    
    tagList(
      # 按钮 1：下载矩阵 (绿色)
      downloadButton("download_matrices", "Download Matrices (ZIP)", 
                     class = "btn-success", 
                     style = "font-size: 1.45rem; font-weight: bold; padding: 12px 5px; width: 100%; margin-top: 10px; white-space: normal; line-height: 1.3; border-radius: 8px;"),
      
      # 按钮 2：下载 Annotated VCF (蓝色)
      downloadButton("download_vcf", "Download Annotated VCF (CSV)", 
                     class = "btn-info", 
                     style = "font-size: 1.45rem; font-weight: bold; padding: 12px 5px; width: 100%; margin-top: 15px; white-space: normal; line-height: 1.3; border-radius: 8px; color: white;")
    )
  })
  
  # 2. 矩阵下载处理逻辑 (只打包三个矩阵)
  output$download_matrices <- downloadHandler(
    filename = function() {
      paste0("Indel_Matrices_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      temp_dir <- tempdir()
      old_wd <- setwd(temp_dir)
      on.exit(setwd(old_wd)) 
      
      write.csv(as.data.frame(vcf_results$cat83), "ID83_matrix.csv", row.names = TRUE)
      write.csv(as.data.frame(vcf_results$cat89), "ID89_matrix.csv", row.names = TRUE)
      write.csv(as.data.frame(vcf_results$cat476), "ID476_matrix.csv", row.names = TRUE)
      
      utils::zip(zipfile = file, 
                 files = c("ID83_matrix.csv", "ID89_matrix.csv", "ID476_matrix.csv"))
    },
    contentType = "application/zip"
  )
  
  # 3. Annotated VCF 下载处理逻辑 (直接输出单个 CSV，无需打包)
  output$download_vcf <- downloadHandler(
    filename = function() {
      paste0("Annotated_VCF_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      # VCF 自己有完善的列名（CHROM, POS, REF, ALT等），不需要 row.names
      write.csv(as.data.frame(vcf_results$annotated), file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  # ==============================================================================
  # URL路由
  #
  # 保存：
  # nav     = 顶部导航页
  # id      = 当前signature
  # section = 当前详情分区
  # ==============================================================================
  
  valid_detail_sections <- c(
    "89",
    "476",
    "83",
    "similar",
    "summary"
  )
  
  observe({
    
    nav <- input$navbar
    
    req(nav)
    
    sig_id <- current_integrated_sig()
    section <- current_detail_section()
    
    query_string <- paste0(
      "?nav=",
      URLencode(
        nav,
        reserved = TRUE
      )
    )
    
    if (
      !is.null(sig_id) &&
      nzchar(sig_id)
    ) {
      
      query_string <- paste0(
        query_string,
        
        "&id=",
        URLencode(
          sig_id,
          reserved = TRUE
        ),
        
        "&section=",
        URLencode(
          section,
          reserved = TRUE
        )
      )
    }
    
    current_query <- isolate(
      session$clientData$url_search
    )
    
    if (!identical(
      query_string,
      current_query
    )) {
      
      updateQueryString(
        query_string,
        mode = "push"
      )
    }
  })
  
  
  observeEvent(
    session$clientData$url_search,
    
    {
      
      query <- getQueryString()
      
      if (length(query) == 0) {
        
        current_integrated_sig(NULL)
        
        return(invisible(NULL))
      }
      
      # 恢复顶部导航
      if (
        !is.null(query$nav) &&
        !identical(
          isolate(input$navbar),
          query$nav
        )
      ) {
        
        updateNavbarPage(
          session,
          "navbar",
          selected = query$nav
        )
      }
      
      # 先恢复详情分区
      if (
        !is.null(query$section) &&
        query$section %in% valid_detail_sections
      ) {
        
        current_detail_section(
          query$section
        )
      }
      
      # 再恢复signature
      if (
        !is.null(query$id) &&
        query$id %in% names(signature_groups)
      ) {
        
        current_integrated_sig(
          query$id
        )
        
      } else {
        
        current_integrated_sig(NULL)
      }
    },
    
    ignoreInit = FALSE
  )
  
  observeEvent(input$home_goto_89, { updateNavbarPage(session, "navbar", selected = "89-type classification") })
  observeEvent(input$home_goto_83, { updateNavbarPage(session, "navbar", selected = "83-type classification") })
  observeEvent(input$home_goto_476, { updateNavbarPage(session, "navbar", selected = "476-type classification") })
  
  # ============================================================================
  # 辅助渲染函数
  # ============================================================================
  
  # 统一图片组件：
  # - 首屏图片使用 priority = TRUE
  # - 其余图片使用浏览器原生懒加载
  # - 所有图片统一通过 open_modal_image 打开弹窗
  img_block <- function(
    img_path,
    width = "100%",
    border = FALSE,
    priority = FALSE
  ) {
    if (is.null(img_path) || is.na(img_path)) {
      return(NULL)
    }
    
    if (length(img_path) > 1) {
      img_path <- img_path[1]
    }
    
    display_img_path <- web_plot_src(
      img_path
    )
    
    div(
      style = "text-align:center; margin-bottom:10px;",
      
      tags$img(
        src = display_img_path,
        class = "signature-img",
        
        loading = if (priority) {
          "eager"
        } else {
          "lazy"
        },
        
        fetchpriority = if (priority) {
          "high"
        } else {
          "auto"
        },
        
        decoding = "async",
        
        style = paste0(
          "width:", width, ";",
          "max-width:900px;",
          if (border) {
            "border:1px solid #ddd; padding:2px;"
          } else {
            ""
          }
        ),
        
        onclick = paste0(
          "Shiny.setInputValue('open_modal_image', '",
          img_path,
          "', {priority: 'event'})"
        )
      )
    )
  }
  
  # ==============================================================================
  # 浏览器图片预加载
  #
  # 鼠标进入缩略图卡片时，提前下载对应详情页的关键图片。
  # 用户点击卡片后，浏览器可直接使用缓存。
  # ==============================================================================
  
  prefetch_images_js <- function(paths) {
    
    paths <- unlist(
      paths,
      use.names = FALSE
    )
    
    paths <- as.character(paths)
    
    paths <- paths[
      !is.na(paths) &
        nzchar(paths)
    ]
    
    # 预加载网页专用图，不预加载原始高清图。
    paths <- vapply(
      paths,
      web_plot_src,
      character(1),
      USE.NAMES = FALSE
    )
    
    paths <- unique(
      paths[
        !is.na(paths) &
          nzchar(paths)
      ]
    )
    
    if (length(paths) == 0) {
      return("")
    }
    
    urls_json <- as.character(
      jsonlite::toJSON(
        paths,
        auto_unbox = FALSE
      )
    )
    
    paste0(
      "(function(){",
      "window.__indelImagePrefetch=",
      "window.__indelImagePrefetch||{};",
      
      urls_json,
      
      ".forEach(function(src){",
      
      "if(!window.__indelImagePrefetch[src]){",
      
      "var img=new Image();",
      "img.decoding='async';",
      "img.src=src;",
      
      "window.__indelImagePrefetch[src]=img;",
      
      "}",
      
      "});",
      
      "})();"
    )
  }
  
  # ==============================================================================
  # 89-type和476-type缩略图统一点击事件
  #
  # 点击卡片时，直接向Shiny发送：
  # 1. 当前signature名称
  # 2. 需要打开的详情分区
  #
  # 不再使用隐藏的actionLink。
  # ==============================================================================
  
  signature_card_click_js <- function(
    sig_name,
    section_name
  ) {
    
    # 使用JSON转换，安全处理α、β、引号等特殊字符
    sig_json <- as.character(
      jsonlite::toJSON(
        sig_name,
        auto_unbox = TRUE
      )
    )
    
    section_json <- as.character(
      jsonlite::toJSON(
        section_name,
        auto_unbox = TRUE
      )
    )
    
    paste0(
      "Shiny.setInputValue(",
      
      "'signature_card_click',",
      
      "{",
      
      "sig:",
      sig_json,
      ",",
      
      "section:",
      section_json,
      ",",
      
      # 保证连续点击同一张卡片时，输入值仍然发生变化
      "nonce:Date.now()",
      
      "},",
      
      "{priority:'event'}",
      
      ");"
    )
  }
  
  # ==============================================================================
  # 83-type缩略图统一点击事件
  # ==============================================================================
  
  id83_card_click_js <- function(
    id83_name
  ) {
    
    id83_json <- as.character(
      jsonlite::toJSON(
        id83_name,
        auto_unbox = TRUE
      )
    )
    
    paste0(
      "Shiny.setInputValue(",
      
      "'id83_card_click',",
      
      "{",
      "id83:",
      id83_json,
      ",",
      "nonce:Date.now()",
      "},",
      
      "{priority:'event'}",
      
      ");"
    )
  }
  
  
  # ==============================================================================
  # 83-type多成员弹窗统一点击事件
  # ==============================================================================
  
  id83_member_click_js <- function(
    member_name
  ) {
    
    member_json <- as.character(
      jsonlite::toJSON(
        member_name,
        auto_unbox = TRUE
      )
    )
    
    paste0(
      "Shiny.setInputValue(",
      
      "'id83_member_click',",
      
      "{",
      "member:",
      member_json,
      ",",
      "nonce:Date.now()",
      "},",
      
      "{priority:'event'}",
      
      ");"
    )
  }
  
  render_styled_pair_block <- function(
    title_text,
    std_img,
    abl_img,
    caution_text = NULL,
    priority = FALSE,
    defer = FALSE
  ) {
    
    # 页面中显示web版本
    std_display_img <- if (!is.null(std_img)) {
      web_plot_src(std_img)
    } else {
      NULL
    }
    
    abl_display_img <- if (!is.null(abl_img)) {
      web_plot_src(abl_img)
    } else {
      NULL
    }
    
    div(
      class = "id83-section",
      
      style = paste0(
        "margin-bottom:30px;",
        "padding:30px;",
        "background:#fff;",
        "box-shadow:0 5px 20px rgba(0,0,0,0.03);"
      ),
      
      if (!is.null(caution_text)) {
        caution_text
      },
      
      h4(
        title_text,
        
        style = paste0(
          "color:#2c3e50;",
          "font-weight:700;",
          "margin-top:0;",
          "margin-bottom:20px;",
          "font-size:1.2rem;"
        )
      ),
      
      if (!is.null(std_img)) {
        
        tags$img(
          src = std_display_img,
          class = "signature-img",
          
          # 当前89/476/83分区中的标准图片立即加载；
          # Similarities分区传入defer = TRUE时继续懒加载。
          loading = if (priority || !defer) {
            "eager"
          } else {
            "lazy"
          },
          
          fetchpriority = if (priority) {
            "high"
          } else if (defer) {
            "low"
          } else {
            "auto"
          },
          
          decoding = "async",
          
          onclick = paste0(
            "Shiny.setInputValue('open_modal_image', '",
            std_img,
            "', {priority: 'event'})"
          ),
          
          style = paste0(
            "width:100%;",
            "height:auto;",
            "display:block;",
            "margin-bottom:15px;"
          )
        )
        
      } else {
        
        div(
          "Standard scale image not available",
          style = "color:#ccc; padding:10px;"
        )
      },
      
      if (!is.null(abl_img)) {
        
        tags$details(
          style = "margin-top:10px;",
          
          tags$summary(
            style = paste0(
              "cursor:pointer;",
              "color:#3498db;",
              "font-weight:500;",
              "font-size:1.35rem;",
              "outline:none;",
              "user-select:none;"
            ),
            
            icon(
              "chevron-circle-right",
              style = "color:#27ae60; margin-right:8px;"
            ),
            
            paste(
              "Click here to see with insertions and deletions",
              "of T in long poly-T suppressed."
            )
          ),
          
          div(
            style = paste0(
              "margin-top:15px;",
              "border-top:1px dashed #eee;",
              "padding-top:15px;"
            ),
            
            tags$img(
              src = abl_display_img,
              class = "signature-img",
              loading = "lazy",
              fetchpriority = "low",
              decoding = "async",
              
              onclick = paste0(
                "Shiny.setInputValue('open_modal_image', '",
                abl_img,
                "', {priority: 'event'})"
              ),
              
              style = paste0(
                "width:100%;",
                "border:none !important;",
                "box-shadow:none !important;"
              )
            )
          )
        )
      }
    )
  }
  
  render_match_group <- function(title_text, std_list, abl_list) {
    if (length(std_list) == 0) return(NULL)
    tagList(
      h4(title_text, style = "color: #34495e; font-weight: bold; margin-top: 25px; margin-bottom: 15px; border-left: 4px solid #3498db; padding-left: 10px;"),
      div(style = "background: #fff; padding: 20px; border: 1px solid #eee; border-radius: 8px; box-shadow: 0 2px 10px rgba(0,0,0,0.02);",
          lapply(seq_along(std_list), function(i) {
            std_img <- std_list[i]
            expected_abl <- sub("\\.png$", "_ablated.png", std_img)
            abl_img <- if (expected_abl %in% abl_list) expected_abl else NULL
            clean_name <- gsub("^.*?_(cosmic|jin)_", "", basename(std_img))
            clean_name <- gsub("\\.png$", "", clean_name)
            
            render_styled_pair_block(
              paste("Match:", clean_name),
              std_img,
              abl_img,
              defer = TRUE
            )
          })
      )
    )
  }
  
  # ==============================================================================
  # 详情页上下文
  #
  # 所有统计值在这里统一读取，避免每个分区重复查表。
  # ==============================================================================
  
  get_detail_context <- function(sig_name) {
    
    sig <- signature_groups[[sig_name]]
    
    search_name <- gsub(
      "\u03b1",
      "_alpha",
      sig_name
    )
    
    search_name <- gsub(
      "\u03b2",
      "_beta",
      search_name
    )
    
    current_stats <- NULL
    
    if (!is.null(sig_stats_df)) {
      
      current_stats <- sig_stats_df[
        sig_stats_df$signature_id == search_name,
        ,
        drop = FALSE
      ]
    }
    
    has_stats <- (
      !is.null(current_stats) &&
        nrow(current_stats) > 0
    )
    
    get_stat_text <- function(
    column_name,
    default_value = "Exemplar Sample"
    ) {
      
      if (
        !has_stats ||
        !column_name %in% names(current_stats)
      ) {
        return(default_value)
      }
      
      value <- current_stats[[column_name]][1]
      
      if (
        length(value) == 0 ||
        is.na(value) ||
        identical(as.character(value), "")
      ) {
        return(default_value)
      }
      
      as.character(value)
    }
    
    get_stat_number <- function(
    column_name,
    default_value = "N/A"
    ) {
      
      if (
        !has_stats ||
        !column_name %in% names(current_stats)
      ) {
        return(default_value)
      }
      
      value <- current_stats[[column_name]][1]
      
      if (
        length(value) == 0 ||
        is.na(value)
      ) {
        return(default_value)
      }
      
      format(
        value,
        digits = 4
      )
    }
    
    polyT_sigs <- c(
      "C_ID7",
      "ID_J",
      "C_ID10",
      "ID_N",
      "ID_O"
    )
    
    tumor_caution <- NULL
    
    if (sig$id83_name %in% polyT_sigs) {
      
      tumor_caution <- div(
        style = paste0(
          "font-size:13px;",
          "color:#c0392b;",
          "background:#fadbd8;",
          "padding:12px;",
          "border-radius:8px;",
          "margin-bottom:20px;",
          "border-left:5px solid #c0392b;",
          "display:flex;",
          "align-items:center;"
        ),
        
        icon(
          "triangle-exclamation",
          style = paste0(
            "margin-right:10px;",
            "font-size:1.2em;"
          )
        ),
        
        div(
          paste(
            "For the supporting tumor plot, mutation counts for",
            "insertions and deletions of T in long-poly-T contexts",
            "were set to 0. They were also set to 0 when calculating",
            "cosine similarity with the signature. The signature was",
            "not altered when computing the cosine similarity."
          )
        )
      )
    }
    
    list(
      sig_name = sig_name,
      sig = sig,
      search_name = search_name,
      current_stats = current_stats,
      has_stats = has_stats,
      
      exemplar_89_name = get_stat_text(
        "exemplar_89"
      ),
      
      exemplar_83_name = get_stat_text(
        "exemplar_83"
      ),
      
      exemplar_476_name = get_stat_text(
        "exemplar_476"
      ),
      
      cos_476_link = get_stat_number(
        "sig476_v_linking_cos"
      ),
      
      cos_476_best = get_stat_number(
        "sig476_v_exemplar_cos"
      ),
      
      tumor_caution = tumor_caution
    )
  }
  
  
  # ==============================================================================
  # 详情页顶部公共区域
  # ==============================================================================
  
  build_detail_header <- function(
    ctx,
    back_btn_id
  ) {
    
    sig <- ctx$sig
    active_section <- current_detail_section()
    
    detail_button <- function(
    label,
    section_name,
    icon_name = NULL
    ) {
      
      active <- identical(
        active_section,
        section_name
      )
      
      tags$button(
        type = "button",
        
        class = paste(
          "btn",
          if (active) {
            "btn-primary"
          } else {
            "btn-default"
          }
        ),
        
        onclick = paste0(
          "Shiny.setInputValue(",
          "'detail_section_request',",
          "'",
          section_name,
          "',",
          "{priority:'event'}",
          ");"
        ),
        
        style = paste0(
          "margin-right:8px;",
          "margin-bottom:8px;",
          "font-weight:600;",
          "border-radius:7px;"
        ),
        
        label
      )
    }
    
    tagList(
      
      div(
        style = paste0(
          "display:flex;",
          "justify-content:space-between;",
          "align-items:center;",
          "margin-bottom:20px;"
        ),
        
        actionButton(
          back_btn_id,
          "\u2190 Back to Thumbnails",
          class = "btn-back"
        ),
        
        div()
      ),
      
      h2(
        paste(
          "Signatures",
          ctx$sig_name,
          "and",
          sig$id83_name
        ),
        
        style = paste0(
          "color:#2c3e50;",
          "font-weight:700;",
          "margin-top:0;",
          "margin-bottom:20px;",
          "border-bottom:2px solid #eee;",
          "padding-bottom:10px;"
        )
      ),
      
      div(
        style = paste0(
          "margin-bottom:20px;",
          "font-size:15px;",
          "background:#f8f9fa;",
          "padding:15px 20px;",
          "border-radius:12px;",
          "border-left:5px solid #34495e;",
          "box-shadow:0 4px 15px rgba(0,0,0,0.05);"
        ),
        
        tags$a(
          href = "#",
          
          onclick = paste0(
            "Shiny.setInputValue(",
            "'repertoire_signature_request',",
            "'",
            ctx$search_name,
            "',",
            "{priority:'event'}",
            ");",
            "return false;"
          ),
          
          style = paste0(
            "color:#3498db;",
            "font-weight:bold;",
            "text-decoration:none;",
            "border-bottom:1px dashed #3498db;",
            "padding-bottom:2px;",
            "cursor:pointer;"
          ),
          
          icon("book-open"),
          " More details about this signature "
        )
      ),
      
      if (!is.null(sig$note)) {
        shiny::markdown(sig$note)
      },
      
      if (
        !is.null(sig$aetiology) &&
        nchar(sig$aetiology) > 0
      ) {
        
        div(
          style = paste0(
            "background:#e8f5e9;",
            "padding:15px;",
            "border-radius:8px;",
            "margin-bottom:20px;",
            "border-left:4px solid #2ecc71;"
          ),
          
          icon("lightbulb"),
          strong(" Etiology: "),
          sig$aetiology
        )
      },
      
      # 分区按钮
      div(
        style = paste0(
          "background:#fff;",
          "padding:14px;",
          "border-radius:10px;",
          "margin-bottom:22px;",
          "box-shadow:0 3px 12px rgba(0,0,0,0.06);"
        ),
        
        detail_button(
          "89-type",
          "89",
          "dna"
        ),
        
        detail_button(
          "476-type",
          "476",
          "table"
        ),
        
        detail_button(
          "83-type",
          "83",
          "layer-group"
        ),
        
        detail_button(
          "Similarities",
          "similar",
          "project-diagram"
        ),
        
        detail_button(
          "Summary",
          "summary",
          "list-alt"
        )
      )
    )
  }
  
  
  # ==============================================================================
  # 89-type分区
  # ==============================================================================
  
  build_89_detail <- function(ctx) {
    
    sig <- ctx$sig
    
    tagList(
      
      h3(
        "1. 89-type classification",
        style = paste0(
          "color:#2c3e50;",
          "font-weight:700;",
          "margin-top:20px;",
          "margin-bottom:20px;"
        )
      ),
      
      if (!is.null(sig$id89_sig)) {
        
        div(
          class = "id83-section",
          
          div(
            class = "id83-label",
            style = "border-left-color:#e67e22;",
            icon("dna"),
            " Signature Profile"
          ),
          
          tags$img(
            src = web_plot_src(
              sig$id89_sig
            ),
            class = "signature-img",
            loading = "eager",
            fetchpriority = "high",
            decoding = "async",
            
            onclick = paste0(
              "Shiny.setInputValue('open_modal_image', '",
              sig$id89_sig,
              "', {priority:'event'})"
            ),
            
            style = "width:100%;"
          )
        )
        
      } else {
        
        div(
          class = "alert alert-warning",
          "No 89-type signature image available."
        )
      },
      
      if (!is.null(sig$id89_mapped)) {
        
        div(
          class = "id83-section",
          
          div(
            class = "id83-label",
            style = "border-left-color:#f39c12;",
            icon("exchange-alt"),
            " Mapped Signature"
          ),
          
          tags$img(
            src = web_plot_src(
              sig$id89_mapped
            ),
            class = "signature-img",
            loading = "eager",
            fetchpriority = "auto",
            decoding = "async",
            
            onclick = paste0(
              "Shiny.setInputValue('open_modal_image', '",
              sig$id89_mapped,
              "', {priority:'event'})"
            ),
            
            style = paste0(
              "width:100%;",
              "height:auto;",
              "display:block;"
            )
          )
        )
      },
      
      if (length(sig$koh_matches) > 0) {
        
        tagList(
          
          h4(
            "Matches to Koh et al. signatures",
            style = paste0(
              "color:#e67e22;",
              "font-weight:bold;",
              "margin-top:25px;",
              "margin-bottom:15px;"
            )
          ),
          
          div(
            style = paste0(
              "background:#fff;",
              "padding:20px;",
              "border:1px solid #eee;",
              "border-radius:8px;"
            ),
            
            lapply(
              sig$koh_matches,
              
              function(p) {
                
                div(
                  style = "margin-bottom:20px;",
                  
                  tags$img(
                    src = web_plot_src(
                      p
                    ),
                    class = "signature-img",
                    loading = "eager",
                    fetchpriority = "auto",
                    decoding = "async",
                    
                    onclick = paste0(
                      "Shiny.setInputValue('open_modal_image', '",
                      p,
                      "', {priority:'event'})"
                    ),
                    
                    style = paste0(
                      "width:100%;",
                      "height:auto;",
                      "display:block;"
                    )
                  )
                )
              }
            )
          )
        )
      },
      
      if (length(sig$id89_decomp) > 0) {
        
        tagList(
          
          h4(
            "Sample Decomposition Analysis",
            style = paste0(
              "color:#7f8c8d;",
              "margin-top:20px;",
              "font-weight:bold;",
              "text-align:center;"
            )
          ),
          
          div(
            class = "id83-section",
            style = "background:#fff;",
            
            fluidRow(
              
              lapply(
                seq_along(sig$id89_decomp),
                
                function(i) {
                  
                  labels <- c(
                    "Spectrum (Observed)",
                    "Target Partial Spectrum",
                    "Residual"
                  )
                  
                  label_text <- if (
                    i <= length(labels)
                  ) {
                    labels[i]
                  } else {
                    paste("Image", i)
                  }
                  
                  column(
                    4,
                    
                    div(
                      class = "img-label",
                      label_text
                    ),
                    
                    tags$img(
                      src = web_plot_src(
                        sig$id89_decomp[i]
                      ),
                      class = "signature-img",
                      loading = "eager",
                      fetchpriority = "auto",
                      decoding = "async",
                      
                      onclick = paste0(
                        "Shiny.setInputValue('open_modal_image', '",
                        sig$id89_decomp[i],
                        "', {priority:'event'})"
                      ),
                      
                      style = paste0(
                        "width:100%;",
                        "height:auto;",
                        "display:block;"
                      )
                    )
                  )
                }
              )
            )
          )
        )
      }
    )
  }
  
  
  # ==============================================================================
  # 476-type分区
  # ==============================================================================
  
  build_476_detail <- function(ctx) {
    
    sig <- ctx$sig
    
    available_paths <- c(
      sig$id476_sig,
      sig$id476_cat_link,
      sig$id476_cat_best
    )
    
    available_paths <- available_paths[
      !is.na(available_paths) &
        nzchar(available_paths)
    ]
    
    first_path <- if (
      length(available_paths) > 0
    ) {
      available_paths[1]
    } else {
      NULL
    }
    
    is_priority <- function(path) {
      
      !is.null(path) &&
        !is.null(first_path) &&
        identical(path, first_path)
    }
    
    tagList(
      
      h3(
        "2. 476-type classification",
        style = paste0(
          "color:#2c3e50;",
          "font-weight:700;",
          "margin-top:20px;",
          "margin-bottom:20px;"
        )
      ),
      
      if (!is.null(sig$id476_sig)) {
        
        render_styled_pair_block(
          "2.1 476-type signature",
          sig$id476_sig,
          NULL,
          priority = is_priority(
            sig$id476_sig
          )
        )
      },
      
      if (!is.null(sig$id476_cat_link)) {
        
        render_styled_pair_block(
          paste0(
            "2.2 476-type spectrum of the linking tumor ",
            ctx$exemplar_89_name,
            "; cosine similarity to the extracted ",
            "476-type signature is ",
            ctx$cos_476_link
          ),
          
          sig$id476_cat_link,
          NULL,
          
          priority = is_priority(
            sig$id476_cat_link
          )
        )
      },
      
      if (!is.null(sig$id476_cat_best)) {
        
        render_styled_pair_block(
          paste0(
            "2.3 476-type spectrum of best 476-type ",
            "matching tumor ",
            ctx$exemplar_476_name,
            "; cosine similarity to the extracted ",
            "476-type signature is ",
            ctx$cos_476_best
          ),
          
          sig$id476_cat_best,
          NULL,
          
          priority = is_priority(
            sig$id476_cat_best
          )
        )
      },
      
      if (length(available_paths) == 0) {
        
        div(
          class = "alert alert-warning",
          "No 476-type representation available."
        )
      }
    )
  }
  
  
  # ==============================================================================
  # 83-type分区
  # ==============================================================================
  
  build_83_detail <- function(ctx) {
    
    sig <- ctx$sig
    
    first_83_path <- c(
      sig$id83_sig,
      sig$id83_cat,
      sig$id83_mapped
    )
    
    first_83_path <- first_83_path[
      !is.na(first_83_path) &
        nzchar(first_83_path)
    ]
    
    first_83_path <- if (
      length(first_83_path) > 0
    ) {
      first_83_path[1]
    } else {
      NULL
    }
    
    is_priority <- function(path) {
      
      !is.null(path) &&
        !is.null(first_83_path) &&
        identical(path, first_83_path)
    }
    
    tagList(
      
      h3(
        "3. 83-type classification",
        style = paste0(
          "color:#2c3e50;",
          "font-weight:700;",
          "margin-top:20px;",
          "margin-bottom:30px;",
          "border-bottom:2px solid #eee;",
          "padding-bottom:10px;"
        )
      ),
      
      if (!is.null(sig$note_id83)) {
        
        div(
          style = paste0(
            "background-color:#e3f2fd;",
            "padding:15px;",
            "border-radius:8px;",
            "margin-bottom:20px;"
          ),
          
          shiny::markdown(sig$note_id83)
        )
      },
      
      if (!is.null(sig$id83_sig)) {
        
        render_styled_pair_block(
          "3.1 De-novo signature",
          sig$id83_sig,
          sig$id83_sig_abl,
          
          priority = is_priority(
            sig$id83_sig
          )
        )
      },
      
      if (!is.null(sig$id83_cat)) {
        
        render_styled_pair_block(
          paste(
            "3.2 83-type spectrum of the linking tumor",
            ctx$exemplar_89_name
          ),
          
          sig$id83_cat,
          sig$id83_cat_abl,
          ctx$tumor_caution,
          
          priority = is_priority(
            sig$id83_cat
          )
        )
      },
      
      if (!is.null(sig$id83_mapped)) {
        
        render_styled_pair_block(
          paste(
            "3.3 83-type spectrum of best 83-type",
            "matching tumor",
            ctx$exemplar_83_name
          ),
          
          sig$id83_mapped,
          sig$id83_mapped_abl,
          ctx$tumor_caution,
          
          priority = is_priority(
            sig$id83_mapped
          )
        )
      },
      
      if (
        is.null(sig$id83_sig) &&
        is.null(sig$id83_cat) &&
        is.null(sig$id83_mapped)
      ) {
        
        div(
          class = "alert alert-warning",
          "No 83-type representation available."
        )
      }
    )
  }
  
  
  # ==============================================================================
  # 相似性分区
  # ==============================================================================
  
  build_similarity_detail <- function(ctx) {
    
    sig <- ctx$sig
    
    tagList(
      
      h3(
        "4. Similarities to other extracted mutational signatures",
        style = paste0(
          "color:#2c3e50;",
          "font-weight:700;",
          "margin-top:20px;",
          "margin-bottom:20px;",
          "border-bottom:2px solid #eee;",
          "padding-bottom:10px;"
        )
      ),
      
      if (length(sig$cosmic_std) > 0) {
        
        render_match_group(
          "Similar to COSMIC signatures",
          sig$cosmic_std,
          sig$cosmic_abl
        )
      },
      
      if (length(sig$jin_std) > 0) {
        
        render_match_group(
          "Similar to Jin et al. signatures",
          sig$jin_std,
          sig$jin_abl
        )
      },
      
      if (
        length(sig$cosmic_std) == 0 &&
        length(sig$jin_std) == 0
      ) {
        
        div(
          class = "alert alert-info",
          "No matching COSMIC or Jin et al. signatures."
        )
      }
    )
  }
  
  
  # ==============================================================================
  # Summary分区
  # ==============================================================================
  
  build_summary_detail <- function(ctx) {
    
    sig <- ctx$sig
    current_stats <- ctx$current_stats
    
    if (
      is.null(current_stats) ||
      nrow(current_stats) == 0
    ) {
      
      return(
        tagList(
          
          h3(
            "5. Similarity Summary",
            style = paste0(
              "color:#2c3e50;",
              "font-weight:700;",
              "margin-top:20px;",
              "border-bottom:2px solid #eee;",
              "padding-bottom:10px;"
            )
          ),
          
          div(
            class = "alert alert-info",
            "No similarity summary is available."
          )
        )
      )
    }
    
    safe_value <- function(
    column_name,
    default_value = "N/A"
    ) {
      
      if (!column_name %in% names(current_stats)) {
        return(default_value)
      }
      
      value <- current_stats[[column_name]][1]
      
      if (
        length(value) == 0 ||
        is.na(value)
      ) {
        return(default_value)
      }
      
      value
    }
    
    safe_number <- function(column_name) {
      
      value <- safe_value(
        column_name,
        "N/A"
      )
      
      if (identical(value, "N/A")) {
        return("N/A")
      }
      
      format(
        value,
        digits = 4
      )
    }
    
    tagList(
      
      h3(
        "5. Similarity Summary",
        style = paste0(
          "color:#2c3e50;",
          "font-weight:700;",
          "margin-top:20px;",
          "border-bottom:2px solid #eee;",
          "padding-bottom:10px;"
        )
      ),
      
      div(
        style = paste0(
          "overflow-x:auto;",
          "background:white;",
          "padding:25px;",
          "border-radius:12px;",
          "box-shadow:0 4px 15px rgba(0,0,0,0.05);"
        ),
        
        tags$table(
          class = "table table-hover",
          
          style = paste0(
            "width:100%;",
            "margin-top:10px;",
            "font-size:1.8rem;"
          ),
          
          tags$thead(
            tags$tr(
              style = "background:#f8f9fa;",
              
              tags$th("Metric"),
              tags$th("Result / Best Match"),
              tags$th("Cosine Similarity")
            )
          ),
          
          tags$tbody(
            
            tags$tr(
              tags$td(
                tags$strong(
                  "83-type Representation"
                )
              ),
              
              tags$td(sig$id83_name),
              
              tags$td(
                safe_number(
                  "sig83_v_exemplar_cos"
                )
              )
            ),
            
            tags$tr(
              tags$td(
                tags$strong(
                  "476-type Representation"
                )
              ),
              
              tags$td("Mapped from 476"),
              
              tags$td(
                safe_number(
                  "sig476_v_exemplar_cos"
                )
              )
            ),
            
            tags$tr(
              tags$td(
                tags$strong(
                  "Original Sample (89-type)"
                )
              ),
              
              tags$td(
                safe_value(
                  "exemplar_89"
                )
              ),
              
              tags$td(
                safe_number(
                  "sig89_v_exemplar_cos"
                )
              )
            ),
            
            tags$tr(
              tags$td(
                tags$strong(
                  "Best COSMIC Match"
                )
              ),
              
              tags$td(
                safe_value(
                  "best_match_cosmic"
                )
              ),
              
              tags$td(
                safe_number(
                  "cosine_v_cosmic"
                )
              )
            ),
            
            tags$tr(
              tags$td(
                tags$strong(
                  "Best Jin Match"
                )
              ),
              
              tags$td(
                safe_value(
                  "best_match_jin"
                )
              ),
              
              tags$td(
                safe_number(
                  "cosine_v_jin"
                )
              )
            ),
            
            tags$tr(
              tags$td(
                tags$strong(
                  "Best Koh Match"
                )
              ),
              
              tags$td(
                safe_value(
                  "best_match_koh"
                )
              ),
              
              tags$td(
                safe_number(
                  "cos_v_koh"
                )
              )
            )
          )
        )
      )
    )
  }
  
  
  # ==============================================================================
  # 详情页总入口
  #
  # 这里只构建当前选中的一个分区，不再一次性构建全部内容。
  # ==============================================================================
  
  build_detail_page <- function(
    sig_name,
    back_btn_id
  ) {
    
    ctx <- get_detail_context(sig_name)
    
    selected_section <- current_detail_section()
    
    body_content <- switch(
      
      selected_section,
      
      "89" = build_89_detail(ctx),
      
      "476" = build_476_detail(ctx),
      
      "83" = build_83_detail(ctx),
      
      "similar" = build_similarity_detail(ctx),
      
      "summary" = build_summary_detail(ctx),
      
      build_89_detail(ctx)
    )
    
    tagList(
      build_detail_header(
        ctx,
        back_btn_id
      ),
      
      body_content
    )
  }
  
  # ==============================================================================
  # 三个缩略图列表的UI缓存
  #
  # 每个列表在当前用户会话中只构建一次。
  # 从详情页返回缩略图时，直接读取已经构建好的HTML标签，
  # 不再重新执行完整的lapply()。
  # ==============================================================================
  
  thumbnail_list_ui_cache <- new.env(
    parent = emptyenv()
  )
  
  
  # ------------------------------------------------------------------------------
  # 获取或创建缩略图列表
  # ------------------------------------------------------------------------------
  
  get_or_build_thumbnail_ui <- function(
    cache_key,
    builder
  ) {
    
    # 当前列表尚未构建时，执行builder并存入缓存
    if (!exists(
      cache_key,
      envir = thumbnail_list_ui_cache,
      inherits = FALSE
    )) {
      
      built_ui <- builder()
      
      assign(
        cache_key,
        built_ui,
        envir = thumbnail_list_ui_cache
      )
    }
    
    # 已经构建过时，直接从内存中取出
    get(
      cache_key,
      envir = thumbnail_list_ui_cache,
      inherits = FALSE
    )
  }
  
  
  # ==============================================================================
  # 页面1：89-type Classification
  # ==============================================================================
  
  build_signature_list_ui <- function() {
    
    sig_names <- names(
      signature_groups
    )
    
    fluidRow(
      
      lapply(
        seq_along(sig_names),
        
        function(i) {
          
          group_name <- sig_names[i]
          sig <- signature_groups[[group_name]]
          thumb <- sig$thumbnail
          
          column(
            4,
            
            div(
              class = "thumbnail-card",
              
              # 鼠标移动到缩略图时，
              # 提前下载89-type前两张详情图
              onmouseenter = prefetch_images_js(
                c(
                  sig$id89_sig,
                  sig$id89_mapped
                )
              ),
              
              # 兼容用户快速点击和触摸设备
              onpointerdown = prefetch_images_js(
                c(
                  sig$id89_sig,
                  sig$id89_mapped
                )
              ),
              
              # 点击卡片后统一发送signature名称和89分区
              onclick = signature_card_click_js(
                group_name,
                "89"
              ),
              
              style = paste0(
                "cursor:pointer;",
                "background:#fff;",
                "border-radius:8px;",
                "margin-bottom:25px;",
                "overflow:hidden;",
                "padding:0 !important;"
              ),
              
              # signature名称
              h4(
                group_name,
                
                style = paste0(
                  "color:#2c3e50;",
                  "font-weight:700;",
                  "margin:12px 0 8px 0;",
                  "font-size:20px;",
                  "text-align:center;"
                )
              ),
              
              # 缩略图区域
              div(
                style = paste0(
                  "padding:4px 8px 10px 8px !important;",
                  "margin:0 !important;",
                  "line-height:0;",
                  "overflow:visible;",
                  "box-sizing:border-box;"
                ),
                
                if (
                  !is.null(thumb) &&
                  length(thumb) == 1 &&
                  !is.na(thumb) &&
                  nzchar(thumb)
                ) {
                  
                  tags$img(
                    src = thumb,
                    
                    loading = if (i <= 9) {
                      "eager"
                    } else {
                      "lazy"
                    },
                    
                    fetchpriority = if (i <= 6) {
                      "high"
                    } else if (i <= 9) {
                      "auto"
                    } else {
                      "low"
                    },
                    
                    decoding = "async",
                    
                    style = paste0(
                      "width:100%;",
                      "max-width:100%;",
                      "height:auto;",
                      "display:block;",
                      "margin:0 auto;",
                      "box-sizing:border-box;",
                      "background:#fff;",
                      "border-radius:8px;",
                      "border:none;"
                    )
                  )
                  
                } else {
                  
                  div(
                    style = paste0(
                      "color:#bdc3c7;",
                      "text-align:center;",
                      "padding:30px 0;"
                    ),
                    
                    icon(
                      "image",
                      class = "fa-3x"
                    )
                  )
                }
              )
            )
          )
        }
      )
    )
  }
  
  
  output$signature_display <- renderUI({
    
    sig_name <- current_integrated_sig()
    
    if (is.null(sig_name)) {
      
      get_or_build_thumbnail_ui(
        cache_key = "signature_89_list",
        builder = build_signature_list_ui
      )
      
    } else {
      
      build_detail_page(
        sig_name,
        "back_to_list"
      )
    }
  })
  
  
  # ==============================================================================
  # 页面2：476-type Classification
  # ==============================================================================
  
  build_id476_list_ui <- function() {
    
    sig_names <- names(
      signature_groups
    )
    
    fluidRow(
      
      lapply(
        seq_along(sig_names),
        
        function(i) {
          
          name <- sig_names[i]
          
          # 当前476卡片对应的完整signature信息
          sig476 <- signature_groups[[name]]
          
          thumb_path <- sig476$id476_thumb
          
          column(
            6,
            
            style = paste0(
              "padding-left:5px;",
              "padding-right:5px;"
            ),
            
            div(
              class = "thumbnail-card",
              
              # 鼠标进入时提前加载476-type前两张详情图
              onmouseenter = prefetch_images_js(
                c(
                  sig476$id476_sig,
                  sig476$id476_cat_link
                )
              ),
              
              onpointerdown = prefetch_images_js(
                c(
                  sig476$id476_sig,
                  sig476$id476_cat_link
                )
              ),
              
              # 点击476缩略图后直接打开476详情分区
              onclick = signature_card_click_js(
                name,
                "476"
              ),
              
              style = paste0(
                "cursor:pointer;",
                "background:#fff;",
                "border-radius:12px;",
                "margin-bottom:15px;",
                "overflow:hidden;",
                "padding:0 !important;"
              ),
              
              # signature名称
              h4(
                name,
                
                style = paste0(
                  "color:#2c3e50;",
                  "font-weight:700;",
                  "margin:15px 0 10px 0;",
                  "font-size:25px;",
                  "text-align:center;"
                )
              ),
              
              # 图片区域
              div(
                style = paste0(
                  "padding:0;",
                  "margin:0;",
                  "line-height:0;"
                ),
                
                if (
                  !is.null(thumb_path) &&
                  length(thumb_path) == 1 &&
                  !is.na(thumb_path) &&
                  nzchar(thumb_path)
                ) {
                  
                  tags$img(
                    src = thumb_path,
                    
                    # 前8张立即请求
                    loading = if (i <= 8) {
                      "eager"
                    } else {
                      "lazy"
                    },
                    
                    # 前4张使用高优先级
                    fetchpriority = if (i <= 4) {
                      "high"
                    } else if (i <= 8) {
                      "auto"
                    } else {
                      "low"
                    },
                    
                    decoding = "async",
                    
                    style = paste0(
                      "width:100%;",
                      "max-width:100%;",
                      "margin-left:0;",
                      "margin-bottom:0;",
                      "height:auto;",
                      "display:block;",
                      "clip-path:none;",
                      "border-bottom-left-radius:8px;",
                      "border-bottom-right-radius:8px;"
                    )
                  )
                  
                } else {
                  
                  div(
                    style = paste0(
                      "color:#bdc3c7;",
                      "text-align:center;",
                      "padding:50px 0;"
                    ),
                    
                    icon(
                      "image",
                      class = "fa-4x"
                    )
                  )
                }
              )
            )
          )
        }
      )
    )
  }
  
  
  output$id476_display <- renderUI({
    
    sig_name <- current_integrated_sig()
    
    if (is.null(sig_name)) {
      
      get_or_build_thumbnail_ui(
        cache_key = "signature_476_list",
        builder = build_id476_list_ui
      )
      
    } else {
      
      build_detail_page(
        sig_name,
        "back_to_476_list"
      )
    }
  })
  
  
  # ==============================================================================
  # 页面3：83-type Classification
  # ==============================================================================
  
  build_id83_list_ui <- function() {
    
    all_names <- names(
      id83_groups
    )
    
    if (length(all_names) == 0) {
      return(NULL)
    }
    
    # 每行显示3张
    chunk_size <- 3L
    
    id_chunks <- split(
      all_names,
      ceiling(
        seq_along(all_names) / chunk_size
      )
    )
    
    tagList(
      
      lapply(
        id_chunks,
        
        function(chunk_names) {
          
          fluidRow(
            style = paste0(
              "margin-left:-5px;",
              "margin-right:-5px;",
              "margin-bottom:10px;"
            ),
            
            lapply(
              chunk_names,
              
              function(id83_name) {
                
                id83_info <- id83_groups[[id83_name]]
                thumb <- id83_info$thumbnail
                
                # 当前缩略图在全部83-type中的实际序号
                thumb_index <- match(
                  id83_name,
                  all_names
                )
                
                # 83-type公共signature图
                prefetch_83_paths <- c(
                  id83_info$id83_all
                )
                
                # 只有一个成员时，
                # 同时预加载该成员的83详情图
                if (length(id83_info$members) == 1) {
                  
                  member_name <- id83_info$members[1]
                  
                  if (
                    member_name %in% names(signature_groups)
                  ) {
                    
                    member_sig <- signature_groups[[
                      member_name
                    ]]
                    
                    prefetch_83_paths <- c(
                      prefetch_83_paths,
                      member_sig$id83_cat,
                      member_sig$id83_mapped
                    )
                  }
                }
                
                column(
                  4,
                  
                  style = paste0(
                    "padding-left:5px;",
                    "padding-right:5px;"
                  ),
                  
                  div(
                    class = "thumbnail-card",
                    
                    onmouseenter = prefetch_images_js(
                      prefetch_83_paths
                    ),
                    
                    onpointerdown = prefetch_images_js(
                      prefetch_83_paths
                    ),
                    
                    onclick = id83_card_click_js(
                      id83_name
                    ),
                    
                    style = paste0(
                      "cursor:pointer;",
                      "background:#fff;",
                      "border-radius:12px;",
                      "margin-bottom:15px;",
                      "overflow:hidden;",
                      "padding:0 !important;",
                      "border:none !important;",
                      "box-shadow:0 4px 12px rgba(0,0,0,0.05);"
                    ),
                    
                    # 83-type名称
                    h4(
                      id83_name,
                      
                      style = paste0(
                        "color:#2c3e50;",
                        "font-weight:700;",
                        "margin:15px 0 10px 0;",
                        "font-size:20px;",
                        "text-align:center;"
                      )
                    ),
                    
                    # 图片区域
                    div(
                      style = paste0(
                        "padding:0 2px 2px 2px;",
                        "margin:0;",
                        "line-height:0;"
                      ),
                      
                      if (
                        !is.null(thumb) &&
                        length(thumb) == 1 &&
                        !is.na(thumb) &&
                        nzchar(thumb)
                      ) {
                        
                        tags$img(
                          src = thumb,
                          
                          # 前9张立即请求
                          loading = if (
                            thumb_index <= 9
                          ) {
                            "eager"
                          } else {
                            "lazy"
                          },
                          
                          # 前3张使用高优先级
                          fetchpriority = if (
                            thumb_index <= 3
                          ) {
                            "high"
                          } else if (
                            thumb_index <= 9
                          ) {
                            "auto"
                          } else {
                            "low"
                          },
                          
                          decoding = "async",
                          
                          style = paste0(
                            "width:100%;",
                            "height:auto;",
                            "display:block;",
                            "border-radius:4px;",
                            "border:none !important;"
                          )
                        )
                        
                      } else {
                        
                        div(
                          style = paste0(
                            "color:#bdc3c7;",
                            "text-align:center;",
                            "padding:50px 0;"
                          ),
                          
                          icon(
                            "image",
                            class = "fa-4x"
                          )
                        )
                      }
                    ),
                    
                    # 对应的89/476成员信息
                    div(
                      style = paste0(
                        "padding:15px 20px;",
                        "background:#fff;",
                        "text-align:left;",
                        "border:none;"
                      ),
                      
                      div(
                        "CORRESPONDS:",
                        
                        style = paste0(
                          "font-size:14px;",
                          "color:#95a5a6;",
                          "margin-bottom:3px;",
                          "font-weight:bold;",
                          "text-transform:uppercase;"
                        )
                      ),
                      
                      div(
                        paste(
                          id83_info$members,
                          collapse = ", "
                        ),
                        
                        style = paste0(
                          "font-size:18px;",
                          "color:#34495e;",
                          "line-height:1.4;",
                          "font-weight:500;"
                        )
                      )
                    )
                  )
                )
              }
            )
          )
        }
      )
    )
  }
  
  
  output$id83_display <- renderUI({
    
    sig_name <- current_integrated_sig()
    
    if (is.null(sig_name)) {
      
      get_or_build_thumbnail_ui(
        cache_key = "signature_83_list",
        builder = build_id83_list_ui
      )
      
    } else {
      
      build_detail_page(
        sig_name,
        "back_to_id83_list"
      )
    }
  })
  
  
  # ==============================================================================
  # 分类页面隐藏时暂停对应的renderUI
  #
  # 用户当前只会看到一个分类页面。
  # 隐藏页面不需要跟随current_integrated_sig反复重新渲染。
  # ==============================================================================
  
  outputOptions(
    output,
    "signature_display",
    suspendWhenHidden = TRUE
  )
  
  outputOptions(
    output,
    "id476_display",
    suspendWhenHidden = TRUE
  )
  
  outputOptions(
    output,
    "id83_display",
    suspendWhenHidden = TRUE
  )
  
  # ==============================================================================
  # 搜索逻辑与跳转逻辑
  # ==============================================================================
  observe({
    search_choices <- list("89-Type Signatures" = names(signature_groups), "83-Type Signatures" = names(id83_groups))
    updateSelectizeInput(session, "search_input", choices = search_choices, selected = character(0), server = FALSE) 
  })
  
  # 1. 制造工具：定义处理 83-type 跳转的辅助函数 (必须保留！)
  handle_83_selection <- function(id83_name) {
    
    if (
      is.null(id83_name) ||
      !id83_name %in% names(id83_groups)
    ) {
      return(invisible(NULL))
    }
    
    members <- id83_groups[[id83_name]]$members
    
    if (length(members) == 0) {
      
      showNotification(
        "No associated signatures were found.",
        type = "warning"
      )
      
      return(invisible(NULL))
    }
    
    # 只有一个成员时，直接进入83-type详情
    if (length(members) == 1) {
      
      open_signature_detail(
        members[1],
        "83"
      )
      
      return(invisible(NULL))
    }
    
    # 多个成员时，先让用户选择具体的89/476成员
    showModal(
      modalDialog(
        
        title = paste(
          "Select a signature from",
          id83_name
        ),
        
        tags$p(
          paste(
            "This 83-type signature represents a group",
            "containing multiple 89/476-type members.",
            "Please select one profile."
          )
        ),
        
        br(),
        
        div(
          style = paste0(
            "display:flex;",
            "flex-wrap:wrap;",
            "gap:10px;",
            "justify-content:center;"
          ),
          
          lapply(
            members,
            
            function(member_name) {
              
              tags$button(
                type = "button",
                
                class = paste(
                  "btn",
                  "btn-primary",
                  "btn-lg"
                ),
                
                onclick = id83_member_click_js(
                  member_name
                ),
                
                member_name
              )
            }
          )
        ),
        
        easyClose = TRUE,
        
        footer = modalButton("Cancel")
      )
    )
  }
  
  # 2. 搜索框逻辑：在这里面使用了上面那个工具
  search_logic <- function(query) {
    
    req(query)
    
    query <- trimws(
      as.character(query)
    )
    
    if (!nzchar(query)) {
      return(invisible(NULL))
    }
    
    names89 <- names(signature_groups)
    names83 <- names(id83_groups)
    
    # 精确匹配89-type
    if (query %in% names89) {
      
      updateNavbarPage(
        session,
        "navbar",
        selected = "89-type classification"
      )
      
      open_signature_detail(
        query,
        "89"
      )
      
      return(invisible(NULL))
    }
    
    # 精确匹配83-type
    if (query %in% names83) {
      
      updateNavbarPage(
        session,
        "navbar",
        selected = "83-type classification"
      )
      
      handle_83_selection(query)
      
      return(invisible(NULL))
    }
    
    # 模糊匹配
    matches89 <- grep(
      query,
      names89,
      ignore.case = TRUE,
      value = TRUE
    )
    
    matches83 <- grep(
      query,
      names83,
      ignore.case = TRUE,
      value = TRUE
    )
    
    total_matches <- length(matches89) + length(matches83)
    
    # 没有结果
    if (total_matches == 0) {
      
      showModal(
        modalDialog(
          
          title = "Not Found",
          
          paste0(
            "No signatures found matching '",
            query,
            "'."
          ),
          
          easyClose = TRUE,
          
          footer = modalButton("Close")
        )
      )
      
      return(invisible(NULL))
    }
    
    # 只有一个89-type结果
    if (
      length(matches89) == 1 &&
      length(matches83) == 0
    ) {
      
      updateNavbarPage(
        session,
        "navbar",
        selected = "89-type classification"
      )
      
      open_signature_detail(
        matches89[1],
        "89"
      )
      
      return(invisible(NULL))
    }
    
    # 只有一个83-type结果
    if (
      length(matches83) == 1 &&
      length(matches89) == 0
    ) {
      
      updateNavbarPage(
        session,
        "navbar",
        selected = "83-type classification"
      )
      
      handle_83_selection(
        matches83[1]
      )
      
      return(invisible(NULL))
    }
    
    # 多个结果
    choices_list <- c(
      
      if (length(matches89) > 0) {
        
        stats::setNames(
          matches89,
          paste0(
            matches89,
            " (89-type)"
          )
        )
      },
      
      if (length(matches83) > 0) {
        
        stats::setNames(
          matches83,
          paste0(
            matches83,
            " (83-type)"
          )
        )
      }
    )
    
    showModal(
      modalDialog(
        
        title = "Multiple Matches Found",
        
        tags$p(
          paste(
            "We found several signatures matching your query.",
            "Please select one."
          )
        ),
        
        radioButtons(
          inputId = "fuzzy_select",
          label = NULL,
          choices = choices_list
        ),
        
        footer = tagList(
          modalButton("Cancel"),
          
          actionButton(
            "confirm_fuzzy_search",
            "Go",
            class = "btn-primary"
          )
        ),
        
        easyClose = TRUE
      )
    )
  }
  
  observeEvent(
    input$search_btn,
    {
      search_logic(
        input$search_input
      )
    }
  )
  
  observeEvent(
    input$confirm_fuzzy_search,
    {
      req(input$fuzzy_select)
      
      selected_value <- input$fuzzy_select
      
      removeModal()
      
      search_logic(
        selected_value
      )
    }
  )
  
  # ==============================================================================
  # 89-type和476-type缩略图统一点击逻辑
  #
  # 所有89和476缩略图共同使用一个Shiny输入：
  # input$signature_card_click
  # ==============================================================================
  
  observeEvent(
    input$signature_card_click,
    
    {
      
      click_data <- input$signature_card_click
      
      # 输入必须是一个列表，并且必须同时包含sig和section
      req(
        is.list(click_data),
        !is.null(click_data$sig),
        !is.null(click_data$section)
      )
      
      sig_name <- as.character(
        click_data$sig
      )[1]
      
      section_name <- as.character(
        click_data$section
      )[1]
      
      # 防止浏览器手动构造不存在的signature名称
      if (
        !sig_name %in% names(signature_groups)
      ) {
        
        showNotification(
          "The requested signature was not found.",
          type = "error"
        )
        
        return(invisible(NULL))
      }
      
      # 这个统一入口目前只允许89和476缩略图
      if (
        !section_name %in% c(
          "89",
          "476"
        )
      ) {
        
        showNotification(
          "The requested signature section is invalid.",
          type = "error"
        )
        
        return(invisible(NULL))
      }
      
      open_signature_detail(
        sig_name,
        section_name
      )
    },
    
    ignoreInit = TRUE
  )
  
  # ==============================================================================
  # 返回缩略图列表
  # ==============================================================================
  
  return_to_thumbnail_list <- function() {
    
    current_integrated_sig(NULL)
    
    shinyjs::runjs(
      "window.scrollTo(0, 0);"
    )
  }
  
  observeEvent(
    input$back_to_list,
    {
      return_to_thumbnail_list()
    },
    ignoreInit = TRUE
  )
  
  observeEvent(
    input$back_to_476_list,
    {
      return_to_thumbnail_list()
    },
    ignoreInit = TRUE
  )
  
  observeEvent(
    input$back_to_id83_list,
    {
      return_to_thumbnail_list()
    },
    ignoreInit = TRUE
  )
  
  # ==============================================================================
  # 详情页分区切换
  #
  # 只有点击某个分区时，才创建该分区对应的DOM和图片。
  # ==============================================================================
  
  change_detail_section <- function(section_name) {
    
    valid_sections <- c(
      "89",
      "476",
      "83",
      "similar",
      "summary"
    )
    
    if (!section_name %in% valid_sections) {
      return(invisible(NULL))
    }
    
    # 没有打开signature时，不执行分区切换
    if (is.null(current_integrated_sig())) {
      return(invisible(NULL))
    }
    
    # 已经处于当前分区时，不重复渲染
    if (identical(
      current_detail_section(),
      section_name
    )) {
      return(invisible(NULL))
    }
    
    current_detail_section(
      section_name
    )
    
    shinyjs::runjs(
      "window.scrollTo(0, 0);"
    )
  }
  
  observeEvent(
    input$detail_section_request,
    {
      change_detail_section(
        input$detail_section_request
      )
    },
    ignoreInit = TRUE
  )
  
  # ==============================================================================
  # 83-type缩略图统一点击逻辑
  # ==============================================================================
  
  observeEvent(
    input$id83_card_click,
    {
      
      click_data <- input$id83_card_click
      
      req(
        is.list(click_data),
        !is.null(click_data$id83)
      )
      
      id83_name <- as.character(
        click_data$id83
      )[1]
      
      if (
        !id83_name %in% names(id83_groups)
      ) {
        
        showNotification(
          "The requested 83-type signature was not found.",
          type = "error"
        )
        
        return(invisible(NULL))
      }
      
      handle_83_selection(
        id83_name
      )
    },
    
    ignoreInit = TRUE
  )
  
  # ==============================================================================
  # 83-type多成员弹窗统一选择逻辑
  # ==============================================================================
  
  observeEvent(
    input$id83_member_click,
    {
      
      click_data <- input$id83_member_click
      
      req(
        is.list(click_data),
        !is.null(click_data$member)
      )
      
      member_name <- as.character(
        click_data$member
      )[1]
      
      if (
        !member_name %in% names(signature_groups)
      ) {
        
        showNotification(
          "The requested signature was not found.",
          type = "error"
        )
        
        return(invisible(NULL))
      }
      
      removeModal()
      
      open_signature_detail(
        member_name,
        "83"
      )
    },
    
    ignoreInit = TRUE
  )
  
  # ==============================================================================
  # 独立signature报告跳转
  #
  # 使用一个统一输入，不再为每个signature创建单独observer。
  # ==============================================================================
  
  valid_report_names <- vapply(
    names(signature_groups),
    
    function(signature_name) {
      
      report_name <- gsub(
        "\u03b1",
        "_alpha",
        signature_name
      )
      
      gsub(
        "\u03b2",
        "_beta",
        report_name
      )
    },
    
    character(1)
  )
  
  observeEvent(
    input$repertoire_signature_request,
    {
      
      search_name <- as.character(
        input$repertoire_signature_request
      )
      
      req(
        length(search_name) == 1,
        nzchar(search_name)
      )
      
      # 防止非法文件名或手动构造路径
      if (!search_name %in% valid_report_names) {
        
        showNotification(
          "The requested signature report was not found.",
          type = "error"
        )
        
        return(invisible(NULL))
      }
      
      message(
        "\n[跳转成功] 正在加载独立报告: ",
        search_name,
        ".html"
      )
      
      current_repertoire_url(
        static_page_url(
          paste0(
            search_name,
            ".html"
          )
        )
      )
      
      iframe_render_version(
        isolate(
          iframe_render_version()
        ) + 1L
      )
      
      # 当前不在 Overview Table 时，
      # 接下来程序会主动切换到 Overview。
      #
      # 设置一次性标记，防止 navbar 观察器把刚打开的独立报告
      # 立刻重置回 overview_table.html。
      if (!identical(
        isolate(input$navbar),
        "Overview Table"
      )) {
        
        skip_next_overview_reset(TRUE)
        
        updateNavbarPage(
          session,
          "navbar",
          selected = "Overview Table"
        )
        
      } else {
        
        # 当前本来就在 Overview，不会发生 navbar 切换，
        # 因此不能留下未使用的跳过标记。
        skip_next_overview_reset(FALSE)
      }
      
    },
    ignoreInit = TRUE
  )
  
  # ==============================================================================
  # 原始Overview Table和独立报告iframe
  #
  # 保留原始overview_table.html中的完整表格内容，不重新构造字段。
  # ==============================================================================
  
  output$dynamic_repertoire_iframe <- renderUI({
    
    # URL或版本变化时重新生成iframe
    iframe_render_version()
    
    tags$iframe(
      id = "repertoire_iframe",
      
      src = current_repertoire_url(),
      
      style = paste0(
        "border:none;",
        "border-radius:8px;",
        "box-shadow:0 4px 12px rgba(0,0,0,0.1);",
        "width:80%;",
        "margin-left:10%;",
        "height:1000px;",
        "zoom:1.2;"
      )
    )
  })
  
  # Overview Table 隐藏时暂停 iframe，
  # 避免它在后台和缩略图争抢加载资源
  outputOptions(
    output,
    "dynamic_repertoire_iframe",
    suspendWhenHidden = TRUE
  )
  # ==============================================================================
  
  # ==============================================================================
  # 图片查看器
  #
  # 所有图片统一发送 input$open_modal_image。
  # 不再在 observe() 中动态创建 observeEvent()，避免观察器不断累积。
  # ==============================================================================
  
  observeEvent(input$open_modal_image, {
    
    req(input$open_modal_image)
    
    img_path <- input$open_modal_image
    
    showModal(
      modalDialog(
        
        title = NULL,
        
        div(
          style = "text-align:center;",
          
          tags$img(
            src = img_path,
            
            style = paste0(
              "max-width:100%;",
              "max-height:85vh;",
              "border-radius:8px;",
              "box-shadow:0 5px 15px rgba(0,0,0,0.2);"
            )
          )
        ),
        
        footer = tagList(
          
          tags$a(
            href = img_path,
            download = basename(img_path),
            class = "btn btn-primary",
            icon("download"),
            "Download",
            style = "color:white;"
          ),
          
          modalButton("Close")
        ),
        
        size = "l",
        easyClose = TRUE,
        fade = TRUE
      )
    )
    
  }, ignoreInit = TRUE)
  
}
