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

# ==============================================================================
# 1. 数据加载与预处理 (路径已适配你现在的 data/ 文件夹)
source("Indel_process.R")
# ==============================================================================

# --- 关键路径定义 ---
data_path_prefix <- "data" 
img_subdir <- "parallel_plots/" 

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
# 2. Server 函数
# ==============================================================================

server <- function(input, output, session) {
  
  observe({ runjs("$('.sidebar-menu li').removeClass('active');") })
  
  current_integrated_sig <- reactiveVal(NULL)
  
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
    current_vcf_path("example_data/test_file_1_hg38.vcf")
    
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
    current_vcf_path("example_data/test_file_2_hg19.vcf")
    
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
          icon("file-upload"), paste(" Custom file loaded:", input$vcf_file$name))
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
    
    # 强制将矩阵的列名（即文件名）传给 plot_title 参数
    mSigPlot::plot_ID83(
      vcf_results$cat83, 
      plot_title = colnames(vcf_results$cat83)[1]  # 👈 这一行是显示文件名的终极秘诀
    )
  })
  
  # 同理，渲染 89 Type 图表
  output$plot_id89 <- renderPlot({
    req(vcf_results$cat89)
    mSigPlot::plot_ID89(
      vcf_results$cat89, 
      plot_title = colnames(vcf_results$cat89)[1] 
    )
  })
  
  # 同理，渲染 476 Type 图表
  output$plot_id476 <- renderPlot({
    req(vcf_results$cat476)
    mSigPlot::plot_ID476(
      vcf_results$cat476, 
      plot_title = colnames(vcf_results$cat476)[1] 
    )
  })
  
  # ============================================================================
  # 4. 下载逻辑 (紧跟在绘图后面)
  # ============================================================================
  
  # 1. 动态渲染下载按钮
  output$download_ui <- renderUI({
    req(vcf_results$cat83, vcf_results$cat89, vcf_results$cat476)
    
    downloadButton("download_all", "Download Matrices (ZIP)", 
                   class = "btn-success", 
                   style = "font-size: 1.45rem; font-weight: bold; padding: 12px 5px; width: 100%; margin-top: 10px; white-space: normal; line-height: 1.3; border-radius: 8px;")
  })
  
  # 2. 真正的下载处理逻辑
  output$download_all <- downloadHandler(
    filename = function() {
      paste0("Indel_Matrices_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      temp_dir <- tempdir()
      old_wd <- setwd(temp_dir)
      on.exit(setwd(old_wd)) 
      
      # 将三个矩阵写成 CSV 文件
      write.csv(as.data.frame(vcf_results$cat83), "ID83_matrix.csv", row.names = TRUE)
      write.csv(as.data.frame(vcf_results$cat89), "ID89_matrix.csv", row.names = TRUE)
      write.csv(as.data.frame(vcf_results$cat476), "ID476_matrix.csv", row.names = TRUE)
      
      # 打包成 ZIP
      utils::zip(zipfile = file, 
                 files = c("ID83_matrix.csv", "ID89_matrix.csv", "ID476_matrix.csv"))
    },
    contentType = "application/zip"
  )
  
  # 2. 真正的下载处理逻辑
  output$download_all <- downloadHandler(
    filename = function() {
      # 生成一个带时间戳的 zip 文件名，例如: Indel_Matrices_20231025.zip
      paste0("Indel_Matrices_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      # 获取当前的临时目录
      temp_dir <- tempdir()
      
      # 保存当前的运行路径，确保打包完能切换回来
      old_wd <- setwd(temp_dir)
      on.exit(setwd(old_wd)) 
      
      # 将三个矩阵写成 CSV 文件
      # 注意：转换为 data.frame 以确保 rownames(突变类型) 被正确保存
      write.csv(as.data.frame(vcf_results$cat83), "ID83_matrix.csv", row.names = TRUE)
      write.csv(as.data.frame(vcf_results$cat89), "ID89_matrix.csv", row.names = TRUE)
      write.csv(as.data.frame(vcf_results$cat476), "ID476_matrix.csv", row.names = TRUE)
      
      # 使用 R 自带的 zip 工具将这三个文件打包成传给浏览器的 file 对象
      utils::zip(zipfile = file, 
                 files = c("ID83_matrix.csv", "ID89_matrix.csv", "ID476_matrix.csv"))
    },
    contentType = "application/zip"
  )
  
  # URL 路由机制
  observe({
    nav <- input$navbar
    id <- current_integrated_sig() 
    
    qs <- paste0("?nav=", URLencode(nav, reserved = TRUE))
    if (!is.null(id)) {
      qs <- paste0(qs, "&id=", URLencode(id, reserved = TRUE))
    }
    
    current_qs <- session$clientData$url_search
    if (qs != current_qs) {
      updateQueryString(qs, mode = "push")
    }
  })
  
  observeEvent(session$clientData$url_search, {
    query <- getQueryString()
    if (length(query) == 0) {
      current_integrated_sig(NULL)
    } else {
      if (!is.null(query$nav) && isolate(input$navbar) != query$nav) {
        updateNavbarPage(session, "navbar", selected = query$nav)
      }
      if (!is.null(query$id)) {
        current_integrated_sig(query$id)
      } else {
        current_integrated_sig(NULL)
      }
    }
  })
  
  observeEvent(input$home_goto_89, { updateNavbarPage(session, "navbar", selected = "89-type classification") })
  observeEvent(input$home_goto_83, { updateNavbarPage(session, "navbar", selected = "83-type classification") })
  observeEvent(input$home_goto_476, { updateNavbarPage(session, "navbar", selected = "476-type classification") })
  
  # ============================================================================
  # 辅助渲染函数
  # ============================================================================
  img_block <- function(img_path, width="100%", border=FALSE) {
    if (is.null(img_path) || is.na(img_path)) return(NULL)
    if(length(img_path) > 1) img_path <- img_path[1] 
    div(style = "text-align: center; margin-bottom: 10px;",
        tags$img(src = img_path, class = "signature-img",
                 style = paste0("width: ", width, "; max-width: 900px; ", if(border) "border: 1px solid #ddd; padding: 2px;" else ""),
                 onclick = sprintf("Shiny.setInputValue('%s', new Date().getTime());", paste0("img_", basename(img_path))))
    )
  }
  
  render_styled_pair_block <- function(title_text, std_img, abl_img, caution_text = NULL) {
    div(class = "id83-section", style = "margin-bottom: 30px; padding: 30px; background: #fff; box-shadow: 0 5px 20px rgba(0,0,0,0.03);",
        if(!is.null(caution_text)) caution_text,
        h4(title_text, style = "color: #2c3e50; font-weight: 700; margin-top: 0; margin-bottom: 20px; font-size: 1.2rem;"),
        if (!is.null(std_img)) {
          tags$img(src = std_img, class = "signature-img",
                   onclick = paste0("Shiny.setInputValue('open_modal_image', '", std_img, "', {priority: 'event'})"),
                   style = "width:100%; margin-bottom: 15px;")
        } else { div("Standard scale image not available", style="color:#ccc; padding: 10px;") },
        
        if (!is.null(abl_img)) {
          tags$details(
            style = "margin-top: 10px;",
            tags$summary(
              style = "cursor: pointer; color: #3498db; font-weight: 500; font-size: 1.35rem; outline: none; user-select: none;",
              icon("chevron-circle-right", style="color: #27ae60; margin-right: 8px;"), 
              "Click here to see with insertions and deletions of T in long poly-T suppressed."
            ),
            div(style = "margin-top: 15px; border-top: 1px dashed #eee; padding-top: 15px;",
                tags$img(src = abl_img, class = "signature-img",
                         onclick = paste0("Shiny.setInputValue('open_modal_image', '", abl_img, "', {priority: 'event'})"),
                         style = "width:100%;border: none !important; box-shadow: none !important")
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
            render_styled_pair_block(paste("Match:", clean_name), std_img, abl_img)
          })
      )
    )
  }
  
  # ============================================================================
  # 终极“三合一”综合详情页生成器 (1:1 复刻 HTML)
  # ============================================================================
  build_integrated_page <- function(sig_name, back_btn_id) {
    sig <- signature_groups[[sig_name]]
    
    current_stats <- if (!is.null(sig_stats_df)) sig_stats_df[sig_stats_df$signature_id == sig_name, ] else NULL
    
    # 提取 Linking Tumor 编号 (用于 3.2)
    exemplar_89_name <- if(!is.null(current_stats)) current_stats$exemplar_89 else "Exemplar Sample"
    
    # 提取 Best 83-type matching tumor 编号 (用于 3.3)
    exemplar_83_name <- if(!is.null(current_stats)) current_stats$exemplar_83 else "Exemplar Sample"
    
    # 提取 476 的相关参数
    exemplar_476_name <- if(!is.null(current_stats)) current_stats$exemplar_476 else "Exemplar Sample"
    cos_476_link <- if(!is.null(current_stats) && !is.na(current_stats$sig476_v_linking_cos)) format(current_stats$sig476_v_linking_cos, digits=4) else "N/A"
    cos_476_best <- if(!is.null(current_stats) && !is.na(current_stats$sig476_v_exemplar_cos)) format(current_stats$sig476_v_exemplar_cos, digits=4) else "N/A"
    
    # Poly-T 警告信息
    polyT_sigs <- c("C_ID7", "ID_J", "C_ID10", "ID_N", "ID_O")
    tumor_caution <- if (sig$id83_name %in% polyT_sigs) {
      div(style="font-size: 13px; color: #c0392b; background: #fadbd8; padding: 12px; border-radius: 8px; margin-bottom: 20px; border-left: 5px solid #c0392b; display: flex; align-items: center;",
          icon("triangle-exclamation", style="margin-right: 10px; font-size: 1.2em;"), 
          div("For the supporting tumor plot, mutation counts for insertions and deletions of T in long-poly-T contexts were set to 0. They were also set to 0 when calculating cosine similarity with the signature. The signature was not altered when computing the cosine similarity.")
      )
    } else { NULL }
    
    tagList(
      # 顶部导航
      div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 20px;", 
          actionButton(back_btn_id, "← Back to Thumbnails", class = "btn-back"), div()),
      
      h2(paste("Integrated Signature Profile:", sig_name), style = "color:#2c3e50; font-weight:700; margin-top: 0; margin-bottom: 20px; border-bottom: 2px solid #eee; padding-bottom: 10px;"),
      
      div(style="margin-bottom: 25px; font-size: 15px; background: #f8f9fa; padding: 20px; border-radius: 12px; border-left: 5px solid #34495e; box-shadow: 0 4px 15px rgba(0,0,0,0.05);",
          div(style="margin-bottom: 8px;", tags$span("Base 89-type Unit: ", style="font-weight:bold; color:#7f8c8d; margin-right: 10px;"), tags$span(sig_name, style="color:#e67e22; font-weight:bold;")),
          div(style="margin-bottom: 8px;", tags$span("Associated 476-type: ", style="font-weight:bold; color:#7f8c8d; margin-right: 10px;"), tags$span(sig_name, style="color:#9b59b6; font-weight:bold;")),
          div(tags$span("Associated 83-type Group: ", style="font-weight:bold; color:#7f8c8d; margin-right: 10px;"), tags$span(sig$id83_name, style="color:#27ae60; font-weight:bold;"))
      ),
      
      if (!is.null(sig$note)) shiny::markdown(sig$note),
      if (nchar(sig$aetiology) > 0) div(style="background:#e8f5e9; padding:15px; border-radius: 8px; margin-bottom: 20px; border-left: 4px solid #2ecc71;", icon("lightbulb"), strong(" Etiology: "), sig$aetiology),
      
      # --- 1. 89-type Classification ---
      h3("1. 89-type classification", style = "color: #2c3e50; font-weight: 700; margin-top: 40px; margin-bottom: 20px;"),
      div(class = "id83-section", div(class = "id83-label", style="border-left-color: #e67e22;", icon("dna"), " Signature Profile"), 
          tags$img(src = sig$id89_sig, class = "signature-img", onclick = paste0("Shiny.setInputValue('open_modal_image', '", sig$id89_sig, "', {priority: 'event'})"), style = "width:100%;")),
      
      if (!is.null(sig$id89_mapped)) {
        div(class = "id83-section", div(class = "id83-label", style="border-left-color: #f39c12;", icon("exchange-alt"), " Mapped Signature"), 
            tags$img(src = sig$id89_mapped, class = "signature-img", onclick = paste0("Shiny.setInputValue('open_modal_image', '", sig$id89_mapped, "', {priority: 'event'})"), style = "width:100%;"))
      },
      
      if (length(sig$koh_matches) > 0) {
        tagList(
          h4("Matches to Koh et al. signatures", style = "color: #e67e22; font-weight: bold; margin-top: 25px; margin-bottom: 15px;"),
          div(style = "background: #fff; padding: 20px; border: 1px solid #eee; border-radius: 8px;",
              lapply(sig$koh_matches, function(p) {
                div(style="margin-bottom:20px;", tags$img(src = p, class = "signature-img", onclick = paste0("Shiny.setInputValue('open_modal_image', '", p, "', {priority: 'event'})"), style = "width:100%;"))
              })
          )
        )
      },
      
      if (length(sig$id89_decomp) > 0) { 
        tagList(h4("Sample Decomposition Analysis", style = "color: #7f8c8d; margin-top: 20px; font-weight: bold; text-align: center;"), 
                div(class = "id83-section", style="background: #fff;", fluidRow(lapply(seq_along(sig$id89_decomp), function(i) { 
                  lbl <- c("Spectrum (Observed)", "Target Partial Spectrum", "Residual")[i]; 
                  column(4, div(class = "img-label", lbl), tags$img(src = sig$id89_decomp[i], class = "signature-img", onclick = paste0("Shiny.setInputValue('open_modal_image', '", sig$id89_decomp[i], "', {priority: 'event'})"), style = "width:100%;"))}))))
      },
      
      # --- 2. 476-type Classification ---
      h3("2. 476-type classification", style = "color: #2c3e50; font-weight: 700; margin-top: 40px; margin-bottom: 20px;"),
      
      if (!is.null(sig$id476_sig)) {
        render_styled_pair_block("2.1 476-type signature", sig$id476_sig, NULL)
      },
      
      if (!is.null(sig$id476_cat_link)) {
        render_styled_pair_block(paste0("2.2 476-type spectrum of the linking tumor ", exemplar_89_name, "; cosine similarity to the extracted 476-type signature is ", cos_476_link), sig$id476_cat_link, NULL)
      },
      
      if (!is.null(sig$id476_cat_best)) {
        render_styled_pair_block(paste0("2.3 476-type spectrum of best 476-type matching tumor ", exemplar_476_name, "; cosine similarity to the extracted 476-type signature is ", cos_476_best), sig$id476_cat_best, NULL)
      },
      
      if (is.null(sig$id476_sig) && is.null(sig$id476_cat_link) && is.null(sig$id476_cat_best)) {
        div(class = "alert alert-warning", "No 476-type representation available.")
      },
      
      # --- 3. 83-type Classification ---
      h3("3. 83-type classification", style = "color: #2c3e50; font-weight: 700; margin-top: 40px; margin-bottom: 30px; border-bottom: 2px solid #eee; padding-bottom: 10px;"),
      if (!is.null(sig$note_id83)) {
        div(style = "background-color: #e3f2fd; padding: 15px; border-radius: 8px; margin-bottom: 20px;", shiny::markdown(sig$note_id83))
      },
      
      # 3.1: De-novo signature
      render_styled_pair_block("3.1 ", sig$id83_sig, sig$id83_sig_abl),
      
      # 3.2: Linking tumor spectrum (使用 id83_cat 图片，匹配 exemplar_89)
      render_styled_pair_block(paste("3.2 83-type spectrum of the linking tumor", exemplar_89_name), sig$id83_cat, sig$id83_cat_abl, tumor_caution),
      
      # 3.3: Best 83-type matching tumor spectrum (使用 83match 图片，匹配 exemplar_83)
      if (!is.null(sig$id83_mapped)) {
        render_styled_pair_block(paste("3.3 83-type spectrum of best 83-type matching tumor", exemplar_83_name), sig$id83_mapped, sig$id83_mapped_abl, tumor_caution)
      },
      
      # --- 4. Similar ---
      if (length(sig$cosmic_std) > 0 || length(sig$jin_std) > 0) {
        tagList(
          h3("4. Similarities to other extracted mutational signatures", style = "color: #2c3e50; font-weight: 700; margin-top: 50px; margin-bottom: 20px; border-bottom: 2px solid #eee; padding-bottom: 10px;"),
          render_match_group("Similar to COSMIC signatures", sig$cosmic_std, sig$cosmic_abl),
          render_match_group("Similar to Jin et al. signatures", sig$jin_std, sig$jin_abl)
        )
      },
      
      # --- 5. Summary Table ---
      if (!is.null(current_stats) && nrow(current_stats) > 0) {
        tagList(
          h3("5. Similarity Summary", style = "color: #2c3e50; font-weight: 700; margin-top: 50px; border-bottom: 2px solid #eee; padding-bottom: 10px;"),
          div(style = "overflow-x: auto; background: white; padding: 25px; border-radius: 12px; box-shadow: 0 4px 15px rgba(0,0,0,0.05);",
              tags$table(class = "table table-hover", style = "width: 100%; margin-top: 10px; font-size: 2.1rem;",
                         tags$thead(tags$tr(style="background:#f8f9fa;", tags$th("Metric"), tags$th("Result / Best Match"), tags$th("Cosine Similarity"))),
                         tags$tbody(
                           tags$tr(tags$td(tags$strong("83-type Representation")), tags$td(sig$id83_name), tags$td(format(current_stats$sig83_v_exemplar_cos, digits=4))),
                           tags$tr(tags$td(tags$strong("476-type Representation")), tags$td("Mapped from 476"), tags$td(format(current_stats$sig476_v_exemplar_cos, digits=4))),
                           tags$tr(tags$td(tags$strong("Original Sample (89-type)")), tags$td(current_stats$exemplar_89), tags$td(format(current_stats$sig89_v_exemplar_cos, digits=4))),
                           tags$tr(tags$td(tags$strong("Best COSMIC Match")), tags$td(current_stats$best_match_cosmic), tags$td(format(current_stats$cosine_v_cosmic, digits=4))),
                           tags$tr(tags$td(tags$strong("Best Jin Match")), tags$td(current_stats$best_match_jin), tags$td(format(current_stats$cosine_v_jin, digits=4))),
                           tags$tr(tags$td(tags$strong("Best Koh Match")), tags$td(current_stats$best_match_koh), tags$td(format(current_stats$cos_v_koh, digits=4)))
                         )
              )
          )
        )
      }
    )
  }
  
  # ============================================================================
  # 页面 1: 89-type Classification
  # ============================================================================
  output$signature_display <- renderUI({
    if (is.null(current_integrated_sig())) {
      fluidRow(
        lapply(names(signature_groups), function(group_name) {
          sig <- signature_groups[[group_name]]
          thumb <- sig$thumbnail
          column(4,
                 div(class = "thumbnail-card",
                     onclick = paste0("$('#show_", group_name, "').click()"),
                     # 【关键】移除 padding，设置 overflow:hidden 让图片圆角贴合边框
                     style = "cursor: pointer; background: #fff; border-radius: 8px; margin-bottom: 25px; overflow: hidden; padding: 0 !important;", 
                     actionLink(inputId = paste0("show_", group_name), label = NULL, style="display:none;"),
                     
                     # 1. 名字区：减少上下 margin，让它更靠近顶部
                     h4(group_name, style = "color:#2c3e50; font-weight:700; margin: 12px 0 8px 0; font-size: 20px; text-align: center;"),
                     
                     # 2. 图片区：取消所有内边距，让图片左右完全撑满
                     div(style = "padding: 0 10px 10px 10px; margin: 0; line-height: 0;", # line-height:0 消除图片下方微小的间隙
                         if (!is.null(thumb) && file.exists(file.path("www", thumb))) {
                           tags$img(src = thumb, style = "width: 100%; height: auto; display: block; border-bottom-left-radius: 8px; border-bottom-right-radius: 8px;")
                         } else { 
                           div(style = "color:#bdc3c7; text-align: center; padding: 30px 0;", icon("image", class="fa-3x")) 
                         }
                     )
                 )
          )
        })
      )
    } else {
      build_integrated_page(current_integrated_sig(), "back_to_list")
    }
  })
  
  # ============================================================================
  # 页面 2: 476-type Classification
  # ============================================================================
  output$id476_display <- renderUI({
    if (is.null(current_integrated_sig())) {
      sig_names <- names(signature_groups)
      fluidRow(
        lapply(sig_names, function(name) {
          thumb_path <- signature_groups[[name]]$id476_thumb
          real_thumb_path <- if(!is.null(thumb_path)) file.path("www", thumb_path) else NULL
          column(6,
                 style = "padding-left: 5px; padding-right: 5px;",
                 div(class = "thumbnail-card",
                     onclick = paste0("$('#show_", name, "').click()"),
                     # 【关键】移除 padding，设置 overflow:hidden 让图片圆角贴合边框
                     style = "cursor: pointer; background: #fff; border-radius: 12px; margin-bottom: 15px; overflow: hidden; padding: 0 !important;", 
                     actionLink(inputId = paste0("show_", name), label = NULL, style="display:none;"),
                     
                     # 1. 名字区：减少上下 margin，让它更靠近顶部
                     h4(name, style = "color:#2c3e50; font-weight:700; margin: 15px 0 10px 0; font-size: 25px; text-align: center;"),
                     
                     # 2. 图片区：取消所有内边距，让图片左右完全撑满
                     div(style = "padding: 0 ; margin: 0; line-height: 0;", # line-height:0 消除图片下方微小的间隙
                         if (!is.null(real_thumb_path) && file.exists(real_thumb_path)) {
                           tags$img(src = thumb_path, style = "width: 103%;max-width: none; margin-left: -2%;height: auto; display: block; border-bottom-left-radius: 8px; border-bottom-right-radius: 8px;margin-bottom: -2%; clip-path: inset(0 0 2% 0);")
                         } else { 
                           div(style = "color:#bdc3c7; text-align: center; padding: 50px 0;", icon("image", class="fa-4x")) 
                         }
                     )
                 )
          )
        })
      )
    } else {
      build_integrated_page(current_integrated_sig(), "back_to_476_list")
    }
  })
  
  # ============================================================================
  # 页面 3: 83-type Classification
  # ============================================================================
  output$id83_display <- renderUI({
    if (is.null(current_integrated_sig())) {
      all_names <- names(id83_groups)
      if (length(all_names) == 0) return(NULL)
      
      # 将 chunk_size 设为 2，确保逻辑上每行只处理两个
      chunk_size <- 2
      id_chunks <- split(all_names, ceiling(seq_along(all_names) / chunk_size))
      
      tagList(
        lapply(id_chunks, function(chunk_names) {
          fluidRow(
            style = "margin-left: -5px; margin-right: -5px; margin-bottom: 10px;", 
            lapply(chunk_names, function(id83_name) {
              id83_info <- id83_groups[[id83_name]]
              thumb <- id83_info$thumbnail
              
              # 将 column(4) 改为 column(6)
              column(6, 
                     style = "padding-left: 5px; padding-right: 5px;",
                     div(class = "thumbnail-card",
                         onclick = paste0("$('#show_id83_", id83_name, "').click()"),
                         # 统一全白背景，无边框，无阴影（或极淡阴影）
                         style = "cursor: pointer; background: #fff; border-radius: 12px; margin-bottom: 15px; overflow: hidden; padding: 0 !important; border: none !important; box-shadow: 0 4px 12px rgba(0,0,0,0.05);",
                         actionLink(inputId = paste0("show_id83_", id83_name), label = NULL, style="display:none;"),
                         
                         # 1. 名字：采用你 Page 2 的 25px 大字体
                         h4(id83_name, style = "color:#2c3e50; font-weight:700; margin: 15px 0 10px 0; font-size: 25px; text-align: center;"),
                         
                         # 2. 图片区：采用 Page 2 的 5px 留白控制，让图片尽可能大
                         div(style = "padding: 0 2px 2px 2px; margin: 0; line-height: 0;",
                             if (!is.null(thumb) && file.exists(file.path("www", thumb))) {
                               tags$img(src = thumb, 
                                        # width: 100% 配合 height: auto 确保不失真地变大
                                        style = "width: 100%; height: auto; display: block; border-radius: 4px; border: none !important;")
                             } else { 
                               div(style = "color:#bdc3c7; text-align: center; padding: 50px 0;", icon("image", class="fa-4x")) 
                             }
                         ),
                         
                         # 3. Member 信息：去掉黑线，统一白色，加大字体
                         div(style = "padding: 15px 20px; background: #fff; text-align: left; border: none;",
                             div(style = "font-size: 14px; color: #95a5a6; margin-bottom: 3px; font-weight: bold; text-transform: uppercase;", "CONTAINS:"),
                             # 加大 Member 名字字体
                             div(style = "font-size: 18px; color: #34495e; line-height: 1.4; font-weight: 500;", 
                                 paste(id83_info$members, collapse = ", "))
                         )
                     )
              )
            })
          )
        })
      )
    } else {
      build_integrated_page(current_integrated_sig(), "back_to_id83_list")
    }
  })
  
  # ==============================================================================
  # 搜索逻辑与跳转逻辑
  # ==============================================================================
  observe({
    search_choices <- list("89-Type Signatures" = names(signature_groups), "83-Type Signatures" = names(id83_groups))
    updateSelectizeInput(session, "search_input", choices = search_choices, selected = character(0), server = FALSE) 
  })
  
  # 1. 制造工具：定义处理 83-type 跳转的辅助函数 (必须保留！)
  handle_83_selection <- function(id83_name) {
    members <- id83_groups[[id83_name]]$members
    if (length(members) == 1) { 
      current_integrated_sig(members[1]) 
    } else {
      showModal(modalDialog(
        title = paste("Select a signature from", id83_name),
        tags$p("This 83-type signature represents a group containing multiple 89/476-type members. Which specific profile would you like to view?"),
        br(), div(style = "display: flex; flex-wrap: wrap; gap: 10px; justify-content: center;", 
                  lapply(members, function(m) { actionButton(paste0("go_to_integrated_", m), m, class = "btn-primary btn-lg") })),
        easyClose = TRUE, footer = modalButton("Cancel")
      ))
    }
  }
  
  # 2. 搜索框逻辑：在这里面使用了上面那个工具
  search_logic <- function(query) {
    req(query)
    query <- trimws(query)
    names89 <- names(signature_groups)
    names83 <- names(id83_groups)
    
    if (query %in% names89) { updateNavbarPage(session, "navbar", selected = "89-type classification"); current_integrated_sig(query); return() }
    if (query %in% names83) { updateNavbarPage(session, "navbar", selected = "83-type classification"); handle_83_selection(query); return() } # <--- 这里用了
    
    matches89 <- grep(query, names89, ignore.case = TRUE, value = TRUE)
    matches83 <- grep(query, names83, ignore.case = TRUE, value = TRUE)
    total_matches <- length(matches89) + length(matches83)
    
    if (total_matches == 0) {
      showModal(modalDialog(title = "Not Found", paste0("No signatures found matching '", query, "'"), easyClose = TRUE, footer = modalButton("Close")))
      return()
    }
    if (total_matches == 1) {
      if (length(matches89) == 1) { updateNavbarPage(session, "navbar", selected = "89-type classification"); current_integrated_sig(matches89) }
      else { updateNavbarPage(session, "navbar", selected = "83-type classification"); handle_83_selection(matches83) } # <--- 这里用了
      return()
    }
    
    choices_list <- c(if(length(matches89)>0) setNames(matches89, paste0(matches89, " (89-type)")), if(length(matches83)>0) setNames(matches83, paste0(matches83, " (83-type)")))
    showModal(modalDialog(title = "Multiple Matches Found", "We found several signatures matching your query. Please select one:",
                          radioButtons("fuzzy_select", NULL, choices = choices_list),
                          footer = tagList(modalButton("Cancel"), actionButton("confirm_fuzzy_search", "Go", class = "btn-primary")), easyClose = TRUE))
  }
  
  observeEvent(input$search_btn, { search_logic(input$search_input) })
  observeEvent(input$confirm_fuzzy_search, { req(input$fuzzy_select); removeModal(); search_logic(input$fuzzy_select) })
  
  # 3. 页面按钮与缩略图点击逻辑
  lapply(names(signature_groups), function(n) {
    observeEvent(input[[paste0("show_", n)]], { current_integrated_sig(n) })
    observeEvent(input[[paste0("show_476_", n)]], { current_integrated_sig(n) })
    observeEvent(input[[paste0("btn_show_476_", n)]], { current_integrated_sig(n) })
  })
  
  observeEvent(input$back_to_list, { current_integrated_sig(NULL) })
  observeEvent(input$back_to_476_list, { current_integrated_sig(NULL) })
  observeEvent(input$back_to_id83_list, { current_integrated_sig(NULL) })
  
  # 4. 83-type 缩略图点击逻辑：在这里面也使用了上面那个工具
  lapply(names(id83_groups), function(n) {
    observeEvent(input[[paste0("show_id83_", n)]], {
      handle_83_selection(n) # <--- 这里用了
    })
  })
  
  # 5. 弹窗里的选择按钮逻辑
  lapply(names(signature_groups), function(m) { 
    observeEvent(input[[paste0("go_to_integrated_", m)]], { removeModal(); current_integrated_sig(m) }) 
  })
  
  # ==============================================================================
  # 图片查看器 (支持全量图片)
  # ==============================================================================
  observe({
    all_imgs <- character()
    if (!is.null(current_integrated_sig())) {
      sig_name <- current_integrated_sig()
      sig <- signature_groups[[sig_name]]
      
      all_imgs <- c(sig$img_89_top, sig$id89_sig, sig$id89_mapped, sig$id89_decomp, sig$koh_matches, sig$id476_sig, sig$id476_cat_link, sig$id476_cat_best,
                    sig$id83_sig, sig$id83_mapped, sig$id83_cat,
                    sig$id83_sig_abl, sig$id83_mapped_abl, sig$id83_cat_abl,
                    sig$cosmic_std, sig$cosmic_abl, sig$jin_std, sig$jin_abl)
      if (sig$id83_name != "Unknown") all_imgs <- c(all_imgs, id83_groups[[sig$id83_name]]$id83_all)
    }
    
    valid_imgs <- unique(all_imgs[!is.null(all_imgs) & !is.na(all_imgs) & all_imgs != ""])
    lapply(valid_imgs, function(p) {
      observeEvent(input[[paste0("img_", basename(p))]], ignoreInit = TRUE, {
        showModal(modalDialog(title = "Image View", easyClose = TRUE, size = "l", footer = NULL, tags$img(src = p, style = "width:100%; height:auto;")))
      })
    })
  })
  
  observeEvent(input$open_modal_image, {
    req(input$open_modal_image)
    img_path <- input$open_modal_image
    showModal(modalDialog(
      title = NULL,
      div(style = "text-align: center;", tags$img(src = img_path, style = "max-width: 100%; max-height: 85vh; border-radius: 8px; box-shadow: 0 5px 15px rgba(0,0,0,0.2);")),
      footer = tagList(tags$a(href = img_path, download = basename(img_path), class = "btn btn-primary", icon("download"), "Download", style = "color: white;"), modalButton("Close")),
      size = "l", easyClose = TRUE, fade = TRUE
    ))
  })
}
