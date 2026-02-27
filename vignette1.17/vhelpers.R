# vignette.Rmd 的辅助函数
# 这些函数将计算与绘图分离，以便于调试

# 确保加载了必要的库，例如 source("plot_83_w_wout_t.R")

#' 使用希腊字母格式化签名名称
#'
#' 将签名名称中的 _alpha 替换为 α，_beta 替换为 β
#'
#' @param name 字符型：签名名称
#' @return 字符型：带有希腊字母的格式化名称
format_signature_name <- function(name) {
  name <- gsub("_alpha", "α", name)
  name <- gsub("_beta", "β", name)
  name
}

#' 创建带有样式类的受限 div (Fenced Div)
#'
#' @param txt 字符型：div 的文本内容
#' @param style 字符型：div 的 CSS 类（默认为 ".callout-note"）
#' @return 带有受限 div 标记的字符串
fenced_div <- function(txt, style = ".callout-note") {
  paste0('\n\n::: {', style, '}\n', txt, '\n:::\n\n')
}

#' 向文档输出受限 div
#'
#' 结合了 cat() 和 fenced_div() 的便捷包装函数
#'
#' @param txt 字符型：文本内容
#' @param style 字符型：CSS 类。
#' @return NULL
catfdiv <- function(txt, style = ".callout-note") {
  cat(fenced_div(txt, style))
}

#' 查找特定签名的文本文件
#'
#' @param sig_id 字符型：签名标识符
#' @return 包含文件内容的字符串，如果文件不存在则返回 NULL
find_sig_txt <- function(sig_id) {
  file_path <- file.path(data_dir, "per_sig_txt", glue::glue("{sig_id}.md"))
  if (!file.exists(file_path)) {
    return(NULL)
  }
  readLines(file_path, warn = FALSE) |> paste(collapse = "\n")
}

#' 计算签名-目录对的余弦相似度
#'
#' @param type89_sig_id 字符型：ID89 签名名称
#' @param exemplar_id 字符型：支持肿瘤的标识符
#' @param ID89_signatures ID89 签名数据框
#' @param ID89_catalogs ID89 目录数据框
#' @param ID83_signatures ICAMS ID83 签名目录
#' @param ID83_catalogs ICAMS ID83 目录
#' @param ID83_catalogs_no_polyT 已移除 polyT 的 ICAMS 目录
#' @param ID476_signatures ID476 签名数据框
#' @param ID476_catalogs ID476 目录数据框
#' @param ID83signature 字符型：对应的 ID83 签名名称
#' @param assignment_matrix 签名分配矩阵数据框
#' @param cosmic_matches COSMIC 匹配列表
#' @param jin_matches Jin 匹配列表
#' @param koh_matches Koh 匹配列表
#' @return 包含余弦相似度和中间数据的列表
compute_sig_data <- function(
    type89_sig_id,
    exemplar_id,
    ID83signature,
    ID89_signatures,
    ID89_catalogs,
    ID83_signatures,
    ID83_catalogs,
    ID83_catalogs_no_polyT,
    ID476_signatures,
    ID476_catalogs,
    assignment_matrix,
    ID89_mapped_signatures = NULL,
    ID83_mapped_signatures = NULL,
    cosmic_matches = NULL,
    jin_matches = NULL,
    koh_matches = NULL
) {
  message("处理签名 ID = ", type89_sig_id)
  
  # =====================================================================
  # 还原历史逻辑：直接删除 InsDel4b，将 InsDel4a 改名为 InsDel4
  # =====================================================================
  if ("InsDel4b" %in% rownames(assignment_matrix)) {
    # 彻底删除 4b
    assignment_matrix <- assignment_matrix[rownames(assignment_matrix) != "InsDel4b", , drop = FALSE]
  }
  
  if ("InsDel4a" %in% rownames(assignment_matrix)) {
    # 将 4a 强制改名为 InsDel4
    rownames(assignment_matrix)[rownames(assignment_matrix) == "InsDel4a"] <- "InsDel4"
  }
  # =====================================================================
  
  # 检查映射的 89-type 签名是否存在（列名为 {signature}_converted）
  mapped_col_name <- paste0(type89_sig_id, "_converted")
  has_mapped_476_sig <- !is.null(ID89_mapped_signatures) &&
    mapped_col_name %in% colnames(ID89_mapped_signatures)
  
  # 检查映射的 83-type 签名是否存在 (仅当提取了 476-type 签名时才考虑)
  has_476_sig <- type89_sig_id %in% colnames(ID476_signatures)
  has_83_mapped_sig <- has_476_sig &&
    !is.null(ID83_mapped_signatures) &&
    mapped_col_name %in% colnames(ID83_mapped_signatures)
  
  # 获取匹配数据 (COSMIC/Jin/Koh)
  cosmic_match_data <- NULL
  if (!is.null(cosmic_matches) && ID83signature %in% names(cosmic_matches)) {
    cosmic_match_data <- cosmic_matches[[ID83signature]]
  }
  
  jin_match_data <- NULL
  if (!is.null(jin_matches) && ID83signature %in% names(jin_matches)) {
    jin_match_data <- jin_matches[[ID83signature]]
  }
  
  koh_match_data <- NULL
  if (!is.null(koh_matches) && type89_sig_id %in% names(koh_matches)) {
    koh_match_data <- koh_matches[[type89_sig_id]]
  }
  
  result <- list(
    type89_sig_id = type89_sig_id,
    exemplar_id = exemplar_id,
    ID83signature = ID83signature,
    is_insdel15_16 = type89_sig_id %in% c("InsDel15", "InsDel16"),
    is_polyT_removed = ID83signature %in% c("C_ID7", "ID_J", "C_ID10", "ID_N", "ID_O"),
    has_476_signature = has_476_sig,
    has_83_signature = ID83signature %in% colnames(ID83_signatures),
    has_mapped_476_sig = has_mapped_476_sig,
    has_83_mapped_signature = has_83_mapped_sig,
    cosmic_matches = cosmic_match_data,
    jin_matches = jin_match_data,
    koh_matches = koh_match_data
  )
  
  # 计算余弦相似度 89（原始目录 vs 签名）
  result$cosine89 <- lsa::cosine(
    as.numeric(ID89_signatures[, type89_sig_id]),
    as.numeric(ID89_catalogs[, exemplar_id])
  )
  
  # 计算余弦相似度 89 (Mapped)
  if (has_mapped_476_sig) {
    result$cosine89_mapped <- lsa::cosine(
      as.numeric(ID89_signatures[, type89_sig_id]),
      as.numeric(ID89_mapped_signatures[, mapped_col_name])
    )
  } else {
    result$cosine89_mapped <- NA
  }
  
  # 验证分配矩阵
  missing_sigs <- setdiff(row.names(assignment_matrix), colnames(ID89_signatures))
  if (length(missing_sigs) > 0) {
    stop("分配矩阵包含签名矩阵中不存在的签名：", paste(missing_sigs, collapse = ", "))
  }
  
  # 计算分解 (Residuals)
  if (!result$is_insdel15_16) {
    common_sigs <- intersect(colnames(ID89_signatures), row.names(assignment_matrix))
    assignment <- assignment_matrix[common_sigs, exemplar_id, drop = FALSE]
    
    sigid = type89_sig_id
    if (sigid == "InsDel_N") sigid <- "InsDel_J"
    
    assignment_others <- assignment
    stopifnot(sigid %in% common_sigs)
    assignment_others[sigid, ] <- 0
    
    result$residual_spectrum <- as.matrix(ID89_signatures[, common_sigs]) %*% as.matrix(assignment_others)
    
    result$target_sig_partial_spectrum <- ID89_catalogs[, exemplar_id, drop = FALSE] - result$residual_spectrum
    result$target_sig_partial_spectrum[result$target_sig_partial_spectrum < 0] <- 0
    
    result$cosine89_diff <- lsa::cosine(
      as.numeric(ID89_signatures[, type89_sig_id]),
      as.numeric(as.matrix(result$target_sig_partial_spectrum))
    )
  } else {
    result$cosine89_diff <- NA
    result$residual_spectrum <- NULL
    result$target_sig_partial_spectrum <- NULL
  }
  
  # 计算余弦相似度 476
  if (result$has_476_signature) {
    result$cosine476 <- lsa::cosine(
      as.numeric(ID476_signatures[, type89_sig_id]),
      as.numeric(ID476_catalogs[, exemplar_id])
    )
  } else {
    result$cosine476 <- NA
  }
  
  # 计算余弦相似度 83
  if (result$is_polyT_removed) {
    result$cosine83 <- lsa::cosine(
      as.numeric(ID83_signatures[, ID83signature]),
      as.numeric(ID83_catalogs_no_polyT[, exemplar_id])
    )
  } else {
    if (ID83signature %in% colnames(ID83_signatures)) {
      result$cosine83 <- lsa::cosine(
        as.numeric(ID83_signatures[, ID83signature]),
        as.numeric(ID83_catalogs[, exemplar_id])
      )
    } else {
      result$cosine83 <- 0
    }
  }
  
  # 计算余弦相似度 83 (Mapped)
  if (has_83_mapped_sig && result$has_83_signature) {
    result$cosine83_mapped <- lsa::cosine(
      as.numeric(ID83_signatures[, ID83signature]),
      as.numeric(ID83_mapped_signatures[, mapped_col_name])
    )
  } else {
    result$cosine83_mapped <- NA
  }
  
  return(result)
}

#' 生成 markdown 页脚文本
generate_section_footer <- function(sig_data) {
  cosine476_text <- if (is.na(sig_data$cosine476)) "不适用" else as.character(sig_data$cosine476)
  
  df <- data.frame(
    `83-类型` = sig_data$cosine83,
    `476-类型` = cosine476_text,
    `89-类型` = sig_data$cosine89,
    check.names = FALSE
  )
  table_output <- knitr::kable(df)
  paste0("\n\n", paste(table_output, collapse = "\n"), "\n\n---\n")
}


#' 为签名生成所有图形并保存到文件
#'
#' @param sig_data compute_sig_data 返回的列表
#' @param plot_dir 保存图形的目录
#' @return 包含生成的文件路径的列表
generate_plots_to_files <- function(
    sig_data,
    ID89_signatures,
    ID89_catalogs,
    ID83_signatures,
    ID83_catalogs,
    ID83_catalogs_no_polyT,
    ID476_signatures,
    ID476_catalogs,
    plot_dir,
    plot476_base_size = 20,
    plot476_label_size = 3,
    plot476_simplify_labels = FALSE,
    ID89_mapped_signatures = NULL,
    ID83_mapped_signatures = NULL,
    cosmic_signatures = NULL,
    jin_signatures = NULL,
    koh_signatures = NULL,
    min_ts_to_trigger = 0.15
) {
  # 创建安全的文件名前缀
  safe_name <- gsub("[^a-zA-Z0-9_]", "_", sig_data$type89_sig_id)
  
  # --- 1. 定义路径 (已包含缩略图和所有新图表) ---
  paths <- list()
  paths$id89_sig <- file.path(plot_dir, paste0(safe_name, "_id89_sig.png"))
  paths$id89_thumb <- file.path(plot_dir, paste0(safe_name, "_Thumbnail.png"))
  paths$id89_mapped <- file.path(plot_dir, paste0(safe_name, "_id89_mapped.png"))
  paths$id89_catalog <- file.path(plot_dir, paste0(safe_name, "_id89_catalog.png"))
  paths$id89_residual <- file.path(plot_dir, paste0(safe_name, "_id89_residual.png"))
  paths$id89_target_sig_partial_spectrum <- file.path(plot_dir, paste0(safe_name, "_id89_target_sig_partial_spectrum.png"))
  
  paths$id476_sig <- file.path(plot_dir, paste0(safe_name, "_id476_sig.png"))
  paths$id476_thumb <- file.path(plot_dir, paste0(safe_name, "_id476_Thumbnail.png"))
  paths$id476_catalog <- file.path(plot_dir, paste0(safe_name, "_id476_catalog.png"))
  
  paths$id83_sig <- file.path(plot_dir, paste0(safe_name, "_id83_sig.png"))
  paths$id83_mapped <- file.path(plot_dir, paste0(safe_name, "_id83_mapped.png"))
  paths$id83_catalog <- file.path(plot_dir, paste0(safe_name, "_id83_catalog.png"))
  paths$id83_sig_ablated <- file.path(plot_dir, paste0(safe_name, "_id83_sig_ablated.png"))
  paths$id83_mapped_ablated <- file.path(plot_dir, paste0(safe_name, "_id83_mapped_ablated.png"))
  paths$id83_catalog_ablated <- file.path(plot_dir, paste0(safe_name, "_id83_catalog_ablated.png"))
  paths$id83_sig_ablated_catalog <- NULL
  paths$id83_mapped_ablated_catalog <- NULL
  paths$id83_catalog_ablated_catalog <- NULL
  
  if (sig_data$has_83_signature) {
    paths$id83_thumb <- file.path(plot_dir, paste0(sig_data$ID83signature, "_Thumbnail.png"))
  } else {
    paths$id83_thumb <- NULL
  }
  
  # 保存 ggplot 的辅助函数
  save_ggplot <- function(p, path, width = 19, height = 3) {
    ggplot2::ggsave(path, p, width = width, height = height, dpi = 150, bg = "white")
  }
  
  # --- 2. ID89 绘图包装 (还原为标准逻辑，不修复字体) ---
  p89 <- function(catalog, plot_title, setyaxis = NULL) {
    mSigPlot::plot_89(
      catalog,
      plot_title = plot_title,
      base_size = getp('basesize89'),
      setyaxis = setyaxis,
      plot_complex = FALSE
    )
  }
  
  save89 = function(myplot, path) {
    save_ggplot(myplot, path, width = getp('w89'), height = getp('h89'))
  }
  
  # ID89 图 1: 签名
  p1 <- p89(
    ID89_signatures[, sig_data$type89_sig_id, drop = FALSE],
    plot_title = sig_data$type89_sig_id
  )
  save89(p1, paths$id89_sig)
  
  # --- 3. ID89 缩略图生成 (保留 theme_void 极简逻辑) ---
  if (!is.null(paths$id89_thumb)) {
    p1_thumb_raw <- mSigPlot::plot_89(
      ID89_signatures[, sig_data$type89_sig_id, drop = FALSE],
      plot_title = "",                # <--- 缩略图不需要标题
      base_size = 8,                  # <--- 将基础字号缩小为 8
      text_cex = 1.5,                 # <--- 【关键修复】强制缩小条带文字
      top_bar_text_cex = 1.5,         # <--- 【关键修复】强制缩小顶部说明文字
      show_x_axis_text = FALSE        # <--- 缩略图不需要 X 轴文字
    )
    
    # 注意：这里不需要手动循环修复字体，因为 plot_89 已生成合适的对象，只需应用 theme_void
    p1_thumb <- p1_thumb_raw + 
      ggplot2::theme_void() + 
      ggplot2::theme(
        plot.title = ggplot2::element_blank(), 
        axis.text = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(), 
        axis.ticks = ggplot2::element_blank(),
        legend.position = "none", 
        plot.margin = ggplot2::margin(0,0,0,0),
        strip.text = ggplot2::element_text(size = 8)
      ) +
      ggplot2::labs(title = NULL, x = NULL, y = NULL)
    
    ggplot2::ggsave(filename = paths$id89_thumb, plot = p1_thumb, width = 4, height = 1, dpi = 300, bg="white")
  }
  
  # ID89 图 1b: 映射签名 (新图)
  if (sig_data$has_mapped_476_sig && !is.null(ID89_mapped_signatures)) {
    mapped_col_name <- paste0(sig_data$type89_sig_id, "_converted")
    p1b <- p89(
      ID89_mapped_signatures[, mapped_col_name, drop = FALSE],
      plot_title = paste0(
        sig_data$type89_sig_id,
        " converted from 476-type signature | cosine similarity to ",
        sig_data$type89_sig_id,
        " = ",
        format(sig_data$cosine89_mapped, digits = getp("cosine_digits"))
      )
    )
    save89(p1b, paths$id89_mapped)
  } else {
    paths$id89_mapped <- NULL
  }
  
  # ID89 图 2: 目录
  catalogtoplot = ID89_catalogs[, sig_data$exemplar_id, drop = FALSE]
  ymax = max(catalogtoplot)
  p2 <- p89(
    catalogtoplot,
    plot_title = paste0(
      "Spectrum A, from ", sig_data$exemplar_id,
      " | cosine similarity to ", sig_data$type89_sig_id, " = ",
      format(sig_data$cosine89, digits = getp("cosine_digits"))
    ),
    setyaxis = ymax
  )
  save89(p2, paths$id89_catalog)
  
  # ID89 图 3 & 4: 分解
  if (!sig_data$is_insdel15_16 && !is.null(sig_data$residual_spectrum)) {
    p3 <- p89(
      sig_data$residual_spectrum,
      plot_title = paste0("Remaining mutations in ", sig_data$exemplar_id, " not due to ", sig_data$type89_sig_id, " (A minus B)"),
      setyaxis = ymax
    )
    save89(p3, paths$id89_residual)
    
    p4 <- p89(
      sig_data$target_sig_partial_spectrum,
      plot_title = paste0("Spectrum B: partial mutational spectrum of ", sig_data$exemplar_id, " due to ", sig_data$type89_sig_id),
      setyaxis = ymax
    )
    save89(p4, paths$id89_target_sig_partial_spectrum)
  } else {
    paths$id89_residual <- NULL
    paths$id89_target_sig_partial_spectrum <- NULL
  }
  
  # --- 4. ID476 绘图包装 (包含标签修复逻辑 22bp -> 2bp) ---
  p476 <- function(catalog, plot_title, custom_text_cex = 0.8, custom_base_size = plot476_base_size) {
    p <- mSigPlot::plot_476(
      catalog,
      plot_title = plot_title,
      block_text_cex = custom_text_cex,  # <--- 修改 1：text_size 改为 block_text_cex
      ggrepel_cex = 0.52,                 # <--- 修改 2：label_size 改为 ggrepel_cex (设为 0.5 比较安全)
      num_labels = 5,
      base_size = custom_base_size,      
      simplify_labels = plot476_simplify_labels,
      plot_complex = FALSE
    )
    
    # 修复 ggplot 对象中的 "22bp" -> "2bp"
    repair_text <- function(obj) {
      if (is.character(obj)) return(gsub("22bp", "2bp", obj, fixed = TRUE))
      if (is.factor(obj)) {
        levels(obj) <- gsub("22bp", "2bp", levels(obj), fixed = TRUE)
        return(obj)
      }
      return(obj)
    }
    
    # 修复主数据
    if (!is.null(p$data)) {
      for (col in names(p$data)) p$data[[col]] <- repair_text(p$data[[col]])
    }
    # 修复图层数据
    for (i in seq_along(p$layers)) {
      if (!is.null(p$layers[[i]]$data) && is.data.frame(p$layers[[i]]$data)) {
        for (col in names(p$layers[[i]]$data)) {
          p$layers[[i]]$data[[col]] <- repair_text(p$layers[[i]]$data[[col]])
        }
      }
    }
    # 修复坐标轴刻度/标签
    for (i in seq_along(p$scales$scales)) {
      if (!is.null(p$scales$scales[[i]]$breaks) && is.character(p$scales$scales[[i]]$breaks)) {
        p$scales$scales[[i]]$breaks <- gsub("22bp", "2bp", p$scales$scales[[i]]$breaks, fixed = TRUE)
      }
      if (!is.null(p$scales$scales[[i]]$labels) && is.character(p$scales$scales[[i]]$labels)) {
        p$scales$scales[[i]]$labels <- gsub("22bp", "2bp", p$scales$scales[[i]]$labels, fixed = TRUE)
      }
    }
    return(p)
  }
  
  save476 = function(myplot, path) {
    save_ggplot(myplot, path, width = getp('w476'), height = getp('h476'))
  }
  
  if (sig_data$has_476_signature) {
    # >>> 情况 A：存在 476 签名数据 <<<
    p5 <- p476(ID476_signatures[, sig_data$type89_sig_id], plot_title = paste0("Extracted 476-type signature corresponding to ", sig_data$type89_sig_id))
    save476(p5, paths$id476_sig)
    
    # 生成缩略图 (签名版)
    if (!is.null(paths$id476_thumb)) {
      p5_mini <- p476(
        ID476_signatures[, sig_data$type89_sig_id], # 使用签名数据
        plot_title = "", 
        custom_text_cex = 0.5, 
        custom_base_size = 8
      )
      
      p5_thumb <- p5_mini + 
        ggplot2::theme_void() + 
        ggplot2::theme(
          plot.title = ggplot2::element_blank(), axis.text = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
          legend.position = "none", plot.margin = ggplot2::margin(0,0,0,0),
          strip.text = ggplot2::element_text(size = 5, margin = ggplot2::margin(1,0,1,0))
        )
      ggplot2::ggsave(filename = paths$id476_thumb, plot = p5_thumb, width = 12, height = 2, dpi = 300, bg="white")
    }
    
    p6 <- p476(ID476_catalogs[, sig_data$exemplar_id], plot_title = "")
    save476(p6, paths$id476_catalog)
    
  } else {
    # >>> 情况 B：不存在 476 签名数据 (Fallback 到 Sample Spectrum) <<<
    # 这里处理 InsDel14 和 InsDelL
    
    # 1. 生成主图 (使用 Spectrum)
    p5 <- p476(ID476_catalogs[, sig_data$exemplar_id], plot_title = paste0("476-type spectrum of the supporting tumor ", sig_data$exemplar_id))
    save476(p5, paths$id476_sig)
    
    # 2. [新增] 生成缩略图 (使用 Spectrum)
    # 以前这里是 paths$id476_thumb <- NULL，现在改为生成
    if (!is.null(paths$id476_thumb)) {
      p5_mini <- p476(
        ID476_catalogs[, sig_data$exemplar_id], # [注意] 这里使用 Spectrum 数据
        plot_title = "", 
        custom_text_cex = 0.5, 
        custom_base_size = 8
      )
      
      p5_thumb <- p5_mini + 
        ggplot2::theme_void() + 
        ggplot2::theme(
          plot.title = ggplot2::element_blank(), axis.text = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
          legend.position = "none", plot.margin = ggplot2::margin(0,0,0,0),
          strip.text = ggplot2::element_text(size = 5, margin = ggplot2::margin(1,0,1,0))
        )
      ggplot2::ggsave(filename = paths$id476_thumb, plot = p5_thumb, width = 12, height = 2, dpi = 300, bg="white")
    }
    
    paths$id476_catalog <- NULL
  }
  
  # --- 5. ID83 绘图逻辑 (包含 Magick 缩略图和消融分析) ---
  
  p83 <- function(catalog, plot_title = NULL, min_ts = min_ts_to_trigger) {
    plot_83_w_wout_t(
      catalog,
      plot_title = plot_title,
      base_size = getp('basesize83'),
      min_ts_to_trigger = min_ts
    )
  }
  save83 = function(myplot, path) {
    save_ggplot(myplot, path, width = getp('w83'), height = getp('h83'))
  }
  
  save83_result <- function(result, path_main, path_ablated = NULL) {
    if (!is.null(result$ablated_catalog)) {
      save83(result$plots[[1]], path_main)
      if (!is.null(path_ablated)) {
        save83(result$plots[[2]], path_ablated)
      }
      return(list(ablated = TRUE, ablated_catalog = result$ablated_catalog))
    } else {
      save83(result$plots, path_main)
      return(list(ablated = FALSE, ablated_catalog = NULL))
    }
  }
  
  if (sig_data$has_83_signature) {
    result <- p83(ID83_signatures[, sig_data$ID83signature, drop = FALSE])
    save_result <- save83_result(result, paths$id83_sig, paths$id83_sig_ablated)
    if (!save_result$ablated) paths$id83_sig_ablated <- NULL
    paths$id83_sig_ablated_catalog <- save_result$ablated_catalog
    
    # ID83 缩略图生成 (Magick 智能切边版)
    if (!is.null(paths$id83_thumb)) {
      tryCatch({
        # 1. 直接利用已经保存好的主图 (scheme C)
        # 确保主图路径存在
        src_img_path <- if(!is.null(result$ablated_catalog)) paths$id83_sig else paths$id83_sig
        
        if (file.exists(src_img_path)) {
          img <- magick::image_read(src_img_path)
          
          # 2. 自动去除周围多余白边
          img <- magick::image_trim(img)
          
          # 3. 获取图片尺寸
          info <- magick::image_info(img)
          w <- info$width
          h <- info$height
          
          # 4. 计算裁切量 (根据你的图片实际情况微调这些比例)
          # 假设底部 X 轴标签占了高度的 20%，左侧 Y 轴占了宽度的 6%
          shave_bottom <- h * 0.20 
          shave_left   <- w * 0.06
          
          # 5. 执行裁切 (gravity = SouthWest 确保从左下角开始算)
          # 逻辑：先切底部，再切左边
          # Shave 语法是 "宽x高"，会同时切两边，所以我们用 crop 更精准
          
          # 步骤 A: 切掉底部 X 轴 (保留顶部的 80%)
          img <- magick::image_crop(img, geometry = paste0(w, "x", (h - shave_bottom), "+0+0"), gravity = "North")
          
          # 步骤 B: 切掉左侧 Y 轴 (保留右侧的 94%)
          img <- magick::image_crop(img, geometry = paste0((w - shave_left), "x", (h - shave_bottom), "+0+0"), gravity = "East")
          
          # 6. 调整大小并保存
          img_thumb <- magick::image_resize(img, "1500x")
          magick::image_write(img_thumb, path = paths$id83_thumb)
        }
        
      }, error = function(e) {
        message(paste("Error generating thumbnail:", e$message))
      })
    }
    
  } else {
    # 如果没有 83 签名，清空相关路径变量
    paths$id83_sig <- NULL
    paths$id83_sig_ablated <- NULL
  }
  
  # ID83 映射签名 (新图)
  if (sig_data$has_83_mapped_signature && !is.null(ID83_mapped_signatures)) {
    mapped_col_name <- paste0(sig_data$type89_sig_id, "_converted")
    result <- p83(
      ID83_mapped_signatures[, mapped_col_name, drop = FALSE],
      plot_title = ""
    )
    save_result <- save83_result(result, paths$id83_mapped, paths$id83_mapped_ablated)
    if (!save_result$ablated) paths$id83_mapped_ablated <- NULL
    paths$id83_mapped_ablated_catalog <- save_result$ablated_catalog
  } else {
    paths$id83_mapped <- NULL
    paths$id83_mapped_ablated <- NULL
  }
  
  # ID83 目录 (总是显示)
  cat83touse = if (sig_data$is_polyT_removed) ID83_catalogs_no_polyT else ID83_catalogs
  result <- p83(cat83touse[, sig_data$exemplar_id, drop = FALSE])
  save_result <- save83_result(result, paths$id83_catalog, paths$id83_catalog_ablated)
  if (!save_result$ablated) paths$id83_catalog_ablated <- NULL
  paths$id83_catalog_ablated_catalog <- save_result$ablated_catalog
  
  # --- 6. 外部数据库匹配 (COSMIC/Jin/Koh) (新图) ---
  
  # COSMIC 匹配签名
  paths$cosmic_plots <- NULL
  if (!is.null(sig_data$cosmic_matches) && !is.null(cosmic_signatures)) {
    cosmic_plot_list <- list()
    for (i in seq_len(nrow(sig_data$cosmic_matches))) {
      cosmic_sig_name <- sig_data$cosmic_matches$cosmic_sig[i]
      cosmic_cosine <- sig_data$cosmic_matches$cosine[i]
      
      plot_path <- file.path(plot_dir, paste0(safe_name, "_cosmic_", cosmic_sig_name, ".png"))
      ablated_path <- file.path(plot_dir, paste0(safe_name, "_cosmic_", cosmic_sig_name, "_ablated.png"))
      
      result <- p83(
        cosmic_signatures[, cosmic_sig_name, drop = FALSE],
        plot_title = paste0("COSMIC ", cosmic_sig_name, " | cosine to ", sig_data$ID83signature, ": ", format(cosmic_cosine, digits = getp("cosine_digits")))
      )
      save_result <- save83_result(result, plot_path, ablated_path)
      
      cosmic_plot_list[[cosmic_sig_name]] <- list(
        path = plot_path,
        path_ablated = if (save_result$ablated) ablated_path else NULL,
        cosine = cosmic_cosine
      )
    }
    paths$cosmic_plots <- cosmic_plot_list
  }
  
  # Jin 匹配签名
  paths$jin_plots <- NULL
  if (!is.null(sig_data$jin_matches) && !is.null(jin_signatures)) {
    jin_plot_list <- list()
    for (i in seq_len(nrow(sig_data$jin_matches))) {
      jin_sig_name <- sig_data$jin_matches$jin_sig[i]
      jin_cosine <- sig_data$jin_matches$cosine[i]
      
      plot_path <- file.path(plot_dir, paste0(safe_name, "_jin_", jin_sig_name, ".png"))
      ablated_path <- file.path(plot_dir, paste0(safe_name, "_jin_", jin_sig_name, "_ablated.png"))
      
      result <- p83(
        jin_signatures[, jin_sig_name, drop = FALSE],
        plot_title = paste0("Jin ", jin_sig_name, " | cosine to ", sig_data$ID83signature, ": ", format(jin_cosine, digits = getp("cosine_digits")))
      )
      save_result <- save83_result(result, plot_path, ablated_path)
      
      jin_plot_list[[jin_sig_name]] <- list(
        path = plot_path,
        path_ablated = if (save_result$ablated) ablated_path else NULL,
        cosine = jin_cosine
      )
    }
    paths$jin_plots <- jin_plot_list
  }
  
  # Koh 匹配签名 (89-type)
  paths$koh_plots <- NULL
  if (!is.null(sig_data$koh_matches) && !is.null(koh_signatures)) {
    koh_plot_list <- list()
    for (i in seq_len(nrow(sig_data$koh_matches))) {
      koh_sig_name <- sig_data$koh_matches$koh_sig[i]
      koh_cosine <- sig_data$koh_matches$cosine[i]
      
      plot_path <- file.path(plot_dir, paste0(safe_name, "_koh_", koh_sig_name, ".png"))
      
      ptmp <- p89(
        koh_signatures[, koh_sig_name, drop = FALSE],
        plot_title = paste0("Similar signature from Koh et al., 2025 ", koh_sig_name, " | cosine to ", sig_data$type89_sig_id, ": ", format(koh_cosine, digits = getp("cosine_digits")))
      )
      save89(ptmp, plot_path)
      
      koh_plot_list[[koh_sig_name]] <- list(
        path = plot_path,
        cosine = koh_cosine
      )
    }
    paths$koh_plots <- koh_plot_list
  }
  
  return(paths)
}

#' 并行生成所有图形
#'
#' @param all_sig_data compute_sig_data 返回的签名数据列表
#' @param ... 传递给 generate_plots_to_files 的其他参数
#' @param n_workers 并行工作进程数 (默认为 10)
#' @return 每个签名的绘图路径列表
generate_all_plots_parallel <- function(
    all_sig_data,
    ID89_signatures,
    ID89_catalogs,
    ID83_signatures,
    ID83_catalogs,
    ID83_catalogs_no_polyT,
    ID476_signatures,
    ID476_catalogs,
    plot_dir,
    plot476_base_size = 20,
    plot476_label_size = 3,
    plot476_simplify_labels = FALSE,
    ID89_mapped_signatures = NULL,
    ID83_mapped_signatures = NULL,
    cosmic_signatures = NULL,
    jin_signatures = NULL,
    koh_signatures = NULL,
    min_ts_to_trigger = 0.15,
    n_workers = 10
) {
  # 1. 创建输出目录
  dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)
  
  # 2. 设置并行后端
  future::plan(future::multisession, workers = n_workers)
  
  # 3. 定义全局导出变量
  # 注意：因为我们将在内部 source 文件，所以不需要导出 ppar 或 plot_83_w_wout_t
  # 我们只需要导出函数参数和主绘图逻辑
  my_globals <- c(
    "generate_plots_to_files",
    "format_signature_name", 
    "fenced_div",
    "save83_result",
    # 数据对象仍然需要导出
    "ID89_signatures", "ID89_catalogs", "ID83_signatures", 
    "ID83_catalogs", "ID83_catalogs_no_polyT", "ID476_signatures", 
    "ID476_catalogs", "ID89_mapped_signatures", "ID83_mapped_signatures",
    "cosmic_signatures", "jin_signatures", "koh_signatures",
    "plot476_base_size", "plot476_label_size", "plot476_simplify_labels",
    "min_ts_to_trigger", "plot_dir"
  )
  
  # 4. 执行并行计算
  all_paths <- furrr::future_map(
    all_sig_data,
    function(sig_data) {
      
      # =======================================================
      # 🔥🔥 核心修复：强制 Worker 重新读取同目录下的脚本 🔥🔥
      # =======================================================
      
      # 1. 加载绘图参数 (包含 ppar 列表, getp 函数, 和其他全局变量)
      # 即使主进程没传过来，Worker 自己读文件也能拿到 ppar
      if(file.exists("ppar.R")) {
        source("ppar.R")
      } else {
        stop("并行 Worker 报错：找不到 ppar.R 文件！")
      }
      
      # 2. 加载绘图函数 (解决 'could not find plot_83_w_wout_t')
      if(file.exists("plot_83_w_wout_t.R")) {
        source("plot_83_w_wout_t.R")
      } else {
        stop("并行 Worker 报错：找不到 plot_83_w_wout_t.R 文件！")
      }
      
      # 3. 执行绘图
      generate_plots_to_files(
        sig_data = sig_data,
        ID89_signatures = ID89_signatures,
        ID89_catalogs = ID89_catalogs,
        ID83_signatures = ID83_signatures,
        ID83_catalogs = ID83_catalogs,
        ID83_catalogs_no_polyT = ID83_catalogs_no_polyT,
        ID476_signatures = ID476_signatures,
        ID476_catalogs = ID476_catalogs,
        plot_dir = plot_dir,
        plot476_base_size = plot476_base_size,
        plot476_label_size = plot476_label_size,
        plot476_simplify_labels = plot476_simplify_labels,
        ID89_mapped_signatures = ID89_mapped_signatures,
        ID83_mapped_signatures = ID83_mapped_signatures,
        cosmic_signatures = cosmic_signatures,
        jin_signatures = jin_signatures,
        koh_signatures = koh_signatures,
        min_ts_to_trigger = min_ts_to_trigger
      )
    },
    .options = furrr::furrr_options(
      seed = TRUE,
      # 这里的 globals 列表变短了，因为最棘手的部分已经通过 source 解决了
      globals = my_globals,
      packages = c("ggplot2", "ICAMS", "mSigPlot", "indelsig.tools.lib", "magick")
    ),
    .progress = TRUE
  )
  
  # 5. 恢复串行模式
  future::plan(future::sequential)
  
  names(all_paths) <- names(all_sig_data)
  return(all_paths)
}


#' 检查绘图缓存是否有效
#'
#' 比较源数据文件的哈希值与存储的哈希值。
#' 如果缓存有效（无需重新生成），则返回 TRUE。
#'
#' @param data_dir 包含源数据文件的目录
#' @param plot_dir 存储图形的目录
#' @param cache_file 缓存哈希文件的名称
#' @return 逻辑值：如果缓存有效则为 TRUE，如果需要重新生成则为 FALSE
check_plot_cache <- function(
    data_dir,
    plot_dir,
    cache_file = "plot_cache_hash.rds"
) {
  cache_path <- file.path(plot_dir, cache_file)
  
  # data_dir 中绘图所依赖的源文件
  data_files <- c(
    "Liu_et_al_final_89_type_signatures.tsv",
    "Liu_et_al_89_type_spectra.tsv",
    "Liu_et_al_final_83_type_signatures.tsv",
    "Liu_et_al_83_type_spectra.tsv",
    "Liu_et_al_final_476_type_signatures.tsv",
    "Liu_et_al_476_type_spectra.tsv",
    "89type_to_83type_connection.tsv",
    "COSMIC_v3.5_ID_GRCh37_signatures.tsv",
    "jin_2024_sup_tab_1_signatures.tsv",
    "Koh_signatures.tsv"
  )
  
  # vignette 目录中影响绘图的文件
  vignette_files <- c(
    "ppar.R",
    "89_mapped_from_476.tsv",
    "83_mapped_from_476.tsv"
  )
  
  # 检查绘图目录是否存在
  if (!dir.exists(plot_dir)) {
    return(FALSE)
  }
  
  # 根据文件修改时间计算当前哈希值
  data_paths <- file.path(data_dir, data_files)
  if (!all(file.exists(data_paths))) {
    return(FALSE)
  }
  
  # 检查 vignette 文件（可选 - 跳过缺失的文件）
  vignette_paths <- vignette_files[file.exists(vignette_files)]
  
  all_paths <- c(data_paths, vignette_paths)
  current_hash <- digest::digest(
    sapply(all_paths, file.mtime)
  )
  
  # 检查缓存是否存在且匹配
  if (file.exists(cache_path)) {
    stored_hash <- readRDS(cache_path)
    if (identical(stored_hash, current_hash)) {
      return(TRUE) # 缓存有效
    }
  }
  
  return(FALSE) # 缓存无效或缺失
}


#' 生成绘图后保存缓存哈希
#'
#' @param data_dir 包含源数据文件的目录
#' @param plot_dir 存储图形的目录
#' @param cache_file 缓存哈希文件的名称
save_plot_cache <- function(
    data_dir,
    plot_dir,
    cache_file = "plot_cache_hash.rds"
) {
  # data_dir 中的源文件
  data_files <- c(
    "Liu_et_al_final_89_type_signatures.tsv",
    "Liu_et_al_89_type_spectra.tsv",
    "Liu_et_al_final_83_type_signatures.tsv",
    "Liu_et_al_83_type_spectra.tsv",
    "Liu_et_al_final_476_type_signatures.tsv",
    "Liu_et_al_476_type_spectra.tsv",
    "89type_to_83type_connection.tsv",
    "COSMIC_v3.5_ID_GRCh37_signatures.tsv",
    "jin_2024_sup_tab_1_signatures.tsv",
    "Koh_signatures.tsv"
  )
  
  # vignette 目录中影响绘图的文件
  vignette_files <- c(
    "ppar.R",
    "89_mapped_from_476.tsv",
    "83_mapped_from_476.tsv"
  )
  
  data_paths <- file.path(data_dir, data_files)
  vignette_paths <- vignette_files[file.exists(vignette_files)]
  
  all_paths <- c(data_paths, vignette_paths)
  current_hash <- digest::digest(
    sapply(all_paths, file.mtime)
  )
  
  saveRDS(current_hash, file.path(plot_dir, cache_file))
}


#' 从现有文件重构绘图路径
#'
#' 当缓存有效时使用，无需重新生成图形即可获取路径。
#'
#' @param signature_names 签名名称向量
#' @param plot_dir 存储图形的目录
#' @return 按签名名称组织的绘图路径列表
reconstruct_plot_paths <- function(signature_names, plot_dir) {
  all_paths <- lapply(signature_names, function(sig_name) {
    safe_name <- gsub("[^a-zA-Z0-9_]", "_", sig_name)
    
    paths <- list(
      id89_sig = file.path(plot_dir, paste0(safe_name, "_id89_sig.png")),
      id89_mapped = file.path(plot_dir, paste0(safe_name, "_id89_mapped.png")),
      id89_catalog = file.path(
        plot_dir,
        paste0(safe_name, "_id89_catalog.png")
      ),
      id89_residual = file.path(
        plot_dir,
        paste0(safe_name, "_id89_residual.png")
      ),
      id89_target_sig_partial_spectrum = file.path(
        plot_dir,
        paste0(safe_name, "_id89_target_sig_partial_spectrum.png")
      ),
      id476_sig = file.path(plot_dir, paste0(safe_name, "_id476_sig.png")),
      id476_thumb = file.path(plot_dir, paste0(safe_name, "_id476_Thumbnail.png")),
      id476_catalog = file.path(
        plot_dir,
        paste0(safe_name, "_id476_catalog.png")
      ),
      id83_sig = file.path(plot_dir, paste0(safe_name, "_id83_sig.png")),
      id83_mapped = file.path(plot_dir, paste0(safe_name, "_id83_mapped.png")),
      id83_catalog = file.path(
        plot_dir,
        paste0(safe_name, "_id83_catalog.png")
      ),
      id83_sig_ablated = file.path(
        plot_dir,
        paste0(safe_name, "_id83_sig_ablated.png")
      ),
      id83_mapped_ablated = file.path(
        plot_dir,
        paste0(safe_name, "_id83_mapped_ablated.png")
      ),
      id83_catalog_ablated = file.path(
        plot_dir,
        paste0(safe_name, "_id83_catalog_ablated.png")
      ),
      id83_sig_ablated_catalog = NULL,
      id83_mapped_ablated_catalog = NULL,
      id83_catalog_ablated_catalog = NULL
    )
    
    # 补充 ID89 缩略图路径
    paths$id89_thumb <- file.path(plot_dir, paste0(safe_name, "_Thumbnail.png"))
    
    # 如果文件不存在，则设置为 NULL (跳过 ablated_catalog 条目，因为它们仅在内存中)
    paths <- lapply(names(paths), function(nm) {
      p <- paths[[nm]]
      if (grepl("_ablated_catalog$", nm)) {
        return(NULL) # ablated_catalog 仅在内存中，缓存中始终为 NULL
      }
      if (is.null(p) || !file.exists(p)) NULL else p
    })
    
    # 重新赋予名称
    names(paths) <- c(
      "id89_sig", "id89_mapped", "id89_catalog", "id89_residual", 
      "id89_target_sig_partial_spectrum", "id476_sig", "id476_catalog", 
      "id83_sig", "id83_mapped", "id83_catalog", 
      "id83_sig_ablated", "id83_mapped_ablated", "id83_catalog_ablated",
      "id83_sig_ablated_catalog", "id83_mapped_ablated_catalog", "id83_catalog_ablated_catalog",
      "id89_thumb"
    )
    
    # 手动处理 ID83 缩略图
    # 由于我们不知道确切的 ID83 签名名称，这里只能尝试列出所有匹配模式
    # 如果代码上下文中需要精确路径，缓存逻辑可能需要更复杂的元数据存储
    # 目前简化处理：
    
    # 查找此签名的任何 COSMIC 绘图
    cosmic_pattern <- paste0(safe_name, "_cosmic_*.png")
    cosmic_files <- list.files(
      plot_dir,
      pattern = glob2rx(cosmic_pattern),
      full.names = TRUE
    )
    if (length(cosmic_files) > 0) {
      main_files <- cosmic_files[!grepl("_ablated\\.png$", cosmic_files)]
      cosmic_plot_list <- list()
      for (cf in main_files) {
        basename_no_ext <- tools::file_path_sans_ext(basename(cf))
        cosmic_sig_name <- sub(
          paste0(safe_name, "_cosmic_"),
          "",
          basename_no_ext
        )
        ablated_file <- sub("\\.png$", "_ablated.png", cf)
        cosmic_plot_list[[cosmic_sig_name]] <- list(
          path = cf,
          path_ablated = if (file.exists(ablated_file)) ablated_file else NULL,
          cosine = NA,
          ablated_catalog = NULL
        )
      }
      paths$cosmic_plots <- cosmic_plot_list
    } else {
      paths$cosmic_plots <- NULL
    }
    
    # 查找此签名的任何 Jin 绘图
    jin_pattern <- paste0(safe_name, "_jin_*.png")
    jin_files <- list.files(
      plot_dir,
      pattern = glob2rx(jin_pattern),
      full.names = TRUE
    )
    if (length(jin_files) > 0) {
      main_files <- jin_files[!grepl("_ablated\\.png$", jin_files)]
      jin_plot_list <- list()
      for (jf in main_files) {
        basename_no_ext <- tools::file_path_sans_ext(basename(jf))
        jin_sig_name <- sub(paste0(safe_name, "_jin_"), "", basename_no_ext)
        ablated_file <- sub("\\.png$", "_ablated.png", jf)
        jin_plot_list[[jin_sig_name]] <- list(
          path = jf,
          path_ablated = if (file.exists(ablated_file)) ablated_file else NULL,
          cosine = NA,
          ablated_catalog = NULL
        )
      }
      paths$jin_plots <- jin_plot_list
    } else {
      paths$jin_plots <- NULL
    }
    
    # 查找此签名的任何 Koh 绘图
    koh_pattern <- paste0(safe_name, "_koh_*.png")
    koh_files <- list.files(
      plot_dir,
      pattern = glob2rx(koh_pattern),
      full.names = TRUE
    )
    if (length(koh_files) > 0) {
      koh_plot_list <- list()
      for (kf in koh_files) {
        basename_no_ext <- tools::file_path_sans_ext(basename(kf))
        koh_sig_name <- sub(paste0(safe_name, "_koh_"), "", basename_no_ext)
        koh_plot_list[[koh_sig_name]] <- list(
          path = kf,
          cosine = NA
        )
      }
      paths$koh_plots <- koh_plot_list
    } else {
      paths$koh_plots <- NULL
    }
    
    return(paths)
  })
  
  names(all_paths) <- signature_names
  return(all_paths)
}