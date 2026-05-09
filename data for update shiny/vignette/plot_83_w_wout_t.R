#' Plot ID83 catalog with optional suppression of high polyT rows
#'
#' Wraps mSigPlot::plot_ID83 to optionally generate a second plot with polyT 5+
#' rows set to -3 when mutation counts exceed a threshold.
#'
#' @param catalog An ID83 catalog (matrix or data frame with row names).
#' @param plot_title Title for the plot(s).
#' @param base_size Base size passed to plot_ID83.
#' @param min_ts_to_trigger If >= 1.1, treated as an absolute count threshold.
#'   If < 1.1, treated as a proportion of total mutations.
#' @param ablate_both If TRUE, the if either the intertion or
#' deletion over the min_ts_to_trigger then suppress both
#' insertions and deletions.
#'
#' @return If no polyT rows exceed threshold, returns a
#' a list with a single element `plots``.
#'   If one or both polyT rows exceed threshold, returns a list
#' with 2 elements: `plots`, a vector of two plots:
#'   the original and one with offending rows set to a negative value
#' and `ablated_catalog`, which is the original catalog with the
#' Plot ID83 catalog - 原生标题 + 全字体绝对控制版
#' @export
plot_83_w_wout_t <- function(
    catalog,
    plot_title = NULL,
    axis_text_x_cex = NULL,
    base_size = NULL,
    min_ts_to_trigger = .1,
    ablate_both = TRUE,
    show_counts = FALSE, 
    count_label_cex = 0.7
) {
  
  # =========================================================
  # 🎛️ 字体大小超级控制台 (控制你手绘的上下彩条)
  # =========================================================
  SIZE_TOP_CLASS   <- 8.5  # 顶部 "1bp deletion" 等归类文字
  SIZE_TOP_BLOCK   <- 7.5  # 顶部色块内的 "C", "T", "2" 等字母
  SIZE_BOTTOM_NUM  <- 6    # 底部 "1 2 3 4 5 6+" 等数字
  SIZE_BOTTOM_DESC <- 7    # 底部 "Homopolymer Length" 等归类文字
  # =========================================================
  
  # 1. 基础消融逻辑
  del_t_row <- "DEL:T:1:5+"
  ins_t_row <- "INS:T:1:5+"
  del_t_count <- catalog[del_t_row, 1]
  ins_t_count <- catalog[ins_t_row, 1]
  
  if (min_ts_to_trigger < 1.1) {
    total_count <- sum(catalog[, 1])
    del_t_val <- del_t_count / total_count
    ins_t_val <- ins_t_count / total_count
  } else {
    del_t_val <- del_t_count
    ins_t_val <- ins_t_count
  }
  
  del_t_offending <- del_t_val >= min_ts_to_trigger
  ins_t_offending <- ins_t_val >= min_ts_to_trigger
  if (ablate_both) {
    del_t_offending <- del_t_offending || ins_t_offending
    ins_t_offending <- del_t_offending || ins_t_offending
  }
  
  # ---------------------------------------------------------
  # 核心：局部处理函数 (完全自己绘制顶部和底部彩条)
  # ---------------------------------------------------------
  fix_my_plot <- function(p) {
    
    # 【关键修改】：彻底移除了暴力删除 GeomText 的 Filter！
    # 现在 mSigPlot 原生的标题层将被完美保留！
    
    # 允许负数坐标存在（防丢弃魔法）
    if (length(p$scales$scales) > 0) {
      for (i in seq_along(p$scales$scales)) {
        if ("y" %in% p$scales$scales[[i]]$aesthetics) {
          p$scales$scales[[i]]$oob <- function(x, ...) x
        }
      }
    }
    
    # 获取最高点用于计算比例，完美复刻 mSigPlot 原生位置
    c_max <- max(p$data$value, na.rm = TRUE)
    if (c_max <= 0) c_max <- 1
    ymax_native <- c_max * 1.3 # 源码中的原生 ymax 计算公式
    
    # --- 坐标计算 ---
    # 精准对接原生顶部坐标，给原生标题留出精确空间
    y_top_rect_min <- ymax_native * 1.02
    y_top_rect_max <- ymax_native * 1.11
    y_top_block_text <- (y_top_rect_min + y_top_rect_max) / 2
    y_top_class_text <- ymax_native * 1.27
    
    # 底部坐标 (位于0轴下方)
    y_strip_top <- -c_max * 0.02      
    y_strip_bottom <- -c_max * 0.07   
    y_text_num <- -c_max * 0.12        
    y_text_group <- -c_max * 0.20      
    
    # --- 颜色与位置数据 ---
    indel_class_col <- c("#fdbe6f", "#ff8001", "#b0dd8b", "#36a12e",
                         "#fdcab5", "#fc8a6a", "#f14432", "#bc141a", "#d0e1f2",
                         "#94c4df", "#4a98c9", "#1764ab", "#e2e2ef", "#b6b6d8",
                         "#8683bd", "#61409b")
    class_sizes <- c(6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 1, 2, 3, 5)
    class_ends <- cumsum(class_sizes)
    class_starts <- c(1, class_ends[-16] + 1)
    
    # 构建矩形色块
    blocks_df <- data.frame(
      xmin = class_starts - 0.5, xmax = class_ends + 0.5, fill = indel_class_col
    )
    
    # === 顶部数据框 ===
    top_block_text_df <- data.frame(
      x = (blocks_df$xmin + blocks_df$xmax) / 2, y = y_top_block_text,
      label = c(rep(c("C", "T"), 2), rep(c("2", "3", "4", "5+"), 3)),
      color = c(rep(c("black", "white"), 2), rep(c("black", "white", "white", "white"), 3))
    )
    top_class_text_df <- data.frame(
      x = c((blocks_df$xmin[1]+blocks_df$xmax[2])/2, (blocks_df$xmin[3]+blocks_df$xmax[4])/2,
            (blocks_df$xmin[5]+blocks_df$xmax[8])/2, (blocks_df$xmin[9]+blocks_df$xmax[12])/2,
            (blocks_df$xmin[13]+blocks_df$xmax[16])/2),
      y = rep(y_top_class_text, 5),
      label = c("1bp deletion", "1bp insertion", ">1bp deletions at repeats\n(Deletion length)", 
                ">1bp insertions at repeats\n(Insertion length)", "Deletions with microhomology\n(Deletion length)")
    )
    
    # === 底部数据框 ===
    bottom_num_df <- data.frame(
      x = 1:83, y = y_text_num, color = "black",
      label = c(rep(c("1", "2", "3", "4", "5", "6+"), 2), rep(c("0", "1", "2", "3", "4", "5+"), 2),
                rep(c("1", "2", "3", "4", "5", "6+"), 4), rep(c("0", "1", "2", "3", "4", "5+"), 4),
                "1", "1", "2", "1", "2", "3", "1", "2", "3", "4", "5+")
    )
    bottom_group_df <- data.frame(
      x = c(6.5, 18.5, 36.5, 60.5, 78), y = rep(y_text_group, 5),
      label = c("Homopolymer Length", "Homopolymer Length", "Number of Repeat Units", 
                "Number of Repeat Units", "Microhomology Length")
    )
    
    # ------------------ 开始绘制图层 ------------------
    # 【关键修改】：不再使用 ggplot2::labs 加我们的标题，完全保留原生标题！
    p <- p + 
      ggplot2::theme(
        # 移除 plot.title 主题控制，交还给 mSigPlot
        plot.margin = ggplot2::margin(t = 40, r = 10, b = 180, l = 10), 
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank()
      ) +
      # 顶部彩条 & 文字 (我们自己画的，大号字体)
      ggplot2::geom_rect(data = blocks_df, ggplot2::aes(xmin=xmin, xmax=xmax, ymin=y_top_rect_min, ymax=y_top_rect_max), fill=blocks_df$fill, inherit.aes = FALSE) +
      ggplot2::geom_text(data = top_block_text_df, ggplot2::aes(x=x, y=y, label=label), color=top_block_text_df$color, size=SIZE_TOP_BLOCK, fontface="bold", inherit.aes=FALSE) +
      ggplot2::geom_text(data = top_class_text_df, ggplot2::aes(x=x, y=y, label=label), color="black", size=SIZE_TOP_CLASS, inherit.aes=FALSE) +
      
      # 底部彩条 & 文字 (我们自己画的，大号字体)
      ggplot2::geom_rect(data = blocks_df, ggplot2::aes(xmin=xmin, xmax=xmax, ymin=y_strip_bottom, ymax=y_strip_top), fill=blocks_df$fill, inherit.aes = FALSE) +
      ggplot2::geom_text(data = bottom_num_df, ggplot2::aes(x=x, y=y, label=label), color="black", size=SIZE_BOTTOM_NUM, fontface="bold", inherit.aes=FALSE) +
      ggplot2::geom_text(data = bottom_group_df, ggplot2::aes(x=x, y=y, label=label), color="black", size=SIZE_BOTTOM_DESC, fontface="bold", inherit.aes=FALSE)
    
    return(p)
  }
  
  # ---------------------------------------------------------
  # 2. 生成图表调用
  # ---------------------------------------------------------
  prepare_cat <- function(cat) {
    new_cat <- cat; colnames(new_cat) <- NULL; names(new_cat) <- NULL; return(new_cat)
  }
  
  # upper = FALSE 让包不要画顶部的方块了（我们上面自己画了），但保留标题逻辑
  common_args <- list(show_counts = FALSE, upper = FALSE, axis_text_x_cex = axis_text_x_cex, base_size = base_size)
  
  # 【关键修改】：将真实的 plot_title 原封不动地传给 mSigPlot
  plot_args_1 <- c(list(catalog = prepare_cat(catalog), plot_title = plot_title), common_args)
  p_with_ts <- do.call(mSigPlot::plot_ID83, plot_args_1)
  p_with_ts <- fix_my_plot(p_with_ts)
  
  if (!del_t_offending && !ins_t_offending) return(list(plots = p_with_ts))
  
  catalog_modified <- catalog
  if (del_t_offending) catalog_modified[del_t_row, 1] <- 0
  if (ins_t_offending) catalog_modified[ins_t_row, 1] <- 0
  
  if (del_t_offending && ins_t_offending) { pref <- "ins T and del T" } else if (ins_t_offending) { pref <- "ins T" } else { pref <- "del T" }
  
  # 构建原汁原味的 suppressed 标题
  modified_title_text <- paste0(plot_title, "\n(", pref, " suppressed)")
  
  plot_args_2 <- c(list(catalog = prepare_cat(catalog_modified), plot_title = modified_title_text), common_args)
  p_wout_ts <- do.call(mSigPlot::plot_ID83, plot_args_2)
  p_wout_ts <- fix_my_plot(p_wout_ts)
  
  return(list(plots = c(p_with_ts, p_wout_ts), ablated_catalog = catalog_modified))
}