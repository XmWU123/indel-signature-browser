# update_signatures.R
library(data.table)
library(dplyr)

# ==============================================================================
# 配置路径 (请根据你的实际情况确认文件夹名)
# ==============================================================================
data_dir <- "Manuscript_data1.17"
img_dir  <- "www/parallel_plots1.17"  # 图片所在的实际目录
summary_file <- "vignette1.17/prot_table_1.csv" # 统计表路径

message("开始执行签名更新...")

# ==============================================================================
# 1. 修改核心数据文件 (.tsv)
# ==============================================================================

# --- A. 修改 89type_to_83type_connection1.tsv (控制 App 的循环列表) ---
conn_file <- file.path(data_dir, "89type_to_83type_connection1.tsv")
if(file.exists(conn_file)) {
  df_conn <- fread(conn_file, data.table = FALSE)
  
  # 1. 删除 InsDel4b
  df_conn <- df_conn %>% filter(InDel89 != "InsDel4b")
  
  # 2. 改名 InsDel4a -> InsDel4
  df_conn$InDel89[df_conn$InDel89 == "InsDel4a"] <- "InsDel4"
  
  # 保存
  write.table(df_conn, conn_file, sep = "\t", quote = FALSE, row.names = FALSE)
  message("✅ 已更新 connection 表：删除了 4b，重命名了 4a -> 4")
} else {
  warning("❌ 找不到 connection 文件，请检查路径！")
}

# --- B. 修改 89 签名定义表 (Liu_et_al_final_89_type_signatures.tsv) ---
# 注意：如果你的 App 是动态读取这个文件来显示矩阵的，这个也得改
sig89_file <- file.path(data_dir, "Liu_et_al_final_89_type_signatures.tsv")
if(file.exists(sig89_file)) {
  df_sig <- fread(sig89_file, data.table = FALSE)
  
  # 假设第一列是 Mutation Type，后面是签名列
  # 1. 删除 InsDel4b 列
  if("InsDel4b" %in% colnames(df_sig)) {
    df_sig <- df_sig %>% select(-InsDel4b)
  }
  
  # 2. 重命名 InsDel4a 列 -> InsDel4
  colnames(df_sig)[colnames(df_sig) == "InsDel4a"] <- "InsDel4"
  
  write.table(df_sig, sig89_file, sep = "\t", quote = FALSE, row.names = FALSE)
  message("✅ 已更新 89 Signature 定义表")
}

# --- C. 修改统计摘要表 (prot_table_1.csv) ---
if(file.exists(summary_file)) {
  df_stats <- read.csv(summary_file, stringsAsFactors = FALSE)
  
  # 1. 删除 InsDel4b 行
  df_stats <- df_stats %>% filter(type89_sig_id != "InsDel4b")
  
  # 2. 改名
  df_stats$type89_sig_id[df_stats$type89_sig_id == "InsDel4a"] <- "InsDel4"
  
  write.csv(df_stats, summary_file, row.names = FALSE)
  message("✅ 已更新统计摘要表 (prot_table_1.csv)")
}

# ==============================================================================
# 2. 批量处理图片文件 (重命名 & 删除)
# ==============================================================================

if(dir.exists(img_dir)) {
  all_files <- list.files(img_dir, full.names = TRUE)
  
  # --- A. 删除 InsDel4b 的所有相关图片 ---
  # 匹配模式：文件名包含 InsDel4b (注意：要确保不误删 InsDel4b1 等，如果有的话)
  # 通常文件名是 InsDel4b_id89_sig.png 这种格式
  files_to_delete <- grep("InsDel4b", all_files, value = TRUE)
  
  if(length(files_to_delete) > 0) {
    unlink(files_to_delete)
    message(paste("🗑️ 已删除", length(files_to_delete), "张 InsDel4b 图片"))
  }
  
  # --- B. 重命名 InsDel4a 图片为 InsDel4 ---
  # 找到所有包含 InsDel4a 的文件
  files_to_rename <- grep("InsDel4a", list.files(img_dir, full.names = TRUE), value = TRUE)
  
  if(length(files_to_rename) > 0) {
    # 构造新文件名：简单地将文件名字符串中的 InsDel4a 替换为 InsDel4
    # 比如：InsDel4a_id89_sig.png -> InsDel4_id89_sig.png
    new_names <- gsub("InsDel4a", "InsDel4", files_to_rename)
    
    # 执行重命名
    file.rename(from = files_to_rename, to = new_names)
    message(paste("✏️ 已重命名", length(files_to_rename), "张图片：InsDel4a -> InsDel4"))
  } else {
    message("⚠️ 未找到 InsDel4a 的图片，可能已经被重命名过？")
  }
  
  # --- C. 特殊处理：如果有 www/89 目录下的 Activity 图 ---
  top_img_dir <- "www/89"
  if(dir.exists(top_img_dir)) {
    top_delete <- list.files(top_img_dir, pattern = "InsDel4b", full.names = TRUE)
    unlink(top_delete)
    
    top_rename <- list.files(top_img_dir, pattern = "InsDel4a", full.names = TRUE)
    if(length(top_rename)>0) {
      file.rename(top_rename, gsub("InsDel4a", "InsDel4", top_rename))
      message("✅ 已更新 www/89 目录下的 Activity 图片")
    }
  }
  
} else {
  warning("❌ 找不到图片目录 www/parallel_plots1.17，无法处理图片！")
}

message("==========================================")
message("🎉 所有更改已完成！请重启 Shiny App 查看效果。")