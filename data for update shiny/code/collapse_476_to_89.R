library(dplyr)


t476_to_89 <- function(t476) {
  mut_type_mapping <- read.delim(
    here::here("data for update shiny","Manuscript_data3.31", "ID476_ID89_mapping.txt"),
    stringsAsFactors = FALSE
  )
  
  # 2. 修复核心BUG：既然 ICAMS 里没有 ID89，我们直接从映射文件里提取这89个类别的正确顺序
  correct_row_order <- unique(mut_type_mapping$indel89.class)
  correct_row_order <- correct_row_order[correct_row_order != ""] # 防止有空行
  
  t476$mut89_class <- mut_type_mapping$indel89.class[match(
    row.names(t476),
    mut_type_mapping$indel476.class
  )]
  
  stopifnot(!is.na(t476$mut89_class))
  
  new89 <- t476 %>%
    group_by(mut89_class) %>%
    summarise(across(
      where(is.numeric),
      ~ sum(.x, na.rm = TRUE),
      .names = "{.col}_converted"
    )) %>%
    as.data.frame()
  
  # 4. 确保两边的名称完全对得上（防止拼写错误或映射丢失）
  stopifnot(length(symdiff(correct_row_order, new89[, 1])) == 0)
  
  # 5. 整理行名并按照 correct_row_order 强制排序
  row.names(new89) <- new89$mut89_class
  new89 <- new89[, -1, drop = FALSE]
  new89 <- new89[correct_row_order, , drop = FALSE]
  
  return(new89)
}

do_not_use_t476_to_83 <- function(t476) {
  # see collapse_476_to_83.R
  mut_type_mapping <- data.table::fread(
    here::here("Manuscript_data", "ID476_ID89_mapping.txt")
  )
  correct_row_order = ICAMS::catalog.row.order$ID
  
  t476$mut83_class <- mut_type_mapping$indel83.class[match(
    row.names(t476),
    mut_type_mapping$indel476.class
  )]
  
  stopifnot(!is.na(t476$mut83_class))
  
  new83 <- t476 %>%
    group_by(mut83_class) %>%
    summarise(across(
      where(is.numeric),
      ~ sum(.x, na.rm = TRUE),
      .names = "{.col}_converted"
    )) %>%
    filter(mut83_class != "unmapped") %>%
    as.data.frame()
  
  row.names(new83) <- new83$mut83_class
  for (rname in c(
    "DEL:repeats:2:5+",
    "DEL:repeats:3:5+",
    "DEL:repeats:4:4",
    "INS:repeats:5+:2",
    "INS:repeats:5+:5+"
  )) {
    if (rname %in% rownames(new83)) {
      stop("Programming error")
    }
    new_row <- data.frame(matrix(0, nrow = 1, ncol = ncol(new83)))
    colnames(new_row) <- colnames(new83)
    rownames(new_row) <- rname
    new83 <- rbind(new83, new_row)
  }
  
  stopifnot(length(symdiff(correct_row_order, rownames(new83))) == 0)
  
  new83 <- new83[, -1, drop = FALSE]
  new83 = new83[correct_row_order, ]
  
  new83
}
