library(mSigSpectra) 
library(mSigPlot)
library(dplyr)

GenerateKoh89CatalogfromAnnotateVcf <- function (muts_list, sample_col){
  muts_list <- as.data.frame(muts_list)
  indel_catalogue <- data.frame(table(muts_list[, sample_col], 
                                      muts_list$Koh_89))
  names(indel_catalogue) <- c("Sample", "IndelType", "freq")
  indel_catalogue <- reshape2::dcast(indel_catalogue, IndelType ~ 
                                       Sample, value.var = "freq")
  indel_catalogue <- merge(justified_indel_template_type_4, indel_catalogue, 
                           by = "IndelType", all.x = T)
  indel_catalogue[is.na(indel_catalogue)] <- 0
  rownames(indel_catalogue) <- indel_catalogue[, "IndelType"]
  return(indel_catalogue[mSigSpectra::catalog.row.order$ID89, -c(1:2), drop = FALSE])
}


GenerateCOSMIC83CatalogfromAnnotateVcf <- function (muts_list, sample_col){
  muts_list <- as.data.frame(muts_list)
  indel_catalogue <- data.frame(table(muts_list[, sample_col], 
                                      muts_list$COSMIC_83))
  names(indel_catalogue) <- c("Sample", "IndelType", "freq")
  indel_catalogue <- reshape2::dcast(indel_catalogue, IndelType ~ 
                                       Sample, value.var = "freq")
  indel_catalogue <- merge(Mo_template_COSMIC83, indel_catalogue, 
                           by = "IndelType", all.x = T)
  indel_catalogue[is.na(indel_catalogue)] <- 0
  rownames(indel_catalogue) <- indel_catalogue[, "IndelType"]
  return(indel_catalogue[mSigSpectra::catalog.row.order$ID, -1,drop = FALSE])
}

GenerateKoh476CatalogfromAnnotateVcf <- function (muts_list, sample_col){
  muts_list <- as.data.frame(muts_list)
  indel_catalogue <- data.frame(table(muts_list[, sample_col], 
                                      muts_list$Koh_476))
  names(indel_catalogue) <- c("Sample", "IndelType", "freq")
  indel_catalogue <- reshape2::dcast(indel_catalogue, IndelType ~ 
                                       Sample, value.var = "freq")
  indel_catalogue <- merge(Mo_template_Koh476, indel_catalogue, 
                           by = "IndelType", all.x = T)
  indel_catalogue[is.na(indel_catalogue)] <- 0
  rownames(indel_catalogue) <- indel_catalogue[, "IndelType"]
  return(indel_catalogue[mSigSpectra::catalog.row.order$ID476, -c(1:2), drop = FALSE])
}

ID476_ID89_mapping <- data.table::fread("./ID476_ID89_mapping.txt")


Convert_Indel476_to_Indel89 <- function(indel476.catalog){
  
  indel476.catalog$mut89_class <- ID476_ID89_mapping$indel89.class[match(
    row.names(indel476.catalog),ID476_ID89_mapping$indel476.class
  )]
  indel476.catalog$mut89_class[is.na(indel476.catalog$mut89_class)] <- "Complex"
  df_summary <- indel476.catalog %>%
    group_by(mut89_class) %>%
    summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE), .names = "{.col}_sum")) %>% as.data.frame()
  row.names(df_summary) <- df_summary$mut89_class
  df_summary <- df_summary[match(mSigSpectra::catalog.row.order$ID89,row.names(df_summary)),]
  return(df_summary[,-1])
}
Convert_Indel476_to_Indel83 <- function(indel476.catalog){
  
  indel476.catalog$mut83_class <- ID476_ID89_mapping$indel83.class[match(
    row.names(indel476.catalog),ID476_ID89_mapping$indel476.class
  )]
  if(anyNA(indel476.catalog$mut83_class)){
    indel476.catalog$mut83_class[is.na(indel476.catalog$mut83_class)] <- "Complex"
    
  }
  df_summary <- indel476.catalog %>%
    group_by(mut83_class) %>%
    summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE), .names = "{.col}_sum")) %>% as.data.frame()
  row.names(df_summary) <- df_summary$mut83_class
  df_summary <- df_summary[match(mSigSpectra::catalog.row.order$ID83,row.names(df_summary)),]
  return(df_summary[,-1])
}
