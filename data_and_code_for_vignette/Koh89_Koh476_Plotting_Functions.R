library(magrittr)
#library(indelsig.tools.lib)
library(gridExtra)
library(data.table)
library(ICAMS)
library(ggplot2) 
library(dplyr)
library(ggrepel)
#' Plot indel profile in a 89-channel bar plot for single sample
#' Show all x labels
#' @param muts_basis A indel catalogue of a single sample
#' @param text_size Size of text
#' @param plot_title Title of the plot
#' @return A 89-channel indel profile plot
#' @export

PlotKoh89Catalog <- function(ID89.catalog, text_size = 3, plot_title = "ID89",setyaxis=NULL,ylabel="Counts") {
  # === 1. Define Indel Type Labels and Categories ===
  indel_type_4_figurelabel <- structure(
    list(
      IndelType = c(
        # Single base deletions - C
        "[Del(C):R1]A", "[Del(C):R1]T", "[Del(C):R2]A", "[Del(C):R2]T",
        "[Del(C):R3]A", "[Del(C):R3]T", "[Del(C):R(4,5)]A", "[Del(C):R(4,5)]T",
        "[Del(C):R(1,5)]G", "Del(C):R(6,9)",
        # Single base deletions - T
        "A[Del(T):R(1,4)]A", "A[Del(T):R(1,4)]C", "A[Del(T):R(1,4)]G",
        "C[Del(T):R(1,4)]A", "C[Del(T):R(1,4)]C", "C[Del(T):R(1,4)]G",
        "G[Del(T):R(1,4)]A", "G[Del(T):R(1,4)]C", "G[Del(T):R(1,4)]G",
        "A[Del(T):R(5,7)]A", "A[Del(T):R(5,7)]C", "A[Del(T):R(5,7)]G",
        "C[Del(T):R(5,7)]A", "C[Del(T):R(5,7)]C", "C[Del(T):R(5,7)]G",
        "G[Del(T):R(5,7)]A", "G[Del(T):R(5,7)]C", "G[Del(T):R(5,7)]G",
        "A[Del(T):R(8,)]A", "A[Del(T):R(8,)]C", "A[Del(T):R(8,)]G",
        "C[Del(T):R(8,)]A", "C[Del(T):R(8,)]C", "C[Del(T):R(8,)]G",
        "G[Del(T):R(8,)]A", "G[Del(T):R(8,)]C", "G[Del(T):R(8,)]G",
        # Single base insertions - C
        "A[Ins(C):R0]A", "A[Ins(C):R0]T", "Ins(C):R(0,3)", "Ins(C):R(4,6)", "Ins(C):R(7,)",
        # Single base insertions - T
        "A[Ins(T):R(0,4)]A", "A[Ins(T):R(0,4)]C", "A[Ins(T):R(0,4)]G",
        "C[Ins(T):R(0,4)]A", "C[Ins(T):R(0,4)]C", "C[Ins(T):R(0,4)]G",
        "G[Ins(T):R(0,4)]A", "G[Ins(T):R(0,4)]C", "G[Ins(T):R(0,4)]G",
        "A[Ins(T):R(5,7)]A", "A[Ins(T):R(5,7)]C", "A[Ins(T):R(5,7)]G",
        "C[Ins(T):R(5,7)]A", "C[Ins(T):R(5,7)]C", "C[Ins(T):R(5,7)]G",
        "G[Ins(T):R(5,7)]A", "G[Ins(T):R(5,7)]C", "G[Ins(T):R(5,7)]G",
        "A[Ins(T):R(8,)]A", "A[Ins(T):R(8,)]C", "A[Ins(T):R(8,)]G",
        "C[Ins(T):R(8,)]A", "C[Ins(T):R(8,)]C", "C[Ins(T):R(8,)]G",
        "G[Ins(T):R(8,)]A", "G[Ins(T):R(8,)]C", "G[Ins(T):R(8,)]G",
        # Longer deletions (no MH)
        "Del(2,4):R1", "Del(5,):R1", "Del(2,8):U(1,2):R(2,4)", "Del(2,):U(1,2):R(5,)",
        "Del(3,):U(3,):R2", "Del(3,):U(3,):R(3,)",
        # Longer insertions
        "Ins(2,4):R0", "Ins(5,):R0", "Ins(2,4):R1", "Ins(5,):R1", "Ins(2,):R(2,4)", "Ins(2,):R(5,)",
        # Deletions with MH
        "Del(2,5):M1", "Del(3,5):M2", "Del(4,5):M(3,4)", "Del(6,):M1", "Del(6,):M2", "Del(6,):M3", "Del(6,):M(4,)",
        # Complex
        "Complex"
      ),
      Indel = c(
        rep("Del(C)", 10), rep("Del(T)", 27),
        rep("Ins(C)", 5), rep("Ins(T)", 27),
        rep("Del(2,):R(0,9)", 6), rep("Ins(2,)", 6),
        rep("Del(2,):M(1,)", 7), "Complex"
      ),
      Indel3 = c(
        rep("Deletion", 37), rep("Insertion", 32),
        rep("Deletion", 6), rep("Insertion", 6),
        rep("Deletion", 7), "Complex"
      ),
      Figlabel = c(
        # Deletion labels
        "[C1]A", "[C1]T", "[C2]A", "[C2]T", "[C3]A", "[C3]T", "[C(4,5)]A", "[C(4,5)]T", "[C(1,5)]G", "C(6,9)",
        "A[T(1,4)]A", "A[T(1,4)]C", "A[T(1,4)]G", "C[T(1,4)]A", "C[T(1,4)]C", "C[T(1,4)]G",
        "G[T(1,4)]A", "G[T(1,4)]C", "G[T(1,4)]G", "A[T(5,7)]A", "A[T(5,7)]C", "A[T(5,7)]G",
        "C[T(5,7)]A", "C[T(5,7)]C", "C[T(5,7)]G", "G[T(5,7)]A", "G[T(5,7)]C", "G[T(5,7)]G",
        "A[T(8,)]A", "A[T(8,)]C", "A[T(8,)]G", "C[T(8,)]A", "C[T(8,)]C", "C[T(8,)]G",
        "G[T(8,)]A", "G[T(8,)]C", "G[T(8,)]G",
        # Insertion labels
        "A[C0]A", "A[C0]T", "C(0,3)", "C(4,6)", "C(7,)",
        "A[T(0,4)]A", "A[T(0,4)]C", "A[T(0,4)]G", "C[T(0,4)]A", "C[T(0,4)]C", "C[T(0,4)]G",
        "G[T(0,4)]A", "G[T(0,4)]C", "G[T(0,4)]G", "A[T(5,7)]A", "A[T(5,7)]C", "A[T(5,7)]G",
        "C[T(5,7)]A", "C[T(5,7)]C", "C[T(5,7)]G", "G[T(5,7)]A", "G[T(5,7)]C", "G[T(5,7)]G",
        "A[T(8,)]A", "A[T(8,)]C", "A[T(8,)]G", "C[T(8,)]A", "C[T(8,)]C", "C[T(8,)]G",
        "G[T(8,)]A", "G[T(8,)]C", "G[T(8,)]G",
        # Longer del/ins/MH/complex
        "L(2,4):R1", "L(5, ):R1", "L(2,8):U(1,2):R(2,4)", "L(2, ):U(1,2):R(5,)",
        "L(3, ):U(3,):R2", "L(3, ):U(3,):R(3,)",
        "L(2,4):R0", "L(5, ):R0", "L(2,4):R1", "L(5, ):R1", "L(2, ):R(2,4)", "L(2, ):R(5,)",
        "L(2,5):M1", "L(3,5):M2", "L(4,5):M(3,4)", "L(6, ):M1", "L(6, ):M2", "L(6, ):M3", "L(6, ):M(4, )",
        "Complex"
      )
    ),
    class = "data.frame", row.names = c(NA, -89L)
  )
  
  # === 2. Prepare Data for Plotting ===
  my_vector <- indel_type_4_figurelabel$IndelType
  muts_basis <- data.frame(Sample = ID89.catalog, IndelType = my_vector)
  muts_basis_melt <- reshape2::melt(muts_basis, "IndelType")
  muts_basis_melt <- merge(indel_type_4_figurelabel, muts_basis_melt, by = "IndelType", all.x = TRUE)
  muts_basis_melt[is.na(muts_basis_melt)] <- 0
  names(muts_basis_melt) <- c("IndelType", "Indel", "Indel3", "Figlabel", "Sample", "freq")
  muts_basis_melt$Sample <- as.character(muts_basis_melt$Sample)
  
  # === 3. Define Palettes and Block Positions ===
  indel_mypalette_fill <- c(
    "#000000", "#61409b", "#f14432", "#fdbe6f",
    "#ff8001", "#4a98c9", "#b0dd8b", "#36a12e"
  )
  
  indel_positions <- indel_type_4_figurelabel$IndelType
  indel_positions_labels <- indel_type_4_figurelabel$Figlabel
  
  entry <- table(indel_type_4_figurelabel$Indel)
  order_entry <- c(
    "Del(C)", "Del(T)", "Ins(C)", "Ins(T)",
    "Del(2,):R(0,9)", "Ins(2,)", "Del(2,):M(1,)", "Complex"
  )
  entry <- entry[order_entry]
  blocks <- data.frame(
    Type = unique(indel_type_4_figurelabel$Indel),
    fill = indel_mypalette_fill,
    xmin = c(0, cumsum(entry)[-length(entry)]) + 0.5,
    xmax = cumsum(entry) + 0.5
  )
  # blocks$ymin <- max(muts_basis_melt$freq) * 1.08 
  #  blocks$ymax <- max(muts_basis_melt$freq) * 1.2
  
  blocks$ymin <- ifelse(!is.null(setyaxis),setyaxis, max(muts_basis_melt$freq)) * 1.08 
  blocks$ymax <-ifelse(!is.null(setyaxis),setyaxis, max(muts_basis_melt$freq)) * 1.2
  blocks$labels <- c("1bp C", "1bp T", "1bp C", "1bp T", ">=2bp", ">=2bp", "Mh", "X")
  blocks$cl <- c("black", "black", "black", "black", "white", "white", "white", "white")
  
  # Grey blocks for insertion/deletion/complex
  indel_mypalette_fill3 <- c("#000000", "#888888", "#DDDDDD")
  
  # === 4. Prepare Block3 for Overhead Labels ===
  blocks3 <- blocks %>%
    dplyr::mutate(Type = ifelse(Type %in% c("Del(C)", "Del(T)"), "Del1", Type)) %>%
    dplyr::mutate(Type = ifelse(Type %in% c("Ins(C)", "Ins(T)"), "Ins1", Type)) %>%
    dplyr::group_by(Type) %>%
    dplyr::summarise(xmin = min(xmin), xmax = max(xmax)) %>%
    dplyr::mutate(labels = substr(Type, 1, 3)) %>%
    dplyr::mutate(labels = ifelse(labels == "Com", "X", labels)) %>%
    dplyr::arrange(xmin)
  blocks3$ymin <- ifelse(!is.null(setyaxis),setyaxis, max(muts_basis_melt$freq)) * 1.2
  blocks3$ymax <- ifelse(!is.null(setyaxis),setyaxis, max(muts_basis_melt$freq)) * 1.32
  blocks3$cl <- "white"
  blocks3$Type <- c("Del1", "Ins1", "Del2", "Ins2", "DelMH", "X")
  blocks3$cl[1:2] <- "black"
  
  
  indel_mypalette_fill_all <- c(
    "Del1" = "#fe9f38", "Ins1" = "#73bf5d", "Del2" = "#f14432",
    "Ins2" = "#4a98c9", "DelMH" = "#61409b", "X" = "black",
    "Del(2,):M(1,)" = "#61409b", "Del(2,):R(0,9)" = "#f14432",
    "Del(C)" = "#fdbe6f", "Del(T)" = "#ff8001", "Ins(2,)" = "#4a98c9",
    "Ins(C)" = "#b0dd8b", "Ins(T)" = "#36a12e", "Complex" = "black"
  )
  
  # === 5. Plotting ===
  p <- ggplot2::ggplot(data = muts_basis_melt, ggplot2::aes(x = IndelType, y = freq, fill = Indel)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge", width = .7) +
    ggplot2::xlab("Indel Types") +
    ggplot2::ylab(ylabel) +
    ggplot2::scale_x_discrete(limits = indel_positions, labels = indel_positions_labels) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::scale_fill_manual(values = indel_mypalette_fill_all) +
    ggplot2::scale_y_continuous(
      limits = c(0, ifelse(!is.null(setyaxis),setyaxis*1.32,unique(blocks3$ymax))),
      labels = scales::number_format(accuracy = 0.01),
      expand = c(0, 0)
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, size = 5, colour = "black", hjust = 1),
      axis.text.y = ggplot2::element_text(size = 10, colour = "black"),
      legend.position = "none",
      axis.title.x = ggplot2::element_text(size = 15),
      axis.title.y = ggplot2::element_text(size = 15)
    ) +
    ggplot2::geom_rect(
      data = blocks3,
      ggplot2::aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, fill = Type, colour = "white"),
      inherit.aes = FALSE
    ) +
    ggplot2::geom_text(
      data = blocks3,
      ggplot2::aes(x = (xmax + xmin) / 2, y = (ymax + ymin) / 2, label = labels, colour = cl),
      size = text_size, fontface = "bold", inherit.aes = FALSE
    ) +
    ggplot2::scale_colour_manual(values = c("black", "white")) +
    ggplot2::geom_rect(
      data = blocks,
      ggplot2::aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, fill = Type, colour = "white"),
      inherit.aes = FALSE
    ) +
    ggplot2::geom_text(
      data = blocks,
      ggplot2::aes(x = (xmax + xmin) / 2, y = (ymax + ymin) / 2, label = labels, colour = cl),
      size = text_size, fontface = "bold", inherit.aes = FALSE
    )
  
  return(p)
}


Mo_PlotID89toPdf<-function(ID89_catalog,filename){
  plot_list <- lapply(1:ncol(ID89_catalog), function(i) {
    Mo_xplot89(ID89.catalog = ID89_catalog[,i], text_size = 3, plot_title = colnames(ID89_catalog)[i])
  })
  plots_per_page <- 5
  
  # Total number of pages
  total_pages <- ceiling(length(plot_list) / plots_per_page)
  library(gridExtra)
  # Open a PDF device
  pdf(filename, width = 8.2677, height = 14.61613)
  
  # Loop through pages and save each group of plots to the same PDF
  for (page in 1:total_pages) {
    # Get the plots for the current page
    start_index <- (page - 1) * plots_per_page + 1
    end_index <- min(page * plots_per_page, length(plot_list))
    plots_on_page <- plot_list[start_index:end_index]
    
    # Arrange the plots in a grid (2 rows x 4 columns)
    do.call(grid.arrange, c(plots_on_page, nrow = 5, ncol = 1))
  }
  
  # Close the PDF device
  dev.off()
}



PlotKoh89CatalogtoPdf<-function(ID89_catalog,filename){
  plot_list <- lapply(1:ncol(ID89_catalog), function(i) {
    PlotKoh89Catalog(ID89.catalog = ID89_catalog[,i], text_size = 3, plot_title = colnames(ID89_catalog)[i])
  })
  plots_per_page <- 5
  
  # Total number of pages
  total_pages <- ceiling(length(plot_list) / plots_per_page)
  library(gridExtra)
  # Open a PDF device
  pdf(filename, width = 8.2677, height = 14.61613)
  
  # Loop through pages and save each group of plots to the same PDF
  for (page in 1:total_pages) {
    # Get the plots for the current page
    start_index <- (page - 1) * plots_per_page + 1
    end_index <- min(page * plots_per_page, length(plot_list))
    plots_on_page <- plot_list[start_index:end_index]
    
    # Arrange the plots in a grid (2 rows x 4 columns)
    do.call(grid.arrange, c(plots_on_page, nrow = 5, ncol = 1))
  }
  
  # Close the PDF device
  dev.off()
}




PlotKoh476Catalog <- function(Koh476.catalog, text_size=3, plot_title="test",top.types.to.show=NULL){
  my_vector <- Koh476_indeltype$IndelType
  muts_basis <- data.frame(Sample = Koh476.catalog, IndelType = my_vector)
  muts_basis_melt <- reshape2::melt(muts_basis, "IndelType")
  muts_basis_melt <- merge(Koh476_indeltype, muts_basis_melt, by = "IndelType", all.x = TRUE)
  muts_basis_melt[is.na(muts_basis_melt)] <- 0
  names(muts_basis_melt) <- c("IndelType", "Indel", "Indel3", "Figlabel", "Sample", "freq")
  muts_basis_melt$Sample <- as.character(muts_basis_melt$Sample)
  
  muts_basis_melt <- reshape2::melt(muts_basis, "IndelType")
  muts_basis_melt <- merge(Koh476_indeltype, 
                           muts_basis_melt, by = "IndelType", all.x = T)
  muts_basis_melt[is.na(muts_basis_melt)] <- 0
  names(muts_basis_melt) <- c("IndelType", "Indel", "Indel3", 
                              "Figlabel", "Sample", "freq")
  muts_basis_melt$Sample <- as.character(muts_basis_melt$Sample)
  indel_mypalette_fill <- c("#000000", #Complex 
                            "#61409b", # Del MH
                            "#f14432", # Del2bp
                            "#fdbe6f", # Del C
                            "#ff8001", # Del T
                            "#4a98c9", # Ins 2bp
                            "#b0dd8b", #Ins C
                            "#36a12e") #Ins T
  indel_positions <- Koh476_indeltype$IndelType
  indel_positions_labels <- Koh476_indeltype$Figlabel
  entry <- table(Koh476_indeltype$Indel)
  order_entry <- c("Del(C)", "Del(T)",
                   "Ins(C)", "Ins(T)", 
                   "Del(2,):R(1,9)", "Ins(2,):R(0,9)",  
                   "Del(2,):M(1,)", "Complex")
  entry <- entry[order_entry]
  blocks <- data.frame(Type = unique(Koh476_indeltype$Indel), 
                       fill = indel_mypalette_fill, xmin = c(0, cumsum(entry)[-length(entry)]) + 
                         0.5, xmax = cumsum(entry) + 0.5)
  blocks$ymin <- max(muts_basis_melt$freq) * 1.08
  blocks$ymax <- max(muts_basis_melt$freq) * 1.2
  blocks$labels <- c("1bp C", "1bp T", "1bp C", "1bp T", ">=2bp", 
                     ">=2bp", "Mh", "X")
  blocks$cl <- c("black", "black", "black", "black", "white", 
                 "white", "white", "white")
  indel_mypalette_fill3 <- c("#000000", "#888888", "#DDDDDD")
  entry3 <- table(Koh476_indeltype$Indel3)
  order_entry3 <- c("Del1","Ins1","Del2","Ins2","DelMH","Complex")
  entry3 <- entry3[order_entry3]
  blocks3 <- data.frame(Type = unique(Koh476_indeltype$Indel3), 
                        fill = indel_mypalette_fill3, xmin = c(0, cumsum(entry3)[-length(entry3)]) + 
                          0.5, xmax = cumsum(entry3) + 0.5)
  blocks3$ymin <- max(muts_basis_melt$freq) * 1.2
  blocks3$ymax <- max(muts_basis_melt$freq) * 1.32
  blocks3$labels <- c("Deletion","Insertion","Del","Ins","DelMH","X")
  blocks3$cl <- c("black","black", "white", "white","white","white")
  indel_mypalette_fill_all <- c("Del1" = "#fe9f38",
                                "Ins1" = "#73bf5d",
                                "Del2" = "#f14432",
                                "Ins2" = "#4a98c9",
                                "DelMH" = "#61409b",
                                "X" = "black",
                                "Del(2,):M(1,)"= "#61409b",
                                "Del(2,):R(1,9)" = "#f14432", 
                                "Del(C)" = "#fdbe6f", 
                                "Del(T)" = "#ff8001",
                                
                                "Ins(2,):R(0,9)" = "#4a98c9",
                                "Ins(C)"="#b0dd8b", 
                                "Ins(T)" = "#36a12e",
                                "Complex" = "black"# Ins(T)
  )
  top5_labels <- muts_basis_melt %>%
    slice_max(order_by = freq, n = top.types.to.show, with_ties = FALSE)
  p <- ggplot2::ggplot(data = muts_basis_melt, ggplot2::aes(x = IndelType, 
                                                            y = freq, fill = Indel)) + 
    ggplot2::geom_bar(stat = "identity", position = "dodge", width = 0.7) + ggplot2::xlab("Indel Types") + 
    ggplot2::ylab("Count")
  if(!is.null(top.types.to.show)){
    if(!is.numeric(top.types.to.show)){
      message("top.type.to.show needs to be numeric")
    }else{
      p <- p+
        geom_text_repel(
          data = top5_labels,
          aes(label = IndelType),
          position = position_dodge(width = 0.7),
          direction = "x",          # 🔑 horizontal movement
          force = 3,
          box.padding = 0.6,
          point.padding = 0.2,
          segment.color = "grey40",
          segment.size = 0.4,
          min.segment.length = 0,
          show.legend = FALSE,
          size = 6
        ) 
    }
  }
  p <- p + ggplot2::scale_x_discrete(limits = indel_positions, 
                                     labels = indel_positions_labels) + ggplot2::ggtitle(plot_title)
  p <- p + ggplot2::scale_fill_manual(values = indel_mypalette_fill_all) + 
    ggplot2::coord_cartesian(ylim = c(0, unique(blocks3$ymax)), 
                             expand = FALSE)
  p <- p + ggplot2::theme_classic() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, 
                                                                                         vjust = 0.5, size = 5, colour = "black", hjust = 1), 
                                                     axis.text.y = ggplot2::element_text(size = 10, colour = "black"), 
                                                     legend.position = "none", axis.title.x = ggplot2::element_text(size = 15), 
                                                     axis.title.y = ggplot2::element_text(size = 15))
  p <- p + ggplot2::geom_rect(data = blocks3, ggplot2::aes(xmin = xmin, 
                                                           ymin = ymin, xmax = xmax, ymax = ymax, fill = Type, colour = "white"), 
                              inherit.aes = F) + 
    ggplot2::geom_text(data = blocks3, 
                       ggplot2::aes(x = (xmax + xmin)/2, y = (ymax + ymin)/2, 
                                    label = labels, colour = cl), size = text_size, fontface = "bold", 
                       inherit.aes = F) + 
    ggplot2::scale_colour_manual(values = c("black", "white"))
  p <- p + ggplot2::geom_rect(data = blocks, ggplot2::aes(xmin = xmin, 
                                                          ymin = ymin, xmax = xmax, ymax = ymax, fill = Type, colour = "white"), 
                              inherit.aes = F) + ggplot2::geom_text(data = blocks, 
                                                                    ggplot2::aes(x = (xmax + xmin)/2, y = (ymax + ymin)/2, 
                                                                                 label = labels, colour = cl), size = text_size, fontface = "bold", 
                                                                    inherit.aes = F)
  return(p)
}

PlotKoh476CatalogtoPdf <- function (Koh476_catalog,filename){
  
  plot_list <- lapply(1:ncol(Koh476_catalog), function(i) {
    PlotKoh476Catalog(Koh476.catalog = Koh476_catalog[,i], text_size = 3, plot_title = colnames(Koh476_catalog)[i])
  })
  plots_per_page <- 5
  
  # Total number of pages
  total_pages <- ceiling(length(plot_list) / plots_per_page)
  library(gridExtra)
  # Open a PDF device
  pdf(filename, width = 8.2677, height = 14.61613)
  
  # Loop through pages and save each group of plots to the same PDF
  for (page in 1:total_pages) {
    # Get the plots for the current page
    start_index <- (page - 1) * plots_per_page + 1
    end_index <- min(page * plots_per_page, length(plot_list))
    plots_on_page <- plot_list[start_index:end_index]
    
    # Arrange the plots in a grid (2 rows x 4 columns)
    do.call(grid.arrange, c(plots_on_page, nrow = 5, ncol = 1))
  }
  
  # Close the PDF device
  dev.off()
}


Koh89_indeltype <- structure(
  list(
    IndelType = c(
      # Single base deletions - C
      "[Del(C):R1]A", "[Del(C):R1]T", "[Del(C):R2]A", "[Del(C):R2]T",
      "[Del(C):R3]A", "[Del(C):R3]T", "[Del(C):R(4,5)]A", "[Del(C):R(4,5)]T",
      "[Del(C):R(1,5)]G", "Del(C):R(6,9)",
      # Single base deletions - T
      "A[Del(T):R(1,4)]A", "A[Del(T):R(1,4)]C", "A[Del(T):R(1,4)]G",
      "C[Del(T):R(1,4)]A", "C[Del(T):R(1,4)]C", "C[Del(T):R(1,4)]G",
      "G[Del(T):R(1,4)]A", "G[Del(T):R(1,4)]C", "G[Del(T):R(1,4)]G",
      "A[Del(T):R(5,7)]A", "A[Del(T):R(5,7)]C", "A[Del(T):R(5,7)]G",
      "C[Del(T):R(5,7)]A", "C[Del(T):R(5,7)]C", "C[Del(T):R(5,7)]G",
      "G[Del(T):R(5,7)]A", "G[Del(T):R(5,7)]C", "G[Del(T):R(5,7)]G",
      "A[Del(T):R(8,)]A", "A[Del(T):R(8,)]C", "A[Del(T):R(8,)]G",
      "C[Del(T):R(8,)]A", "C[Del(T):R(8,)]C", "C[Del(T):R(8,)]G",
      "G[Del(T):R(8,)]A", "G[Del(T):R(8,)]C", "G[Del(T):R(8,)]G",
      # Single base insertions - C
      "A[Ins(C):R0]A", "A[Ins(C):R0]T", "Ins(C):R(0,3)", "Ins(C):R(4,6)", "Ins(C):R(7,)",
      # Single base insertions - T
      "A[Ins(T):R(0,4)]A", "A[Ins(T):R(0,4)]C", "A[Ins(T):R(0,4)]G",
      "C[Ins(T):R(0,4)]A", "C[Ins(T):R(0,4)]C", "C[Ins(T):R(0,4)]G",
      "G[Ins(T):R(0,4)]A", "G[Ins(T):R(0,4)]C", "G[Ins(T):R(0,4)]G",
      "A[Ins(T):R(5,7)]A", "A[Ins(T):R(5,7)]C", "A[Ins(T):R(5,7)]G",
      "C[Ins(T):R(5,7)]A", "C[Ins(T):R(5,7)]C", "C[Ins(T):R(5,7)]G",
      "G[Ins(T):R(5,7)]A", "G[Ins(T):R(5,7)]C", "G[Ins(T):R(5,7)]G",
      "A[Ins(T):R(8,)]A", "A[Ins(T):R(8,)]C", "A[Ins(T):R(8,)]G",
      "C[Ins(T):R(8,)]A", "C[Ins(T):R(8,)]C", "C[Ins(T):R(8,)]G",
      "G[Ins(T):R(8,)]A", "G[Ins(T):R(8,)]C", "G[Ins(T):R(8,)]G",
      # Longer deletions (no MH)
      "Del(2,4):R1", "Del(5,):R1", "Del(2,8):U(1,2):R(2,4)", "Del(2,):U(1,2):R(5,)",
      "Del(3,):U(3,):R2", "Del(3,):U(3,):R(3,)",
      # Longer insertions
      "Ins(2,4):R0", "Ins(5,):R0", "Ins(2,4):R1", "Ins(5,):R1", "Ins(2,):R(2,4)", "Ins(2,):R(5,)",
      # Deletions with MH
      "Del(2,5):M1", "Del(3,5):M2", "Del(4,5):M(3,4)", "Del(6,):M1", "Del(6,):M2", "Del(6,):M3", "Del(6,):M(4,)",
      # Complex
      "Complex"
    ),
    Indel = c(
      rep("Del(C)", 10), rep("Del(T)", 27),
      rep("Ins(C)", 5), rep("Ins(T)", 27),
      rep("Del(2,):R(0,9)", 6), rep("Ins(2,)", 6),
      rep("Del(2,):M(1,)", 7), "Complex"
    ),
    Indel3 = c(
      rep("Deletion", 37), rep("Insertion", 32),
      rep("Deletion", 6), rep("Insertion", 6),
      rep("Deletion", 7), "Complex"
    ),
    Figlabel = c(
      # Deletion labels
      "[C1]A", "[C1]T", "[C2]A", "[C2]T", "[C3]A", "[C3]T", "[C(4,5)]A", "[C(4,5)]T", "[C(1,5)]G", "C(6,9)",
      "A[T(1,4)]A", "A[T(1,4)]C", "A[T(1,4)]G", "C[T(1,4)]A", "C[T(1,4)]C", "C[T(1,4)]G",
      "G[T(1,4)]A", "G[T(1,4)]C", "G[T(1,4)]G", "A[T(5,7)]A", "A[T(5,7)]C", "A[T(5,7)]G",
      "C[T(5,7)]A", "C[T(5,7)]C", "C[T(5,7)]G", "G[T(5,7)]A", "G[T(5,7)]C", "G[T(5,7)]G",
      "A[T(8,)]A", "A[T(8,)]C", "A[T(8,)]G", "C[T(8,)]A", "C[T(8,)]C", "C[T(8,)]G",
      "G[T(8,)]A", "G[T(8,)]C", "G[T(8,)]G",
      # Insertion labels
      "A[C0]A", "A[C0]T", "C(0,3)", "C(4,6)", "C(7,)",
      "A[T(0,4)]A", "A[T(0,4)]C", "A[T(0,4)]G", "C[T(0,4)]A", "C[T(0,4)]C", "C[T(0,4)]G",
      "G[T(0,4)]A", "G[T(0,4)]C", "G[T(0,4)]G", "A[T(5,7)]A", "A[T(5,7)]C", "A[T(5,7)]G",
      "C[T(5,7)]A", "C[T(5,7)]C", "C[T(5,7)]G", "G[T(5,7)]A", "G[T(5,7)]C", "G[T(5,7)]G",
      "A[T(8,)]A", "A[T(8,)]C", "A[T(8,)]G", "C[T(8,)]A", "C[T(8,)]C", "C[T(8,)]G",
      "G[T(8,)]A", "G[T(8,)]C", "G[T(8,)]G",
      # Longer del/ins/MH/complex
      "L(2,4):R1", "L(5, ):R1", "L(2,8):U(1,2):R(2,4)", "L(2, ):U(1,2):R(5,)",
      "L(3, ):U(3,):R2", "L(3, ):U(3,):R(3,)",
      "L(2,4):R0", "L(5, ):R0", "L(2,4):R1", "L(5, ):R1", "L(2, ):R(2,4)", "L(2, ):R(5,)",
      "L(2,5):M1", "L(3,5):M2", "L(4,5):M(3,4)", "L(6, ):M1", "L(6, ):M2", "L(6, ):M3", "L(6, ):M(4, )",
      "Complex"
    )
  ),
  class = "data.frame", row.names = c(NA, -89L)
)
Koh476_indeltype <- structure(list(IndelType = c("A[Del(C):R1]A", "A[Del(C):R1]G", "A[Del(C):R1]T", "A[Del(C):R2]A", "A[Del(C):R2]G", 
                                                 "A[Del(C):R2]T", "A[Del(C):R3]A", "A[Del(C):R3]G", "A[Del(C):R3]T", 
                                                 "A[Del(C):R4]A", "A[Del(C):R4]G", "A[Del(C):R4]T", "A[Del(C):R5]A", 
                                                 "A[Del(C):R5]G", "A[Del(C):R5]T", "A[Del(C):R6]A", "A[Del(C):R6]G", 
                                                 "A[Del(C):R6]T", "A[Del(C):R7]A", "A[Del(C):R7]G", "A[Del(C):R7]T", 
                                                 "A[Del(C):R8]A", "A[Del(C):R8]G", "A[Del(C):R8]T", "A[Del(C):R(9,)]A", 
                                                 "A[Del(C):R(9,)]G", "A[Del(C):R(9,)]T", "G[Del(C):R1]A", "G[Del(C):R1]G", 
                                                 "G[Del(C):R1]T", "G[Del(C):R2]A", "G[Del(C):R2]G", "G[Del(C):R2]T", 
                                                 "G[Del(C):R3]A", "G[Del(C):R3]G", "G[Del(C):R3]T", "G[Del(C):R4]A", 
                                                 "G[Del(C):R4]G", "G[Del(C):R4]T", "G[Del(C):R5]A", "G[Del(C):R5]G", 
                                                 "G[Del(C):R5]T", "G[Del(C):R6]A", "G[Del(C):R6]G", "G[Del(C):R6]T", 
                                                 "G[Del(C):R7]A", "G[Del(C):R7]G", "G[Del(C):R7]T", "G[Del(C):R8]A", 
                                                 "G[Del(C):R8]G", "G[Del(C):R8]T", "G[Del(C):R(9,)]A", "G[Del(C):R(9,)]G", 
                                                 "G[Del(C):R(9,)]T", "T[Del(C):R1]A", "T[Del(C):R1]G", "T[Del(C):R1]T", 
                                                 "T[Del(C):R2]A", "T[Del(C):R2]G", "T[Del(C):R2]T", "T[Del(C):R3]A", 
                                                 "T[Del(C):R3]G", "T[Del(C):R3]T", "T[Del(C):R4]A", "T[Del(C):R4]G", 
                                                 "T[Del(C):R4]T", "T[Del(C):R5]A", "T[Del(C):R5]G", "T[Del(C):R5]T", 
                                                 "T[Del(C):R6]A", "T[Del(C):R6]G", "T[Del(C):R6]T", "T[Del(C):R7]A", 
                                                 "T[Del(C):R7]G", "T[Del(C):R7]T", "T[Del(C):R8]A", "T[Del(C):R8]G", 
                                                 "T[Del(C):R8]T", "T[Del(C):R(9,)]A", "T[Del(C):R(9,)]G", "T[Del(C):R(9,)]T", 
                                                 "A[Del(T):R1]A", "A[Del(T):R1]C", "A[Del(T):R1]G", "A[Del(T):R2]A", 
                                                 "A[Del(T):R2]C", "A[Del(T):R2]G", "A[Del(T):R3]A", "A[Del(T):R3]C", 
                                                 "A[Del(T):R3]G", "A[Del(T):R4]A", "A[Del(T):R4]C", "A[Del(T):R4]G", 
                                                 "A[Del(T):R5]A", "A[Del(T):R5]C", "A[Del(T):R5]G", "A[Del(T):R6]A", 
                                                 "A[Del(T):R6]C", "A[Del(T):R6]G", "A[Del(T):R7]A", "A[Del(T):R7]C", 
                                                 "A[Del(T):R7]G", "A[Del(T):R8]A", "A[Del(T):R8]C", "A[Del(T):R8]G", 
                                                 "A[Del(T):R(9,)]A", "A[Del(T):R(9,)]C", "A[Del(T):R(9,)]G", "C[Del(T):R1]A", 
                                                 "C[Del(T):R1]C", "C[Del(T):R1]G", "C[Del(T):R2]A", "C[Del(T):R2]C", 
                                                 "C[Del(T):R2]G", "C[Del(T):R3]A", "C[Del(T):R3]C", "C[Del(T):R3]G", 
                                                 "C[Del(T):R4]A", "C[Del(T):R4]C", "C[Del(T):R4]G", "C[Del(T):R5]A", 
                                                 "C[Del(T):R5]C", "C[Del(T):R5]G", "C[Del(T):R6]A", "C[Del(T):R6]C", 
                                                 "C[Del(T):R6]G", "C[Del(T):R7]A", "C[Del(T):R7]C", "C[Del(T):R7]G", 
                                                 "C[Del(T):R8]A", "C[Del(T):R8]C", "C[Del(T):R8]G", "C[Del(T):R(9,)]A", 
                                                 "C[Del(T):R(9,)]C", "C[Del(T):R(9,)]G", "G[Del(T):R1]A", "G[Del(T):R1]C", 
                                                 "G[Del(T):R1]G", "G[Del(T):R2]A", "G[Del(T):R2]C", "G[Del(T):R2]G", 
                                                 "G[Del(T):R3]A", "G[Del(T):R3]C", "G[Del(T):R3]G", "G[Del(T):R4]A", 
                                                 "G[Del(T):R4]C", "G[Del(T):R4]G", "G[Del(T):R5]A", "G[Del(T):R5]C", 
                                                 "G[Del(T):R5]G", "G[Del(T):R6]A", "G[Del(T):R6]C", "G[Del(T):R6]G", 
                                                 "G[Del(T):R7]A", "G[Del(T):R7]C", "G[Del(T):R7]G", "G[Del(T):R8]A", 
                                                 "G[Del(T):R8]C", "G[Del(T):R8]G", "G[Del(T):R(9,)]A", "G[Del(T):R(9,)]C", 
                                                 "G[Del(T):R(9,)]G",
                                                 "A[Ins(C):R0]A", "A[Ins(C):R0]G", "A[Ins(C):R0]T", "A[Ins(C):R1]A", "A[Ins(C):R1]G", 
                                                 "A[Ins(C):R1]T", "A[Ins(C):R2]A", "A[Ins(C):R2]G", "A[Ins(C):R2]T", 
                                                 "A[Ins(C):R3]A", "A[Ins(C):R3]G", "A[Ins(C):R3]T", "A[Ins(C):R4]A", 
                                                 "A[Ins(C):R4]G", "A[Ins(C):R4]T", "A[Ins(C):R5]A", "A[Ins(C):R5]G", 
                                                 "A[Ins(C):R5]T", "A[Ins(C):R6]A", "A[Ins(C):R6]G", "A[Ins(C):R6]T", 
                                                 "A[Ins(C):R7]A", "A[Ins(C):R7]G", "A[Ins(C):R7]T", "A[Ins(C):R8]A", 
                                                 "A[Ins(C):R8]G", "A[Ins(C):R8]T", "A[Ins(C):R(9,)]A", "A[Ins(C):R(9,)]G", 
                                                 "A[Ins(C):R(9,)]T", "G[Ins(C):R0]A", "G[Ins(C):R0]G", "G[Ins(C):R0]T", 
                                                 "G[Ins(C):R1]A", "G[Ins(C):R1]G", "G[Ins(C):R1]T", "G[Ins(C):R2]A", 
                                                 "G[Ins(C):R2]G", "G[Ins(C):R2]T", "G[Ins(C):R3]A", "G[Ins(C):R3]G", 
                                                 "G[Ins(C):R3]T", "G[Ins(C):R4]A", "G[Ins(C):R4]G", "G[Ins(C):R4]T", 
                                                 "G[Ins(C):R5]A", "G[Ins(C):R5]G", "G[Ins(C):R5]T", "G[Ins(C):R6]A", 
                                                 "G[Ins(C):R6]G", "G[Ins(C):R6]T", "G[Ins(C):R7]A", "G[Ins(C):R7]G", 
                                                 "G[Ins(C):R7]T", "G[Ins(C):R8]A", "G[Ins(C):R8]G", "G[Ins(C):R8]T", 
                                                 "G[Ins(C):R(9,)]A", "G[Ins(C):R(9,)]G", "G[Ins(C):R(9,)]T", "T[Ins(C):R0]A", 
                                                 "T[Ins(C):R0]G", "T[Ins(C):R0]T", "T[Ins(C):R1]A", "T[Ins(C):R1]G", 
                                                 "T[Ins(C):R1]T", "T[Ins(C):R2]A", "T[Ins(C):R2]G", "T[Ins(C):R2]T", 
                                                 "T[Ins(C):R3]A", "T[Ins(C):R3]G", "T[Ins(C):R3]T", "T[Ins(C):R4]A", 
                                                 "T[Ins(C):R4]G", "T[Ins(C):R4]T", "T[Ins(C):R5]A", "T[Ins(C):R5]G", 
                                                 "T[Ins(C):R5]T", "T[Ins(C):R6]A", "T[Ins(C):R6]G", "T[Ins(C):R6]T", 
                                                 "T[Ins(C):R7]A", "T[Ins(C):R7]G", "T[Ins(C):R7]T", "T[Ins(C):R8]A", 
                                                 "T[Ins(C):R8]G", "T[Ins(C):R8]T", "T[Ins(C):R(9,)]A", "T[Ins(C):R(9,)]G", 
                                                 "T[Ins(C):R(9,)]T", "A[Ins(T):R0]A", "A[Ins(T):R0]C", "A[Ins(T):R0]G", 
                                                 "A[Ins(T):R1]A", "A[Ins(T):R1]C", "A[Ins(T):R1]G", "A[Ins(T):R2]A", 
                                                 "A[Ins(T):R2]C", "A[Ins(T):R2]G", "A[Ins(T):R3]A", "A[Ins(T):R3]C", 
                                                 "A[Ins(T):R3]G", "A[Ins(T):R4]A", "A[Ins(T):R4]C", "A[Ins(T):R4]G", 
                                                 "A[Ins(T):R5]A", "A[Ins(T):R5]C", "A[Ins(T):R5]G", "A[Ins(T):R6]A", 
                                                 "A[Ins(T):R6]C", "A[Ins(T):R6]G", "A[Ins(T):R7]A", "A[Ins(T):R7]C", 
                                                 "A[Ins(T):R7]G", "A[Ins(T):R8]A", "A[Ins(T):R8]C", "A[Ins(T):R8]G", 
                                                 "A[Ins(T):R(9,)]A", "A[Ins(T):R(9,)]C", "A[Ins(T):R(9,)]G", "C[Ins(T):R0]A", 
                                                 "C[Ins(T):R0]C", "C[Ins(T):R0]G", "C[Ins(T):R1]A", "C[Ins(T):R1]C", 
                                                 "C[Ins(T):R1]G", "C[Ins(T):R2]A", "C[Ins(T):R2]C", "C[Ins(T):R2]G", 
                                                 "C[Ins(T):R3]A", "C[Ins(T):R3]C", "C[Ins(T):R3]G", "C[Ins(T):R4]A", 
                                                 "C[Ins(T):R4]C", "C[Ins(T):R4]G", "C[Ins(T):R5]A", "C[Ins(T):R5]C", 
                                                 "C[Ins(T):R5]G", "C[Ins(T):R6]A", "C[Ins(T):R6]C", "C[Ins(T):R6]G", 
                                                 "C[Ins(T):R7]A", "C[Ins(T):R7]C", "C[Ins(T):R7]G", "C[Ins(T):R8]A", 
                                                 "C[Ins(T):R8]C", "C[Ins(T):R8]G", "C[Ins(T):R(9,)]A", "C[Ins(T):R(9,)]C", 
                                                 "C[Ins(T):R(9,)]G", "G[Ins(T):R0]A", "G[Ins(T):R0]C", "G[Ins(T):R0]G", 
                                                 "G[Ins(T):R1]A", "G[Ins(T):R1]C", "G[Ins(T):R1]G", "G[Ins(T):R2]A", 
                                                 "G[Ins(T):R2]C", "G[Ins(T):R2]G", "G[Ins(T):R3]A", "G[Ins(T):R3]C", 
                                                 "G[Ins(T):R3]G", "G[Ins(T):R4]A", "G[Ins(T):R4]C", "G[Ins(T):R4]G", 
                                                 "G[Ins(T):R5]A", "G[Ins(T):R5]C", "G[Ins(T):R5]G", "G[Ins(T):R6]A", 
                                                 "G[Ins(T):R6]C", "G[Ins(T):R6]G", "G[Ins(T):R7]A", "G[Ins(T):R7]C", 
                                                 "G[Ins(T):R7]G", "G[Ins(T):R8]A", "G[Ins(T):R8]C", "G[Ins(T):R8]G", 
                                                 "G[Ins(T):R(9,)]A", "G[Ins(T):R(9,)]C", "G[Ins(T):R(9,)]G", 
                                                 "Del2:U1:R1", "Del3:U1:R1", "Del4:U1:R1", 
                                                 "Del5:U1:R1", "Del6:U1:R1", "Del7:U1:R1", "Del8:U1:R1", 
                                                 "Del9:U1:R1", "Del(10,):U1:R1", "Del2:U(2,):R1", "Del3:U(2,):R1", 
                                                 "Del4:U(2,):R1", "Del5:U(2,):R1", "Del6:U(2,):R1", "Del7:U(2,):R1", 
                                                 "Del8:U(2,):R1", "Del9:U(2,):R1", "Del(10,):U(2,):R1", 
                                                 "Del2:U1:R3", "Del2:U1:R4", "Del2:U1:R(5,9)", "Del2:U2:R2", 
                                                 "Del2:U2:R3", "Del2:U2:R4", "Del2:U2:R(5,9)", "Del3:U1:R4", 
                                                 "Del3:U1:R(5,9)", "Del3:U3:R2", "Del3:U3:R3", "Del3:U3:R4", 
                                                 "Del3:U3:R(5,9)", "Del4:U1:R(5,9)", "Del4:U2:R3", "Del4:U2:R4", 
                                                 "Del4:U2:R(5,9)", "Del4:U4:R2", "Del4:U4:R3", "Del4:U4:R4", 
                                                 "Del4:U4:R(5,9)", "Del5:U1:R(5,9)", "Del5:U5:R2", "Del5:U5:R3", 
                                                 "Del5:U5:R4", "Del5:U5:R(5,9)", "Del(6,):U1:R(7,9)", 
                                                 "Del(6,):U2:R(4,9)", "Del(6,):U3:R(3,9)", "Del(6,):U(4,):R(2,9)", 
                                                 
                                                 "Ins(2,4):M","Ins(5,):M", "Ins2:U1:R0", "Ins3:U1:R0", "Ins4:U1:R0", 
                                                 "Ins2:U2:R0", "Ins3:U3:R0", "Ins4:U2:R0", "Ins4:U4:R0", 
                                                 "Ins(5,):R0", "Ins2:U1:R1", "Ins2:U1:R2", "Ins2:U1:R3", 
                                                 "Ins2:U1:R4", "Ins2:U1:R(5,9)", "Ins2:U2:R1", "Ins2:U2:R2", 
                                                 "Ins2:U2:R3", "Ins2:U2:R4", "Ins2:U2:R(5,9)", "Ins3:U1:R1", 
                                                 "Ins3:U1:R2", "Ins3:U1:R3", "Ins3:U1:R4", "Ins3:U1:R(5,9)", 
                                                 "Ins3:U3:R1", "Ins3:U3:R2", "Ins3:U3:R3", "Ins3:U3:R4", 
                                                 "Ins3:U3:R(5,9)", "Ins4:U1:R1", "Ins4:U1:R2", "Ins4:U1:R3", 
                                                 "Ins4:U1:R4", "Ins4:U1:R(5,9)", "Ins4:U2:R1", "Ins4:U2:R2", 
                                                 "Ins4:U2:R3", "Ins4:U2:R4", "Ins4:U2:R(5,9)", "Ins4:U4:R1", 
                                                 "Ins4:U4:R2", "Ins4:U4:R3", "Ins4:U4:R4", "Ins4:U4:R(5,9)", 
                                                 "Ins(5,):U1:R1", "Ins(5,):U1:R2", "Ins(5,):U1:R3", "Ins(5,):U1:R4", 
                                                 "Ins(5,):U1:R(5,9)", "Ins(5,):U2:R1", "Ins(5,):U2:R2", 
                                                 "Ins(5,):U2:R3", "Ins(5,):U2:R4", "Ins(5,):U2:R(5,9)", 
                                                 "Ins(5,):U(3,):R1", "Ins(5,):U(3,):R2", "Ins(5,):U(3,):R3", 
                                                 "Ins(5,):U(3,):R4", "Ins(5,):U(3,):R(5,9)", 
                                                 
                                                 "Del2:M1", "Del3:M1", "Del3:M2", "Del4:M1", "Del4:M2", 
                                                 "Del4:M3", "Del5:M1", "Del5:M2", "Del5:M3", "Del5:M4", 
                                                 "Del6:M1", "Del6:M2", "Del6:M3", "Del6:M4", "Del6:M5", 
                                                 "Del(7,):M1", "Del(7,):M2", "Del(7,):M3", "Del(7,):M4", 
                                                 "Del(7,):M5", "Del(7,):M(6,)", 
                                                 "Complex(0,1)", "Complex(2,5)", "Complex(6,10)", "Complex(11,20)", "Complex(21,)"), 
                                   Indel = c("Del(C)", "Del(C)", 
                                             "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", 
                                             "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", 
                                             "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", 
                                             "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", 
                                             "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", 
                                             "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", 
                                             "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", 
                                             "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", 
                                             "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", 
                                             "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", 
                                             "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", 
                                             "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", 
                                             "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", "Del(C)", 
                                             "Del(C)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
                                             "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
                                             "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
                                             "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
                                             "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
                                             "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
                                             "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
                                             "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
                                             "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
                                             "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
                                             "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
                                             "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
                                             "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", "Del(T)", 
                                             "Del(T)", "Del(T)", "Del(T)", "Del(T)",
                                             "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
                                             "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
                                             "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
                                             "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
                                             "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
                                             "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
                                             "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
                                             "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
                                             "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
                                             "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
                                             "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
                                             "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
                                             "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
                                             "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", 
                                             "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(C)", "Ins(T)", 
                                             "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
                                             "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
                                             "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
                                             "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
                                             "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
                                             "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
                                             "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
                                             "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
                                             "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
                                             "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
                                             "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
                                             "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
                                             "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
                                             "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
                                             "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", "Ins(T)", 
                                             "Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
                                             "Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
                                             "Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
                                             "Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
                                             "Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
                                             "Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
                                             "Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
                                             "Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
                                             "Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
                                             "Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
                                             "Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
                                             "Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
                                             "Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
                                             "Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
                                             "Del(2,):R(1,9)", "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
                                             "Del(2,):R(1,9)", "Del(2,):R(1,9)", 
                                             "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
                                             "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
                                             "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
                                             "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
                                             "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
                                             "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
                                             "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
                                             "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
                                             "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
                                             "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
                                             "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
                                             "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
                                             "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
                                             "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
                                             "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
                                             "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
                                             "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
                                             "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
                                             "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", "Ins(2,):R(0,9)", 
                                             "Ins(2,):R(0,9)", "Ins(2,):R(0,9)",  "Del(2,):R(1,9)", 
                                             "Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", 
                                             "Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", 
                                             "Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", 
                                             "Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", 
                                             "Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", "Del(2,):M(1,)", 
                                             "Complex", "Complex", "Complex", "Complex", "Complex"), 
                                   Indel3 = c( "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", 
                                               
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion",
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Insertion", "Insertion", "Insertion", "Insertion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", "Deletion", "Deletion", "Deletion", "Deletion", 
                                               "Deletion", 
                                               "Complex", "Complex", "Complex", "Complex", "Complex"), 
                                   Figlabel = c( "A[Del(C):R1]A", 
                                                 "A[Del(C):R1]G", "A[Del(C):R1]T", "A[Del(C):R2]A", 
                                                 "A[Del(C):R2]G", "A[Del(C):R2]T", "A[Del(C):R3]A", 
                                                 "A[Del(C):R3]G", "A[Del(C):R3]T", "A[Del(C):R4]A", 
                                                 "A[Del(C):R4]G", "A[Del(C):R4]T", "A[Del(C):R5]A", 
                                                 "A[Del(C):R5]G", "A[Del(C):R5]T", "A[Del(C):R6]A", 
                                                 "A[Del(C):R6]G", "A[Del(C):R6]T", "A[Del(C):R7]A", 
                                                 "A[Del(C):R7]G", "A[Del(C):R7]T", "A[Del(C):R8]A", 
                                                 "A[Del(C):R8]G", "A[Del(C):R8]T", "A[Del(C):R(9,)]A", 
                                                 "A[Del(C):R(9,)]G", "A[Del(C):R(9,)]T", "G[Del(C):R1]A", 
                                                 "G[Del(C):R1]G", "G[Del(C):R1]T", "G[Del(C):R2]A", 
                                                 "G[Del(C):R2]G", "G[Del(C):R2]T", "G[Del(C):R3]A", 
                                                 "G[Del(C):R3]G", "G[Del(C):R3]T", "G[Del(C):R4]A", 
                                                 "G[Del(C):R4]G", "G[Del(C):R4]T", "G[Del(C):R5]A", 
                                                 "G[Del(C):R5]G", "G[Del(C):R5]T", "G[Del(C):R6]A", 
                                                 "G[Del(C):R6]G", "G[Del(C):R6]T", "G[Del(C):R7]A", 
                                                 "G[Del(C):R7]G", "G[Del(C):R7]T", "G[Del(C):R8]A", 
                                                 "G[Del(C):R8]G", "G[Del(C):R8]T", "G[Del(C):R(9,)]A", 
                                                 "G[Del(C):R(9,)]G", "G[Del(C):R(9,)]T", "T[Del(C):R1]A", 
                                                 "T[Del(C):R1]G", "T[Del(C):R1]T", "T[Del(C):R2]A", 
                                                 "T[Del(C):R2]G", "T[Del(C):R2]T", "T[Del(C):R3]A", 
                                                 "T[Del(C):R3]G", "T[Del(C):R3]T", "T[Del(C):R4]A", 
                                                 "T[Del(C):R4]G", "T[Del(C):R4]T", "T[Del(C):R5]A", 
                                                 "T[Del(C):R5]G", "T[Del(C):R5]T", "T[Del(C):R6]A", 
                                                 "T[Del(C):R6]G", "T[Del(C):R6]T", "T[Del(C):R7]A", 
                                                 "T[Del(C):R7]G", "T[Del(C):R7]T", "T[Del(C):R8]A", 
                                                 "T[Del(C):R8]G", "T[Del(C):R8]T", "T[Del(C):R(9,)]A", 
                                                 "T[Del(C):R(9,)]G", "T[Del(C):R(9,)]T", "A[Del(T):R1]A", 
                                                 "A[Del(T):R1]C", "A[Del(T):R1]G", "A[Del(T):R2]A", 
                                                 "A[Del(T):R2]C", "A[Del(T):R2]G", "A[Del(T):R3]A", 
                                                 "A[Del(T):R3]C", "A[Del(T):R3]G", "A[Del(T):R4]A", 
                                                 "A[Del(T):R4]C", "A[Del(T):R4]G", "A[Del(T):R5]A", 
                                                 "A[Del(T):R5]C", "A[Del(T):R5]G", "A[Del(T):R6]A", 
                                                 "A[Del(T):R6]C", "A[Del(T):R6]G", "A[Del(T):R7]A", 
                                                 "A[Del(T):R7]C", "A[Del(T):R7]G", "A[Del(T):R8]A", 
                                                 "A[Del(T):R8]C", "A[Del(T):R8]G", "A[Del(T):R(9,)]A", 
                                                 "A[Del(T):R(9,)]C", "A[Del(T):R(9,)]G", "C[Del(T):R1]A", 
                                                 "C[Del(T):R1]C", "C[Del(T):R1]G", "C[Del(T):R2]A", 
                                                 "C[Del(T):R2]C", "C[Del(T):R2]G", "C[Del(T):R3]A", 
                                                 "C[Del(T):R3]C", "C[Del(T):R3]G", "C[Del(T):R4]A", 
                                                 "C[Del(T):R4]C", "C[Del(T):R4]G", "C[Del(T):R5]A", 
                                                 "C[Del(T):R5]C", "C[Del(T):R5]G", "C[Del(T):R6]A", 
                                                 "C[Del(T):R6]C", "C[Del(T):R6]G", "C[Del(T):R7]A", 
                                                 "C[Del(T):R7]C", "C[Del(T):R7]G", "C[Del(T):R8]A", 
                                                 "C[Del(T):R8]C", "C[Del(T):R8]G", "C[Del(T):R(9,)]A", 
                                                 "C[Del(T):R(9,)]C", "C[Del(T):R(9,)]G", "G[Del(T):R1]A", 
                                                 "G[Del(T):R1]C", "G[Del(T):R1]G", "G[Del(T):R2]A", 
                                                 "G[Del(T):R2]C", "G[Del(T):R2]G", "G[Del(T):R3]A", 
                                                 "G[Del(T):R3]C", "G[Del(T):R3]G", "G[Del(T):R4]A", 
                                                 "G[Del(T):R4]C", "G[Del(T):R4]G", "G[Del(T):R5]A", 
                                                 "G[Del(T):R5]C", "G[Del(T):R5]G", "G[Del(T):R6]A", 
                                                 "G[Del(T):R6]C", "G[Del(T):R6]G", "G[Del(T):R7]A", 
                                                 "G[Del(T):R7]C", "G[Del(T):R7]G", "G[Del(T):R8]A", 
                                                 "G[Del(T):R8]C", "G[Del(T):R8]G", "G[Del(T):R(9,)]A", 
                                                 "G[Del(T):R(9,)]C", "G[Del(T):R(9,)]G", 
                                                 "A[Ins(C):R0]A", "A[Ins(C):R0]G", 
                                                 "A[Ins(C):R0]T", "A[Ins(C):R1]A", "A[Ins(C):R1]G", 
                                                 "A[Ins(C):R1]T", "A[Ins(C):R2]A", "A[Ins(C):R2]G", 
                                                 "A[Ins(C):R2]T", "A[Ins(C):R3]A", "A[Ins(C):R3]G", 
                                                 "A[Ins(C):R3]T", "A[Ins(C):R4]A", "A[Ins(C):R4]G", 
                                                 "A[Ins(C):R4]T", "A[Ins(C):R5]A", "A[Ins(C):R5]G", 
                                                 "A[Ins(C):R5]T", "A[Ins(C):R6]A", "A[Ins(C):R6]G", 
                                                 "A[Ins(C):R6]T", "A[Ins(C):R7]A", "A[Ins(C):R7]G", 
                                                 "A[Ins(C):R7]T", "A[Ins(C):R8]A", "A[Ins(C):R8]G", 
                                                 "A[Ins(C):R8]T", "A[Ins(C):R(9,)]A", "A[Ins(C):R(9,)]G", 
                                                 "A[Ins(C):R(9,)]T", "G[Ins(C):R0]A", "G[Ins(C):R0]G", 
                                                 "G[Ins(C):R0]T", "G[Ins(C):R1]A", "G[Ins(C):R1]G", 
                                                 "G[Ins(C):R1]T", "G[Ins(C):R2]A", "G[Ins(C):R2]G", 
                                                 "G[Ins(C):R2]T", "G[Ins(C):R3]A", "G[Ins(C):R3]G", 
                                                 "G[Ins(C):R3]T", "G[Ins(C):R4]A", "G[Ins(C):R4]G", 
                                                 "G[Ins(C):R4]T", "G[Ins(C):R5]A", "G[Ins(C):R5]G", 
                                                 "G[Ins(C):R5]T", "G[Ins(C):R6]A", "G[Ins(C):R6]G", 
                                                 "G[Ins(C):R6]T", "G[Ins(C):R7]A", "G[Ins(C):R7]G", 
                                                 "G[Ins(C):R7]T", "G[Ins(C):R8]A", "G[Ins(C):R8]G", 
                                                 "G[Ins(C):R8]T", "G[Ins(C):R(9,)]A", "G[Ins(C):R(9,)]G", 
                                                 "G[Ins(C):R(9,)]T", "T[Ins(C):R0]A", "T[Ins(C):R0]G", 
                                                 "T[Ins(C):R0]T", "T[Ins(C):R1]A", "T[Ins(C):R1]G", 
                                                 "T[Ins(C):R1]T", "T[Ins(C):R2]A", "T[Ins(C):R2]G", 
                                                 "T[Ins(C):R2]T", "T[Ins(C):R3]A", "T[Ins(C):R3]G", 
                                                 "T[Ins(C):R3]T", "T[Ins(C):R4]A", "T[Ins(C):R4]G", 
                                                 "T[Ins(C):R4]T", "T[Ins(C):R5]A", "T[Ins(C):R5]G", 
                                                 "T[Ins(C):R5]T", "T[Ins(C):R6]A", "T[Ins(C):R6]G", 
                                                 "T[Ins(C):R6]T", "T[Ins(C):R7]A", "T[Ins(C):R7]G", 
                                                 "T[Ins(C):R7]T", "T[Ins(C):R8]A", "T[Ins(C):R8]G", 
                                                 "T[Ins(C):R8]T", "T[Ins(C):R(9,)]A", "T[Ins(C):R(9,)]G", 
                                                 "T[Ins(C):R(9,)]T", "A[Ins(T):R0]A", "A[Ins(T):R0]C", 
                                                 "A[Ins(T):R0]G", "A[Ins(T):R1]A", "A[Ins(T):R1]C", 
                                                 "A[Ins(T):R1]G", "A[Ins(T):R2]A", "A[Ins(T):R2]C", 
                                                 "A[Ins(T):R2]G", "A[Ins(T):R3]A", "A[Ins(T):R3]C", 
                                                 "A[Ins(T):R3]G", "A[Ins(T):R4]A", "A[Ins(T):R4]C", 
                                                 "A[Ins(T):R4]G", "A[Ins(T):R5]A", "A[Ins(T):R5]C", 
                                                 "A[Ins(T):R5]G", "A[Ins(T):R6]A", "A[Ins(T):R6]C", 
                                                 "A[Ins(T):R6]G", "A[Ins(T):R7]A", "A[Ins(T):R7]C", 
                                                 "A[Ins(T):R7]G", "A[Ins(T):R8]A", "A[Ins(T):R8]C", 
                                                 "A[Ins(T):R8]G", "A[Ins(T):R(9,)]A", "A[Ins(T):R(9,)]C", 
                                                 "A[Ins(T):R(9,)]G", "C[Ins(T):R0]A", "C[Ins(T):R0]C", 
                                                 "C[Ins(T):R0]G", "C[Ins(T):R1]A", "C[Ins(T):R1]C", 
                                                 "C[Ins(T):R1]G", "C[Ins(T):R2]A", "C[Ins(T):R2]C", 
                                                 "C[Ins(T):R2]G", "C[Ins(T):R3]A", "C[Ins(T):R3]C", 
                                                 "C[Ins(T):R3]G", "C[Ins(T):R4]A", "C[Ins(T):R4]C", 
                                                 "C[Ins(T):R4]G", "C[Ins(T):R5]A", "C[Ins(T):R5]C", 
                                                 "C[Ins(T):R5]G", "C[Ins(T):R6]A", "C[Ins(T):R6]C", 
                                                 "C[Ins(T):R6]G", "C[Ins(T):R7]A", "C[Ins(T):R7]C", 
                                                 "C[Ins(T):R7]G", "C[Ins(T):R8]A", "C[Ins(T):R8]C", 
                                                 "C[Ins(T):R8]G", "C[Ins(T):R(9,)]A", "C[Ins(T):R(9,)]C", 
                                                 "C[Ins(T):R(9,)]G", "G[Ins(T):R0]A", "G[Ins(T):R0]C", 
                                                 "G[Ins(T):R0]G", "G[Ins(T):R1]A", "G[Ins(T):R1]C", 
                                                 "G[Ins(T):R1]G", "G[Ins(T):R2]A", "G[Ins(T):R2]C", 
                                                 "G[Ins(T):R2]G", "G[Ins(T):R3]A", "G[Ins(T):R3]C", 
                                                 "G[Ins(T):R3]G", "G[Ins(T):R4]A", "G[Ins(T):R4]C", 
                                                 "G[Ins(T):R4]G", "G[Ins(T):R5]A", "G[Ins(T):R5]C", 
                                                 "G[Ins(T):R5]G", "G[Ins(T):R6]A", "G[Ins(T):R6]C", 
                                                 "G[Ins(T):R6]G", "G[Ins(T):R7]A", "G[Ins(T):R7]C", 
                                                 "G[Ins(T):R7]G", "G[Ins(T):R8]A", "G[Ins(T):R8]C", 
                                                 "G[Ins(T):R8]G", "G[Ins(T):R(9,)]A", "G[Ins(T):R(9,)]C", 
                                                 "G[Ins(T):R(9,)]G", 
                                                 "Del2:U1:R1", "Del3:U1:R1", 
                                                 "Del4:U1:R1", "Del5:U1:R1", "Del6:U1:R1", "Del7:U1:R1", 
                                                 "Del8:U1:R1", "Del9:U1:R1", "Del(10,):U1:R1", "Del2:U(2,):R1", 
                                                 "Del3:U(2,):R1", "Del4:U(2,):R1", "Del5:U(2,):R1", 
                                                 "Del6:U(2,):R1", "Del7:U(2,):R1", "Del8:U(2,):R1", 
                                                 "Del9:U(2,):R1", "Del(10,):U(2,):R1", "Del2:U1:R3", 
                                                 "Del2:U1:R4", "Del2:U1:R(5,9)", "Del2:U2:R2", "Del2:U2:R3", 
                                                 "Del2:U2:R4", "Del2:U2:R(5,9)", "Del3:U1:R4", "Del3:U1:R(5,9)", 
                                                 "Del3:U3:R2", "Del3:U3:R3", "Del3:U3:R4", "Del3:U3:R(5,9)", 
                                                 "Del4:U1:R(5,9)", "Del4:U2:R3", "Del4:U2:R4", "Del4:U2:R(5,9)", 
                                                 "Del4:U4:R2", "Del4:U4:R3", "Del4:U4:R4", "Del4:U4:R(5,9)", 
                                                 "Del5:U1:R(5,9)", "Del5:U5:R2", "Del5:U5:R3", "Del5:U5:R4", 
                                                 "Del5:U5:R(5,9)", "Del(6,):U1:R(7,9)", "Del(6,):U2:R(4,9)", 
                                                 "Del(6,):U3:R(3,9)", "Del(6,):U(4,):R(2,9)", 
                                                 "Ins(2,4):M", "Ins(5,):M", "Ins2:U1:R0", 
                                                 "Ins3:U1:R0", "Ins4:U1:R0", "Ins2:U2:R0", "Ins3:U3:R0", 
                                                 "Ins4:U2:R0", "Ins4:U4:R0", "Ins(5,):R0", "Ins2:U1:R1", 
                                                 "Ins2:U1:R2", "Ins2:U1:R3", "Ins2:U1:R4", "Ins2:U1:R(5,9)", 
                                                 "Ins2:U2:R1", "Ins2:U2:R2", "Ins2:U2:R3", "Ins2:U2:R4", 
                                                 "Ins2:U2:R(5,9)", "Ins3:U1:R1", "Ins3:U1:R2", "Ins3:U1:R3", 
                                                 "Ins3:U1:R4", "Ins3:U1:R(5,9)", "Ins3:U3:R1", "Ins3:U3:R2", 
                                                 "Ins3:U3:R3", "Ins3:U3:R4", "Ins3:U3:R(5,9)", "Ins4:U1:R1", 
                                                 "Ins4:U1:R2", "Ins4:U1:R3", "Ins4:U1:R4", "Ins4:U1:R(5,9)", 
                                                 "Ins4:U2:R1", "Ins4:U2:R2", "Ins4:U2:R3", "Ins4:U2:R4", 
                                                 "Ins4:U2:R(5,9)", "Ins4:U4:R1", "Ins4:U4:R2", "Ins4:U4:R3", 
                                                 "Ins4:U4:R4", "Ins4:U4:R(5,9)", "Ins(5,):U1:R1", 
                                                 "Ins(5,):U1:R2", "Ins(5,):U1:R3", "Ins(5,):U1:R4", 
                                                 "Ins(5,):U1:R(5,9)", "Ins(5,):U2:R1", "Ins(5,):U2:R2", 
                                                 "Ins(5,):U2:R3", "Ins(5,):U2:R4", "Ins(5,):U2:R(5,9)", 
                                                 "Ins(5,):U(3,):R1", "Ins(5,):U(3,):R2", "Ins(5,):U(3,):R3", 
                                                 "Ins(5,):U(3,):R4", "Ins(5,):U(3,):R(5,9)", 
                                                 "Del2:M1", 
                                                 "Del3:M1", "Del3:M2", "Del4:M1", "Del4:M2", "Del4:M3", 
                                                 "Del5:M1", "Del5:M2", "Del5:M3", "Del5:M4", "Del6:M1", 
                                                 "Del6:M2", "Del6:M3", "Del6:M4", "Del6:M5", "Del(7,):M1", 
                                                 "Del(7,):M2", "Del(7,):M3", "Del(7,):M4", "Del(7,):M5", 
                                                 "Del(7,):M(6,)", "Complex(0,1)", "Complex(2,5)", 
                                                 "Complex(6,10)", "Complex(11,20)", "Complex(21,)")), 
                              class = "data.frame", row.names = c(NA, -476L))  

