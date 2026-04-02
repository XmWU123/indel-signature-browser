# Helper functions for vignette.Rmd
# These functions separate computation from plotting for easier debugging

assign_log_file <- here::here("data for update shiny", "vignette", "assign_for_residual.log")
if (file.exists(assign_log_file)) {
  file.remove(assign_log_file)
  message("removed ", assign_log_file)
}

source(here::here("data for update shiny", "vignette", "plot_83_w_wout_t.R"))

# Source collapse functions for Sankey plot generation
source(here::here("data for update shiny","code", "collapse_476_to_83.R"))

# Global path to finalized data directory
finalized_dir <- here::here(
  "data for update shiny",
  "Manuscript_data3.31", 
  "Mo_CAP9_analysis",
  "finalized_cap9"
)

# Named paths to finalized data files
finalized_files <- c(
  "89_assignment" = file.path(finalized_dir, "liu_et_al_89_assignment.tsv"),
  "89_signatures" = file.path(finalized_dir, "liu_et_al_89_signatures.tsv"),
  "89_spectra" = file.path(finalized_dir, "liu_et_al_89_spectra.tsv"),
  "83_signatures" = file.path(finalized_dir, "liu_et_al_83_signatures.tsv"),
  "83_spectra" = file.path(finalized_dir, "liu_et_al_83_spectra.tsv"),
  "476_signatures" = file.path(finalized_dir, "liu_et_al_476_signatures.tsv"),
  "476_spectra" = file.path(finalized_dir, "liu_et_al_476_spectra.tsv"),
  "connection_table" = file.path(finalized_dir, "connection_table.tsv")
)

#' Read a finalized data file
#'
#' @param name Short name from `finalized_files` (e.g. "83_spectra").
#' @return Data frame with row names from first column.
read_finalized <- function(name, row.names = 1) {
  path <- finalized_files[[name]]
  if (is.null(path)) {
    stop("Unknown finalized file: ", name)
  }
  read.delim(path, sep = "\t", row.names = row.names, check.names = FALSE)
}

#' Format signature name with Greek letters
#'
#' Replaces _alpha with α and _beta with β in signature names
#'
#' @param name Character: the signature name
#' @return Character: formatted name with Greek letters
format_signature_name <- function(name) {
  name <- gsub("_alpha", "α", name)
  name <- gsub("_beta", "β", name)
  name
}

#' Create HTML hyperlink for signature ID
#'
#' Converts a signature ID to an HTML hyperlink pointing to its section.
#' The display name uses Greek letters, and the anchor uses the original
#' ASCII sig_id (lowercased) to match explicit IDs in section headers.
#'
#' @param sig_id Character: the signature identifier (e.g., "InsDel1a", "InsDel_A_alpha")
#' @return Character: HTML anchor tag (e.g., '<a href="#insdel_a_alpha">InsDel_Aα</a>')
make_sig_hyperlink <- function(sig_id) {
  display_name <- format_signature_name(sig_id)
  anchor_id <- tolower(sig_id) # Use original ASCII ID for anchor
  paste0('<a href="#', anchor_id, '">', display_name, '</a>')
}

#' Create a fenced div with a style class
#'
#' @param txt Character: the text content for the div
#' @param style Character: the CSS class for the div (default ".callout-note")
#' @return Character string with fenced div markdown
fenced_div <- function(txt, style = ".callout-note") {
  paste0('\n\n::: {', style, '}\n', txt, '\n:::\n\n')
}

#' Output a fenced div to the document
#'
#' Convenience wrapper that combines cat() and fenced_div()
#'
#' @param txt Character: the text content for the div
#' @param style Character: the CSS class for the div.
#' Alternatives would be .callout-{tip (green),
#' warning (yellow or oranage), caution (orange),
#' important (red)}.
#' @return NULL (called for side effect of outputting to document)
catfdiv <- function(txt, style = ".callout-note") {
  cat(fenced_div(txt, style))
}

#' Find signature-specific text file
#'
#' @param sig_id Character: the signature identifier
#' @return Character string with file contents, or NULL if file doesn't exist
find_sig_txt <- function(sig_id) {
  file_path <- file.path(data_dir, "per_sig_txt", glue::glue("{sig_id}.md"))
  if (!file.exists(file_path)) {
    return(NULL)
  }
  readLines(file_path, warn = FALSE) |> paste(collapse = "\n")
}

#' Compute cosine similarities for a signature-catalog pair
#'
#' @param type89_sig_id Character: the ID89 signature name
#' @param exemplar_89 Character: the BestMatch89_1 linking tumor ID
#' @param exemplar_83 Character: the BestMatch83_1 linking tumor ID
#' @param exemplar_476 Character: the BestMatch476_1 linking tumor ID
#' @param ID83signature Character: the corresponding ID83 signature name
#' @param ID89_signatures Data frame of ID89 signatures
#' @param ID89_catalogs Data frame of ID89 catalogs
#' @param ID83_signatures ICAMS catalog of ID83 signatures
#' @param ID83_catalogs ICAMS catalog of ID83 catalogs
#' @param ID83_catalogs_no_polyT ICAMS catalog with polyT removed
#' @param ID476_signatures Data frame of ID476 signatures
#' @param ID476_catalogs Data frame of ID476 catalogs
#' @param assignment_matrix Data frame of signature assignments
#' @param cosmic_matches Named list of data frames with COSMIC signature matches
#' @param jin_matches Named list of data frames with Jin signature matches
#' @param koh_matches Named list of data frames with Koh signature matches
#' @return List with cosine similarities and intermediate data
compute_sig_data <- function(
    type89_sig_id,
    exemplar_89,
    exemplar_83,
    exemplar_476,
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
  message("type89_sig_id = ", type89_sig_id)
  
  has_476_sig <- type89_sig_id %in% colnames(ID476_signatures)
  # Check if mapped 89-type signature exists (column name is {signature}_converted)
  mapped_col_name <- paste0(type89_sig_id, "_converted")
  has_mapped_476_sig <- has_476_sig &&
    !is.null(ID89_mapped_signatures) &&
    mapped_col_name %in% colnames(ID89_mapped_signatures)
  
  # Check if mapped 83-type signature exists
  # Only consider it if a 476-type signature was extracted
  has_83_mapped_sig <- has_476_sig &&
    !is.null(ID83_mapped_signatures) &&
    mapped_col_name %in% colnames(ID83_mapped_signatures)
  
  # Get COSMIC matches for this 83-type signature
  cosmic_match_data <- NULL
  if (!is.null(cosmic_matches) && ID83signature %in% names(cosmic_matches)) {
    cosmic_match_data <- cosmic_matches[[ID83signature]]
  }
  
  # Get Jin matches for this 83-type signature
  jin_match_data <- NULL
  if (!is.null(jin_matches) && ID83signature %in% names(jin_matches)) {
    jin_match_data <- jin_matches[[ID83signature]]
  }
  
  # Get Koh matches for this 89-type signature
  koh_match_data <- NULL
  if (!is.null(koh_matches) && type89_sig_id %in% names(koh_matches)) {
    koh_match_data <- koh_matches[[type89_sig_id]]
  }
  
  result <- list(
    type89_sig_id = type89_sig_id,
    exemplar_89 = exemplar_89,
    exemplar_83 = exemplar_83,
    exemplar_476 = exemplar_476,
    ID83signature = ID83signature,
    is_insdel15_16 = type89_sig_id %in% c("InsDel15", "InsDel16"),
    is_polyT_removed = ID83signature %in%
      c("C_ID7", "ID_J", "C_ID10"), ## remove ID_N
    has_476_signature = has_476_sig,
    has_83_signature = ID83signature %in% colnames(ID83_signatures),
    has_mapped_476_sig = has_mapped_476_sig,
    has_83_mapped_signature = has_83_mapped_sig,
    cosmic_matches = cosmic_match_data,
    jin_matches = jin_match_data,
    koh_matches = koh_match_data
  )
  
  # Compute cosine89: sig89 vs exemplar_89's 89-type spectrum
  result$cosine89 <-
    lsa::cosine(
      as.numeric(ID89_signatures[, type89_sig_id]),
      as.numeric(ID89_catalogs[, exemplar_89])
    )
  # Compute cosine similarity between main signature and mapped signature
  if (has_mapped_476_sig) {
    result$cosine89_mapped <-
      lsa::cosine(
        as.numeric(ID89_signatures[, type89_sig_id]),
        as.numeric(ID89_mapped_signatures[, mapped_col_name])
      )
  } else {
    result$cosine89_mapped <- NA
  }
  
  # For non-InsDel15/16, compute the decomposition using exemplar_89
  if (!result$is_insdel15_16) {
    # Find signatures present in both the signature matrix and assignment matrix
    common_sigs <- intersect(
      colnames(ID89_signatures),
      row.names(assignment_matrix)
    )
    
    # Check if exemplar_89 exists in assignment matrix columns
    if (exemplar_89 %in% colnames(assignment_matrix)) {
      # Get assignment for this catalog, filtered to common signatures
      assignment <- assignment_matrix[common_sigs, exemplar_89, drop = FALSE]
      
      # Workaround for assignments in which the number of mutations is doubled:
      # check if assignment total is approx 2x the spectrum total; if so, halve it
      spectrum_total <- sum(ID89_catalogs[, exemplar_89])
      assignment_total <- sum(assignment)
      if (spectrum_total > 0) {
        ratio <- assignment_total / spectrum_total
        if (ratio > 1.8 && ratio < 2.2) {
          assignment <- assignment / 2
        }
      }
      
      # Check if this signature (or its alias) is in common_sigs
      sigid <- type89_sig_id
      if (FALSE && !(sigid %in% common_sigs)) {
        # Try known aliases
        if (
          sigid %in%
          c("InsDel_N_alpha", "InsDel_N_beta") &&
          "InsDel_J" %in% common_sigs
        ) {
          sigid <- "InsDel_J"
        }
      }
      
      if (sigid %in% common_sigs) {
        result$target_sig_partial_spectrum <-
          assignment[sigid, ] * ID89_signatures[, sigid]
        
        result$residual_spectrum <- ID89_catalogs[, exemplar_89] -
          result$target_sig_partial_spectrum
        
        result$residual_spectrum[result$residual_spectrum < 0] <- 0
        
        # Log assignment and sigid for debugging
        log_con <- file(assign_log_file, open = "a")
        writeLines(paste0("=== sigid: ", sigid, " ==="), log_con)
        sigid_pct <- 100 * sum(assignment[sigid, ]) / sum(assignment)
        writeLines(paste("sigid_pct = ", sigid_pct), log_con)
        if (sigid_pct < 50) {
          writeLines(
            paste(
              "WARNING:",
              sigid,
              "has only",
              round(sigid_pct, 1),
              "% of mutations"
            ),
            log_con
          )
        }
        writeLines("assignment (with row names):", log_con)
        capture.output(print(assignment), file = log_con, append = TRUE)
        writeLines("", log_con)
        close(log_con)
        message("closed ", assign_log_file)
        
        if (FALSE) {
          # Zero out the current signature to get contribution by other signatures
          assignment_others <- assignment
          assignment_others[sigid, ] <- 0
          
          # Reconstruct catalog without this signature
          result$residual_spectrum <- as.matrix(
            ID89_signatures[, common_sigs]
          ) %*%
            as.matrix(assignment_others)
          
          # Difference = mutations attributed to this signature
          result$target_sig_partial_spectrum <- ID89_catalogs[,
                                                              exemplar_89,
                                                              drop = FALSE
          ] -
            result$residual_spectrum
          result$target_sig_partial_spectrum[
            result$target_sig_partial_spectrum < 0
          ] <- 0
        }
        
        # Cosine of diff vs signature
        result$cosine_sig_89_v_partial_spec <-
          lsa::cosine(
            as.numeric(ID89_signatures[, type89_sig_id]),
            as.numeric(as.matrix(result$target_sig_partial_spectrum))
          )
      } else {
        message(
          "Skipping decomposition for ",
          type89_sig_id,
          ": signature not found in assignment matrix"
        )
        result$cosine_sig_89_v_partial_spec <- NA
        result$residual_spectrum <- NULL
        result$target_sig_partial_spectrum <- NULL
      }
    } else {
      message(
        "Skipping decomposition for ",
        type89_sig_id,
        ": exemplar ",
        exemplar_89,
        " not found in assignment matrix"
      )
      result$cosine_sig_89_v_partial_spec <- NA
      result$residual_spectrum <- NULL
      result$target_sig_partial_spectrum <- NULL
    }
  } else {
    result$cosine_sig_89_v_partial_spec <- NA
    result$residual_spectrum <- NULL
    result$target_sig_partial_spectrum <- NULL
  }
  
  # Compute cosine476: sig476 vs exemplar_476's 476-type spectrum
  if (result$has_476_signature) {
    result$cosine476 <-
      lsa::cosine(
        as.numeric(ID476_signatures[, type89_sig_id]),
        as.numeric(ID476_catalogs[, exemplar_476])
      )
  } else {
    result$cosine476 <- NA
  }
  
  # Also compute cosine476_linking: sig476 vs exemplar_89's 476-type spectrum
  if (result$has_476_signature && exemplar_89 %in% colnames(ID476_catalogs)) {
    result$cosine476_linking <-
      lsa::cosine(
        as.numeric(ID476_signatures[, type89_sig_id]),
        as.numeric(ID476_catalogs[, exemplar_89])
      )
  } else {
    result$cosine476_linking <- NA
  }
  
  # Compute cosine83: sig83 vs exemplar_83's 83-type spectrum
  if (result$is_polyT_removed) {
    result$cosine83 <-
      lsa::cosine(
        as.numeric(ID83_signatures[, ID83signature]),
        as.numeric(ID83_catalogs_no_polyT[, exemplar_83])
      )
  } else {
    if (ID83signature %in% colnames(ID83_signatures)) {
      result$cosine83 <-
        lsa::cosine(
          as.numeric(ID83_signatures[, ID83signature]),
          as.numeric(ID83_catalogs[, exemplar_83])
        )
    } else {
      result$cosine83 <- 0
    }
  }
  
  # Also compute cosine83_linking: sig83 vs exemplar_89's 83-type spectrum
  if (
    ID83signature %in%
    colnames(ID83_signatures) &&
    exemplar_89 %in% colnames(ID83_catalogs)
  ) {
    if (result$is_polyT_removed) {
      result$cosine83_linking <-
        lsa::cosine(
          as.numeric(ID83_signatures[, ID83signature]),
          as.numeric(ID83_catalogs_no_polyT[, exemplar_89])
        )
    } else {
      result$cosine83_linking <-
        lsa::cosine(
          as.numeric(ID83_signatures[, ID83signature]),
          as.numeric(ID83_catalogs[, exemplar_89])
        )
    }
  } else {
    result$cosine83_linking <- NA
  }
  
  # Compute cosine similarity between native 83-type and mapped 83-type signature
  if (has_83_mapped_sig && result$has_83_signature) {
    result$cosine83_mapped <-
      lsa::cosine(
        as.numeric(ID83_signatures[, ID83signature]),
        as.numeric(ID83_mapped_signatures[, mapped_col_name])
      )
  } else {
    result$cosine83_mapped <- NA
  }
  
  return(result)
}

#' Generate markdown footer text with cosine summary
#'
#' @param sig_data List returned from compute_sig_data
#' @return Character string with markdown text
generate_section_footer <- function(sig_data) {
  cosine476_text <- if (is.na(sig_data$cosine476)) {
    "N/A"
  } else {
    as.character(sig_data$cosine476)
  }
  
  df <- data.frame(
    `83-type` = sig_data$cosine83,
    `476-type` = cosine476_text,
    `89-type` = sig_data$cosine89,
    check.names = FALSE
  )
  
  table_output <- knitr::kable(df)
  
  paste0(
    "\n\n",
    paste(table_output, collapse = "\n"),
    "\n\n---\n"
  )
}

#' Generate all plots for a signature and save to files
#'
#' @param sig_data List returned from compute_sig_data
#' @param ID89_signatures Data frame of ID89 signatures
#' @param ID89_catalogs Data frame of ID89 catalogs
#' @param ID83_signatures ICAMS catalog of ID83 signatures
#' @param ID83_catalogs ICAMS catalog of ID83 catalogs
#' @param ID83_catalogs_no_polyT ICAMS catalog with polyT removed
#' @param ID476_signatures Data frame of ID476 signatures
#' @param ID476_catalogs Data frame of ID476 catalogs
#' @param plot_dir Directory to save plots
#' @param cosmic_signatures Data frame of COSMIC signatures for matching plots
#' @param jin_signatures Data frame of Jin signatures for matching plots
#' @param koh_signatures Data frame of Koh signatures for matching plots
#' @param collapse_flows List of flow data frames from bipartite matching collapse
#' @return List with paths to all generated plot files
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
    plot476_label_size = 3,
    ID89_mapped_signatures = NULL,
    ID83_mapped_signatures = NULL,
    cosmic_signatures = NULL,
    jin_signatures = NULL,
    koh_signatures = NULL,
    min_ts_to_trigger = 0.15,
    collapse_flows = NULL
) {
  # Create safe filename prefix from signature name
  safe_name <- gsub("[^a-zA-Z0-9_]", "_", sig_data$type89_sig_id)
  
  # [修正：保留了最新版所有的路径，包括 476match 和 83match，并加上 thumb]
  paths <- list(
    id89_sig = file.path(plot_dir, paste0(safe_name, "_id89_sig.png")),
    id89_thumb = file.path(plot_dir, paste0(safe_name, "_Thumbnail.png")), # [新增] 89缩略图
    id89_mapped = file.path(plot_dir, paste0(safe_name, "_id89_mapped.png")),
    id89_catalog = file.path(plot_dir, paste0(safe_name, "_id89_catalog.png")),
    id89_residual = file.path(
      plot_dir,
      paste0(safe_name, "_id89_residual.png")
    ),
    id89_target_sig_partial_spectrum = file.path(
      plot_dir,
      paste0(safe_name, "_id89_target_sig_partial_spectrum.png")
    ),
    id476_sig = file.path(plot_dir, paste0(safe_name, "_id476_sig.png")),
    id476_thumb = file.path(plot_dir, paste0(safe_name, "_id476_Thumbnail.png")), # [新增] 476缩略图
    id476_catalog = file.path(
      plot_dir,
      paste0(safe_name, "_id476_catalog.png")
    ),
    id476_catalog_476match = file.path(
      plot_dir,
      paste0(safe_name, "_id476_catalog_476match.png")
    ),
    id83_sig = file.path(plot_dir, paste0(safe_name, "_id83_sig.png")),
    id83_mapped = file.path(plot_dir, paste0(safe_name, "_id83_mapped.png")),
    id83_catalog = file.path(plot_dir, paste0(safe_name, "_id83_catalog.png")),
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
    id83_catalog_ablated_catalog = NULL,
    id83_catalog_83match = file.path(
      plot_dir,
      paste0(safe_name, "_id83_catalog_83match.png")
    ),
    id83_catalog_83match_ablated = file.path(
      plot_dir,
      paste0(safe_name, "_id83_catalog_83match_ablated.png")
    ),
    id83_catalog_83match_ablated_catalog = NULL
  )
  
  if (sig_data$has_83_signature) {
    paths$id83_thumb <- file.path(plot_dir, paste0(sig_data$ID83signature, "_Thumbnail.png"))
  } else {
    paths$id83_thumb <- NULL
  }
  
  # Helper to save ggplot
  save_ggplot <- function(p, path, width = 19, height = 3) {
    ggplot2::ggsave(
      path,
      p,
      width = width,
      height = height,
      dpi = 150,
      bg = "white"
    )
  }
  
  p89 <- function(catalog, plot_title, setyaxis = NULL) {
    mSigPlot::plot_89(
      catalog,
      plot_title = plot_title,
      base_size = getp('basesize89'),
      setyaxis = setyaxis,
      count_label_size = 0.9,
      plot_complex = FALSE # 关掉复杂标注使图更清爽
    )
  }
  
  save89 = function(myplot, path) {
    save_ggplot(myplot, path, width = getp('w89'), height = getp('h89'))
  }
  
  # ID89 Plot 1: Signature
  p1 <- p89(
    ID89_signatures[, sig_data$type89_sig_id, drop = FALSE],
    plot_title = sig_data$type89_sig_id
  )
  save89(p1, paths$id89_sig)
  
  # --- [新增注入] ID89 缩略图生成 ---
  if (!is.null(paths$id89_thumb)) {
    p1_thumb_raw <- mSigPlot::plot_89(
      ID89_signatures[, sig_data$type89_sig_id, drop = FALSE],
      plot_title = "", base_size = 8, text_cex = 1.5, top_bar_text_cex = 1.5,
      show_x_axis_text = FALSE, plot_complex = FALSE
    )
    p1_thumb <- p1_thumb_raw + 
      ggplot2::theme_void() + 
      ggplot2::theme(
        plot.title = ggplot2::element_blank(), axis.text = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
        legend.position = "none", plot.margin = ggplot2::margin(0,0,0,0),
        strip.text = ggplot2::element_text(size = 8)
      ) + ggplot2::labs(title = NULL, x = NULL, y = NULL)
    ggplot2::ggsave(filename = paths$id89_thumb, plot = p1_thumb, width = 4, height = 1, dpi = 300, bg="white")
  }
  
  # ID89 Plot 1b: Mapped signature (from 476-type)
  if (sig_data$has_mapped_476_sig && !is.null(ID89_mapped_signatures)) {
    mapped_col_name <- paste0(sig_data$type89_sig_id, "_converted")
    p1b <- p89(
      ID89_mapped_signatures[, mapped_col_name, drop = FALSE],
      plot_title = paste0(
        sig_data$type89_sig_id,
        " converted from signature discovered in the 476-type spectra | cosine similarity to ",
        sig_data$type89_sig_id,
        " = ",
        format(sig_data$cosine89_mapped, digits = getp("cosine_digits"))
      )
    )
    save89(p1b, paths$id89_mapped)
  } else {
    paths$id89_mapped <- NULL
  }
  
  # ID89 Plot 2: Catalog (using exemplar_89)
  catalogtoplot = ID89_catalogs[, sig_data$exemplar_89, drop = FALSE]
  #ymax = max(c(catalogtoplot,max(sig_data$target_sig_partial_spectrum)))  ## Mo comment: some peaks exceed the ymax in partial credit
  print(max(catalogtoplot))
  ymax = max(catalogtoplot) * 1.1
  p2 <- p89(
    catalogtoplot,
    plot_title = paste0(
      "Spectrum of linking-tumor ",
      sig_data$exemplar_89,
      " | cosine similarity to ",
      sig_data$type89_sig_id,
      " = ",
      format(sig_data$cosine89, digits = getp("cosine_digits"))
    ),
    setyaxis = ymax
  )
  save89(p2, paths$id89_catalog)
  
  # ID89 Plots 3 & 4: Decomposition (only for non-InsDel15/16)
  if (!sig_data$is_insdel15_16 && !is.null(sig_data$residual_spectrum)) {
    target_sig_title <- paste0(
      "Partial mutational spectrum of linking-tumor ",
      sig_data$exemplar_89,
      " due to ",
      sig_data$type89_sig_id
    )
    
    residual_title <- paste0(
      "Remaining mutations in ",
      sig_data$exemplar_89,
      " not due to ",
      sig_data$type89_sig_id
    )
    
    p3 <- p89(
      sig_data$residual_spectrum,
      plot_title = residual_title,
      setyaxis = ymax
    )
    save89(p3, paths$id89_residual)
    
    p4 <- p89(
      sig_data$target_sig_partial_spectrum,
      plot_title = target_sig_title,
      setyaxis = ymax
    )
    save89(p4, paths$id89_target_sig_partial_spectrum)
  } else {
    paths$id89_residual <- NULL
    paths$id89_target_sig_partial_spectrum <- NULL
  }
  
  # ID476 plots
  p476 <- function(catalog, plot_title) {
    mSigPlot::plot_476(
      catalog,
      plot_title = plot_title,
      num_labels = 5,
      base_size = ppar[["plot476_base_size"]],
      plot_complex = FALSE # 关掉复杂标注使图更清爽
    )
  }
  
  save476 = function(myplot, path) {
    save_ggplot(myplot, path, width = getp('w476'), height = getp('h476'))
  }
  
  if (sig_data$has_476_signature) {
    p5 <- p476(
      ID476_signatures[, sig_data$type89_sig_id],
      plot_title = paste0(
        "Extracted 476-type signature corresponding to ",
        sig_data$type89_sig_id
      )
    )
    save476(p5, paths$id476_sig)
    
    # --- [新增注入] ID476 缩略图生成 (使用 Signature) ---
    if (!is.null(paths$id476_thumb)) {
      p5_mini <- mSigPlot::plot_476(ID476_signatures[, sig_data$type89_sig_id], plot_title = "", block_text_cex = 0.5, base_size = 8, num_labels = 5, plot_complex = FALSE)
      p5_thumb <- p5_mini + ggplot2::theme_void() + ggplot2::theme(plot.title = ggplot2::element_blank(), axis.text = ggplot2::element_blank(), axis.title = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(), legend.position = "none", plot.margin = ggplot2::margin(0,0,0,0), strip.text = ggplot2::element_text(size = 5, margin = ggplot2::margin(1,0,1,0)))
      ggplot2::ggsave(filename = paths$id476_thumb, plot = p5_thumb, width = 12, height = 2, dpi = 300, bg="white")
    }
    # --------------------------------------------------
    
    # 476-type spectrum of BestMatch89_1 (linking tumor)
    p6 <- p476(
      ID476_catalogs[, sig_data$exemplar_89],
      plot_title = ""
    )
    save476(p6, paths$id476_catalog)
    
    # 476-type spectrum of BestMatch476_1 (type-specific best match)
    if (
      sig_data$exemplar_476 != sig_data$exemplar_89 &&
      sig_data$exemplar_476 %in% colnames(ID476_catalogs)
    ) {
      p6b <- p476(
        ID476_catalogs[, sig_data$exemplar_476],
        plot_title = ""
      )
      save476(p6b, paths$id476_catalog_476match)
    } else {
      paths$id476_catalog_476match <- NULL
    }
  } else {
    p5 <- p476(
      ID476_catalogs[, sig_data$exemplar_89],
      plot_title = paste0(
        "476-type spectrum of the linking-tumor ",
        sig_data$exemplar_89
      )
    )
    save476(p5, paths$id476_sig)
    
    # --- [新增注入] ID476 缩略图生成 (使用 Spectrum) ---
    if (!is.null(paths$id476_thumb)) {
      p5_mini <- mSigPlot::plot_476(ID476_catalogs[, sig_data$exemplar_89], plot_title = "", block_text_cex = 0.5, base_size = 8, num_labels = 5, plot_complex = FALSE)
      p5_thumb <- p5_mini + ggplot2::theme_void() + ggplot2::theme(plot.title = ggplot2::element_blank(), axis.text = ggplot2::element_blank(), axis.title = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(), legend.position = "none", plot.margin = ggplot2::margin(0,0,0,0), strip.text = ggplot2::element_text(size = 5, margin = ggplot2::margin(1,0,1,0)))
      ggplot2::ggsave(filename = paths$id476_thumb, plot = p5_thumb, width = 12, height = 2, dpi = 300, bg="white")
    }
    # --------------------------------------------------
    
    paths$id476_catalog <- NULL
    paths$id476_catalog_476match <- NULL
  }
  
  # ID83 signature (only if exists)
  
  p83 <- function(catalog, plot_title = NULL, min_ts = min_ts_to_trigger) {
    plot_83_w_wout_t(
      catalog,
      plot_title = plot_title,
      base_size = getp('basesize83'),
      min_ts_to_trigger = min_ts
    )
  }
  save83 <- function(myplot, path) {
    save_ggplot(myplot, path, width = getp('w83'), height = getp('h83'))
  }
  
  # Helper to save p83 result and return ablation info
  save83_result <- function(result, path_main, path_ablated = NULL) {
    # Check if ablation occurred (ablated_catalog present means 2 plots)
    if (!is.null(result$ablated_catalog)) {
      # Two plots case - save both
      save83(result$plots[[1]], path_main)
      if (!is.null(path_ablated)) {
        save83(result$plots[[2]], path_ablated)
      }
      return(list(ablated = TRUE, ablated_catalog = result$ablated_catalog))
    } else {
      # Single plot case
      save83(result$plots, path_main)
      return(list(ablated = FALSE, ablated_catalog = NULL))
    }
  }
  
  if (sig_data$has_83_signature) {
    result <- p83(ID83_signatures[, sig_data$ID83signature, drop = FALSE])
    save_result <- save83_result(result, paths$id83_sig, paths$id83_sig_ablated)
    if (!save_result$ablated) {
      paths$id83_sig_ablated <- NULL
    }
    paths$id83_sig_ablated_catalog <- save_result$ablated_catalog
    
    # --- [新增注入] ID83 缩略图生成 (Magick 切边版) ---
    if (!is.null(paths$id83_thumb)) {
      tryCatch({
        src_img_path <- if(!is.null(save_result$ablated_catalog)) paths$id83_sig else paths$id83_sig
        if (file.exists(src_img_path)) {
          img <- magick::image_read(src_img_path)
          img <- magick::image_trim(img)
          info <- magick::image_info(img)
          w <- info$width; h <- info$height
          shave_bottom <- h * 0.20; shave_left <- w * 0.06
          img <- magick::image_crop(img, geometry = paste0(w, "x", (h - shave_bottom), "+0+0"), gravity = "North")
          img <- magick::image_crop(img, geometry = paste0((w - shave_left), "x", (h - shave_bottom), "+0+0"), gravity = "East")
          img_thumb <- magick::image_resize(img, "1500x")
          magick::image_write(img_thumb, path = paths$id83_thumb)
        }
      }, error = function(e) { message(paste("Error generating thumbnail:", e$message)) })
    }
    # --------------------------------------------------
    
  } else {
    paths$id83_sig <- NULL
    paths$id83_sig_ablated <- NULL
    paths$id83_sig_ablated_catalog <- NULL
  }
  
  # ID83 mapped signature (from 476-type)
  if (sig_data$has_83_mapped_signature && !is.null(ID83_mapped_signatures)) {
    mapped_col_name <- paste0(sig_data$type89_sig_id, "_converted")
    cosine_text <- if (!is.na(sig_data$cosine83_mapped)) {
      paste0(
        " | cosine to native = ",
        format(sig_data$cosine83_mapped, digits = getp("cosine_digits"))
      )
    } else {
      ""
    }
    result <- p83(
      ID83_mapped_signatures[, mapped_col_name, drop = FALSE],
      plot_title = ""
    )
    save_result <- save83_result(
      result,
      paths$id83_mapped,
      paths$id83_mapped_ablated
    )
    if (!save_result$ablated) {
      paths$id83_mapped_ablated <- NULL
    }
    paths$id83_mapped_ablated_catalog <- save_result$ablated_catalog
  } else {
    paths$id83_mapped <- NULL
    paths$id83_mapped_ablated <- NULL
    paths$id83_mapped_ablated_catalog <- NULL
  }
  
  # ID83 spectrum catalog: exemplar_89 (linking tumor)
  if (sig_data$is_polyT_removed) {
    cat83touse = ID83_catalogs_no_polyT
  } else {
    cat83touse = ID83_catalogs
  }
  result <- p83(cat83touse[, sig_data$exemplar_89, drop = FALSE])
  save_result <- save83_result(
    result,
    paths$id83_catalog,
    paths$id83_catalog_ablated
  )
  if (!save_result$ablated) {
    paths$id83_catalog_ablated <- NULL
  }
  paths$id83_catalog_ablated_catalog <- save_result$ablated_catalog
  
  # ID83 spectrum catalog: exemplar_83 (type-specific best match)
  if (
    sig_data$exemplar_83 != sig_data$exemplar_89 &&
    sig_data$exemplar_83 %in% colnames(cat83touse)
  ) {
    result <- p83(cat83touse[, sig_data$exemplar_83, drop = FALSE])
    save_result <- save83_result(
      result,
      paths$id83_catalog_83match,
      paths$id83_catalog_83match_ablated
    )
    if (!save_result$ablated) {
      paths$id83_catalog_83match_ablated <- NULL
    }
    paths$id83_catalog_83match_ablated_catalog <- save_result$ablated_catalog
  } else {
    paths$id83_catalog_83match <- NULL
    paths$id83_catalog_83match_ablated <- NULL
    paths$id83_catalog_83match_ablated_catalog <- NULL
  }
  
  # COSMIC matching signatures
  paths$cosmic_plots <- NULL
  if (!is.null(sig_data$cosmic_matches) && !is.null(cosmic_signatures)) {
    cosmic_plot_list <- list()
    for (i in seq_len(nrow(sig_data$cosmic_matches))) {
      cosmic_sig_name <- sig_data$cosmic_matches$cosmic_sig[i]
      cosmic_cosine <- sig_data$cosmic_matches$cosine[i]
      
      plot_path <- file.path(
        plot_dir,
        paste0(safe_name, "_cosmic_", cosmic_sig_name, ".png")
      )
      ablated_path <- file.path(
        plot_dir,
        paste0(safe_name, "_cosmic_", cosmic_sig_name, "_ablated.png")
      )
      
      result <- p83(
        cosmic_signatures[, cosmic_sig_name, drop = FALSE],
        plot_title = paste0(
          "COSMIC ",
          cosmic_sig_name,
          " | cosine to ",
          sig_data$ID83signature,
          ": ",
          format(cosmic_cosine, digits = getp("cosine_digits"))
        )
      )
      save_result <- save83_result(result, plot_path, ablated_path)
      
      cosmic_plot_list[[cosmic_sig_name]] <- list(
        path = plot_path,
        path_ablated = if (save_result$ablated) ablated_path else NULL,
        cosine = cosmic_cosine,
        ablated_catalog = save_result$ablated_catalog
      )
    }
    paths$cosmic_plots <- cosmic_plot_list
  }
  
  # Jin matching signatures
  paths$jin_plots <- NULL
  if (!is.null(sig_data$jin_matches) && !is.null(jin_signatures)) {
    jin_plot_list <- list()
    for (i in seq_len(nrow(sig_data$jin_matches))) {
      jin_sig_name <- sig_data$jin_matches$jin_sig[i]
      jin_cosine <- sig_data$jin_matches$cosine[i]
      
      plot_path <- file.path(
        plot_dir,
        paste0(safe_name, "_jin_", jin_sig_name, ".png")
      )
      ablated_path <- file.path(
        plot_dir,
        paste0(safe_name, "_jin_", jin_sig_name, "_ablated.png")
      )
      
      result <- p83(
        jin_signatures[, jin_sig_name, drop = FALSE],
        plot_title = paste0(
          "Jin ",
          jin_sig_name,
          " | cosine to ",
          sig_data$ID83signature,
          ": ",
          format(jin_cosine, digits = getp("cosine_digits"))
        )
      )
      save_result <- save83_result(result, plot_path, ablated_path)
      
      jin_plot_list[[jin_sig_name]] <- list(
        path = plot_path,
        path_ablated = if (save_result$ablated) ablated_path else NULL,
        cosine = jin_cosine,
        ablated_catalog = save_result$ablated_catalog
      )
    }
    paths$jin_plots <- jin_plot_list
  }
  
  # Koh matching signatures (89-type)
  paths$koh_plots <- NULL
  if (!is.null(sig_data$koh_matches) && !is.null(koh_signatures)) {
    koh_plot_list <- list()
    for (i in seq_len(nrow(sig_data$koh_matches))) {
      koh_sig_name <- sig_data$koh_matches$koh_sig[i]
      koh_cosine <- sig_data$koh_matches$cosine[i]
      
      plot_path <- file.path(
        plot_dir,
        paste0(safe_name, "_koh_", koh_sig_name, ".png")
      )
      
      ptmp <- p89(
        koh_signatures[, koh_sig_name, drop = FALSE],
        plot_title = paste0(
          "Similar signature from Koh et al., 2025 ",
          koh_sig_name,
          " | cosine to ",
          sig_data$type89_sig_id,
          ": ",
          format(koh_cosine, digits = getp("cosine_digits"))
        )
      )
      save89(ptmp, plot_path)
      
      koh_plot_list[[koh_sig_name]] <- list(
        path = plot_path,
        cosine = koh_cosine
      )
    }
    paths$koh_plots <- koh_plot_list
  }
  
  # Sankey plots for 476-to-83 collapse (if flow data available)
  paths$sankey_other_ins <- NULL
  paths$sankey_other_del <- NULL
  if (
    !is.null(collapse_flows) &&
    sig_data$type89_sig_id %in% names(collapse_flows)
  ) {
    flows <- collapse_flows[[sig_data$type89_sig_id]]
    
    # Create a result-like object for plot_collapse_sankey
    collapse_result <- list(flows = flows)
    
    title_prefix <- sprintf(
      "%s -> %s",
      sig_data$type89_sig_id,
      sig_data$ID83signature
    )
    sankey_plots <- plot_collapse_sankey(
      collapse_result,
      min_flow = 0.001,
      title_prefix = title_prefix
    )
    
    # Save "other insertions" Sankey plot
    if (!is.null(sankey_plots$other_ins)) {
      sankey_ins_path <- file.path(
        plot_dir,
        paste0(safe_name, "_sankey_other_ins.png")
      )
      ggplot2::ggsave(
        sankey_ins_path,
        sankey_plots$other_ins,
        width = 12,
        height = 8,
        dpi = 150,
        bg = "white"
      )
      paths$sankey_other_ins <- sankey_ins_path
    }
    
    # Save "other deletions" Sankey plot
    if (!is.null(sankey_plots$other_del)) {
      sankey_del_path <- file.path(
        plot_dir,
        paste0(safe_name, "_sankey_other_del.png")
      )
      ggplot2::ggsave(
        sankey_del_path,
        sankey_plots$other_del,
        width = 12,
        height = 8,
        dpi = 150,
        bg = "white"
      )
      paths$sankey_other_del <- sankey_del_path
    }
  }
  
  return(paths)
}


#' Generate all plots in parallel
#'
#' @param all_sig_data List of signature data from compute_sig_data
#' @param ... Additional arguments passed to generate_plots_to_files
#' @param n_workers Number of parallel workers
#' @return List of plot paths for each signature
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
    ID89_mapped_signatures = NULL,
    ID83_mapped_signatures = NULL,
    cosmic_signatures = NULL,
    jin_signatures = NULL,
    koh_signatures = NULL,
    min_ts_to_trigger = 0.15,
    collapse_flows = NULL,
    n_workers = 10
) {
  # Create plot directory
  dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Try parallel, fall back to sequential if it fails
  # (e.g., R/Rscript version mismatch with parallelly)
  use_parallel <- TRUE
  if (n_workers > 1) {
    tryCatch(
      {
        future::plan(future::multisession, workers = n_workers)
      },
      error = function(e) {
        warning(
          "Parallel setup failed, falling back to sequential: ",
          conditionMessage(e)
        )
        use_parallel <<- FALSE
      }
    )
  } else {
    use_parallel <- FALSE
  }
  
  map_fn <- if (use_parallel) furrr::future_map else lapply
  map_args <- list()
  
  plot_one <- function(sig_data) {
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
      ID89_mapped_signatures = ID89_mapped_signatures,
      ID83_mapped_signatures = ID83_mapped_signatures,
      cosmic_signatures = cosmic_signatures,
      jin_signatures = jin_signatures,
      koh_signatures = koh_signatures,
      min_ts_to_trigger = min_ts_to_trigger,
      collapse_flows = collapse_flows
    )
  }
  
  if (use_parallel) {
    all_paths <- furrr::future_map(
      all_sig_data,
      plot_one,
      .options = furrr::furrr_options(
        seed = TRUE,
        packages = c(
          "ggplot2",
          "ICAMS",
          "mSigPlot",
          "indelsig.tools.lib",
          "scales",
          "ggalluvial",
          "magick" # [新增注入] magick 依赖
        )
      ),
      .progress = TRUE
    )
    future::plan(future::sequential)
  } else {
    all_paths <- lapply(all_sig_data, plot_one)
  }
  
  names(all_paths) <- names(all_sig_data)
  return(all_paths)
}


#' Check if plot cache is valid
#'
#' Compares hash of source data files against stored hash.
#' Returns TRUE if cache is valid (no regeneration needed).
#'
#' @param data_dir Directory containing source data files
#' @param plot_dir Directory where plots are stored
#' @param cache_file Name of the cache hash file
#' @return Logical: TRUE if cache is valid, FALSE if regeneration needed
check_plot_cache <- function(
    data_dir,
    plot_dir,
    cache_file = "plot_cache_hash.rds"
) {
  cache_path <- file.path(plot_dir, cache_file)
  
  # Source files in data_dir that plots depend on
  data_files <- file.path(
    data_dir,
    c(
      "COSMIC_v3.5_ID_GRCh37_signatures.tsv",
      "jin_2024_sup_tab_1_signatures.tsv",
      "Koh_signatures.tsv"
    )
  )
  
  # finalized_files is a global named vector defined at top of file
  
  # Files in vignette directory that affect plotting
  vignette_files <- c(
    "ppar.R",
    "89_mapped_from_476.tsv",
    "83_mapped_from_476.tsv"
  )
  
  # Check if plot directory exists
  if (!dir.exists(plot_dir)) {
    return(FALSE)
  }
  
  # Compute current hash from file modification times
  all_data_paths <- c(data_files, finalized_files)
  if (!all(file.exists(all_data_paths))) {
    return(FALSE)
  }
  
  # Check vignette files (optional - skip missing files)
  vignette_paths <- vignette_files[file.exists(vignette_files)]
  
  all_paths <- c(all_data_paths, vignette_paths)
  current_hash <- digest::digest(
    sapply(all_paths, file.mtime)
  )
  
  # Check if cache exists and matches
  if (file.exists(cache_path)) {
    stored_hash <- readRDS(cache_path)
    if (identical(stored_hash, current_hash)) {
      return(TRUE) # Cache is valid
    }
  }
  
  return(FALSE) # Cache invalid or missing
}


#' Save cache hash after generating plots
#'
#' @param data_dir Directory containing source data files
#' @param plot_dir Directory where plots are stored
#' @param cache_file Name of the cache hash file
save_plot_cache <- function(
    data_dir,
    plot_dir,
    cache_file = "plot_cache_hash.rds"
) {
  # Source files in data_dir
  data_files <- file.path(
    data_dir,
    c(
      "COSMIC_v3.5_ID_GRCh37_signatures.tsv",
      "jin_2024_sup_tab_1_signatures.tsv",
      "Koh_signatures.tsv"
    )
  )
  
  # finalized_files is a global named vector defined at top of file
  
  # Files in vignette directory that affect plotting
  vignette_files <- c(
    "ppar.R",
    "89_mapped_from_476.tsv",
    "83_mapped_from_476.tsv"
  )
  
  all_data_paths <- c(data_files, finalized_files)
  vignette_paths <- vignette_files[file.exists(vignette_files)]
  
  all_paths <- c(all_data_paths, vignette_paths)
  current_hash <- digest::digest(
    sapply(all_paths, file.mtime)
  )
  
  saveRDS(current_hash, file.path(plot_dir, cache_file))
}


#' Reconstruct plot paths from existing files (融入了 Thumbnail 路径识别)
reconstruct_plot_paths <- function(signature_names, plot_dir) {
  all_paths <- lapply(signature_names, function(sig_name) {
    safe_name <- gsub("[^a-zA-Z0-9_]", "_", sig_name)
    
    paths <- list(
      id89_sig = file.path(plot_dir, paste0(safe_name, "_id89_sig.png")),
      id89_thumb = file.path(plot_dir, paste0(safe_name, "_Thumbnail.png")), # [新增]
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
      id476_thumb = file.path(plot_dir, paste0(safe_name, "_id476_Thumbnail.png")), # [新增]
      id476_catalog = file.path(
        plot_dir,
        paste0(safe_name, "_id476_catalog.png")
      ),
      id476_catalog_476match = file.path(
        plot_dir,
        paste0(safe_name, "_id476_catalog_476match.png")
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
      id83_catalog_83match = file.path(
        plot_dir,
        paste0(safe_name, "_id83_catalog_83match.png")
      ),
      id83_catalog_83match_ablated = file.path(
        plot_dir,
        paste0(safe_name, "_id83_catalog_83match_ablated.png")
      ),
      id83_sig_ablated_catalog = NULL,
      id83_mapped_ablated_catalog = NULL,
      id83_catalog_ablated_catalog = NULL,
      id83_catalog_83match_ablated_catalog = NULL
    )
    
    # Set to NULL if file doesn't exist (skip non-path entries)
    path_names <- names(paths)
    paths <- lapply(path_names, function(nm) {
      p <- paths[[nm]]
      # Skip entries that are data frames, not file paths
      if (grepl("_ablated_catalog$", nm)) {
        return(NULL) # In-memory only, always NULL from cache
      }
      if (is.null(p) || !file.exists(p)) NULL else p
    })
    names(paths) <- path_names
    
    # Find any COSMIC plots for this signature
    cosmic_pattern <- paste0(safe_name, "_cosmic_*.png")
    cosmic_files <- list.files(
      plot_dir,
      pattern = glob2rx(cosmic_pattern),
      full.names = TRUE
    )
    if (length(cosmic_files) > 0) {
      # Extract COSMIC signature names from filenames
      # Filter out ablated versions first, then pair them
      main_files <- cosmic_files[!grepl("_ablated\\.png$", cosmic_files)]
      cosmic_plot_list <- list()
      for (cf in main_files) {
        # Extract cosmic sig name from filename like "InsDel1_cosmic_ID5.png"
        basename_no_ext <- tools::file_path_sans_ext(basename(cf))
        cosmic_sig_name <- sub(
          paste0(safe_name, "_cosmic_"),
          "",
          basename_no_ext
        )
        # Check for corresponding ablated file
        ablated_file <- sub("\\.png$", "_ablated.png", cf)
        cosmic_plot_list[[cosmic_sig_name]] <- list(
          path = cf,
          path_ablated = if (file.exists(ablated_file)) ablated_file else NULL,
          cosine = NA,
          ablated_catalog = NULL # Not available from cache
        )
      }
      paths$cosmic_plots <- cosmic_plot_list
    } else {
      paths$cosmic_plots <- NULL
    }
    
    # Find any Jin plots for this signature
    jin_pattern <- paste0(safe_name, "_jin_*.png")
    jin_files <- list.files(
      plot_dir,
      pattern = glob2rx(jin_pattern),
      full.names = TRUE
    )
    if (length(jin_files) > 0) {
      # Filter out ablated versions first, then pair them
      main_files <- jin_files[!grepl("_ablated\\.png$", jin_files)]
      jin_plot_list <- list()
      for (jf in main_files) {
        basename_no_ext <- tools::file_path_sans_ext(basename(jf))
        jin_sig_name <- sub(paste0(safe_name, "_jin_"), "", basename_no_ext)
        # Check for corresponding ablated file
        ablated_file <- sub("\\.png$", "_ablated.png", jf)
        jin_plot_list[[jin_sig_name]] <- list(
          path = jf,
          path_ablated = if (file.exists(ablated_file)) ablated_file else NULL,
          cosine = NA,
          ablated_catalog = NULL # Not available from cache
        )
      }
      paths$jin_plots <- jin_plot_list
    } else {
      paths$jin_plots <- NULL
    }
    
    # Find any Koh plots for this signature
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
    
    # Sankey plots for 476-to-83 collapse
    sankey_ins_path <- file.path(
      plot_dir,
      paste0(safe_name, "_sankey_other_ins.png")
    )
    sankey_del_path <- file.path(
      plot_dir,
      paste0(safe_name, "_sankey_other_del.png")
    )
    paths$sankey_other_ins <- if (file.exists(sankey_ins_path)) {
      sankey_ins_path
    } else {
      NULL
    }
    paths$sankey_other_del <- if (file.exists(sankey_del_path)) {
      sankey_del_path
    } else {
      NULL
    }
    
    return(paths)
  })
  
  names(all_paths) <- signature_names
  return(all_paths)
}

#' Create overview kable table with hyperlinks to signature sections
#'
#' Generates a kable table for the Overview section with:
#' - Signature IDs hyperlinked to their corresponding sections
#' - User-friendly column names
#' - Centered text, smaller font
#' - Scrollable with fixed header
#'
#' @param sig_table Data frame: the signature summary table (sig_table2)
#' @return HTML styled table (using native HTML/CSS)
create_overview_table <- function(sig_table) {
  source(here::here("data for update shiny", "vignette", "table_1_col_name_mapping.R"))
  
  df <- sig_table
  
  # Remove is_polyT_removed column (not needed in display)
  df$is_polyT_removed <- NULL
  
  # Remove the last column
  df <- df[, -ncol(df)]
  
  # Clean up best_match_jin: remove 'jin' prefix
  df$best_match_jin <- sub("^jin", "", df$best_match_jin)
  
  # Round cos_v_koh and cosine_v_jin first (before any HTML wrapping)
  df$cos_v_koh <- round(df$cos_v_koh, 4)
  df$cosine_v_jin <- round(df$cosine_v_jin, 4)
  
  # Handle best_match_koh duplicates: mark best match with asterisk, gray out non-best
  koh_values <- df$best_match_koh
  non_na_idx <- which(!is.na(koh_values))
  rows_to_gray <- c()
  
  unique_koh <- unique(koh_values[non_na_idx])
  for (koh_val in unique_koh) {
    matching_rows <- which(koh_values == koh_val)
    if (length(matching_rows) > 1) {
      # Duplicate: find row with highest cos_v_koh
      cos_vals <- df$cos_v_koh[matching_rows]
      best_idx <- matching_rows[which.max(cos_vals)]
      non_best_idx <- setdiff(matching_rows, best_idx)
      # Add asterisk to best match
      df$best_match_koh[best_idx] <- paste0(df$best_match_koh[best_idx], "*")
      # Track non-best rows for graying
      rows_to_gray <- c(rows_to_gray, non_best_idx)
    }
  }
  
  # Gray out non-best duplicate rows for best_match_koh and cos_v_koh
  if (length(rows_to_gray) > 0) {
    df$best_match_koh[rows_to_gray] <- paste0(
      '<span style="color: #B0B0B0;">',
      df$best_match_koh[rows_to_gray],
      '</span>'
    )
    df$cos_v_koh[rows_to_gray] <- paste0(
      '<span style="color: #B0B0B0;">',
      df$cos_v_koh[rows_to_gray],
      '</span>'
    )
  }
  
  # Handle best_match_jin duplicates: mark best match with asterisk, gray out non-best
  jin_values <- df$best_match_jin
  jin_non_na_idx <- which(!is.na(jin_values))
  jin_rows_to_gray <- c()
  
  unique_jin <- unique(jin_values[jin_non_na_idx])
  for (jin_val in unique_jin) {
    matching_rows <- which(jin_values == jin_val)
    if (length(matching_rows) > 1) {
      cos_vals <- df$cosine_v_jin[matching_rows]
      best_idx <- matching_rows[which.max(cos_vals)]
      non_best_idx <- setdiff(matching_rows, best_idx)
      df$best_match_jin[best_idx] <- paste0(df$best_match_jin[best_idx], "*")
      jin_rows_to_gray <- c(jin_rows_to_gray, non_best_idx)
    }
  }
  
  # Gray out non-best duplicate rows for best_match_jin and cosine_v_jin
  if (length(jin_rows_to_gray) > 0) {
    df$best_match_jin[jin_rows_to_gray] <- paste0(
      '<span style="color: #B0B0B0;">',
      df$best_match_jin[jin_rows_to_gray],
      '</span>'
    )
    df$cosine_v_jin[jin_rows_to_gray] <- paste0(
      '<span style="color: #B0B0B0;">',
      df$cosine_v_jin[jin_rows_to_gray],
      '</span>'
    )
  }
  
  # Add hyperlinks to signature IDs
  df$signature_id <- sapply(df$signature_id, make_sig_hyperlink)
  
  # Rename columns using the mapping
  names(df) <- table_1_col_name_mapping(names(df))
  
  # Round numeric columns to 4 decimal places
  numeric_cols <- sapply(df, is.numeric)
  df[numeric_cols] <- lapply(df[numeric_cols], function(x) round(x, 4))
  
  # 彻底摆脱 kableExtra，使用原生 HTML 和 CSS
  tbl <- knitr::kable(df, format = "html", escape = FALSE, align = 'c', 
                      table.attr = 'class="table table-striped table-hover table-condensed" style="width: 100%;"')
  
  htmltools::div(
    style = "font-size: 70%; height: 600px; overflow-y: auto; width: 100%; border: 1px solid #ccc;", 
    htmltools::HTML(tbl)
  )
}