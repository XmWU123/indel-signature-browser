# Helper functions for interactive dendrogram v2
# Loads ALL signature files from Koh89/Koh476 subdirectories

library(ggdendro)
library(lsa)
library(plotly)

source(here::here("code", "collapse_476_to_89.R"))
source(here::here("code", "find_best_match_spectra.R"))

# Colors for source groups
dendro2_colors <- c(
  Liu = "#FF0000",
  C.H = "#006400",
  C.P = "#8B008B",
  C.PH = "#FF8C00",
  N.H = "#4169E1",
  Liu.m = "#FF6666",
  C.H.m = "#66B266",
  C.P.m = "#CC66CC",
  C.PH.m = "#FFB366",
  N.H.m = "#809FFF",
  Koh = "#8B4513",
  COSMIC = "#FF1493",
  Sp.C = "#20B2AA",
  Sp.N = "#DC143C",
  CV476 = "#00CED1",
  `~Sp.C` = "gray50",
  `~Sp.N` = "gray50"
)

#' Parse new-format signature file and rename columns
#'
#' Filename scheme: {CAP9,NoCAP}.mSigHdp.{Hartwig,PCAWG,PH}.{Koh89,Koh476}.{cancertype}.txt
#' Returns data.frame with `source_group` attribute
rename_sigs2 <- function(file_path) {
  sigs <- read.table(file_path, header = TRUE, sep = "\t")
  fname <- tools::file_path_sans_ext(basename(file_path))

  # Split on ".mSigHdp." to get cap prefix and remainder
  parts <- strsplit(fname, "\\.mSigHdp\\.")[[1]]
  cap_raw <- parts[1] # "CAP9" or "NoCAP"
  remainder <- parts[2] # e.g. "PH.Koh89.Bone.SoftTissue"

  cap <- if (cap_raw == "CAP9") "C" else "N"

  # Find dataset token (Koh89, Koh476, or COSMIC83) to split from cancertype
  ds_match <- regexpr("(Koh(89|476)|COSMIC83)", remainder)
  ds_str <- regmatches(remainder, ds_match) # "Koh89", "Koh476", or "COSMIC83"
  ds_pos <- ds_match[1]

  # Dataset is everything before the token (minus trailing dot)
  dataset_raw <- substr(remainder, 1, ds_pos - 2) # e.g. "PH", "Hartwig", "PCAWG"
  dataset <- switch(dataset_raw, Hartwig = "H", PCAWG = "P", PH = "PH")

  # Cancertype is everything after the dataset token and a dot
  cancertype <- sub(paste0(".*", ds_str, "\\."), "", remainder)

  # Build column names
  clean_names <- gsub("hdp\\.", "", colnames(sigs))
  colnames(sigs) <- paste0(cap, ".", dataset, ".", cancertype, ".", clean_names)

  source_group <- paste0(cap, ".", dataset)
  attr(sigs, "source_group") <- source_group
  sigs
}

#' Strip CancerType:: prefix from sample names
#'
#' Koh89 catalogs/spectra use "CancerType::SampleName" format while
#' Koh476 uses plain "SampleName". This extracts the bare sample ID.
bare_sample_name <- function(x) sub("^.*::", "", x)

#' Load cross-type spectra from the other classification's selected spectra
#'
#' Reads sample names from the other type's selected spectra file, finds
#' samples not already present, and looks them up in current-type catalogs.
#' Handles CancerType::SampleName vs plain name mismatches.
#'
#' @param cross_spectra_file Path to the other type's selected spectra file
#' @param existing_bare_names Bare sample names already present in combined
#' @param catalog_paths Paths to current-type catalog files to look up samples
#' @return Data frame of spectra for new samples, or NULL
load_cross_type_spectra <- function(
  cross_spectra_file,
  existing_bare_names,
  catalog_paths
) {
  if (!file.exists(cross_spectra_file)) {
    return(NULL)
  }
  cross_sp <- read.table(
    cross_spectra_file,
    header = TRUE,
    sep = "\t",
    row.names = 1,
    check.names = FALSE
  )
  # Compare bare names to find truly new samples
  cross_bare <- bare_sample_name(colnames(cross_sp))
  new_bare <- setdiff(cross_bare, existing_bare_names)
  if (length(new_bare) == 0) {
    return(NULL)
  }

  # Look up new samples in current-type catalogs (by bare name matching)
  result <- NULL
  remaining <- new_bare
  for (cat_path in catalog_paths) {
    if (length(remaining) == 0) {
      break
    }
    if (!file.exists(cat_path)) {
      next
    }
    cat <- read.table(
      cat_path,
      header = TRUE,
      sep = "\t",
      row.names = 1,
      check.names = FALSE
    )
    cat_bare <- bare_sample_name(colnames(cat))
    # Match remaining bare names to catalog columns
    matched_idx <- which(cat_bare %in% remaining)
    if (length(matched_idx) > 0) {
      found_cols <- colnames(cat)[matched_idx]
      found_bare <- cat_bare[matched_idx]
      chunk <- cat[, found_cols, drop = FALSE]
      # Rename to bare names for consistent output
      colnames(chunk) <- found_bare
      result <- if (is.null(result)) chunk else cbind(result, chunk)
      remaining <- setdiff(remaining, found_bare)
    }
  }
  result
}

#' Load all signatures for a dataset type + Liu reference
#'
#' @param dataset_type "Koh89" or "Koh476"
#' @param find_similar If TRUE, search for spectra similar to signatures, and store these
#'    in `file.path(data_dir, "Mo_CAP9_analysis", "selected_spectra")`
#' @param sig_dir Base directory containing Koh89/ and Koh476/ subdirs
#' @param data_dir Directory containing Liu reference signatures
#' @return List with: combined (features-as-rows data.frame), source_vec (named character vector)
load_all_signatures <- function(
  dataset_type,
  find_similar = FALSE,
  sig_dir = here::here("Manuscript_data", "Mo_CAP9_analysis", "Signatures"),
  catalog_dir = here::here("Manuscript_data", "Mo_CAP9_analysis", "Catalogs"),
  data_dir = here::here("Manuscript_data")
) {
  sig_files <- Sys.glob(file.path(sig_dir, dataset_type, "*.txt"))

  all_sigs <- lapply(sig_files, rename_sigs2)

  # Filter to matching row counts (expected number of mutation types)
  nrows <- sapply(all_sigs, nrow)
  expected <- max(nrows)
  all_sigs <- all_sigs[nrows == expected]

  # ICAMS canonical row order for this dataset type
  icams_order <- if (dataset_type == "Koh476") {
    ICAMS::catalog.row.order$ID476
  } else if (dataset_type == "COSMIC83") {
    ICAMS::catalog.row.order$ID
  } else {
    ICAMS::catalog.row.order$ID89
  }

  # Get mutation type row names from a catalog file
  cat_files <- Sys.glob(file.path(
    catalog_dir,
    paste0("*", dataset_type, "*.txt")
  ))
  cat_rownames <- rownames(read.table(
    cat_files[1],
    header = TRUE,
    sep = "\t",
    row.names = 1,
    check.names = FALSE
  ))

  # Validate catalog row order against ICAMS
  stopifnot(
    "Catalog rownames do not match ICAMS::catalog.row.order" = identical(
      cat_rownames,
      icams_order
    )
  )

  # Validate/assign row names to extraction sigs
  for (i in seq_along(all_sigs)) {
    if (dataset_type == "Koh476") {
      stopifnot(
        "Koh476 signature rownames do not match ICAMS::catalog.row.order$ID476" = identical(
          rownames(all_sigs[[i]]),
          icams_order
        )
      )
    } else {
      # Koh89 signature files lack rownames; assign from validated catalog
      rownames(all_sigs[[i]]) <- cat_rownames
    }
  }

  # Build source vector
  source_vec <- character(0)
  for (s in all_sigs) {
    grp <- attr(s, "source_group")
    source_vec <- c(source_vec, setNames(rep(grp, ncol(s)), colnames(s)))
  }

  # Combine extraction sigs
  cap9_combined <- do.call(cbind, all_sigs)

  # Read Liu reference
  liu_suffix <- if (dataset_type == "Koh89") "89" else if (dataset_type == "Koh476") "476" else "83"
  liu_file <- file.path(
    data_dir,
    paste0("Liu_et_al_final_", liu_suffix, "_type_signatures.tsv")
  )
  liu_sigs <- read.table(
    liu_file,
    header = TRUE,
    sep = "\t",
    row.names = 1,
    check.names = FALSE
  )

  # Validate Liu reference row order against ICAMS
  stopifnot(
    "Liu signature rownames do not match ICAMS::catalog.row.order" = identical(
      rownames(liu_sigs),
      icams_order
    )
  )

  source_vec <- c(
    source_vec,
    setNames(rep("Liu", ncol(liu_sigs)), colnames(liu_sigs))
  )

  combined <- cbind(cap9_combined, liu_sigs)

  # For Koh89: also load Koh et al. reference signatures
  if (dataset_type == "Koh89") {
    koh_file <- file.path(data_dir, "Koh_signatures.tsv")
    koh_sigs <- read.table(
      koh_file,
      header = TRUE,
      sep = "\t",
      row.names = 1,
      check.names = FALSE
    )
    stopifnot(
      "Koh signature rownames do not match ICAMS::catalog.row.order$ID89" = identical(
        rownames(koh_sigs),
        icams_order
      )
    )
    source_vec <- c(
      source_vec,
      setNames(rep("Koh", ncol(koh_sigs)), colnames(koh_sigs))
    )
    combined <- cbind(combined, koh_sigs)
  }

  # For COSMIC83: load COSMIC v3.5 reference signatures
  if (dataset_type == "COSMIC83") {
    cosmic_file <- file.path(data_dir, "COSMIC_v3.5_ID_GRCh37_signatures.tsv")
    cosmic_sigs <- read.table(
      cosmic_file,
      header = TRUE,
      sep = "\t",
      row.names = 1,
      check.names = FALSE
    )
    stopifnot(
      "COSMIC signature rownames do not match ICAMS::catalog.row.order$ID" = identical(
        rownames(cosmic_sigs),
        icams_order
      )
    )
    source_vec <- c(
      source_vec,
      setNames(rep("COSMIC", ncol(cosmic_sigs)), colnames(cosmic_sigs))
    )
    combined <- cbind(combined, cosmic_sigs)
  }

  # For Koh89: also load Koh476 signatures, map to 89-type, and include with '.m' suffix
  if (dataset_type == "Koh89") {
    # Load all Koh476 extraction signatures
    sig_files_476 <- Sys.glob(file.path(sig_dir, "Koh476", "*.txt"))
    all_sigs_476 <- lapply(sig_files_476, rename_sigs2)
    nrows_476 <- sapply(all_sigs_476, nrow)
    expected_476 <- max(nrows_476)
    all_sigs_476 <- all_sigs_476[nrows_476 == expected_476]
    cap9_476 <- do.call(cbind, all_sigs_476)

    # Load Liu 476-type signatures
    liu_file_476 <- file.path(
      data_dir,
      "Liu_et_al_final_476_type_signatures.tsv"
    )
    liu_476 <- read.table(
      liu_file_476,
      header = TRUE,
      sep = "\t",
      row.names = 1,
      check.names = FALSE
    )

    all_476 <- cbind(cap9_476, liu_476)

    # Map 476 -> 89
    mapped_89 <- t476_to_89(all_476)

    # Rename columns: append '.m' and strip '_converted' suffix from t476_to_89
    colnames(mapped_89) <- sub("_converted$", ".m", colnames(mapped_89))

    # Build source vector for mapped sigs (with .m source groups)
    mapped_source <- character(0)
    for (s in all_sigs_476) {
      grp <- paste0(attr(s, "source_group"), ".m")
      mapped_source <- c(
        mapped_source,
        setNames(rep(grp, ncol(s)), paste0(colnames(s), ".m"))
      )
    }
    mapped_source <- c(
      mapped_source,
      setNames(rep("Liu.m", ncol(liu_476)), paste0(colnames(liu_476), ".m"))
    )

    source_vec <- c(source_vec, mapped_source)
    combined <- cbind(combined, mapped_89)
  }

  # Find best-matching spectra for each signature and write to disk
  if (find_similar) {
    spectra_dir <- file.path(data_dir, "Mo_CAP9_analysis", "selected_spectra")
    dir.create(spectra_dir, showWarnings = FALSE, recursive = TRUE)

    # CAP9-searchable: all groups except N.H (and its mapped variant N.H.m)
    cap9_groups <- c(
      "C.H",
      "C.P",
      "C.PH",
      "Liu",
      if (dataset_type == "Koh89") c("Koh", "C.H.m", "C.P.m", "C.PH.m", "Liu.m"),
      if (dataset_type == "COSMIC83") "COSMIC"
    )
    cap9_cols <- names(source_vec)[source_vec %in% cap9_groups]
    cap9_sigs <- combined[, cap9_cols, drop = FALSE]

    cat_suffix <- paste0(dataset_type, ".catalog.txt")
    cap9_pcawg <- find_best_match_spectra(
      cap9_sigs,
      file.path(catalog_dir, paste0("CAP9.PCAWG.", cat_suffix))
    )
    cap9_hartwig <- find_best_match_spectra(
      cap9_sigs,
      file.path(catalog_dir, paste0("CAP9.Hartwig.", cat_suffix))
    )
    cap9_spectra <- cbind(cap9_pcawg, cap9_hartwig)
    cap9_spectra <- cap9_spectra[,
      !duplicated(colnames(cap9_spectra)),
      drop = FALSE
    ]

    # nonclip-searchable: N.H (and N.H.m if Koh89); COSMIC83 has no NoCAP sigs
    nonclip_groups <- c("N.H", if (dataset_type == "Koh89") "N.H.m")
    nonclip_cols <- names(source_vec)[source_vec %in% nonclip_groups]

    write.table(
      cap9_spectra,
      file.path(
        spectra_dir,
        paste0("CAP9.selected_spectra.", dataset_type, ".tsv")
      ),
      sep = "\t",
      quote = FALSE,
      col.names = NA
    )

    if (length(nonclip_cols) > 0) {
      nonclip_sigs <- combined[, nonclip_cols, drop = FALSE]
      nonclip_spectra <- find_best_match_spectra(
        nonclip_sigs,
        file.path(catalog_dir, paste0("nonclip.Hartwig.", cat_suffix))
      )
      write.table(
        nonclip_spectra,
        file.path(
          spectra_dir,
          paste0("nonclip.selected_spectra.", dataset_type, ".tsv")
        ),
        sep = "\t",
        quote = FALSE,
        col.names = NA
      )
    }

    message("Wrote selected spectra to ", spectra_dir)
  }

  # Load previously-saved selected spectra if they exist
  {
    spectra_dir <- file.path(data_dir, "Mo_CAP9_analysis", "selected_spectra")

    cap9_sp_file <- file.path(
      spectra_dir,
      paste0("CAP9.selected_spectra.", dataset_type, ".tsv")
    )
    if (file.exists(cap9_sp_file)) {
      cap9_sp <- read.table(
        cap9_sp_file,
        header = TRUE,
        sep = "\t",
        row.names = 1,
        check.names = FALSE
      )
      colnames(cap9_sp) <- paste0("Sp.C.", colnames(cap9_sp))
      source_vec <- c(
        source_vec,
        setNames(rep("Sp.C", ncol(cap9_sp)), colnames(cap9_sp))
      )
      combined <- cbind(combined, cap9_sp)
    }

    nonclip_sp_file <- file.path(
      spectra_dir,
      paste0("nonclip.selected_spectra.", dataset_type, ".tsv")
    )
    if (file.exists(nonclip_sp_file)) {
      nonclip_sp <- read.table(
        nonclip_sp_file,
        header = TRUE,
        sep = "\t",
        row.names = 1,
        check.names = FALSE
      )
      colnames(nonclip_sp) <- paste0("Sp.N.", colnames(nonclip_sp))
      source_vec <- c(
        source_vec,
        setNames(rep("Sp.N", ncol(nonclip_sp)), colnames(nonclip_sp))
      )
      combined <- cbind(combined, nonclip_sp)
    }
  }

  # Load cross-type spectra (from the other classification's selected spectra)
  # Only applies to Koh89/Koh476 (they share the same samples in different classifications)
  if (dataset_type %in% c("Koh89", "Koh476")) {
    other_type <- if (dataset_type == "Koh89") "Koh476" else "Koh89"

    # Collect existing bare sample names (strip Sp.C./Sp.N. prefix and CancerType::)
    existing_sp_names <- bare_sample_name(
      sub("^Sp\\.[CN]\\.", "", grep("^Sp\\.", names(source_vec), value = TRUE))
    )

    # CAP9 cross-type
    cap9_cross_file <- file.path(
      spectra_dir,
      paste0("CAP9.selected_spectra.", other_type, ".tsv")
    )
    cap9_cat_paths <- c(
      file.path(
        catalog_dir,
        paste0("CAP9.PCAWG.", dataset_type, ".catalog.txt")
      ),
      file.path(
        catalog_dir,
        paste0("CAP9.Hartwig.", dataset_type, ".catalog.txt")
      )
    )
    cap9_cross <- load_cross_type_spectra(
      cap9_cross_file,
      existing_sp_names,
      cap9_cat_paths
    )
    if (!is.null(cap9_cross)) {
      colnames(cap9_cross) <- paste0("~Sp.C.", colnames(cap9_cross))
      source_vec <- c(
        source_vec,
        setNames(rep("~Sp.C", ncol(cap9_cross)), colnames(cap9_cross))
      )
      combined <- cbind(combined, cap9_cross)
    }

    # nonclip cross-type
    nonclip_cross_file <- file.path(
      spectra_dir,
      paste0("nonclip.selected_spectra.", other_type, ".tsv")
    )
    nonclip_cat_paths <- file.path(
      catalog_dir,
      paste0("nonclip.Hartwig.", dataset_type, ".catalog.txt")
    )
    nonclip_cross <- load_cross_type_spectra(
      nonclip_cross_file,
      existing_sp_names,
      nonclip_cat_paths
    )
    if (!is.null(nonclip_cross)) {
      colnames(nonclip_cross) <- paste0("~Sp.N.", colnames(nonclip_cross))
      source_vec <- c(
        source_vec,
        setNames(rep("~Sp.N", ncol(nonclip_cross)), colnames(nonclip_cross))
      )
      combined <- cbind(combined, nonclip_cross)
    }
  } # end cross-type spectra (Koh89/Koh476 only)

  list(combined = combined, source_vec = source_vec)
}

#' Filter signatures by cosine similarity to a seed signature
#'
#' @param combined Features-as-rows data.frame
#' @param seed_name Column name of the seed signature
#' @param min_cos_sim Minimum cosine similarity threshold
#' @return Subset of combined keeping only columns >= threshold (always includes seed)
filter_by_seed <- function(combined, seed_name, min_cos_sim) {
  seed_vec <- combined[, seed_name]
  cos_sims <- sapply(colnames(combined), function(s) {
    lsa::cosine(seed_vec, combined[, s])[1, 1]
  })
  keep <- names(cos_sims)[cos_sims >= min_cos_sim]
  if (!(seed_name %in% keep)) {
    keep <- c(seed_name, keep)
  }
  combined[, keep, drop = FALSE]
}

#' Compute dendrogram from combined signature matrix
#'
#' @param combined Features-as-rows matrix
#' @return List with: hc, cosine_sim, dend_data
compute_dendrogram <- function(combined) {
  combined_t <- t(combined)
  combined_norm <- combined_t / sqrt(rowSums(combined_t^2))
  cosine_sim <- combined_norm %*% t(combined_norm)
  cosine_dist <- as.dist(1 - cosine_sim)

  hc <- hclust(cosine_dist, method = "average")
  dend_data <- dendro_data(hc)

  list(hc = hc, cosine_sim = cosine_sim, dend_data = dend_data)
}

#' Build an interactive plotly dendrogram
#'
#' @param dend_data Output from dendro_data()
#' @param source_vec Named vector mapping sig name -> source group
#' @param colors Named color vector
#' @param cut_height Height at which to draw the dashed cut line
#' @param medoids Character vector of medoid signature names to highlight
#'   with bold text in a box, prepended with cluster ID
#' @param initial_xrange Numeric vector of length 2 for initial x-axis zoom
#' @param initial_yrange Numeric vector of length 2 for initial y-axis range
#' @param clusters Named vector of cluster assignments (names = sig names,
#'   values = cluster IDs). Used to prepend cluster ID to medoid labels.
#' @param strip_prefix Character prefix to remove from all labels
#' @param label_filter Character vector of leaf names to annotate with text
#'   labels. NULL (default) shows all labels. Markers and hover remain for
#'   all leaves regardless.
#' @param cluster_dropdown If TRUE and medoids/clusters are provided, add a
#'   dropdown menu to jump to each cluster's position in the dendrogram.
#' @param height Plot height in pixels. NULL uses plotly default.
#' @return plotly object with source="dendrogram"
build_plotly_dendrogram <- function(
  dend_data,
  source_vec,
  colors = dendro2_colors,
  cut_height = 0.1,
  medoids = NULL,
  initial_xrange = NULL,
  initial_yrange = NULL,
  clusters = NULL,
  strip_prefix = NULL,
  label_filter = NULL,
  cluster_dropdown = FALSE,
  height = NULL
) {
  seg_df <- dend_data$segments
  label_df <- dend_data$labels
  label_df$source <- source_vec[as.character(label_df$label)]
  label_df$color <- colors[label_df$source]

  seg_x <- as.vector(rbind(seg_df$x, seg_df$xend, NA))
  seg_y <- as.vector(rbind(seg_df$y, seg_df$yend, NA))

  p <- plot_ly(source = "dendrogram", height = height) |>
    add_trace(
      x = seg_x,
      y = seg_y,
      type = "scatter",
      mode = "lines",
      line = list(color = "gray40", width = 1),
      hoverinfo = "none",
      showlegend = FALSE
    )

  for (grp in names(colors)) {
    grp_labels <- label_df[label_df$source == grp, ]
    if (nrow(grp_labels) == 0) {
      next
    }

    p <- p |>
      add_trace(
        x = grp_labels$x,
        y = rep(0, nrow(grp_labels)),
        type = "scatter",
        mode = "markers",
        marker = list(size = 7, color = colors[grp], symbol = "circle"),
        text = as.character(grp_labels$label),
        customdata = as.character(grp_labels$label),
        hovertemplate = "%{text}<extra></extra>",
        name = grp,
        legendgroup = grp
      )
  }

  base_font_size <- 13.5  # 1.5x the original 9

  # Filter annotations to only labeled leaves if label_filter is set
  label_indices <- seq_len(nrow(label_df))
  if (!is.null(label_filter)) {
    label_indices <- which(as.character(label_df$label) %in% label_filter)
  }

  annotations <- lapply(label_indices, function(i) {
    lab <- as.character(label_df$label[i])
    display_lab <- lab
    if (!is.null(strip_prefix)) {
      display_lab <- sub(paste0("^", strip_prefix), "", display_lab)
    }
    is_medoid <- !is.null(medoids) && lab %in% medoids
    if (is_medoid && !is.null(clusters)) {
      cid <- clusters[lab]
      display_lab <- paste0("<b>", cid, " ", display_lab, "</b>")
    }
    list(
      x = label_df$x[i],
      y = -0.01,
      text = display_lab,
      textangle = -90,
      xref = "x",
      yref = "y",
      showarrow = FALSE,
      font = list(
        size = if (is_medoid) base_font_size + 1.5 else base_font_size,
        color = label_df$color[i]
      ),
      xanchor = "center",
      yanchor = "top",
      bordercolor = if (is_medoid) label_df$color[i] else NA,
      borderwidth = if (is_medoid) 1.5 else 0,
      borderpad = if (is_medoid) 2 else 0
    )
  })

  # Build cluster dropdown menu if requested
  dropdown_menu <- NULL
  top_margin <- 30
  if (cluster_dropdown && !is.null(medoids) && !is.null(clusters)) {
    top_margin <- 60
    # Build one button per cluster
    cluster_ids <- sort(unique(clusters[medoids]))
    buttons <- list()

    # "Show All" button
    buttons[[1]] <- list(
      method = "relayout",
      args = list(list(`xaxis.range` = list(
        min(label_df$x) - 1, max(label_df$x) + 1
      ))),
      label = "Show All"
    )

    for (cid in cluster_ids) {
      members <- names(clusters[clusters == cid])
      member_x <- label_df$x[label_df$label %in% members]
      if (length(member_x) == 0) next
      span <- max(member_x) - min(member_x)
      center <- mean(range(member_x))
      padding <- max(span * 0.15, 2)
      x_lo <- center - span / 2 - padding
      x_hi <- center + span / 2 + padding

      # Find medoid name for this cluster
      cluster_medoid <- intersect(medoids, members)
      medoid_label <- if (length(cluster_medoid) > 0) {
        ml <- cluster_medoid[1]
        if (!is.null(strip_prefix)) ml <- sub(paste0("^", strip_prefix), "", ml)
        ml
      } else {
        ""
      }

      buttons[[length(buttons) + 1]] <- list(
        method = "relayout",
        args = list(list(`xaxis.range` = list(x_lo, x_hi))),
        label = paste0(cid, " ", medoid_label)
      )
    }

    dropdown_menu <- list(list(
      type = "dropdown",
      active = -1,
      buttons = buttons,
      x = 0,
      y = 1.15,
      xanchor = "left",
      yanchor = "top",
      showactive = TRUE
    ))
  }

  p |>
    layout(
      title = "Hierarchical Clustering (Cosine Distance)",
      xaxis = list(
        title = "",
        showticklabels = FALSE,
        showgrid = FALSE,
        zeroline = FALSE,
        range = initial_xrange
      ),
      yaxis = list(
        title = "Cosine Distance",
        zeroline = FALSE,
        range = if (!is.null(initial_yrange)) initial_yrange
                else c(-0.12, max(seg_df$y) * 1.05)
      ),
      annotations = annotations,
      margin = list(b = 100, t = top_margin, l = 50, r = 20),
      shapes = list(
        list(
          type = "line",
          x0 = 0,
          x1 = max(label_df$x) + 1,
          y0 = cut_height,
          y1 = cut_height,
          xref = "x",
          yref = "y",
          line = list(color = "gray50", dash = "dash", width = 1)
        )
      ),
      updatemenus = dropdown_menu,
      hovermode = "closest",
      dragmode = "select"
    ) |>
    config(displayModeBar = TRUE, displaylogo = FALSE) |>
    event_register("plotly_click") |>
    event_register("plotly_selected")
}
