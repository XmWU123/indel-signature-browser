parse_args <- function(args) {
  defaults <- list(
    input_dir = "Manuscript_data/Mo_CAP9_analysis/Signatures/Koh89",
    output_dir = "Manuscript_data/Mo_CAP9_analysis/codex_Koh89_cluster_outputs",
    min_similarity = 0.95,
    min_cluster_size = 3,
    row_label_catalog = "Manuscript_data/Mo_CAP9_analysis/Catalogs/CAP9.Hartwig.Koh89.catalog.txt"
  )

  if (length(args) == 0) {
    return(defaults)
  }

  for (arg in args) {
    if (!grepl("^--", arg)) {
      stop("Arguments must use the form --name=value")
    }
    parts <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1]]
    if (length(parts) != 2) {
      stop("Arguments must use the form --name=value")
    }
    key <- gsub("-", "_", parts[1])
    defaults[[key]] <- parts[2]
  }

  defaults$min_similarity <- as.numeric(defaults$min_similarity)
  defaults$min_cluster_size <- as.integer(defaults$min_cluster_size)
  defaults
}

sanitize_token <- function(x) {
  x <- tolower(x)
  gsub("[^a-z0-9]+", "", x)
}

extract_file_metadata <- function(path) {
  stem <- sub("\\.txt$", "", basename(path))
  parts <- strsplit(stem, "\\.", fixed = FALSE)[[1]]
  koh_idx <- match("Koh89", parts)

  if (is.na(koh_idx) || koh_idx >= length(parts)) {
    stop(sprintf("Could not parse source/tissue from file name: %s", path))
  }

  suffix_parts <- parts[(koh_idx + 1):length(parts)]
  source <- sanitize_token(parts[koh_idx - 1])
  tissue_last <- sanitize_token(tail(suffix_parts, 1))
  tissue_full <- paste(suffix_parts, collapse = ".")

  list(
    stem = stem,
    source = source,
    tissue_last = tissue_last,
    tissue_full = tissue_full
  )
}

read_signatures <- function(path, expected_rows = NULL) {
  x <- read.table(path, header = TRUE, sep = "\t", check.names = FALSE)
  meta <- extract_file_metadata(path)

  if (!is.null(expected_rows) && nrow(x) != expected_rows) {
    stop(sprintf("Row count mismatch in %s: expected %s, found %s", path, expected_rows, nrow(x)))
  }

  prefix <- paste(meta$source, meta$tissue_last, sep = "_")
  original_names <- colnames(x)
  renamed <- sub("^hdp", prefix, original_names)
  colnames(x) <- make.unique(renamed, sep = "_dup")

  col_meta <- data.frame(
    column_name = colnames(x),
    original_column = original_names,
    file = path,
    file_stem = meta$stem,
    source = meta$source,
    tissue_last = meta$tissue_last,
    tissue_full = meta$tissue_full,
    stringsAsFactors = FALSE
  )

  list(matrix = as.matrix(x), metadata = col_meta)
}

cosine_similarity_matrix <- function(x) {
  norms <- sqrt(colSums(x * x))
  zero_cols <- which(norms == 0)
  if (length(zero_cols) > 0) {
    stop(sprintf("Found zero-norm columns: %s", paste(colnames(x)[zero_cols], collapse = ", ")))
  }
  x_scaled <- sweep(x, 2, norms, "/", check.margin = FALSE)
  sim <- crossprod(x_scaled)
  sim[sim > 1] <- 1
  sim[sim < -1] <- -1
  sim
}

cluster_medoids <- function(distance_matrix, cluster_ids) {
  split_indices <- split(seq_along(cluster_ids), cluster_ids)
  split_indices <- split_indices[!is.na(names(split_indices))]

  medoid_rows <- lapply(names(split_indices), function(cluster_name) {
    idx <- split_indices[[cluster_name]]
    d <- distance_matrix[idx, idx, drop = FALSE]
    local_medoid <- idx[which.min(rowMeans(d))]
    min_similarity <- if (length(idx) > 1) {
      1 - max(d[upper.tri(d, diag = FALSE)])
    } else {
      1
    }

    data.frame(
      cluster_id = as.integer(cluster_name),
      size = length(idx),
      medoid_column = colnames(distance_matrix)[local_medoid],
      medoid_index = local_medoid,
      min_within_similarity = min_similarity,
      mean_within_distance = mean(d[upper.tri(d, diag = FALSE)]),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, medoid_rows)
}

plot_heatmap <- function(similarity, hc, cluster_assignments, path_png) {
  ordered <- hc$order
  ordered_sim <- similarity[ordered, ordered, drop = FALSE]
  ordered_clusters <- cluster_assignments[ordered]

  png(path_png, width = 2400, height = 2200, res = 220)
  layout(matrix(c(1, 2), nrow = 1), widths = c(4.6, 1.4))
  par(mar = c(5, 5, 4, 1))
  image(
    1:ncol(ordered_sim),
    1:nrow(ordered_sim),
    t(ordered_sim[nrow(ordered_sim):1, ]),
    col = colorRampPalette(c("#f7fbff", "#6baed6", "#08306b"))(256),
    axes = FALSE,
    xlab = "Columns",
    ylab = "Columns",
    main = "Koh89 CAP9 cosine similarity"
  )
  axis(1, at = pretty(seq_len(ncol(ordered_sim))))
  axis(2, at = pretty(seq_len(nrow(ordered_sim))), labels = rev(pretty(seq_len(nrow(ordered_sim)))))
  box()

  par(mar = c(5, 1, 4, 6))
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0.5, length(ordered_clusters) + 0.5))
  uniq <- sort(unique(ordered_clusters), na.last = TRUE)
  colors <- grDevices::rainbow(max(sum(!is.na(uniq)), 1))
  color_map <- setNames(colors[seq_len(sum(!is.na(uniq)))], uniq[!is.na(uniq)])
  rect_y <- seq_along(ordered_clusters)
  rect(
    0,
    rect_y - 0.5,
    1,
    rect_y + 0.5,
    col = ifelse(
      is.na(ordered_clusters),
      "#d9d9d9",
      unname(color_map[as.character(ordered_clusters)])
    ),
    border = NA
  )
  axis(4, at = c(length(ordered_clusters), 1), labels = c("first", "last"), las = 1)
  mtext("Cluster", side = 4, line = 3)
  dev.off()
}

plot_dendrogram <- function(hc, min_similarity, path_png) {
  png(path_png, width = 2200, height = 1200, res = 200)
  par(mar = c(5, 4, 4, 2))
  plot(hc, main = "Complete-linkage clustering", xlab = "", sub = "", ylab = "1 - cosine similarity")
  abline(h = 1 - min_similarity, col = "#cb181d", lwd = 2, lty = 2)
  dev.off()
}

plot_mds <- function(distance_matrix, metadata, path_png) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    warning("Skipping MDS plot because ggplot2 is not installed")
    return(invisible(NULL))
  }

  coords <- cmdscale(as.dist(distance_matrix), k = 2, eig = TRUE)
  plot_df <- cbind(
    metadata,
    data.frame(
      x = coords$points[, 1],
      y = coords$points[, 2],
      stringsAsFactors = FALSE
    )
  )

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = x, y = y, color = factor(cluster_id), shape = source)
  ) +
    ggplot2::geom_point(size = 2.2, alpha = 0.9) +
    ggplot2::scale_color_discrete(na.value = "grey75") +
    ggplot2::labs(
      title = "MDS of CAP9 signature columns",
      subtitle = "Distance = 1 - cosine similarity",
      color = "Cluster",
      shape = "Source",
      x = "Coordinate 1",
      y = "Coordinate 2"
    ) +
    ggplot2::theme_minimal()

  ggplot2::ggsave(path_png, p, width = 10, height = 7, dpi = 200)
}

plot_medoid_profiles <- function(matrix_data, metadata, medoid_summary, row_labels, path_pdf) {
  if (nrow(medoid_summary) == 0) {
    return(invisible(NULL))
  }

  pdf(path_pdf, width = 12, height = 5)
  on.exit(dev.off(), add = TRUE)

  for (i in seq_len(nrow(medoid_summary))) {
    cluster_id <- medoid_summary$cluster_id[i]
    members <- which(metadata$cluster_id == cluster_id)
    medoid_idx <- medoid_summary$medoid_index[i]
    y_max <- max(matrix_data[, members, drop = FALSE])

    matplot(
      x = seq_len(nrow(matrix_data)),
      y = matrix_data[, members, drop = FALSE],
      type = "l",
      lty = 1,
      col = grDevices::adjustcolor("grey50", alpha.f = 0.5),
      xlab = "Mutation type",
      ylab = "Probability",
      main = sprintf(
        "Cluster %s (%s columns, medoid = %s)",
        cluster_id,
        length(members),
        metadata$column_name[medoid_idx]
      ),
      xaxt = if (is.null(row_labels)) "s" else "n",
      ylim = c(0, y_max)
    )
    lines(
      seq_len(nrow(matrix_data)),
      matrix_data[, medoid_idx],
      col = "#08519c",
      lwd = 3
    )
    if (!is.null(row_labels)) {
      axis(1, at = seq_len(nrow(matrix_data)), labels = row_labels, las = 2, cex.axis = 0.45)
    }
  }
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
dir.create(args$output_dir, recursive = TRUE, showWarnings = FALSE)

files <- list.files(args$input_dir, pattern = "^CAP9.*\\.txt$", full.names = TRUE)
if (length(files) == 0) {
  stop(sprintf("No CAP9 files found in %s", args$input_dir))
}
files <- sort(files)

row_labels <- NULL
if (file.exists(args$row_label_catalog)) {
  catalog <- read.delim(
    args$row_label_catalog,
    sep = "\t",
    row.names = 1,
    check.names = FALSE
  )
  row_labels <- rownames(catalog)
}

first_matrix <- read.table(files[1], header = TRUE, sep = "\t", check.names = FALSE)
expected_rows <- nrow(first_matrix)
if (!is.null(row_labels) && length(row_labels) != expected_rows) {
  stop("Row label catalog row count does not match signature files")
}

pieces <- lapply(files, read_signatures, expected_rows = expected_rows)
matrix_data <- do.call(cbind, lapply(pieces, `[[`, "matrix"))
metadata <- do.call(rbind, lapply(pieces, `[[`, "metadata"))

rownames(matrix_data) <- if (is.null(row_labels)) sprintf("row_%02d", seq_len(nrow(matrix_data))) else row_labels
metadata$column_index <- seq_len(ncol(matrix_data))
metadata$column_sum <- colSums(matrix_data)

similarity <- cosine_similarity_matrix(matrix_data)
distance_matrix <- 1 - similarity
diag(distance_matrix) <- 0

hc <- hclust(as.dist(distance_matrix), method = "complete")
raw_cluster_id <- cutree(hc, h = 1 - args$min_similarity)

cluster_sizes <- table(raw_cluster_id)
retained <- as.integer(names(cluster_sizes)[cluster_sizes >= args$min_cluster_size])
metadata$raw_cluster_id <- raw_cluster_id
metadata$cluster_id <- ifelse(raw_cluster_id %in% retained, raw_cluster_id, NA_integer_)

if (length(retained) > 0) {
  new_ids <- setNames(seq_along(sort(retained)), sort(retained))
  metadata$cluster_id <- ifelse(
    is.na(metadata$cluster_id),
    NA_integer_,
    unname(new_ids[as.character(metadata$cluster_id)])
  )
}

medoid_summary <- if (all(is.na(metadata$cluster_id))) {
  data.frame(
    cluster_id = integer(),
    size = integer(),
    medoid_column = character(),
    medoid_index = integer(),
    min_within_similarity = numeric(),
    mean_within_distance = numeric(),
    stringsAsFactors = FALSE
  )
} else {
  cluster_medoids(distance_matrix, metadata$cluster_id)
}

metadata$is_retained <- !is.na(metadata$cluster_id)
metadata$is_medoid <- FALSE
if (nrow(medoid_summary) > 0) {
  metadata$is_medoid[medoid_summary$medoid_index] <- TRUE
}

cluster_quality <- if (nrow(medoid_summary) == 0) {
  data.frame()
} else {
  do.call(rbind, lapply(seq_len(nrow(medoid_summary)), function(i) {
    cluster_id <- medoid_summary$cluster_id[i]
    members <- which(metadata$cluster_id == cluster_id)
    medoid_idx <- medoid_summary$medoid_index[i]
    sims_to_medoid <- similarity[medoid_idx, members]
    data.frame(
      cluster_id = cluster_id,
      size = length(members),
      medoid_column = medoid_summary$medoid_column[i],
      min_within_similarity = medoid_summary$min_within_similarity[i],
      median_similarity_to_medoid = stats::median(sims_to_medoid),
      min_similarity_to_medoid = min(sims_to_medoid),
      mean_within_distance = medoid_summary$mean_within_distance[i],
      stringsAsFactors = FALSE
    )
  }))
}

write.table(
  matrix_data,
  file = file.path(args$output_dir, "combined_matrix.tsv"),
  sep = "\t",
  quote = FALSE,
  col.names = NA
)
write.table(
  metadata,
  file = file.path(args$output_dir, "column_metadata.tsv"),
  sep = "\t",
  quote = FALSE,
  row.names = FALSE
)
write.table(
  medoid_summary,
  file = file.path(args$output_dir, "cluster_medoids.tsv"),
  sep = "\t",
  quote = FALSE,
  row.names = FALSE
)
write.table(
  cluster_quality,
  file = file.path(args$output_dir, "cluster_quality.tsv"),
  sep = "\t",
  quote = FALSE,
  row.names = FALSE
)

summary_lines <- c(
  sprintf("Input directory: %s", args$input_dir),
  sprintf("Number of files: %s", length(files)),
  sprintf("Rows: %s", nrow(matrix_data)),
  sprintf("Columns: %s", ncol(matrix_data)),
  sprintf("min_similarity: %.4f", args$min_similarity),
  sprintf("min_cluster_size: %s", args$min_cluster_size),
  sprintf("Retained clusters: %s", nrow(medoid_summary)),
  sprintf("Retained columns: %s", sum(metadata$is_retained)),
  sprintf("Dropped columns: %s", sum(!metadata$is_retained))
)
writeLines(summary_lines, con = file.path(args$output_dir, "run_summary.txt"))

plot_heatmap(
  similarity = similarity,
  hc = hc,
  cluster_assignments = metadata$cluster_id,
  path_png = file.path(args$output_dir, "cosine_similarity_heatmap.png")
)
plot_dendrogram(
  hc = hc,
  min_similarity = args$min_similarity,
  path_png = file.path(args$output_dir, "cluster_dendrogram.png")
)
plot_mds(
  distance_matrix = distance_matrix,
  metadata = metadata,
  path_png = file.path(args$output_dir, "mds_clusters.png")
)
plot_medoid_profiles(
  matrix_data = matrix_data,
  metadata = metadata,
  medoid_summary = medoid_summary,
  row_labels = row_labels,
  path_pdf = file.path(args$output_dir, "cluster_medoid_profiles.pdf")
)

cat(sprintf("Wrote outputs to %s\n", args$output_dir))
