#!/usr/bin/env Rscript

library(ggplot2)
library(ggdendro)
library(cluster)
library(reshape2)
library(stats)

#' Compute cosine similarity between columns of a matrix
cosine_sim <- function(m) {
  # Normalize columns to unit vectors
  m_norm <- apply(m, 2, function(x) x / sqrt(sum(x^2)))
  # Dot product of normalized columns is cosine similarity
  sim <- t(m_norm) %*% m_norm
  return(sim)
}

# Parameters
dir_path <- "Manuscript_data/Mo_CAP9_analysis/Signatures/Koh89/"
min_similarity <- 0.95
min_cluster_size <- 3
output_pdf <- "plot_output/koh89_cap9_clustering.pdf"
output_tsv <- "test_output/koh89_cap9_medoids.tsv"

# 1. Load data
files <- list.files(dir_path, pattern = "^CAP9.*\\.txt$", full.names = TRUE)
all_sigs_list <- list()

for (f in files) {
  # Extract type name from filename
  # Pattern: CAP9.mSigHdp.[Source].Koh89.[Type].txt
  type_name <- gsub("CAP9\\.mSigHdp\\.[^.]+\\.Koh89\\.(.*)\\.txt", "\\1", basename(f))
  
  # Read file
  data <- read.table(f, header = TRUE, sep = "\t")
  
  # Rename columns: hdp.1 -> TypeName.1
  colnames(data) <- gsub("hdp\\.", paste0(type_name, "."), colnames(data))
  
  all_sigs_list[[type_name]] <- data
}

# Combine all signatures into one large dataframe
# Assuming they all have the same 89 rows in the same order
all_sigs <- do.call(cbind, all_sigs_list)
all_sigs_matrix <- as.matrix(all_sigs)

message("Total signatures loaded: ", ncol(all_sigs_matrix))

# 2. Clustering
sim_matrix <- cosine_sim(all_sigs_matrix)
dist_matrix <- as.dist(1 - sim_matrix)

# Complete linkage ensures all pairs in a cluster meet the similarity threshold
hc <- hclust(dist_matrix, method = "complete")

# Cut tree at height corresponding to min_similarity
clusters <- cutree(hc, h = 1 - min_similarity)
cluster_counts <- table(clusters)

# Filter clusters by size
valid_clusters <- names(cluster_counts[cluster_counts >= min_cluster_size])
filtered_clusters <- clusters[clusters %in% valid_clusters]

message("Number of clusters found: ", length(unique(clusters)))
message("Number of clusters with size >= ", min_cluster_size, ": ", length(valid_clusters))

# 3. Find Medoids
medoids_list <- list()
cluster_info <- data.frame(
  Signature = colnames(all_sigs_matrix),
  Cluster = as.vector(clusters),
  IsMedoid = FALSE,
  stringsAsFactors = FALSE
)

for (cl_id in valid_clusters) {
  sigs_in_cluster <- names(clusters[clusters == cl_id])
  
  if (length(sigs_in_cluster) == 1) {
    medoid_name <- sigs_in_cluster
  } else {
    # Sub-similarity matrix for this cluster
    sub_sim <- sim_matrix[sigs_in_cluster, sigs_in_cluster]
    # Medoid is the one with the highest average similarity to others
    avg_sim <- rowMeans(sub_sim)
    medoid_name <- names(which.max(avg_sim))
  }
  
  medoids_list[[as.character(cl_id)]] <- all_sigs_matrix[, medoid_name]
  cluster_info$IsMedoid[cluster_info$Signature == medoid_name] <- TRUE
}

# Create medoids matrix
if (length(medoids_list) > 0) {
  medoids_matrix <- do.call(cbind, medoids_list)
  colnames(medoids_matrix) <- paste0("Cluster_", names(medoids_list), "_Medoid")
  dir.create(dirname(output_tsv), showWarnings = FALSE, recursive = TRUE)
  write.table(medoids_matrix, output_tsv, sep = "\t", quote = FALSE, col.names = NA)
  message("Medoids saved to ", output_tsv)
} else {
  message("No clusters met the size criteria.")
}

# 4. Visualization
dir.create(dirname(output_pdf), showWarnings = FALSE, recursive = TRUE)
cairo_pdf(output_pdf, width = 12, height = 10, onefile = TRUE)

# A. Dendrogram
dend_data <- dendro_data(hc)
p_dend <- ggplot() +
  geom_segment(data = segment(dend_data), aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_hline(yintercept = 1 - min_similarity, color = "red", linetype = "dashed") +
  labs(title = "Hierarchical Clustering (Complete Linkage)",
       subtitle = paste0("Red line at distance ", 1 - min_similarity, " (similarity ", min_similarity, ")"),
       y = "Cosine Distance (1 - similarity)", x = "") +
  theme_minimal() +
  theme(axis.text.x = element_blank())
print(p_dend)

# B. Similarity Heatmap (ordered by clustering)
ord <- hc$order
ordered_sim <- sim_matrix[ord, ord]
melted_sim <- melt(ordered_sim)
p_heat <- ggplot(melted_sim, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.5, limit = c(0, 1)) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank()) +
  labs(title = "Cosine Similarity Heatmap", subtitle = "Ordered by Dendrogram")
print(p_heat)

# C. MDS / PCA-like Plot
# Classical Multidimensional Scaling
mds <- cmdscale(dist_matrix, k = 2)
mds_df <- data.frame(
  X = mds[, 1],
  Y = mds[, 2],
  Cluster = factor(clusters),
  Label = colnames(all_sigs_matrix)
)
mds_df$InLargeCluster <- mds_df$Cluster %in% valid_clusters

p_mds <- ggplot(mds_df, aes(X, Y, color = Cluster)) +
  geom_point(aes(shape = InLargeCluster), size = 3, alpha = 0.7) +
  theme_minimal() +
  labs(title = "MDS Plot of Signatures", 
       subtitle = "Shapes indicate clusters meeting size criteria",
       x = "Coordinate 1", y = "Coordinate 2") +
  theme(legend.position = "none") # Too many clusters to show legend usually
print(p_mds)

# D. Silhouette Plot
if (length(unique(clusters)) > 1) {
  sil <- silhouette(clusters, dist_matrix)
  plot(sil, main = "Silhouette Plot of Clustering", col = "steelblue", border = NA)
}

# E. Medoid Spectra (if any)
if (length(medoids_list) > 0) {
  # Plot first few medoids as example
  num_to_plot <- min(length(medoids_list), 4)
  for (i in 1:num_to_plot) {
    cl_id <- names(medoids_list)[i]
    m_data <- data.frame(
      Index = 1:89,
      Value = medoids_list[[i]]
    )
    p_medoid <- ggplot(m_data, aes(x = Index, y = Value)) +
      geom_bar(stat = "identity", fill = "darkblue") +
      labs(title = paste0("Medoid for Cluster ", cl_id),
           subtitle = paste0("Cluster size: ", cluster_counts[cl_id])) +
      theme_minimal()
    print(p_medoid)
  }
}

dev.off()
message("Plots saved to ", output_pdf)
