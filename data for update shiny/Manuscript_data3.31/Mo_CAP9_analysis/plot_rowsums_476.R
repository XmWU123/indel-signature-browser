library(ggplot2)
library(ggrepel)

files <- c(
  "CAP9.PCAWG"    = "Signatures/Koh476/CAP9.mSigHdp.PCAWG.Koh476.All.txt",
  "CAP9.Hartwig"  = "Signatures/Koh476/CAP9.mSigHdp.Hartwig.Koh476.All.txt",
  "NoCAP.Hartwig" = "Signatures/Koh476/NoCAP.mSigHdp.Hartwig.Koh476.All.txt"
)

df_list <- lapply(names(files), function(nm) {
  sigs <- read.table(files[nm], header = TRUE, sep = "\t")
  data.frame(
    row_index = seq_len(nrow(sigs)),
    row_name  = NA_character_,
    row_sum   = rowSums(sigs),
    source    = nm
  )
})

# Add Liu 476 reference
liu_sigs <- read.table(
  "../../Manuscript_data/Liu_et_al_final_476_type_signatures.tsv",
  header = TRUE, sep = "\t", row.names = 1, check.names = FALSE
)
df_list[["Liu"]] <- data.frame(
  row_index = seq_len(nrow(liu_sigs)),
  row_name  = rownames(liu_sigs),
  row_sum   = rowSums(liu_sigs),
  source    = "Liu"
)

all_sources <- c(names(files), "Liu")
df <- do.call(rbind, df_list)
df$source <- factor(df$source, levels = all_sources)

# Flag top peaks per source for labeling
df$label <- NA_character_
for (src in levels(df$source)) {
  idx <- which(df$source == src)
  n_top <- if (src == "Liu") 15 else 10
  top_idx <- idx[order(df$row_sum[idx], decreasing = TRUE)[1:n_top]]
  if (src == "Liu") {
    df$label[top_idx] <- paste0(df$row_index[top_idx], ":", df$row_name[top_idx])
  } else {
    df$label[top_idx] <- as.character(df$row_index[top_idx])
  }
}

p <- ggplot(df, aes(x = row_index, y = row_sum)) +
  geom_col(width = 1) +
  geom_label_repel(
    aes(label = label),
    size = 2.5,
    max.overlaps = 20,
    direction = "y",
    nudge_y = 0.002,
    segment.size = 0.3,
    na.rm = TRUE
  ) +
  facet_wrap(~source, ncol = 1, scales = "free_y") +
  labs(x = "Row index (mutation type)", y = "Row sum across signatures") +
  theme_minimal()

ggsave("/tmp/rowsums_476.png", p, width = 12, height = 8)
system("xdg-open /tmp/rowsums_476.png")
