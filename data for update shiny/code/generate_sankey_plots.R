# generate_sankey_plots.R
#
# Generate Sankey/alluvial plots for 476-type to 83-type signature collapse

source("code/collapse_476_to_83.R")

result <- collapse_476_to_83("InsDel1a", "C_ID1")
plots <- plot_collapse_sankey(result, min_flow = 0.001, title_prefix = "InsDel1a -> C_ID1")

outdir <- "output"
dir.create(outdir, showWarnings = FALSE)

ggplot2::ggsave(file.path(outdir, "sankey_insertions.pdf"), plots$insertions, width = 14, height = 10)
ggplot2::ggsave(file.path(outdir, "sankey_insertions_c.pdf"), plots$insertions_c, width = 14, height = 10)
ggplot2::ggsave(file.path(outdir, "sankey_insertions_t.pdf"), plots$insertions_t, width = 14, height = 10)
ggplot2::ggsave(file.path(outdir, "sankey_deletions.pdf"), plots$deletions, width = 14, height = 10)
ggplot2::ggsave(file.path(outdir, "sankey_other.pdf"), plots$other, width = 14, height = 10)
ggplot2::ggsave(file.path(outdir, "sankey_insertions_t_no5plus.pdf"), plots$insertions_t_no5plus, width = 14, height = 10)

cat("Saved plots to", outdir, "\n")
