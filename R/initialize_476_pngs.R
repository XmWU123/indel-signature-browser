library(ggplot2)
library(mSigPlot)
library(Cairo)
source("config.R")
initialize_476_pngs = function() {
  sigfile = get_config("type_476_signatures")
  if (!file.exists(sigfile)) {
    stop(sigfile, "does not exist")
  }
  df = read.delim(sigfile, sep = '\t')

  for (i in 1:34) {
    # This is a hack; we need to get up to date file

    plot_title = colnames(df)[i]

    # 保存为 PNG 文件
    outfile <- file.path(
      get_config("images_dir"),
      paste0(plot_title, "_476all.png")
    )

    Cairo(
      outfile,
      width = 300 * 7,
      height = 300 * 2,
      type = "png",
      bg = "white"
    )
    p = plot_476(
      df[, i, drop = FALSE],
      plot_title = plot_title,
      base_size = 30,
      simplify_labels = FALSE,
      num_labels = 5
    )
    print(p)

    dev.off()

    cat("✅ 已保存:", outfile, "\n")
  }
}
