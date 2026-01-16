library(Cairo)
library(mSigPlot)
library(gridExtra)

source("map_476_to_other.R")
source("cosine.R")

testit = function() {
  t476 = read.delim(
    "../Manuscript_data/Liu_et_al_final_476_type_signatures.tsv",
    sep = '\t',
    row.names = 1
  )

  xx = as.data.frame(t476_to_89(t476))
  write.table(xx, "89_mapped_from_476.tsv", sep = '\t')

  yy = read.delim(
    "../Manuscript_data/Liu_et_al_final_89_type_signatures.tsv",
    row.names = 1,
    sep = '\t'
  )

  simlist = list()
  for (cname in colnames(yy)) {
    cplus = paste0(cname, "_converted")
    if (cplus %in% colnames(xx)) {
      sim = cosine(yy[, cname], xx[, cplus])
      simlist[[cplus]] = sim
    }
  }

  cairo_pdf("89_mapped_from_476.pdf", width = 8.5, height = 11)

  # Process plots in batches of 4
  plots <- list()
  for (i in 1:ncol(xx)) {
    title = colnames(xx)[i]
    sim = simlist[[title]]
    if (!is.null(sim)) {
      title = paste(title, sim)
    }

    plots[[length(plots) + 1]] <- plot_89(
      xx[, i, drop = FALSE],
      plot_title = title
    )

    # When we have 4 plots, print them and reset
    if (length(plots) == 4) {
      print(grid.arrange(grobs = plots, nrow = 4, ncol = 1))
      plots <- list()
    }
    cat(i, "\n")
  }

  # Print any remaining plots (less than 4)
  if (length(plots) > 0) {
    print(grid.arrange(grobs = plots, nrow = 4, ncol = 1))
  }

  dev.off()
}

testit()
