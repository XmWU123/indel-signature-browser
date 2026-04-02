# Examples of using cluster_catalogs()

source("code/cluster_catalogs.R")

plot_output = "plot_output"

# Example 1: ID83 signatures (COSMIC, Liu, Jin)
cluster_catalogs(
  file.path(plot_output, "dendrogram_83_type_signatures.pdf"),
  COSMIC = "Manuscript_data/COSMIC_v3.5_ID_GRCh37_signatures.tsv",
  Liu = "Manuscript_data/Liu_et_al_final_83_type_signatures.tsv",
  Jin = "Manuscript_data/jin_2024_sup_tab_1_signatures.tsv"
)

# Example 2: ID89 signatures (Koh, Liu)
cluster_catalogs(
  file.path(plot_output, "dendrogram_89_type_signatures.pdf"),
  Koh = "Manuscript_data/Koh_signatures.tsv",
  Liu = "Manuscript_data/Liu_et_al_final_89_type_signatures.tsv"
)

# Example 3: ID476 spectra (Liu only)
cluster_catalogs(
  file.path(plot_output, "dendrogram_476_type_signatures.pdf"),
  Liu = "Manuscript_data/Liu_et_al_final_476_type_signatures.tsv"
)
