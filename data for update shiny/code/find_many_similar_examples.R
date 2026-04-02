library(gridExtra)
library(ggplot2)
library(philentropy)
library(mSigPlot)
library(glue)

source(here::here("code/find_many_similar.R"))

sigs = read.table(
  "Manuscript_data/Liu_et_al_final_89_type_signatures.tsv",
  sep = "\t",
  header = TRUE,
  row.names = 1
)

# for (signame in colnames(sigs)) {
#  message(signame)
#   find_samples_similar_to_sig(signame)
# }

# five_b = find_samples_similar_to_sig(
#  "InsDel5b",
#  max_num_similar = 1000,
#  cosine_cutoff = 0.95,
#  do_plot = FALSE
#)

########################################################

res1 <- find_many_similar(
  sig_path = here::here(
    "Manuscript_data/Liu_et_al_final_476_type_signatures.tsv"
  ),
  "InsDel_J",
  here::here("Manuscript_data/Liu_et_al_476_type_spectra.tsv"),
  cosine_cutoff = 0.9,
  num_exemplars = 300,
  out_pdf = "exemplars_for_IndDelJ_sim_0.9_num_300.pdf",
  min_mutations = 50
)
fwrite(res1$above_cutoff, here::here("msi_study/J_hits_ge_0.0.csv"))

res2 <- find_many_similar(
  sig_path = here::here(
    "Manuscript_data/Liu_et_al_final_476_type_signatures.tsv"
  ),
  "InsDel7",
  here::here("Manuscript_data/Liu_et_al_476_type_spectra.tsv"),
  cosine_cutoff = 0.9,
  num_exemplars = 300,
  out_pdf = "exemplars_for_IndDel7_sim_0.9_num_300.pdf",
  min_mutations = 50
)
fwrite(res2$above_cutoff, here::here("msi_study/7_hits_ge_0.0.csv"))

find_many_similar(
  sig_path = here::here(
    "Manuscript_data/Liu_et_al_final_476_type_signatures.tsv"
  ),
  "InsDel_K_beta",
  here::here("Manuscript_data/Liu_et_al_476_type_spectra.tsv"),
  cosine_cutoff = 0.85,
  num_exemplars = 30,
  out_pdf = "exemplars_for_IndDel_K_beta.pdf",
  min_mutations = 50
)


find_many_similar(
  sig_path = "../Manuscript_data/Liu_et_al_final_89_type_signatures.tsv",
  "InsDel_F",
  "../Manuscript_data/Liu_et_al_89_type_spectra.tsv",
  cosine_cutoff = 0.9,
  num_exemplars = 30,
  out_pdf = "exemplars_for_IndDelF.pdf",
  min_mutations = 50
)

library(mSigPlot)
find_many_similar(
  sig_path = "../Manuscript_data/Liu_et_al_final_476_type_signatures.tsv",
  "InsDel_F",
  "../Manuscript_data/Liu_et_al_476_type_spectra.tsv",
  cosine_cutoff = 0.9,
  num_exemplars = 30,
  out_pdf = "exemplars_for_IndDelF_476.pdf",
  min_mutations = 50
)

find_many_similar(
  sig_path = "../Manuscript_data/Liu_et_al_final_89_type_signatures.tsv",
  "InsDell1",
  "../Manuscript_data/Liu_et_al_89_type_spectra.tsv",
  cosine_cutoff = 0.9,
  num_exemplars = 30,
  out_pdf = "exemplars_for_IndDel1a.pdf",
  min_mutations = 50
)

library(mSigPlot)
find_many_similar(
  sig_path = "../Manuscript_data/Liu_et_al_final_476_type_signatures.tsv",
  "InsDel1a",
  "../Manuscript_data/Liu_et_al_476_type_spectra.tsv",
  cosine_cutoff = 0.9,
  num_exemplars = 30,
  out_pdf = "exemplars_for_IndDel1a_476.pdf",
  min_mutations = 50
)

find_many_similar(
  sig_path = "../Manuscript_data/Liu_et_al_final_89_type_signatures.tsv",
  "InsDel_P",
  "../Manuscript_data/Liu_et_al_89_type_spectra.tsv",
  cosine_cutoff = 0.9,
  num_exemplars = 100,
  out_pdf = "exemplars_for_IndDel_P.pdf",
  min_mutations = 50
)

find_many_similar(
  "../Manuscript_data/COSMIC_v3.5_ID_GRCh37_signatures.tsv",
  "ID10",
  "../Manuscript_data/Liu_et_al_83_type_spectra.tsv",
  cosine_cutoff = 0.5,
  out_pdf = "exemplars_for_ID10.pdf",
  min_mutations = 50
)

find_many_similar(
  "../Manuscript_data/COSMIC_v3.5_ID_GRCh37_signatures.tsv",
  "ID10",
  "../Manuscript_data/Liu_et_al_83_type_spectra.tsv",
  cosine_cutoff = 0.5,
  out_pdf = "exemplars_for_ID10_min_20.pdf",
  min_mutations = 20
)


find_many_similar(
  "../Manuscript_data/Liu_et_al_final_83_type_signatures.tsv",
  "C_ID10",
  "../Manuscript_data/Liu_et_al_83_type_spectra.tsv",
  cosine_cutoff = 0.5,
  out_pdf = "exemplars_for_C_ID10.pdf",
  min_mutations = 50
)


find_many_similar(
  "../Manuscript_data/Liu_et_al_final_83_type_signatures.tsv",
  "C_ID7",
  "../Manuscript_data/Liu_et_al_83_type_spectra.tsv",
  cosine_cutoff = 0.5,
  out_pdf = "exemplars_for_C_ID7_min_50.pdf",
  min_mutations = 50
)

find_many_similar(
  "../Manuscript_data/COSMIC_v3.5_ID_GRCh37_signatures.tsv",
  "ID7",
  "../Manuscript_data/Liu_et_al_83_type_spectra.tsv",
  cosine_cutoff = 0.5,
  out_pdf = "exemplars_for_ID7_min_50.pdf",
  min_mutations = 50
)
