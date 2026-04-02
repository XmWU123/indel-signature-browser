library(here)

read.delim(
  here("Manuscript_data/Liu_et_al_final_476_type_signatures.tsv"),
  sep = '\t',
  row.names = 1
) %>%
  rownames() -> oldroworder


read.delim(
  here(
    "Manuscript_data/Mo_CAP9_analysis/Catalogs/CAP9.Hartwig.Koh476.catalog.txt"
  ),
  sep = '\t',
  row.names = 1
) %>%
  rownames() -> cliphartwig

read.delim(
  here(
    "Manuscript_data/Mo_CAP9_analysis/Catalogs/nonclip.Hartwig.Koh476.catalog.txt"
  ),
  sep = '\t',
  row.names = 1
) %>%
  rownames() -> nocliphartwig

read.delim(
  here(
    "Manuscript_data/Mo_CAP9_analysis/Catalogs/CAP9.PCAWG.Koh476.catalog.txt"
  ),
  sep = '\t',
  row.names = 1
) %>%
  rownames() -> pcawg


all(oldroworder == cliphartwig)

all(oldroworder == nocliphartwig)

all(oldroworder == pcawg)

all(cliphartwig == nocliphartwig)

all(cliphartwig == pcawg)

all(nocliphartwig == pcawg)
