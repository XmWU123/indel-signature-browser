# test_map_476_to_other

zz = read.delim(
  "Manuscript_data/Liu_et_al_final_476_type_signatures.tsv",
  sep = '\t',
  row.names = 1
)
uu = t476_to_89(zz)
uu2 = t476_to_89(zz)
ff = read.delim("vignette/89_mapped_from_476.tsv", sep = '\t', row.names = 1)

vv = t476_to_83(zz)
vv2 = t476_to_83(zz)
xx = read.delim("vignette/x83_mapped_from_476.tsv", sep = '\t', row.names = 1)
