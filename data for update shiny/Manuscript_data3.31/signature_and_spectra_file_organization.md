All files are tab-separated.
All 476 catalogs/spectra and signatures have rownames in the first column.
89 catalogs/spectra have rownames in the first column
89 signatures to not, Assume the row order for type89 signatures is correct.
Rowname orders should be as in ICAMS::catalog.row.order, slots ID (for the 83-type classifictaion) ID89, ID476

## in folder `Manuscript_data/`

files `Liu_et_al_final_{476,89,38}_type_signatures.tsv` contain current signatures as of Jan 1, 2026

## in folder `Manuscript_data/Mo_CAP9_analysis/Catalogs/`

files `CAP9.{Hartwig,PCAWG}.{Koh476,Koh89,CSOMIC83}.catalog.txt` contain spectra from Hartwig or PCAWG, with all indels with R > 9 removed (CAP9).

files `nonclip.Hartwig{Koh476,Koh89,CSOMIC83}` contain spectra from Hartwig with all indels included (no clipping / no cap9)

## in folder `Manuscript_data/Mo_CAP9_analysis/Signatures/`

File names have 6 parts separated by "."

The scheme is: `{CAP9,NoCAP}.mSigHdp.{Hartwig,PCAWG}.{Koh476,Koh89},<cancertype>.txt`

Part1: CAP9 indicates no indels with R > 9; NoCAP indicates all indels
Part 2: constant (the extraction software)
Part 3: Hartwig or PCAWG: the data set from which the signatures was obtained
Part 4: Koh476 or Koh89; the numeric parts indicate the number of rows and the indel classification scheme
Part 5: cancertype or "MSI" or "All". Examples of cancertype "Head", "Breast", "Bone.SoftTissue" **IMPORTANT** cancertype may in include "."
Part 6: constant, always ".txt'
