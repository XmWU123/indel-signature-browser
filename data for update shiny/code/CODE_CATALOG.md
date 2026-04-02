# Code Catalog

Brief descriptions of each R file in `code/`.

| File | Description |
|------|-------------|
| `all_pairwise_89_476.R` | Computes pairwise cosine similarities between all 89-type and 476-type signatures, producing heatmaps, dendrograms, and side-by-side plots for high-similarity pairs. |
| `all_pairwise_cosine.R` | Computes pairwise cosine similarities for a single signature file (83, 89, or 476), producing a heatmap, dendrogram, and side-by-side plots for similar pairs. |
| `annotate_fmh_indel_vcfs.R` | Batch-annotates FMH (Hartwig) `.purple.somatic.vcf.gz` files with ICAMS indel classifications using parallel processing (`doFuture`). |
| `annotate_pcawg_graylist_indel_vcfs.R` | Batch-annotates PCAWG graylist `.consensus.indel.vcf.gz` files with ICAMS indel classifications. |
| `annotate_vcfs.R` | Batch-annotates PCAWG `.consensus.indel.vcf.gz` files with ICAMS indel classifications (command-line script taking number of files as argument). |
| `build_clipped_spectra_with_graylist.R` | Builds clipped (repeat count <= 9) spectra catalogs for ID83/ID89/ID476 from all annotated VCF files (PCAWG, FMH, graylist), writing to HDF5 then converting to TSV. |
| `cluster_catalogs.R` | Provides `cluster_catalogs()`, a general function for hierarchical clustering of signature catalogs from multiple sources using cosine distance, with colored dendrogram output. |
| `code.to.map.476.89.R` | Mo's original code to convert ID476 catalogs to ID89 using a mapping template and `ID476_ID89_mapping.txt`. |
| `collapse_476_to_83.R` | Collapses 476-type signatures to 83-type using quadratic programming (bipartite matching via OSQP), with Sankey/alluvial plot visualization of the mapping flows. |
| `compare_476_clipped_vs_cap9.R` | Compares clipped 476-type spectra to the corresponding CAP9 Hartwig/PCAWG catalog entries via cosine similarity and paired plots. |
| `compare_assignments.R` | Compares recompressed vs. original 83-type signature assignments via per-signature scatter plots with robust regression. |
| `compare_spectrum_to_vcf.R` | Compares a sample's published 476-type spectrum to one recomputed from its annotated VCF, checking for discrepancies. |
| `compulsive_test.R` | Tests that ICAMS `clip_le_9=TRUE` and manual pre-filtering to R<=9 produce identical 476-type catalogs, and compares against the published spectra. |
| `dendro2_helpers.R` | Helper functions for the interactive dendrogram v2 (`interactive_dendro2.qmd`): loads signatures from multiple sources (CAP9, Liu, Koh, COSMIC), builds plotly dendrograms, and finds best-matching spectra. |
| `dendrogram_helpers.R` | Earlier version of dendrogram helpers for the interactive dendrogram v1: loads CAP9 + Liu signatures and builds plotly dendrograms. |
| `explore_assignments.R` | Quick exploratory script that reads assignment tables (table_s2, s8, s9) and checks overlap between 83-type and 89-type assignments. |
| `extract_cosmic_koh_pairs.R` | Scans all annotated VCF files to extract unique (COSMIC_83, Koh_476) mutation-type pairs and writes them to `unique_pairs.tsv`. |
| `filter_vcf_by_mutation_type.R` | Filters an annotated indel VCF to rows matching a single 476-type mutation type. |
| `find_many_similar.R` | Finds sample spectra most similar to a given signature (by cosine similarity) and generates exemplar plots as PDF. |
| `find_many_similar_examples.R` | Driver script calling `find_many_similar()` for various signatures (InsDel_J, InsDel7, InsDel_K_beta, etc.) to generate exemplar PDFs. |
| `generate_sankey_plots.R` | Generates Sankey/alluvial plot PDFs for the InsDel1a -> C_ID1 signature collapse (476-to-83). |
| `map_476_to_other.R` | Provides `t476_to_89()` and `t476_to_83()` functions that aggregate 476-type signatures/catalogs to 89-type or 83-type using `ID476_ID89_mapping.txt`. |
| `plot_signature_assignments.R` | Generates "hamburger plots" (snake/swarm plots) showing per-signature mutation counts across cancer types, with MSI-H sample highlighting. |
| `read_annotated_vcf.R` | Reads an annotated indel VCF file given a tumor identifier (SP pattern for PCAWG, or `CancerType::SampleID` for Hartwig). |
| `recompress_assignments.R` | Combines sub-signature rows (e.g. InsDel1a-d -> C_ID1) in the 89-type assignment matrix by summing, producing a compressed assignment file. |
| `older/rename_aliquot_to_SP.R` | Renames PCAWG VCF files from UUID-style aliquot IDs to SP IDs using `PCAWG7::map_aliquot_ID_to_SP_ID`. |
| `older/rename_aliquot_to_SP2.R` | Same as `rename_aliquot_to_SP.R` but operates on a different directory (graylist indels). |
| `run_cluster_cap9_catalogs.R` | Runs `cluster_catalogs()` on CAP9 extraction signatures (Koh476 and Koh89) combined with Liu reference signatures, with source-colored dendrograms. |
| `run_cluster_catalogs.R` | Example driver for `cluster_catalogs()` showing how to cluster ID83 (COSMIC+Liu+Jin), ID89 (Koh+Liu), and ID476 (Liu) signatures. |
| `solve_bipartite_match.R` | Solves a bipartite flow-matching problem via quadratic programming (OSQP): given supply nodes A and target nodes B connected by edges, finds flows minimizing squared deviation from targets. |
| `subtract_known_component.R` | Subtracts a known signature component from a mixed spectrum, finding the maximum weight that keeps residuals above a small negative threshold. |
| `test-solve_bipartite_match.R` | Unit tests (testthat) for `solve_bipartite_match()`: checks feasibility, mass conservation, duplicate-edge handling, and error conditions. |
| `test_map_476_to_other.R` | Quick test script verifying that `t476_to_89()` and `t476_to_83()` produce expected results by comparing to saved reference files. |
