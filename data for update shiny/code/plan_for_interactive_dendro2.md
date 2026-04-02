# Interactive Dendrogram v2: Seed-filtered, all signatures

## Context
The v1 `interactive_dendrogram.qmd` only loads 4 old-format CAP9 files per dataset type. The signature files have been reorganized into `Manuscript_data/Mo_CAP9_analysis/Signatures/Koh89/` and `Koh476/` subdirs with a new naming scheme (28 files each). The new version should load ALL signature files, allow the user to pick a "seed" Liu signature, and show only signatures with cosine similarity >= threshold to that seed.

## New files
- `code/interactive_dendro2.qmd` — new Quarto Shiny document
- `code/dendro2_helpers.R` — new helper file (don't modify existing helpers)

## Input file naming scheme
Files: `{CAP9,NoCAP}.mSigHdp.{Hartwig,PCAWG,PH}.{Koh89,Koh476}.{cancertype}.txt`
- No row names column; columns are `hdp.1`, `hdp.2`, etc.
- `PH` = PCAWG + Hartwig combined
- Cancertype can contain dots (e.g., `Bone.SoftTissue`)
- Liu reference: `Manuscript_data/Liu_et_al_final_{89,476}_type_signatures.tsv` (has row names column)

## Signature label construction
Parse filename into parts: cap (`CAP9`→`C`, `NoCAP`→`N`), dataset (`Hartwig`→`H`, `PCAWG`→`P`, `PH`→`PH`), cancertype (everything between Koh89/Koh476 and `.txt`). Strip `hdp.` from column names.

Label format: `{cap}.{dataset}.{cancertype}.{signum}`
Examples:
- `C.PH.Breast.7` — CAP9, PCAWG+Hartwig, Breast, sig 7
- `C.H.All.3` — CAP9, Hartwig only, All cancertypes, sig 3
- `N.H.All.2` — NoCAP, Hartwig, All, sig 2
- `C.PH.Bone.SoftTissue.10` — cancertype with dot preserved

Liu signatures keep their original names (e.g., `InsDel1a`).

## Source groups for coloring
- `Liu` — red (#FF0000)
- `C.H` — CAP9 Hartwig-only
- `C.P` — CAP9 PCAWG-only
- `C.PH` — CAP9 PCAWG+Hartwig
- `N.H` — NoCAP Hartwig

Use 5 distinct colors. Liu stays red.

## `dendro2_helpers.R` functions

### `rename_sigs2(file_path)`
Parse the new naming scheme. Extract cap/dataset/cancertype from filename. Build column names as `{cap}.{dataset}.{cancertype}.{signum}`. Determine source_group as `{cap}.{dataset}`. Return data.frame with `source_group` attribute.

Parsing: strip `.txt`, split on `.mSigHdp.` to get cap prefix and remainder. From remainder, find `Koh89` or `Koh476` to split dataset from cancertype. Everything after `Koh{89,476}.` is the cancertype.

### `load_all_signatures(dataset_type, sig_dir, data_dir)`
- `dataset_type`: `"Koh89"` or `"Koh476"`
- Glob all `*.txt` files from `sig_dir/{dataset_type}/`
- Call `rename_sigs2()` on each, filter to matching row counts
- Read Liu reference file
- Return `list(combined, source_vec)` — same structure as v1

### `filter_by_seed(combined, seed_name, min_cos_sim)`
- Compute cosine similarity of all signatures to the seed signature
- Return subset of `combined` keeping only columns with similarity >= `min_cos_sim` (always include the seed itself)

### `compute_dendrogram(combined)` — same as v1

### `build_plotly_dendrogram(dend_data, source_vec, colors)` — same as v1

## `interactive_dendro2.qmd` structure

### Setup chunk (context: setup)
- Load libraries, source `dendro2_helpers.R`
- Pre-load both full datasets: `all_data[["Koh89"]]`, `all_data[["Koh476"]]`
- Read Liu signature names for seed dropdown

### Controls (panel: input)
- `selectInput("dataset")` — Koh89 or Koh476
- `selectInput("seed_signature")` — dynamically populated from Liu sigs for selected dataset
- `numericInput("cosine_sim_to_seed", value = 0.8, min = 0, max = 1, step = 0.05)`
- `actionButton("update_dendro", "Update Dendrogram")` — recompute on click (not on every param change, since hclust is slow)
- `uiOutput("selection_display")` — badges as in v1

### Dendrogram section
- `plotlyOutput("dendrogram", height = "400px", width = "100%")`
- Server: on `update_dendro` click (or dataset/seed change), filter signatures, compute dendrogram, render plotly
- Use `eventReactive` tied to `update_dendro` button for the filtered+clustered data
- Click handling same as v1

### Signature profiles section
- Same as v1: `plotOutput` with dynamic height, stacked mSigPlot plots

### Cosine similarity section
- Same as v1: plotly heatmap of selected signatures

## Key design decisions
- **Reactive flow**: Full dataset loaded at startup. Filtering + dendrogram computed only when "Update Dendrogram" is clicked (via `eventReactive`). This avoids recomputing on every slider tweak.
- **Seed dropdown**: Populated from Liu reference column names, updates when dataset changes.
- **No re-render loop**: Dendrogram render depends on the `eventReactive` result, not on `selected_sigs()`.
- **Wide layout**: Same CSS approach as v1 (`tags$style`).

## Verification
1. `quarto serve code/interactive_dendro2.qmd`
2. Select Koh89, pick a seed signature (e.g., `InsDel1a`), set threshold 0.8, click Update
3. Verify dendrogram shows only similar signatures
4. Lower threshold to 0.5 — more signatures should appear
5. Click leaves, verify signature plots and cosine heatmap
6. Switch to Koh476, verify seed dropdown updates
7. Check labels include cancertype (e.g., `C.PH.Breast.7`)
