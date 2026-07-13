# indelsig
# indelsig

**indelsig** is an interactive Shiny web browser for exploring small insertion and deletion (indel) mutational signatures in human cancer.

Online browser: [https://indelsig.net](https://indelsig.net)

## Overview

Small insertion and deletion mutations can reflect distinct mutational processes in cancer genomes. **indelsig** provides an interactive interface for browsing, visualizing, comparing, and analyzing indel mutational signatures across multiple classification systems.

The browser currently supports three indel classification schemes:

- **ID83**
- **ID89**
- **ID476**

Users can explore signature profiles, inspect integrated signature information, compare signatures across classification systems, and generate indel mutation count matrices from uploaded VCF files.

## Main Functions

### 1. Browse indel mutational signatures

The browser provides dedicated pages for different indel signature classification systems:

- **83-type classification**
- **89-type classification**
- **476-type classification**

Each page allows users to browse signature profiles and access detailed visualization pages for individual signatures.

### 2. View integrated signature information

The **Overview Table** summarizes integrated information across different indel signature systems.

Users can use this page to inspect signature names, related annotations, classification information, and links to detailed signature reports.

### 3. Search signatures

The **Search** page allows users to search signatures by name or related keywords.

This is useful for quickly locating a specific signature or a group of related signatures.

### 4. Upload and analyze VCF files

The **Upload & Analyze** module allows users to upload VCF files and generate indel mutation count matrices.

Supported input formats include:

- `.vcf`
- `.vcf.gz`

Users need to select the correct reference genome before running the analysis.

Supported reference genomes:

- **GRCh37 / hg19**
- **GRCh38 / hg38**

The analysis module can generate:

- ID83 mutation count matrix
- ID89 mutation count matrix
- ID476 mutation count matrix
- Annotated VCF-derived result files

### 5. Download results

After VCF analysis, users can download generated result files, including mutation count matrices and annotated outputs.

### 6. Submit feedback

The **About** page includes a feedback form where users can report:

- Bugs
- Data issues
- Visualization issues
- Suggestions
- Other comments

## Online Usage

The online version is available at:

[https://indelsig.net](https://indelsig.net)

No installation is required for online browsing.

The main navigation tabs include:

- **Home** — project introduction and entry points
- **Upload & Analyze** — VCF upload and indel matrix generation
- **89-type classification** — browse 89-type indel signatures
- **476-type classification** — browse 476-type indel signatures
- **83-type classification** — browse 83-type indel signatures
- **Overview Table** — integrated signature summary table
- **Search** — search signatures by name or keyword
- **About** — project information, contact details, and feedback form

## Local Installation

To run the Shiny app locally, clone this repository:

```bash
git clone https://github.com/XmWU123/indelsig.git
cd indelsig
```

Then start the Shiny app in R:

```r
shiny::runApp("indel_shiny_clean")
```

## R Package Requirements

The app requires the following R packages:

```r
install.packages(c(
  "shiny",
  "shinyjs",
  "shinydashboard",
  "readxl",
  "dplyr",
  "tidyr",
  "data.table",
  "ggplot2",
  "R.utils"
))
```

Additional packages used for mutational spectrum processing and plotting include:

```r
install.packages("remotes")

remotes::install_github("steverozen/mSigSpectra")
remotes::install_github("steverozen/mSigPlot")
```

Reference genome packages may be required for VCF annotation:

```r
install.packages("BiocManager")

BiocManager::install(c(
  "BSgenome.Hsapiens.1000genomes.hs37d5",
  "BSgenome.Hsapiens.UCSC.hg38"
))
```

## Input Notes for VCF Analysis

Before uploading a VCF file, users should make sure that:

1. The selected reference genome matches the VCF file.
2. The VCF contains small insertion and deletion variants.
3. The file is properly formatted as `.vcf` or `.vcf.gz`.
4. Multi-allelic or complex variants have been handled appropriately if needed.

Selecting the wrong reference genome may lead to incorrect sequence-context annotation.

## Repository Structure

```text
indelsig/
├── indel_shiny_clean/
│   ├── ui.R
│   ├── server.R
│   ├── Indel_process.R
│   ├── config.R
│   ├── deploy.R
│   ├── R/
│   ├── data/
│   ├── example_data/
│   ├── per_sig_txt/
│   ├── ui_components/
│   └── www/
├── LICENSE
├── README.md
└── indel-signature-browser.Rproj
```

Main directories:

- `indel_shiny_clean/R/`: supporting R functions
- `indel_shiny_clean/data/`: data files used by the app
- `indel_shiny_clean/example_data/`: example input files
- `indel_shiny_clean/per_sig_txt/`: per-signature text resources
- `indel_shiny_clean/ui_components/`: modular Shiny UI components
- `indel_shiny_clean/www/`: Shiny static web assets, including images, CSS files, logos, and static HTML reports

## Project Status

The current version includes:

- Online Shiny browser
- ID83, ID89, and ID476 signature browsing
- Integrated overview table
- Signature search page
- VCF upload and analysis module
- Downloadable matrix outputs
- Static detailed signature reports
- User feedback form

The project is under active development.

## Citation

If you use **indelsig** in published work, please cite the associated publication when available.

You may also cite this repository:

```text
Wu X, Liu M, Rozen SG. indelsig: an interactive browser for indel mutational signatures.
GitHub repository: https://github.com/XmWU123/indelsig
```

## Contact

For questions, suggestions, or collaboration opportunities, please contact:

- Xueming Wu: wuxm8523@gmail.com
- Mo Liu: lmliumo@foxmail.com
- Steve G. Rozen: steverozen@pm.me

## License

This project is licensed under the GNU General Public License v3.0. See the [LICENSE](LICENSE) file for details.

Copyright (C) 2026 Xueming Wu, Mo Liu, and Steve Rozen.
