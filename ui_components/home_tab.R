# Home Tab Component
create_home_tab <- function() {
  tabPanel(
    "Home",
    icon = icon("house"),
    div(class = "img-container",
        h1("Small Insertions and Deletions (ID) Signatures",
           style = "color:#2c3e50; font-weight:700; margin-bottom:25px;"),
        h3("Small insertions and deletions (ID), also known as indels, are defined as the incorporation or loss of small fragments of DNA (usually between 1 and 50 base pairs) in a specific genomic location.Although there is no single intuitive and naturally constrained set of ID mutation types (as there arguably are for single base substitutions and doublet base substitutions), a compilation of 83 different types considering size, nucleotides affected and presence on repetitive and/or microhomology regions was used to extract mutational signatures. It can be found here.Click on any signature below to learn more about its details..",
           style = "color:#34495e; font-size:18px; line-height:1.8; margin-bottom:20px;"),
        h3("Signature extraction methodsWith a few exceptions, the current set of reference signatures were extracted using SigProfiler (as described in Alexandrov, L.B. et al., 2020) from the 2,780 whole-genome variant calls produced by the ICGC/TCGA Pan Cancer Analysis of Whole Genomes (PCAWG) Network. The stability and reproducibility of the signatures were assessed on somatic mutations from an additional 1,865 whole genomes and 19,184 exomes. All input data and references for original sources are available from synapse.org ID syn11801889.",
           style = "color:#34495e; font-size:18px; line-height:1.8; margin-bottom:20px;"),
        h3("Please select a group from the navigation bar above.",
           style = "color:#3498db; font-size:20px; margin-top:25px; font-weight:600;")
    )
  )
}
