# Home Tab Component
library(shiny)
library(shinyjs)

h3_style = "color:#34495e; font-size:18px; line-height:1.8; margin-bottom:20px;"

create_home_tab <- function() {
   tabPanel(
      "Home",
      icon = icon("house"),
      div(
         class = "img-container",
         h1(
            "Mutational Signatures of Small Insertions and Deletions",
            style = "color:#2c3e50; font-weight:700; margin-bottom:25px;"
         ),
         h3(
            paste(
               "Small insertions and deletions, \"indels\" for short, are mutations that ",
               "add or delete small sequences of DNA (conventionally < ~50 base pairs long).",
               "There is no single intuitive and naturally",
               "constrained classification of indel mutation types (as there arguably is for single base mutations)",
               "but two classifications are useful and widely used:"
            ),
            style = h3_style
         ),
         tags$ul(
            style = h3_style,
            tags$li(
               tags$a(
                  tags$strong("COSMIC83:"),
                  " ",
                  icon("link", style = "font-size:14px;"),
                  href = "#",
                  onclick = "Shiny.setInputValue('home_goto_cosmic83', Math.random(), {priority: 'event'}); return false;",
                  style = "color:#3498db; text-decoration:none; cursor:pointer; transition: color 0.3s;"
               ),
               paste(
                  " The most widely used classification, which classifies indels into 83 types.",
                  "Described in Alexandrov et al., 2020 and used on the COSMIC mutational signatures web site."
               )
            ),
            tags$li(
               tags$a(
                  tags$strong("Koh89:"),
                  " ",
                  icon("link", style = "font-size:14px;"),
                  href = "#",
                  onclick = "Shiny.setInputValue('home_goto_koh89', Math.random(), {priority: 'event'}); return false;",
                  style = "color:#3498db; text-decoration:none; cursor:pointer; transition: color 0.3s;"
               ),
               paste(
                  " New indel classifiction scheme (Koh et al., 2025), which classifies indels into 89 types.",
                  "Provides more informative granularity for indels in homopolymers (e.g. ATTTTTG → ATTTTG).",
                  "Koh et al., 2025 also present an even more granular classifiction of 476 mutation types."
               )
            )
         ),
         h3(
            paste(
               "With a few exceptions, the reference signatures on this web site were extracted using both",
               "SigProfiler (as described in Alexandrov et al., 2020) and mSigHdp (Liu et al., 2023) from the",
               "2,780 whole-genome from ICGC/TCGA Pan Cancer Analysis of Whole Genomes (PCAWG) Network and",
               ">4,000 whole-genome from Hartwig Medical Foundation."
            ),
            style = h3_style
         )
      )
   )
}
