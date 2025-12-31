# Home Tab Component
# Note: Packages loaded via R/dependencies.R from ui.R

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
            "Small insertions and deletions, \"indels\", are mutations that 
            add or delete short (< ~50 base pairs) sequences of DNA.",
            style = h3_style
         ),
         h3(
            paste(
               "This web site presents signatures extracted from somatic indel mutations in 6,975 tumors from 32 cancer types using ",
               "methods based on non-negative factorization and hierarchical Dirichelet processes."
            ),
            style = h3_style
         ),
         h3(
            paste(
               "There is no single intuitive and naturally",
               "constrained classification of indel mutation types (as there arguably is for single base mutations)",
               "but two classifications have beeen useful."
            ),
            style = h3_style
         ),
         tags$ul(
            style = h3_style,
            tags$li(
               actionLink(
                  "home_goto_cosmic83",
                  tagList(
                     tags$strong("83-type classification scheme"),
                     " ",
                     icon("link", style = "font-size:14px;")
                  ),
                  style = "color:#3498db; text-decoration:none; cursor:pointer;"
               ),
               paste(
                  " This widely used classification scheme recognizes 83 types of indel.",
                  "Described in Alexandrov et al., 2020 and used on the COSMIC mutational signatures web site."
               )
            ),
            tags$li(
               actionLink(
                  "home_goto_koh89",
                  tagList(
                     tags$strong("89-type classification scheme:"),
                     " ",
                     icon("link", style = "font-size:14px;")
                  ),
                  style = "color:#3498db; text-decoration:none; cursor:pointer;"
               ),
               paste(
                  " This new indel classifiction scheme (Koh et al., 2025) recognizes 89 types of indel.",
                  "Often provides more informative granularity for indels in homopolymers (e.g. ATTTTTG → ATTTTG).",
                  "Koh et al., 2025 also present an even more granular classifiction of 476 types of indel."
               )
            )
         ), # tags$ul
         h3(
            "Algorithmic translation between signatures in the 83-type and 89-type 
               classification schemes is not possible. 
               However, this web site leverages tumor mutational spectra dominated by 
               individual signatures to exhaustively elucidate the correspondences between 
               signatures in the two classification schemes plus the 476-type classification scheme.",
            style = h3_style
         )
      )
   )
}
