create_home_tab <- function() {
   tabPanel(
      title = "Home",
      icon = icon("house"),
      
      # -----------------------------------------------------------
      # 1. Hero Section
      # -----------------------------------------------------------
      div(
         class = "hero-section",
         h1(
            tagList(
               tags$i(class = "fa-solid fa-dna logo-icon"), 
               "Mutational Signatures of Small Insertions and Deletions"
            ),
            class = "hero-title"
         )
      ), 
      
      # -----------------------------------------------------------
      # 2. Main Content Container
      # -----------------------------------------------------------
      div(
         class = "container",
         style = "max-width: 1200px; padding-bottom: 50px;",
         
         # --- Context Section (背景介绍) ---
         div(
            class = "intro-text-section",
            
            # 第一段：数据来源 + Indel 定义 (合并到一个 HTML paste0 中，避免括号混乱)
            p(class = "intro-lead",
              HTML(paste0(
                 "Small insertions and deletions, \"indels\", are mutations that add or delete short (< ~50 base pairs) sequences of DNA.",
                 "<br><br>",
                 "This web site presents signatures extracted from somatic indel mutations in ",
                 "<span class='data-badge'>6,975 tumors</span> from ",
                 "<span class='data-badge'>32 cancer types</span> using methods based on ",
                 "<strong>Non-negative Matrix Factorization</strong><sup>1</sup> and <strong>Hierarchical Dirichlet Processes</strong><sup>2</sup>."
              ))
            ),
            
            hr(style = "border-top: 1px solid #eee; margin: 50px 0;")
         ), # <--- Intro Section 结束，加逗号
         
         # --- Cards Section Header ---
         h3("Explore Classification Schemes", 
            style = "text-align: center; font-weight: 700; color: #2c3e50; margin-bottom: 40px; text-transform: uppercase; font-size: 1.2rem; letter-spacing: 1px;"), # <--- 加逗号
         
         # --- Cards Grid ---
         fluidRow(
            # Card 1: 83-type
            column(4,
                   div(
                      class = "feature-card card-83",
                      onclick = "$('#home_goto_83').click();",
                      div(icon("cubes"), class = "card-icon-large", style = "color: #27ae60;"),
                      h3("83-Type Scheme", style = "font-weight: 700; color: #2c3e50; margin-top: 0;"),
                      p("This widely used classification scheme recognizes 83 types of indel. Described in Alexandrov et al., 2020 and used on the COSMIC mutational signatures web site.", 
                        style = "color: #7f8c8d; min-height: 60px; margin-bottom: 15px;"), 
                      div("Explore", class = "card-btn-fake"),
                      actionLink("home_goto_83", "", style = "display: none;") 
                   )
            ),
            # Card 2: 89-type
            column(4,
                   div(
                      class = "feature-card card-89",
                      onclick = "$('#home_goto_89').click();",
                      div(icon("dna"), class = "card-icon-large", style = "color: #e67e22;"),
                      h3("89-Type Scheme", style = "font-weight: 700; color: #2c3e50; margin-top: 0;"),
                      p("This new indel classification scheme (Koh et al., 2025) recognizes 89 types of indel. Often provides more informative granularity for indels in homopolymers.", 
                        style = "color: #7f8c8d; min-height: 60px; margin-bottom: 15px;"),
                      div("Explore", class = "card-btn-fake"),
                      actionLink("home_goto_89", "", style = "display: none;")
                   )
            ),
            # Card 3: 476-type
            column(4,
                   div(
                      class = "feature-card card-476",
                      onclick = "$('#home_goto_476').click();",
                      div(icon("microscope"), class = "card-icon-large", style = "color: #9b59b6;"),
                      h3("476-Type Scheme", style = "font-weight: 700; color: #2c3e50; margin-top: 0;"),
                      p("Koh et al., 2025 also present an even more granular classification of 476 types of indel. This scheme provides the highest resolution for analyzing complex patterns.", 
                        style = "color: #7f8c8d; min-height: 60px; margin-bottom: 15px;"),
                      div("Explore", class = "card-btn-fake"),
                      actionLink("home_goto_476", "", style = "display: none;")
                   )
            )
         ), # <--- fluidRow 结束，加逗号
         
         # -----------------------------------------------------------
         # 4. Note Section (简约蓝白样式)
         # -----------------------------------------------------------
         div(
            class = "note-box",
            
            # 标题
            h4(class = "note-title", icon("circle-info"), " Note on Translation"), # <--- 逗号
            
            # 第一段
            p(class = "note-content",
              HTML("There is no single intuitive and naturally constrained classification of indel mutation types (as there arguably is for single base mutations), but <strong>two classifications have been useful</strong>.")
            ), # <--- 逗号
            
            # 第二段
            p(class = "note-content",
              "Algorithmic translation between signatures in the 83-type and 89-type classification schemes is not possible. However, this web site leverages tumor mutational spectra dominated by individual signatures to exhaustively elucidate the correspondences between signatures in the two classification schemes plus the 476-type classification scheme."
            ), # <--- 逗号
            
            # 底部注脚 (引用)
            div(style = "margin-top: 20px; color: #7f8c8d; font-size: 1.3rem; font-weight: 500; line-height: 1.6;", # <--- 修正: font-style 改为 font-weight
                HTML("<sup>1</sup> Islam, S.M.A., et al. <strong>Uncovering novel mutational signatures by de novo extraction with SigProfilerExtractor.</strong> Cell Genomics (2022).<br>"),
                HTML("<sup>2</sup> Liu, M., et al. <strong>mSigHdp: hierarchical Dirichlet process mixture modeling for mutational signature discovery.</strong> NAR Genomics and Bioinformatics (2023).")
            ) 
         ) 
         
      ) 
   ) 
} 