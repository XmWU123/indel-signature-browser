create_about_tab <- function() {
  tabPanel(
    title = "About",
    icon = icon("circle-info"),
    
    # 2. 悬浮的白色大卡片
    div(
      class = "about-paper-card",
      style = "margin-top: 20px !important; padding-top: 20px !important;",
      
      # --- Section: Introduction ---
      h3("Project Overview", class = "about-section-title",
         style = "color: #2980b9; border-bottom-color: #2980b9; margin-top: 0 !important; padding-top: 0 !important;"),
      
      p(class = "about-text",
        "Indel mutational signatures provide crucial insights into the mutational processes operative in human cancer. ",
        "While single base substitution (SBS) signatures have been extensively cataloged, the classification and analysis of small insertions and deletions (indels) present unique challenges due to their complexity."
      ),
      p(class = "about-text",
        "This web application serves as a comprehensive interactive browser for exploring indel signatures derived from ",
        tags$strong("6,975 tumors across 36 cancer types"), ". It integrates multiple classification schemes (83-type, 89-type, and 476-type) to provide multi-granular perspectives."
      ),
      
      # --- Section: Key Features (Icon Grid) ---
      h3("Key Features", class = "about-section-title",
         style = "color: #d35400; border-bottom-color: #d35400;"),
      
      div(
        class = "about-feature-grid",
        
        # Feature 1 (删除了 Seamlessly)
        div(class = "about-feature-item",
            icon("layer-group", class = "about-icon"),
            h4("Multi-Granularity", style="font-weight:700; margin-top:0;"),
            p("Switch between 83, 89, and 476 classification systems to view data at different resolutions.")
        ),
        
        # Feature 2
        div(class = "about-feature-item",
            icon("chart-pie", class = "about-icon"),
            h4("Interactive Visualization", style="font-weight:700; margin-top:0;"),
            p("High-resolution, interactive plots for signature profiles and sample decomposition analysis.")
        ),
        
        # Feature 3
        div(class = "about-feature-item",
            icon("database", class = "about-icon"),
            h4("Comprehensive Data", style="font-weight:700; margin-top:0;"),
            p("Based on a robust dataset of somatic mutations processed with non-negative matrix factorization and Hierarchical Dirichlet Processes.")
        )
      ),
      
      # --- Section: Methodology (已修正 DOI) ---
      h3("Methodology", class = "about-section-title",
         style = "color: #8e44ad; border-bottom-color: #8e44ad;"),
      
      p(class = "about-text",
        "Mutational signatures were extracted using the Hierarchical Dirichlet Process (HDP) framework via the R package ",
        tags$a(href = "https://github.com/steverozen/mSigHdp", target = "_blank", style = "color: #8e44ad; font-weight: bold;", "mSigHdp"),
        " (Liu et al., 2023, ", 
        # 👇 修改点 1：更新了 HDP 的正确 DOI 和跳转链接
        tags$a(href = "https://doi.org/10.1093/nargab/lqad005", target = "_blank", style = "color: #8e44ad;", "DOI: 10.1093/nargab/lqad005"),
        ") and ",
        tags$a(href = "https://github.com/AlexandrovLab/SigProfilerExtractor", target = "_blank", style = "color: #8e44ad; font-weight: bold;", "SigProfilerExtractor"),
        " (Islam et al., 2022, ",
        # 👇 修改点 2：确认保留正确的 SigProfilerExtractor DOI
        tags$a(href = "https://doi.org/10.1016/j.xgen.2022.100179", target = "_blank", style = "color: #8e44ad;", "DOI: 10.1016/j.xgen.2022.100179"),
        "). The classification schemes were developed to capture specific biological phenomena, such as repeat-mediated mutagenesis and microhomology-mediated deletions."
      ),
      
      # --- Section: Contact ---
      h3("Contact Us", class = "about-section-title",
         style = "color: #16a085; border-bottom-color: #16a085;"),
      
      p(class = "about-text",
        "We welcome feedback, questions, and collaboration opportunities. Please direct your inquiries to:"
      ),
      
      div(style = "background: #f8f9fa; padding: 25px; border-radius: 12px; border-left: 5px solid #3498db; margin-top: 20px;",
          tags$ul(style = "list-style: none; padding-left: 10px; margin-bottom: 0; font-size: 1.75rem; line-height: 2.2; color: #2c3e50;",
                  
                  tags$li(icon("envelope", style="margin-right:12px; color:#16a085;"), 
                          strong("Xueming Wu: "), "wuxm8523@gmail.com"),
                  
                  tags$li(icon("envelope", style="margin-right:12px; color:#16a085;"), 
                          strong("Mo Liu : "), "lmliumo@foxmail.com"),
                  
                  tags$li(icon("envelope", style="margin-right:12px; color:#16a085;"), 
                          strong("Steve.G.Rozen: "), "steverozen@pm.me")
          )
      ),
      
      # 底部 GitHub 按钮
      div(style = "text-align: center; margin-top: 40px;",
          a(href = "https://github.com/XmWU123/indel-signature-browser", target = "_blank",
            class = "btn btn-default btn-lg", 
            icon("github"), " View Project on GitHub", 
            style = "border-radius: 50px; padding: 12px 35px; border: 1px solid #bdc3c7; color: #555; transition: all 0.3s;")
      )
    )
  )
}