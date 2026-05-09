# ui_components/analysis_tab.R

create_analysis_tab <- function() {
  tabPanel(
    title = "Upload & Analyze", 
    value = "analysis",         
    icon = icon("upload"),      
    
    # 1. 父容器：使用 align-items: stretch 让左右两列等高
    div(
      style = "display: flex; align-items: stretch; gap: 25px; margin-top: 30px; padding: 0 15px;",
      
      # 左侧控制面板
      div(
        style = "flex: 0 0 20%; max-width: 20%;", 
        
        # 内部支持弹性排版
        div(class = "upload-card", 
            style = "margin: 0 !important; height: 100%; display: flex; flex-direction: column;",
            
            # 1. Upload 部分
            h4(icon("file-alt"), " 1. Upload VCF File", 
               style = "margin-top: 0; font-size: 1.8rem; font-weight: 800;"), 
            
            p("Upload your variant calling file to generate Mutational Signatures.", 
              class = "text-muted", 
              style = "font-size: 1.35rem; line-height: 1.6; margin-bottom: 30px;"),
            
            div(style = "font-size: 1.6rem; margin-bottom: 25px;",
                fileInput("vcf_file", "Select File", accept = c(".vcf", ".txt"), width = "100%")
            ),
            
            div(style = "font-size: 1.6rem; margin-bottom: 15px;",
                selectInput("ref_genome", "Reference Genome", 
                            choices = c("hg19", "hg38"), 
                            selected = "hg19", width = "100%")
            ),
            
            # 虚线分隔
            hr(style = "border-top: 1px dashed #e0e0e0; margin-top: 30px; margin-bottom: 30px;"),
            
            # 2. Process 部分
            h4(icon("cogs"), " 2. Process", 
               style = "font-size: 1.6rem; font-weight: 700; margin-bottom: 20px;"),
            
            # 【核心修改】：用一个 div 把“运行按钮”和“下载区”包起来，统一推到底部
            # 使用 flex-direction: column 和 gap: 15px 让它们上下排列且中间有间隙
            div(style = "margin-top: auto; display: flex; flex-direction: column; gap: 15px; width: 100%;",
                
                # 运行按钮 (去掉了 margin-top: auto)
                actionButton("run_analysis_btn", "Run Analysis", 
                             class = "btn-primary", 
                             style = "font-size: 1.6rem; font-weight: bold; padding: 15px; width: 100%;",
                             icon = icon("play")),
                
                # 预留给下载按钮的位置（跑完分析后才会在这里渲染出按钮）
                uiOutput("download_ui")
            )
        )
      ),
      
      # 右侧结果展示面板
      div(
        style = "flex: 1; min-width: 0;", 
        div(class = "result-card", style = "width: 100%; margin: 0 !important; height: 100%;",
            tabsetPanel(
              id = "analysis_result_tabs", 
              tabPanel("83 Type", 
                       br(), 
                       plotOutput("plot_id83", height = "480px", width = "100%")
              ),
              tabPanel("89 Type", 
                       br(), 
                       plotOutput("plot_id89", height = "480px", width = "100%")
              ),
              tabPanel("476 Type",
                       br(), 
                       plotOutput("plot_id476", height = "480px", width = "100%")
              )
            )
        )
      )
    )
  )
}