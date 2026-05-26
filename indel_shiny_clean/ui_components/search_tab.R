create_search_tab <- function(all_sig_names = NULL) {
  tabPanel(
    "Search",
    icon = icon("search"),
    div(class = "search-page-container",
        div(class = "search-box-large",
            h1(class = "search-title", "Search Signatures"),
            # 👇 在这里直接用内联 CSS 把字号放大 (比如 1.8rem)，可以自己微调数字
            p(class = "search-subtitle", 
              style = "font-size: 2.1rem; font-weight: 500; color: #7f8c8d; margin-top: 15px;", 
              "Enter a signature name (e.g., 'InsDel') to find matches"),
            
            div(class = "search-input-container",
                div(style = "flex-grow: 1;", 
                    selectizeInput(
                      inputId = "search_input",
                      label = NULL, 
                      choices = NULL, 
                      width = "100%",
                      options = list(
                        placeholder = "Type signature name...",
                        maxOptions = 100, # 显示更多候选项
                        create = TRUE,    # 允许自由输入，不必非要选下拉框
                        persist = FALSE   # 搜索后不保留刚才自己造的词
                      )
                    )
                ),
                actionButton("search_btn", "Search", icon = icon("search"), class = "search-btn-large")
            )
        )
    )
  )
}