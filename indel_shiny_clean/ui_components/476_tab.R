# 476-type Browser Tab Component
create_476_tab <- function() {
  tabPanel(
    "476-type classification",       # 用户看到的标题
    value = "476-type classification", # Server 端跳转用的 ID (Home页链接需要用到这个)
    icon = icon("border-all"),       # 使用 "border-all" 图标表示更高密度的分类
    # 主内容占位符 (需要在 server.R 中定义 output$id476_display)
    uiOutput("id476_display")
   )
}