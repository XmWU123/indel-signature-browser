library(shiny)
library(shinydashboard)
library(shinyjs)


# ---------------- 数据定义 ----------------
df <- read.delim("/home/wuxueming/shinyapp/ID89_ID83_connection_example.txt",
                 header = TRUE, stringsAsFactors = FALSE)
# 读取 476 数据
df1 <- read.csv("/home/wuxueming/shinyapp/mSigHdp.indel476.final.signatures.csv",
                header = TRUE, check.names = FALSE, stringsAsFactors = FALSE,row.names = NULL)

# 提取 476 的列名（签名名）
ID476_list <- colnames(df1)

all_pngs <- list.files("/home/wuxueming/shinyapp/www", pattern = "\\.png$", full.names = FALSE)
# 初始化列表
signature_groups <- list()

# 循环每行构建每个 InDel89 的图片信息
for (i in 1:nrow(df)) {
  ID89 <- df$InDel89[i]
  ID83 <- df$InDel83[i]
  ID476 <- ID476_list[i]
  
  # 生成 89 的四张图
  imgs <- paste0(ID89, c("_signature.89spectrum.png","_89spectrumA.png","_89spectrumB.png","_89spectrumC.png"))
  # 生成 83 的两张图
  id83_imgs <- paste0(ID89, "_", ID83, c("_83all.png", "_83filtered.png"))
  # 匹配 insdel 对应的 all.png 图
  id476_imgs <- grep(paste0("^", ID89, "_476all.*\\.png$"), 
                     all_pngs, 
                     value = TRUE, 
                     ignore.case = TRUE)
  print(paste("🔍 Checking:", ID476, "→", paste(id476_imgs, collapse = ",")))
  # ✅ 只在 ID89 不为空时才加入列表
  if (ID89 != "" && (length(imgs) > 0 || length(id83_imgs) > 0 || length(id476_imgs) > 0)) {
    signature_groups[[ID89]] <- list(
      imgs = imgs,
      id83 = id83_imgs,
      id476 = id476_imgs
    )
  } 
}
# 手动加载 InsDel39 的图片
signature_groups[["InsDel39"]] <- list(
  imgs = c("InsDel39_signature.89spectrum.png",
           "InsDel39_89spectrumA.png",
           "InsDel39_89spectrumB.png",
           "InsDel39_89spectrumC.png"),
  id83 = NULL,
  desc = "insdel39 does not have corresponding ID83 signature"
)

# 检查结果
str(signature_groups)

# ---------------- UI ----------------
ui <- dashboardPage(
  dashboardHeader(title = "Indel Signature Browser"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Signature Browser", tabName = "browser", icon = icon("dna")),
                # ✅ 新增复选框控制显示哪些图
                checkboxGroupInput(
                  inputId = "show_types",
                  label = "Select signature types to display:",
                  choices = c("ID89", "ID83", "ID476"),
                  selected = c("ID89", "ID83", "ID476")
                )
    )
  ),
  dashboardBody(
    useShinyjs(),
    uiOutput("signature_display")
  )
 )


# ---------------- Server ----------------
server <- function(input, output, session){
  current_group <- reactiveVal(NULL)
  
  # ---------------- 动态生成页面 ----------------
  output$signature_display <- renderUI({
    if (is.null(current_group())) {
      # 列表页面
      fluidRow(
        lapply(names(signature_groups), function(group_name){
          sig <- signature_groups[[group_name]]
          # 取第一张图片作为缩略图
          thumb <- sig$imgs[1]
          box(width = 6, title = group_name, solidHeader = TRUE,
              # 缩略图
              if (!is.null(thumb)) {
                tags$img(
                  src = thumb,
                  style = "width:100%; max-width:200px; height:auto; display:block; margin-bottom:10px; border:1px solid #ccc;"
                )
              },
              # 描述
              p(sig$desc),
              # 按钮
              actionButton(paste0("show_", group_name), "View Details", class = "btn-primary")
          )
        })
      )
    } else {
      # 详情页面
      sig <- signature_groups[[current_group()]]
      
      # ✅ 根据侧边栏复选框筛选显示的图片类型
      show_types <- input$show_types
      # 初始化
      all_imgs <- c()
      
      # 仅在非空时添加
      if ("ID89" %in% show_types && !is.null(sig$imgs) && length(sig$imgs) > 0) {
        all_imgs <- c(all_imgs, setNames(sig$imgs, rep("ID89", length(sig$imgs))))
      }
      if ("ID83" %in% show_types && !is.null(sig$id83) && length(sig$id83) > 0) {
        all_imgs <- c(all_imgs, setNames(sig$id83, rep("ID83", length(sig$id83))))
      }
      if ("ID476" %in% show_types && !is.null(sig$id476) && length(sig$id476) > 0) {
        all_imgs <- c(all_imgs, setNames(sig$id476, rep("ID476", length(sig$id476))))
      }
      
      
      img_sizes <- list(
        "ID89"  = "width:1000px; max-width:600px; height:auto; border:1px solid #ccc; cursor:pointer;",
        "ID83"  = "width:1000px; max-width:600px; height:auto; border:1px solid #ccc; cursor:pointer;",
        "ID476" = "width:2000px; max-width:1200px; height:auto; border:2px solid #ccc; cursor:pointer;"
      )
      
      tagList(
        # ✅ 返回按钮
        actionButton("back_to_list", "← Back", class = "btn btn-secondary mb-3"),
        h3(current_group()),
        hr(),
        fluidRow(
          lapply(seq_along(all_imgs), function(i){
            img_file <- all_imgs[i]
            label_type <- names(all_imgs)[i]
            
            column(
              width = 6,
              div(
                style = "position: relative; text-align: center; margin-bottom:20px;",
                tags$img(
                  src = img_file,
                  style = img_sizes[[label_type]],
                  onclick = sprintf("Shiny.setInputValue('%s', Math.random());", paste0("img_", img_file))
                ),
                tags$div(
                  label_type,
                  style = "position: absolute; top: 5px; right: 5px; 
                     background-color: rgba(0,0,0,0.5); color: white; 
                     padding: 2px 6px; border-radius: 4px; font-weight: bold;"
                )
              )
            )
          })
        )
      )
    }
  })
  
  # 点击“View Details”显示组详情
  lapply(names(signature_groups), function(group_name){
    observeEvent(input[[paste0("show_", group_name)]], {
      current_group(group_name)
    })
  })
  
  # ✅ 返回列表
  observeEvent(input$back_to_list, {
    current_group(NULL)
  })
  
  # 点击图片弹出高清图
  observe({
    req(current_group())
    sig <- signature_groups[[current_group()]]
    all_imgs <- c(sig$imgs, sig$id83, sig$id476)
    
    lapply(all_imgs, function(img){
      img_id <- paste0("img_", img)
      observeEvent(input[[img_id]], {
        showModal(modalDialog(
          title = paste("Full-size view:", img),
          easyClose = TRUE,
          size = "l",
          footer = NULL,
          tags$img(src = img, style = "width:100%; height:auto;")
        ))
      })
    })
  })
}

# ---------------- Run App ----------------
shinyApp(ui, server)
