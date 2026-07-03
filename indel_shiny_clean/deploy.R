# 锁定云端 3.19 源
options(bioconductor.version = "3.19")
options(repos = BiocManager::repositories(version = "3.19"))

# 👇 核心：替换成你刚刚拿到的全新钥匙 👇
rsconnect::setAccountInfo(
  name='indelsigbrowser', 
  token='995555ABA1FC658CF6B08D5FA1481595', 
  secret='6eMMno3l/j/6llJLalq+xPCdnU47Iyds1ItVMswI'
)

# 启动部署
rsconnect::deployApp(appName="main", forceUpdate=TRUE)

#每次运行 Rscript deploy.R