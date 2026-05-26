# 告诉 rsconnect 生成清单时使用 3.19 版本的源
options(repos = BiocManager::repositories(version = "3.19"))

rsconnect::setAccountInfo(
  name='indelsigbrowser', 
  token='9D661811D9FB6F8A6631117C8CBB002B', 
  secret='TsiRN70KjElM1sW/8zaHlWAJBhrrKw46z7awgOTA'
)

rsconnect::deployApp(appName="main", forceUpdate=TRUE)
