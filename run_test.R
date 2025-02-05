# 设置工作目录
setwd("D:/学习/table1sci")

# 安装必要的包
if (!require("devtools")) install.packages("devtools")
if (!require("roxygen2")) install.packages("roxygen2")
if (!require("openxlsx")) install.packages("openxlsx")

# 生成文档并构建包
devtools::document()
devtools::load_all()

# 运行测试脚本
source("test_example.R") 