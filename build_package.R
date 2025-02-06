# 安装必要的包
if (!require("devtools")) install.packages("devtools")
if (!require("roxygen2")) install.packages("roxygen2")
if (!require("openxlsx")) install.packages("openxlsx")
if (!require("knitr")) install.packages("knitr")

# 设置工作目录
setwd("D:/学习")

# 生成文档
devtools::document("table1sci")

# 构建源码包
pkg_file <- devtools::build("table1sci")

# 输出包的位置
cat("\n源码包已生成：", pkg_file, "\n")

# 检查包（可选）
devtools::check("table1sci") 