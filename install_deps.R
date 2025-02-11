# 设置CRAN镜像
options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))

# 安装必要的包
if (!require("tableone")) install.packages("tableone")
if (!require("dplyr")) install.packages("dplyr")

# 检查安装结果
installed_packages <- installed.packages()[,"Package"]
required_packages <- c("tableone", "dplyr")

cat("\n检查安装结果：\n")
for (pkg in required_packages) {
    if (pkg %in% installed_packages) {
        cat(sprintf("%s: 已安装成功\n", pkg))
    } else {
        cat(sprintf("%s: 安装失败\n", pkg))
    }
} 