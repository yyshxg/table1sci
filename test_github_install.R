# 安装必要的包
if (!require("devtools")) install.packages("devtools")

# 从GitHub安装table1sci包（二进制安装，不需要Rtools）
remotes::install_github("yyshxg/table1sci", 
                        build_vignettes = FALSE, 
                        build_manual = FALSE)

# 加载包
library(table1sci)

# 创建示例数据
set.seed(123)
data <- data.frame(
    age = rnorm(100, mean = 60, sd = 10),
    sex = factor(sample(c("Male", "Female"), 100, replace = TRUE)),
    bmi = rnorm(100, mean = 24, sd = 3),
    group = factor(sample(c("Treatment", "Control"), 100, replace = TRUE))
)

# 设置变量标签
var_labels <- c(
    age = "Age (years)",
    sex = "Sex",
    bmi = "BMI (kg/m²)"
)

# 生成Table 1
result <- table1_sci(
    data = data,
    vars = c("age", "sex", "bmi"),
    group = "group",
    var_labels = var_labels,
    show_test_stats = TRUE
)

# 打印结果
print(result)

# 导出为Excel
table1_sci_export(result, "github_install_test.xlsx")

cat("\n安装和测试完成！请检查生成的Excel文件。\n") 