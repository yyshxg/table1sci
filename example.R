# 加载包
library(table1sci)

#--------------------
# 基本用法示例
#--------------------

# 创建示例数据
set.seed(123)
n <- 200
data <- data.frame(
    age = rnorm(n, mean = 60, sd = 10),
    sex = factor(sample(c("Male", "Female"), n, replace = TRUE)),
    bmi = rnorm(n, mean = 24, sd = 3),
    group = factor(sample(c("Treatment", "Control"), n, replace = TRUE))
)

# 生成基本的Table 1
table1 <- table1_sci(data, vars = c("age", "sex", "bmi"), group = "group")
print(table1)

#--------------------
# 高级用法示例
#--------------------

# 创建更复杂的数据集
data_advanced <- data.frame(
    age = rnorm(n, mean = 60, sd = 10),
    sex = factor(sample(c("Male", "Female"), n, replace = TRUE)),
    bmi = rnorm(n, mean = 24, sd = 3),
    ldl = exp(rnorm(n, log(100), 0.4)),
    sbp = rgamma(n, shape = 100, rate = 0.7),
    smoking = factor(sample(c("Never", "Former", "Current"), n, replace = TRUE)),
    diabetes = factor(sample(c("Yes", "No"), n, replace = TRUE)),
    group = factor(sample(c("Treatment", "Control"), n, replace = TRUE))
)

# 设置变量标签
var_labels <- c(
    age = "Age (years)",
    sex = "Sex",
    bmi = "BMI (kg/m²)",
    ldl = "LDL Cholesterol (mg/dL)",
    sbp = "Systolic Blood Pressure (mmHg)",
    smoking = "Smoking Status",
    diabetes = "Diabetes"
)

# 生成高级Table 1
table1_advanced <- table1_sci(
    data = data_advanced,
    vars = c("age", "sex", "bmi", "ldl", "sbp", "smoking", "diabetes"),
    group = "group",
    var_labels = var_labels,
    digits = 1,
    p_digits = 3,
    adjust_method = "fdr",  # 使用FDR方法进行多重比较校正
    show_test_stats = TRUE
)

# 打印结果
print(table1_advanced)

#--------------------
# 导出为Excel文件
#--------------------

# 创建Excel工作簿
wb <- openxlsx::createWorkbook()

# 添加基本示例工作表
openxlsx::addWorksheet(wb, "Basic Example")
openxlsx::writeData(wb, "Basic Example", table1)

# 添加高级示例工作表
openxlsx::addWorksheet(wb, "Advanced Example")
openxlsx::writeData(wb, "Advanced Example", table1_advanced)

# 设置样式
hs <- openxlsx::createStyle(
    textDecoration = "bold",
    border = "bottom",
    borderStyle = "thin"
)

# 应用样式到表头
openxlsx::addStyle(wb, "Basic Example", hs, rows = 1, cols = 1:ncol(table1))
openxlsx::addStyle(wb, "Advanced Example", hs, rows = 1, cols = 1:ncol(table1_advanced))

# 自动调整列宽
openxlsx::setColWidths(wb, "Basic Example", cols = 1:ncol(table1), widths = "auto")
openxlsx::setColWidths(wb, "Advanced Example", cols = 1:ncol(table1_advanced), widths = "auto")

# 保存文件
openxlsx::saveWorkbook(wb, "table1_examples.xlsx", overwrite = TRUE) 