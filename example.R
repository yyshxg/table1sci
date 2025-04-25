# 加载包
devtools::install_github("yyshxg/table1sci")
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

# 添加一些缺失值
set.seed(456)
missing_indices <- sample(1:n, 20)
data_advanced$ldl[missing_indices[1:10]] <- NA
data_advanced$smoking[missing_indices[11:20]] <- NA

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

# 生成高级Table 1（不显示缺失值）
table1_advanced <- table1_sci(
    data = data_advanced,
    vars = c("age", "sex", "bmi", "ldl", "sbp", "smoking", "diabetes"),
    group = "group",
    var_labels = var_labels,
    digits = 1,  # 连续变量保留1位小数
    p_digits = 3,
    adjust_method = "fdr",  # 使用FDR方法进行多重比较校正
    show_test_stats = TRUE,  # 显示检验统计量
    show_missing = FALSE  # 默认不显示缺失值比例
)

# 生成显示缺失值的Table 1
table1_with_missing <- table1_sci(
    data = data_advanced,
    vars = c("age", "sex", "bmi", "ldl", "sbp", "smoking", "diabetes"),
    group = "group",
    var_labels = var_labels,
    digits = 1,
    p_digits = 3,
    adjust_method = "fdr",
    show_test_stats = TRUE,
    show_missing = TRUE  # 显示缺失值比例
)

# 打印结果
cat("\n标准Table 1（不显示缺失值）:\n")
print(table1_advanced)

# 打印带缺失值信息的结果
cat("\n带缺失值信息的Table 1:\n")
print(table1_with_missing)

# 说明缺失值显示的效果
cat("\n说明：\n")
cat("1. 当show_missing=TRUE时，变量名后会显示缺失值比例，如[5.0% missing]\n")
cat("2. 对于分类变量，百分比计算使用非缺失值总数作为分母\n")
cat("3. 对于连续变量，统计量计算自动排除缺失值\n")

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

# 添加带缺失值信息的工作表
openxlsx::addWorksheet(wb, "With Missing Info")
openxlsx::writeData(wb, "With Missing Info", table1_with_missing)

# 设置样式
hs <- openxlsx::createStyle(
    textDecoration = "bold",
    border = "bottom",
    borderStyle = "thin"
)

# 应用样式到表头
openxlsx::addStyle(wb, "Basic Example", hs, rows = 1, cols = 1:ncol(table1))
openxlsx::addStyle(wb, "Advanced Example", hs, rows = 1, cols = 1:ncol(table1_advanced))
openxlsx::addStyle(wb, "With Missing Info", hs, rows = 1, cols = 1:ncol(table1_with_missing))

# 自动调整列宽
openxlsx::setColWidths(wb, "Basic Example", cols = 1:ncol(table1), widths = "auto")
openxlsx::setColWidths(wb, "Advanced Example", cols = 1:ncol(table1_advanced), widths = "auto")
openxlsx::setColWidths(wb, "With Missing Info", cols = 1:ncol(table1_with_missing), widths = "auto")

# 保存文件
openxlsx::saveWorkbook(wb, "table1_examples.xlsx", overwrite = TRUE)