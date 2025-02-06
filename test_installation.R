# 安装必要的包
if (!require("devtools")) install.packages("devtools")

# 安装刚生成的包
install.packages("D:/学习/table1sci_0.1.0.tar.gz", repos = NULL, type = "source")

# 加载包
library(table1sci)

# 创建模拟数据集
set.seed(123)  # 确保结果可重复
n <- 200

# 生成各种类型的变量
data <- data.frame(
    # 连续变量（正态分布）
    age = rnorm(n, mean = 60, sd = 10),
    bmi = rnorm(n, mean = 24, sd = 3),
    
    # 连续变量（非正态分布）
    ldl = exp(rnorm(n, log(100), 0.4)),  # 对数正态分布
    crp = rgamma(n, shape = 2, rate = 0.5),  # 偏态分布
    
    # 分类变量
    sex = factor(sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.55, 0.45))),
    smoking = factor(sample(c("Never", "Former", "Current"), n, replace = TRUE, 
                          prob = c(0.5, 0.3, 0.2))),
    diabetes = factor(sample(c("Yes", "No"), n, replace = TRUE, 
                           prob = c(0.25, 0.75))),
    
    # 分组变量
    group = factor(sample(c("Treatment", "Control"), n, replace = TRUE))
)

# 设置变量标签
var_labels <- c(
    age = "Age (years)",
    sex = "Sex",
    bmi = "BMI (kg/m²)",
    ldl = "LDL Cholesterol (mg/dL)",
    crp = "C-reactive protein (mg/L)",
    smoking = "Smoking Status",
    diabetes = "Diabetes"
)

# 测试1：检查变量类型检测
cat("\n=== 测试1：变量类型检测 ===\n")
var_types <- detect_var_type(data)
print(var_types)

# 测试2：生成基本Table 1（无分组）
cat("\n=== 测试2：基本Table 1 ===\n")
table1_basic <- table1_sci(
    data = data,
    vars = c("age", "sex", "bmi", "ldl", "crp", "smoking", "diabetes"),
    var_labels = var_labels
)
print(table1_basic)

# 测试3：生成分组Table 1（带统计检验）
cat("\n=== 测试3：分组Table 1（带统计检验）===\n")
table1_group <- table1_sci(
    data = data,
    vars = c("age", "sex", "bmi", "ldl", "crp", "smoking", "diabetes"),
    group = "group",
    var_labels = var_labels,
    digits = 1,
    p_digits = 3,
    adjust_method = "none",
    show_test_stats = TRUE
)
print(table1_group)

# 测试4：导出为不同格式
cat("\n=== 测试4：导出功能 ===\n")

# Excel导出（默认格式）
table1_sci_export(table1_group, "table1_default.xlsx")
cat("已导出: table1_default.xlsx\n")

# Excel导出（自定义格式）
table1_sci_export(table1_group, "table1_custom.xlsx",
                 font_name = "Arial",
                 font_size = 12)
cat("已导出: table1_custom.xlsx\n")

# CSV导出
table1_sci_export(table1_group, "table1.csv", format = "csv")
cat("已导出: table1.csv\n")

# 测试5：多重比较校正
cat("\n=== 测试5：多重比较校正 ===\n")
table1_adjusted <- table1_sci(
    data = data,
    vars = c("age", "sex", "bmi", "ldl", "crp", "smoking", "diabetes"),
    group = "group",
    var_labels = var_labels,
    adjust_method = "fdr",  # 使用FDR校正
    show_test_stats = TRUE
)
print(table1_adjusted)

cat("\n所有测试完成！请检查生成的文件。\n") 