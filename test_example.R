# 安装和加载必要的包
if (!require("devtools")) install.packages("devtools")
if (!require("openxlsx")) install.packages("openxlsx")

# 安装table1sci包
devtools::load_all(".")  # 直接从当前目录加载包

# 创建示例数据
set.seed(123)
n <- 200  # 样本量

# 生成连续型变量（正态和非正态）
age <- rnorm(n, mean = 60, sd = 10)  # 年龄，正态分布
bmi <- rnorm(n, mean = 24, sd = 3)   # BMI，正态分布
ldl <- exp(rnorm(n, log(100), 0.4))  # LDL胆固醇，对数正态分布
sbp <- rgamma(n, shape = 100, rate = 0.7)  # 收缩压，gamma分布

# 生成分类变量
sex <- factor(sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.55, 0.45)))
smoking <- factor(sample(c("Never", "Former", "Current"), n, replace = TRUE, prob = c(0.5, 0.3, 0.2)))
diabetes <- factor(sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.25, 0.75)))

# 生成治疗组
group <- factor(sample(c("Treatment", "Control"), n, replace = TRUE))

# 创建数据框
data <- data.frame(
  age = age,
  sex = sex,
  bmi = bmi,
  ldl = ldl,
  sbp = sbp,
  smoking = smoking,
  diabetes = diabetes,
  group = group
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

# 生成Table 1
table1_result <- table1_sci(
  data = data,
  vars = c("age", "sex", "bmi", "ldl", "sbp", "smoking", "diabetes"),
  group = "group",
  var_labels = var_labels,
  digits = 1,
  p_digits = 3,
  adjust_method = "none",
  show_test_stats = TRUE
)

# 打印结果到控制台
print(table1_result)

# 将结果保存为Excel文件
# 创建一个新的工作簿
wb <- createWorkbook()

# 添加一个工作表
addWorksheet(wb, "Table 1")

# 写入数据
writeData(wb, "Table 1", table1_result, rowNames = FALSE)

# 设置列宽
setColWidths(wb, "Table 1", cols = 1:ncol(table1_result), widths = "auto")

# 添加样式
hs <- createStyle(
  textDecoration = "bold",
  border = "bottom",
  borderStyle = "thin"
)

# 应用样式到表头
addStyle(wb, "Table 1", hs, rows = 1, cols = 1:ncol(table1_result))

# 保存文件
saveWorkbook(wb, "table1_output.xlsx", overwrite = TRUE)

# 显示基本的描述性统计
cat("\n变量类型检测结果：\n")
print(detect_var_type(data, vars = c("age", "sex", "bmi", "ldl", "sbp", "smoking", "diabetes"))) 