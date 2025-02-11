# 安装和加载必要的包
if (!require("tableone")) install.packages("tableone")
if (!require("devtools")) install.packages("devtools")
if (!require("dplyr")) install.packages("dplyr")

# 加载包
library(tableone)
library(table1sci)
library(dplyr)

# 创建测试数据集
set.seed(123)
n <- 200

data <- data.frame(
    # 连续变量（正态分布）
    age = rnorm(n, mean = 60, sd = 10),
    bmi = rnorm(n, mean = 24, sd = 3),
    
    # 连续变量（非正态分布）
    ldl = exp(rnorm(n, log(100), 0.4)),
    crp = rgamma(n, shape = 2, rate = 0.5),
    
    # 分类变量
    sex = factor(sample(c("Male", "Female"), n, replace = TRUE)),
    smoking = factor(sample(c("Never", "Former", "Current"), n, replace = TRUE)),
    diabetes = factor(sample(c("Yes", "No"), n, replace = TRUE)),
    
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

# 指定变量类型
vars <- c("age", "sex", "bmi", "ldl", "crp", "smoking", "diabetes")
catVars <- c("sex", "smoking", "diabetes")

# 使用table1sci生成结果
cat("\n=== table1sci的结果（均数标准差保留两位小数）===\n")
table1sci_result <- table1_sci(
    data = data,
    vars = vars,
    group = "group",
    var_labels = var_labels,
    digits = 2,  # 修改为2位小数
    p_digits = 3,
    show_test_stats = TRUE
)
print(table1sci_result)

# 使用tableone生成结果
cat("\n=== tableone的结果（用于比较）===\n")
tableone_result <- CreateTableOne(
    vars = vars,
    strata = "group",
    data = data,
    factorVars = catVars,
    test = TRUE,
    testNormal = oneway.test,
    testNonNormal = kruskal.test,
    testApprox = chisq.test,
    testExact = fisher.test
)
print(tableone_result, nonnormal = c("ldl", "crp"), exact = catVars)

# 导出比较结果
table1_sci_export(table1sci_result, "comparison_table1sci.xlsx")
write.csv(print(tableone_result, nonnormal = c("ldl", "crp"), 
                exact = catVars, printToggle = FALSE),
          "comparison_tableone.csv")

# 结果比较分析
cat("\n=== 结果比较分析 ===\n")

# 检查连续变量的描述统计
cat("\n1. 连续变量比较（重点关注小数位数）：\n")
continuous_vars <- c("age", "bmi", "ldl", "crp")
for(var in continuous_vars) {
    cat(sprintf("\n%s:\n", var_labels[var]))
    # 检查正态性
    shapiro_test <- shapiro.test(data[[var]])
    cat(sprintf("正态性检验 p值: %.3f\n", shapiro_test$p.value))
    
    # 计算描述统计
    if(shapiro_test$p.value > 0.05) {
        cat("正态分布 - 使用均数±标准差（应保留2位小数）\n")
        cat(sprintf("table1sci: %s\n", 
            table1sci_result[table1sci_result$Variable == var_labels[var], "Overall"]))
        # 手动计算用于比较
        cat(sprintf("手动计算: %.2f ± %.2f\n", 
            mean(data[[var]], na.rm = TRUE),
            sd(data[[var]], na.rm = TRUE)))
    } else {
        cat("非正态分布 - 使用中位数(IQR)（保留1位小数）\n")
        cat(sprintf("table1sci: %s\n", 
            table1sci_result[table1sci_result$Variable == var_labels[var], "Overall"]))
    }
}

# 检查分类变量的频数和比例
cat("\n2. 分类变量比较：\n")
for(var in catVars) {
    cat(sprintf("\n%s:\n", var_labels[var]))
    # table1sci的结果
    cat("table1sci结果：\n")
    subset_rows <- grep(var_labels[var], table1sci_result$Variable, fixed = TRUE)
    print(table1sci_result[subset_rows, ])
}

cat("\n比较分析完成！请查看生成的Excel和CSV文件进行详细比较。\n")
cat("注意：两个包可能在格式和显示方式上有所不同，但数值结果应该是一致的。\n") 