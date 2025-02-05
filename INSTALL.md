# table1sci 安装指南

## 依赖包安装

首先需要安装必要的R包：

```R
# 安装devtools包（如果尚未安装）
install.packages("devtools")

# 安装其他依赖包
install.packages(c("openxlsx", "knitr"))
```

## 从GitHub安装table1sci

```R
# 安装table1sci包
devtools::install_github("yyshxg/table1sci")
```

## 使用示例

```R
# 加载包
library(table1sci)

# 创建示例数据
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
table1 <- table1_sci(
    data = data,
    vars = c("age", "sex", "bmi"),
    group = "group",
    var_labels = var_labels,
    digits = 1,
    p_digits = 3,
    show_test_stats = TRUE
)

# 查看结果
print(table1)

# 导出为Excel文件
library(openxlsx)
wb <- createWorkbook()
addWorksheet(wb, "Table 1")
writeData(wb, "Table 1", table1)
saveWorkbook(wb, "table1_output.xlsx", overwrite = TRUE)
```

## 常见问题

1. 如果安装过程中出现依赖包相关错误，请确保已安装所有必要的依赖包：
```R
install.packages(c("stats", "utils", "openxlsx", "knitr"))
```

2. 如果使用Windows系统，可能需要安装Rtools：
   - 访问 https://cran.r-project.org/bin/windows/Rtools/
   - 下载并安装对应R版本的Rtools

3. 如果需要更新包：
```R
devtools::install_github("yyshxg/table1sci", force = TRUE)
```

## 功能说明

1. 自动检测变量类型：
   - 连续型变量：自动进行正态性检验
   - 分类变量：自动识别并合理展示

2. 统计检验：
   - 连续型正态分布：t检验/ANOVA
   - 连续型非正态分布：Mann-Whitney U检验/Kruskal-Wallis检验
   - 分类变量：卡方检验/Fisher精确检验

3. 输出格式：
   - 连续型正态分布：均数 ± 标准差
   - 连续型非正态分布：中位数 (四分位数间距)
   - 分类变量：频数 (百分比)

4. 支持功能：
   - 自定义变量标签
   - 调整小数位数
   - 显示检验统计量
   - 多重比较校正
   - 导出Excel格式 