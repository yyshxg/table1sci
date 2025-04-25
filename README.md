# table1sci

## 简介

`table1sci` 是一个R包，用于生成符合科学期刊标准的Table 1（基线特征表）。该包具有以下特点：

- 自动检测变量类型（连续型/分类型）
- 自动进行正态性检验并选择合适的描述统计方法（可通过auto_normal参数控制）
- 自动选择合适的统计检验方法
- 输出格式严格遵循SCI论文要求
- 支持多组比较
- 提供完整的统计量信息

## 安装

```R
# 安装devtools包（如果尚未安装）
install.packages("devtools")

# 从GitHub安装table1sci包（默认方式，需要Rtools）
devtools::install_github("yyshxg/table1sci")

# 或者使用二进制安装方式（不需要Rtools）
devtools::install_github("yyshxg/table1sci", 
                        build_vignettes = FALSE, 
                        build_manual = FALSE)
```

## 使用方法

### 基本用法

```R
library(table1sci)

# 创建示例数据
data <- data.frame(
  age = rnorm(100, mean = 50, sd = 10),
  sex = factor(sample(c("Male", "Female"), 100, replace = TRUE)),
  bmi = rnorm(100, mean = 25, sd = 5),
  group = factor(sample(c("Treatment", "Control"), 100, replace = TRUE))
)

# 生成基本的Table 1
table1 <- table1_sci(data, vars = c("age", "sex", "bmi"))

# 生成分组比较的Table 1
table1_group <- table1_sci(data, 
                          vars = c("age", "sex", "bmi"),
                          group = "group")
```

### 高级选项

```R
# 自定义变量标签
var_labels <- c(
  age = "Age (years)",
  sex = "Sex",
  bmi = "Body Mass Index (kg/m²)"
)

# 使用自定义选项生成Table 1
table1_custom <- table1_sci(
  data = data,
  vars = c("age", "sex", "bmi"),
  group = "group",
  var_labels = var_labels,
  digits = 2,              # 小数位数
  p_digits = 3,           # p值小数位数
  adjust_method = "fdr",  # p值校正方法
  auto_normal = TRUE,     # 自动进行正态性检验
  show_test_stats = TRUE, # 显示检验统计量
  show_missing = TRUE     # 显示缺失值比例
)
```

## 功能特点

### 变量类型检测

- 自动识别连续型和分类型变量
- 对连续型变量进行正态性检验
- 使用categorical_threshold参数控制分类变量的判定（默认值为10，即少于10个唯一值的数值变量会被视为分类变量）

### 描述统计

连续型变量：
- 正态分布：均数 ± 标准差
- 非正态分布：中位数 (四分位数间距)

分类型变量：
- 频数和百分比：n (%)
- 百分比计算已优化，使用非缺失值总数作为分母，确保比例计算准确

### 统计检验

连续型变量：
- 正态分布：t检验/ANOVA
- 非正态分布：Mann-Whitney U检验/Kruskal-Wallis检验

分类型变量：
- 卡方检验
- Fisher精确检验（当期望频数较小时自动选择）

### 缺失值处理

- 支持通过show_missing参数显示每个变量的缺失值比例
- 变量名后会显示缺失值百分比，如[5.0% missing]
- 对于分类变量，百分比计算使用非缺失值总数作为分母
- 对于连续变量，统计量计算自动排除缺失值

### 输出格式

- 支持导出为data.frame格式
- 使用knitr::kable美化输出
- 可自定义小数位数和p值格式

## 贡献

欢迎提交问题和建议到[GitHub Issues](https://github.com/yyshxg/table1sci/issues)页面。如果您想贡献代码，请提交Pull Request。

## 许可证

MIT