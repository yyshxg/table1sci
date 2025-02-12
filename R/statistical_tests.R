#' Perform Statistical Tests
#' 
#' @param data A data frame containing the variables
#' @param var Variable name to test
#' @param group Grouping variable name
#' @param var_type Type of variable ("continuous" or "categorical")
#' @param is_normal Logical, whether the continuous variable is normally distributed
#' @param adjust_method Method for p-value adjustment in multiple comparisons
#' @return A list containing test results including test type, statistic, and p-value
#' @export
perform_test <- function(data, var, group, var_type, is_normal = NULL, 
                        adjust_method = "none") {
  if (!var %in% names(data) || !group %in% names(data)) {
    stop("Variable or group not found in data")
  }
  
  result <- list(
    test_type = NA,
    statistic = NA,
    statistic_name = NA,
    p_value = NA,
    df = NA,
    n_analyzed = NA,  # 添加实际参与分析的样本量
    n_missing = NA    # 添加缺失值数量
  )
  
  # 计算缺失值信息
  total_n <- nrow(data)
  complete_cases <- complete.cases(data[c(var, group)])
  n_analyzed <- sum(complete_cases)
  n_missing <- total_n - n_analyzed
  
  result$n_analyzed <- n_analyzed
  result$n_missing <- n_missing
  
  # 如果缺失值过多（例如超过20%），给出警告
  if (n_missing/total_n > 0.2) {
    warning(sprintf("High proportion of missing data (%.1f%%) in variable '%s'", 
                   100*n_missing/total_n, var))
  }
  
  # 获取完整数据用于分析
  analysis_data <- data[complete_cases, ]
  
  if (var_type == "continuous") {
    if (is.null(is_normal)) {
      stop("is_normal must be specified for continuous variables")
    }
    
    if (length(unique(analysis_data[[group]])) == 2) {
      if (is_normal) {
        # t-test for normal distribution
        test_result <- t.test(as.formula(paste(var, "~", group)), 
                            data = analysis_data)
        result$test_type <- "t-test"
        result$statistic <- test_result$statistic
        result$statistic_name <- "t"
        result$p_value <- test_result$p.value
        result$df <- test_result$parameter
      } else {
        # Mann-Whitney U test for non-normal distribution
        test_result <- wilcox.test(as.formula(paste(var, "~", group)), 
                                 data = analysis_data)
        result$test_type <- "Mann-Whitney U test"
        result$statistic <- test_result$statistic
        result$statistic_name <- "W"
        result$p_value <- test_result$p.value
      }
    } else {
      if (is_normal) {
        # ANOVA for normal distribution
        test_result <- summary(aov(as.formula(paste(var, "~", group)), 
                                 data = analysis_data))[[1]]
        result$test_type <- "ANOVA"
        result$statistic <- test_result$"F value"[1]
        result$statistic_name <- "F"
        result$p_value <- test_result$"Pr(>F)"[1]
        result$df <- c(test_result$Df[1], test_result$Df[2])
      } else {
        # Kruskal-Wallis test for non-normal distribution
        test_result <- kruskal.test(as.formula(paste(var, "~", group)), 
                                  data = analysis_data)
        result$test_type <- "Kruskal-Wallis test"
        result$statistic <- test_result$statistic
        result$statistic_name <- "H"
        result$p_value <- test_result$p.value
        result$df <- test_result$parameter
      }
    }
  } else if (var_type == "categorical") {
    # 创建列联表
    cont_table <- table(analysis_data[[group]], analysis_data[[var]])
    
    # 检查卡方检验的条件
    expected <- chisq.test(cont_table)$expected
    min_expected <- min(expected)
    prop_small_expected <- sum(expected < 5) / length(expected)
    total_n <- sum(cont_table)
    
    # 判断使用哪种检验方法：
    # 1. Fisher精确检验的条件：
    #    - 任何期望频数 < 1
    #    - 超过1/4的格子期望频数 < 5
    #    - 样本量 < 40
    # 2. 连续性校正的卡方检验条件（仅适用于2x2表）：
    #    - 是2x2表
    #    - 所有期望频数 ≥ 1
    #    - 有期望频数 < 5
    #    - 样本量 ≥ 40
    # 3. 其他情况使用普通卡方检验
    
    is_2x2_table <- all(dim(cont_table) == 2)
    
    if (min_expected < 1 || prop_small_expected > 0.25 || total_n < 40) {
      # 使用Fisher精确检验
      test_result <- fisher.test(cont_table, simulate.p.value = TRUE)
      result$test_type <- "Fisher's exact test"
      result$p_value <- test_result$p.value
    } else if (is_2x2_table && min_expected >= 1 && prop_small_expected > 0 && total_n >= 40) {
      # 使用连续性校正的卡方检验（仅用于2x2表）
      test_result <- chisq.test(cont_table, correct = TRUE)
      result$test_type <- "Chi-square test with Yates' correction"
      result$statistic <- test_result$statistic
      result$statistic_name <- "χ²"
      result$p_value <- test_result$p.value
      result$df <- test_result$parameter
    } else {
      # 使用普通卡方检验（Pearson卡方）
      test_result <- chisq.test(cont_table, correct = FALSE)
      result$test_type <- "Pearson's Chi-square test"
      result$statistic <- test_result$statistic
      result$statistic_name <- "χ²"
      result$p_value <- test_result$p.value
      result$df <- test_result$parameter
    }
  }
  
  # Adjust p-value if needed
  if (adjust_method != "none") {
    result$p_value <- p.adjust(result$p_value, method = adjust_method)
  }
  
  class(result) <- c("table1sci_test", "list")
  return(result)
}

#' Print Method for Statistical Test Results
#' 
#' @param x Object of class table1sci_test
#' @param ... Additional arguments passed to print
#' @export
print.table1sci_test <- function(x, ...) {
  cat("Statistical Test Results:\n")
  cat(sprintf("Test type: %s\n", x$test_type))
  if (!is.na(x$statistic)) {
    cat(sprintf("%s = %.3f", x$statistic_name, x$statistic))  # 修改为3位小数
    if (!is.na(x$df)) {
      if (length(x$df) == 1) {
        cat(sprintf(", df = %d", x$df))
      } else {
        cat(sprintf(", df = (%d, %d)", x$df[1], x$df[2]))
      }
    }
    cat("\n")
  }
  cat(sprintf("p-value = %.4f\n", x$p_value))
  if (!is.na(x$n_missing) && x$n_missing > 0) {
    cat(sprintf("Note: %d cases excluded due to missing data\n", x$n_missing))
  }
} 