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
    df = NA
  )
  
  # Remove NA values
  complete_data <- na.omit(data[c(var, group)])
  
  if (var_type == "continuous") {
    if (is.null(is_normal)) {
      stop("is_normal must be specified for continuous variables")
    }
    
    if (length(unique(complete_data[[group]])) == 2) {
      if (is_normal) {
        # t-test for normal distribution
        test_result <- t.test(as.formula(paste(var, "~", group)), 
                            data = complete_data)
        result$test_type <- "t-test"
        result$statistic <- test_result$statistic
        result$statistic_name <- "t"
        result$p_value <- test_result$p.value
        result$df <- test_result$parameter
      } else {
        # Mann-Whitney U test for non-normal distribution
        test_result <- wilcox.test(as.formula(paste(var, "~", group)), 
                                 data = complete_data)
        result$test_type <- "Mann-Whitney U test"
        result$statistic <- test_result$statistic
        result$statistic_name <- "W"
        result$p_value <- test_result$p.value
      }
    } else {
      if (is_normal) {
        # ANOVA for normal distribution
        test_result <- summary(aov(as.formula(paste(var, "~", group)), 
                                 data = complete_data))[[1]]
        result$test_type <- "ANOVA"
        result$statistic <- test_result$"F value"[1]
        result$statistic_name <- "F"
        result$p_value <- test_result$"Pr(>F)"[1]
        result$df <- c(test_result$Df[1], test_result$Df[2])
      } else {
        # Kruskal-Wallis test for non-normal distribution
        test_result <- kruskal.test(as.formula(paste(var, "~", group)), 
                                  data = complete_data)
        result$test_type <- "Kruskal-Wallis test"
        result$statistic <- test_result$statistic
        result$statistic_name <- "H"
        result$p_value <- test_result$p.value
        result$df <- test_result$parameter
      }
    }
  } else if (var_type == "categorical") {
    # Create contingency table
    cont_table <- table(complete_data[[group]], complete_data[[var]])
    
    # Check conditions for Chi-square test
    expected <- chisq.test(cont_table)$expected
    if (any(expected < 1) || sum(expected < 5)/length(expected) > 0.2) {
      # Use Fisher's exact test
      test_result <- fisher.test(cont_table, simulate.p.value = TRUE)
      result$test_type <- "Fisher's exact test"
      result$p_value <- test_result$p.value
    } else {
      # Use Chi-square test
      test_result <- chisq.test(cont_table)
      result$test_type <- "Chi-square test"
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
    cat(sprintf("%s = %.3f", x$statistic_name, x$statistic))
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
} 