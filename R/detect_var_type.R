#' Detect Variable Types and Distribution
#' 
#' @param data A data frame containing the variables to be analyzed
#' @param vars Character vector of variable names to be analyzed
#' @param normality_threshold Numeric value for Shapiro-Wilk test p-value threshold (default: 0.05)
#' @param categorical_threshold Integer. Variables with unique values less than this will be treated as categorical
#' @return A list containing variable types and normality test results
#' @export
detect_var_type <- function(data, vars = NULL, normality_threshold = 0.05, 
                           categorical_threshold = 10) {
  if (is.null(vars)) {
    vars <- names(data)
  }
  
  results <- list()
  for (var in vars) {
    if (!var %in% names(data)) {
      stop(sprintf("Variable '%s' not found in data", var))
    }
    
    var_data <- data[[var]]
    
    # Initialize result structure
    var_info <- list(
      type = NA,
      is_normal = NA,
      normality_test = list(
        statistic = NA,
        p_value = NA
      )
    )
    
    # Detect type
    if (is.factor(var_data) || is.character(var_data) || 
        (is.numeric(var_data) && length(unique(na.omit(var_data))) < categorical_threshold)) {
      var_info$type <- "categorical"
    } else if (is.numeric(var_data)) {
      var_info$type <- "continuous"
      
      # Perform normality test if enough unique values and observations
      clean_data <- na.omit(var_data)
      if (length(unique(clean_data)) > 3 && length(clean_data) >= 3) {
        sw_test <- shapiro.test(clean_data)
        var_info$is_normal <- sw_test$p.value > normality_threshold
        var_info$normality_test$statistic <- sw_test$statistic
        var_info$normality_test$p_value <- sw_test$p.value
      }
    } else {
      var_info$type <- "unknown"
    }
    
    results[[var]] <- var_info
  }
  
  class(results) <- c("table1sci_vartype", "list")
  return(results)
}

#' Print Method for Variable Type Detection Results
#' 
#' @param x Object of class table1sci_vartype
#' @param ... Additional arguments passed to print
#' @export
print.table1sci_vartype <- function(x, ...) {
  cat("Variable Type Detection Results:\n\n")
  for (var_name in names(x)) {
    var_info <- x[[var_name]]
    cat(sprintf("Variable: %s\n", var_name))
    cat(sprintf("Type: %s\n", var_info$type))
    if (var_info$type == "continuous") {
      if (!is.na(var_info$is_normal)) {
        cat(sprintf("Distribution: %s\n", 
                   ifelse(var_info$is_normal, "Normal", "Non-normal")))
        cat(sprintf("Shapiro-Wilk test p-value: %.4f\n", 
                   var_info$normality_test$p_value))
      }
    }
    cat("\n")
  }
} 