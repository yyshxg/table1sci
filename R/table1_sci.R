#' Create Table 1 in Scientific Paper Format
#' 
#' @param data A data frame containing the variables
#' @param vars Character vector of variable names to include
#' @param group Grouping variable name
#' @param var_labels Named character vector of variable labels
#' @param digits Integer indicating number of decimal places
#' @param p_digits Integer indicating number of decimal places for p-values
#' @param adjust_method Method for p-value adjustment in multiple comparisons
#' @param show_test_stats Logical, whether to show test statistics
#' @return A data frame containing the formatted Table 1
#' @export
table1_sci <- function(data, vars = NULL, group = NULL, var_labels = NULL,
                      digits = 1, p_digits = 3, adjust_method = "none",
                      show_test_stats = TRUE) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  
  if (is.null(vars)) {
    vars <- setdiff(names(data), group)
  }
  
  # Detect variable types
  var_types <- detect_var_type(data, vars)
  
  # Initialize results table with all necessary columns
  col_names <- c("Variable", "Overall")
  if (!is.null(group)) {
    group_levels <- levels(factor(data[[group]]))
    col_names <- c(col_names, group_levels)
    if (show_test_stats) {
      col_names <- c(col_names, "P-value", "Test Statistics")
    } else {
      col_names <- c(col_names, "P-value")
    }
  }
  
  # Create empty data frame with all necessary columns
  results <- as.data.frame(matrix(nrow = 0, ncol = length(col_names)))
  names(results) <- col_names
  
  # Process each variable
  for (var in vars) {
    var_info <- var_types[[var]]
    var_label <- if (!is.null(var_labels) && var %in% names(var_labels)) {
      var_labels[var]
    } else {
      var
    }
    
    if (var_info$type == "continuous") {
      # Create row with all columns
      row <- as.data.frame(matrix("", nrow = 1, ncol = length(col_names)))
      names(row) <- col_names
      row$Variable <- var_label
      
      if (var_info$is_normal) {
        row$Overall <- sprintf("%.1f ± %.1f",
                             mean(data[[var]], na.rm = TRUE),
                             sd(data[[var]], na.rm = TRUE))
        
        if (!is.null(group)) {
          group_stats <- tapply(data[[var]], data[[group]], function(x) {
            sprintf("%.1f ± %.1f", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
          })
          for (level in group_levels) {
            row[[level]] <- group_stats[level]
          }
        }
      } else {
        row$Overall <- sprintf("%.1f (%.1f-%.1f)",
                             median(data[[var]], na.rm = TRUE),
                             quantile(data[[var]], 0.25, na.rm = TRUE),
                             quantile(data[[var]], 0.75, na.rm = TRUE))
        
        if (!is.null(group)) {
          group_stats <- tapply(data[[var]], data[[group]], function(x) {
            sprintf("%.1f (%.1f-%.1f)",
                    median(x, na.rm = TRUE),
                    quantile(x, 0.25, na.rm = TRUE),
                    quantile(x, 0.75, na.rm = TRUE))
          })
          for (level in group_levels) {
            row[[level]] <- group_stats[level]
          }
        }
      }
      
      if (!is.null(group)) {
        test_result <- perform_test(data, var, group, var_info$type,
                                  var_info$is_normal, adjust_method)
        
        row$`P-value` <- if (test_result$p_value < 0.001) {
          "<0.001"
        } else {
          sprintf(paste0("%.", p_digits, "f"), test_result$p_value)
        }
        
        if (show_test_stats) {
          row$`Test Statistics` <- if (!is.na(test_result$statistic)) {
            sprintf("%s = %.2f", test_result$statistic_name, test_result$statistic)
          } else {
            ""
          }
        }
      }
      
      results <- rbind(results, row)
      
    } else if (var_info$type == "categorical") {
      # Add variable label row
      row_label <- as.data.frame(matrix("", nrow = 1, ncol = length(col_names)))
      names(row_label) <- col_names
      row_label$Variable <- var_label
      
      # Get test results for categorical variable
      if (!is.null(group)) {
        test_result <- perform_test(data, var, group, var_info$type,
                                  var_info$is_normal, adjust_method)
        
        row_label$`P-value` <- if (test_result$p_value < 0.001) {
          "<0.001"
        } else {
          sprintf(paste0("%.", p_digits, "f"), test_result$p_value)
        }
        
        if (show_test_stats) {
          row_label$`Test Statistics` <- if (!is.na(test_result$statistic)) {
            sprintf("%s = %.2f", test_result$statistic_name, test_result$statistic)
          } else {
            ""
          }
        }
      }
      
      results <- rbind(results, row_label)
      
      # Add rows for each level
      var_levels <- levels(factor(data[[var]]))
      overall_table <- table(data[[var]])
      overall_pct <- prop.table(overall_table) * 100
      
      for (var_level in var_levels) {
        row <- as.data.frame(matrix("", nrow = 1, ncol = length(col_names)))
        names(row) <- col_names
        row$Variable <- sprintf("  %s", var_level)
        
        # Overall statistics
        n_overall <- overall_table[var_level]
        pct_overall <- overall_pct[var_level]
        row$Overall <- sprintf("%d (%.1f%%)", n_overall, pct_overall)
        
        # Group statistics
        if (!is.null(group)) {
          for (g_level in group_levels) {
            subset_data <- data[data[[group]] == g_level, ]
            n <- sum(subset_data[[var]] == var_level, na.rm = TRUE)
            pct <- 100 * n / nrow(subset_data)
            row[[g_level]] <- sprintf("%d (%.1f%%)", n, pct)
          }
        }
        
        results <- rbind(results, row)
      }
    }
  }
  
  class(results) <- c("table1sci", "data.frame")
  return(results)
}

#' Print Method for Table 1
#' 
#' @param x Object of class table1sci
#' @param ... Additional arguments passed to print
#' @export
print.table1sci <- function(x, ...) {
  # Remove row names for cleaner output
  row.names(x) <- NULL
  
  # Print using knitr::kable if available
  if (requireNamespace("knitr", quietly = TRUE)) {
    print(knitr::kable(x, format = "pipe"))
  } else {
    print.data.frame(x)
  }
} 