#' Create Table 1 in Scientific Paper Format
#' 
#' @param data A data frame containing the variables
#' @param vars Character vector of variable names to include
#' @param group Grouping variable name
#' @param var_labels Named character vector of variable labels
#' @param digits Integer indicating number of decimal places for continuous variables (default: 2 for normal distribution, 1 for non-normal)
#' @param p_digits Integer indicating number of decimal places for p-values
#' @param adjust_method Method for p-value adjustment in multiple comparisons
#' @param show_test_stats Logical, whether to show test statistics
#' @param auto_normal Logical, whether to automatically determine normality for continuous variables
#' @return A data frame containing the formatted Table 1
#' @export
table1_sci <- function(data, vars = NULL, group = NULL, var_labels = NULL,
                      digits = 2, p_digits = 3, adjust_method = "none",
                      show_test_stats = TRUE, auto_normal = TRUE) {
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
  
  # Add sample size row
  n_row <- as.data.frame(matrix("", nrow = 1, ncol = length(col_names)))
  names(n_row) <- col_names
  n_row$Variable <- "N"
  n_row$Overall <- sprintf("n = %d", nrow(data))
  
  if (!is.null(group)) {
    for (level in group_levels) {
      n_row[[level]] <- sprintf("n = %d", sum(data[[group]] == level, na.rm = TRUE))
    }
  }
  
  results <- rbind(n_row, results)
  
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
      
      # 获取实际使用的正态性状态（考虑auto_normal参数）
      actual_is_normal <- if (!auto_normal) {
        # 如果不自动判断正态性，则使用正态分布的描述方法
        TRUE
      } else {
        # 否则使用检测到的正态性状态
        var_info$is_normal
      }
      
      if (actual_is_normal) {
        # 正态分布变量使用digits参数指定的小数位数（默认2位）
        fmt <- paste0("%.", digits, "f ± %.", digits, "f")
        row$Overall <- sprintf(fmt,
                             mean(data[[var]], na.rm = TRUE),
                             sd(data[[var]], na.rm = TRUE))
        
        if (!is.null(group)) {
          group_stats <- tapply(data[[var]], data[[group]], function(x) {
            sprintf(fmt, mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
          })
          for (level in group_levels) {
            row[[level]] <- group_stats[level]
          }
        }
      } else {
        # 非正态分布变量保持1位小数
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
                                  var_info$is_normal, auto_normal, adjust_method)
        
        row$`P-value` <- if (test_result$p_value < 0.001) {
          "<0.001"
        } else {
          sprintf(paste0("%.", p_digits, "f"), test_result$p_value)
        }
        
        if (show_test_stats) {
          row$`Test Statistics` <- if (!is.na(test_result$statistic)) {
            sprintf("%s = %.3f", test_result$statistic_name, test_result$statistic)
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
                                  var_info$is_normal, auto_normal, adjust_method)
        
        row_label$`P-value` <- if (test_result$p_value < 0.001) {
          "<0.001"
        } else {
          sprintf(paste0("%.", p_digits, "f"), test_result$p_value)
        }
        
        if (show_test_stats) {
          row_label$`Test Statistics` <- if (!is.na(test_result$statistic)) {
            sprintf("%s = %.3f", test_result$statistic_name, test_result$statistic)
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
        row$Overall <- sprintf("%d (%.1f)", n_overall, pct_overall)
        
        # Group statistics
        if (!is.null(group)) {
          for (g_level in group_levels) {
            subset_data <- data[data[[group]] == g_level, ]
            n <- sum(subset_data[[var]] == var_level, na.rm = TRUE)
            denom <- sum(!is.na(subset_data[[var]]))
            pct <- ifelse(denom > 0, 100 * n / denom, NA)
            row[[g_level]] <- sprintf("%d (%.1f)", n, pct)
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

#' Export Table 1 to File
#' 
#' This function exports a table1sci object to either an Excel (.xlsx) or CSV file.
#' When exporting to Excel, it applies publication-ready formatting including proper
#' fonts, alignment, and borders.
#' 
#' @param table1_result A table1sci object created by table1_sci()
#' @param filename Character string naming the file to write to (should end in .xlsx or .csv)
#' @param format Output format, either "xlsx" or "csv"
#' @param font_name Font name to use in Excel output (default: "Times New Roman")
#' @param font_size Font size to use in Excel output (default: 11)
#' 
#' @details 
#' When exporting to Excel format, the function applies the following formatting:
#' \itemize{
#'   \item Bold headers with bottom border
#'   \item Center-aligned data cells
#'   \item Left-aligned variable names
#'   \item Automatic column width adjustment
#'   \item Publication-ready font and size
#' }
#' 
#' @return Invisible NULL
#' 
#' @examples
#' \dontrun{
#' # Create example data
#' data <- data.frame(
#'   age = rnorm(100, mean = 60, sd = 10),
#'   sex = factor(sample(c("Male", "Female"), 100, replace = TRUE)),
#'   group = factor(sample(c("Treatment", "Control"), 100, replace = TRUE))
#' )
#' 
#' # Generate Table 1
#' result <- table1_sci(data, vars = c("age", "sex"), group = "group")
#' 
#' # Export to Excel
#' table1_sci_export(result, "table1.xlsx")
#' 
#' # Export to CSV
#' table1_sci_export(result, "table1.csv", format = "csv")
#' 
#' # Customize Excel format
#' table1_sci_export(result, "table1_custom.xlsx",
#'                  font_name = "Arial",
#'                  font_size = 12)
#' }
#' 
#' @seealso \code{\link{table1_sci}} for creating the table
#' 
#' @export
table1_sci_export <- function(table1_result, filename, 
                            format = c("xlsx", "csv"),
                            font_name = "Times New Roman",
                            font_size = 11) {
    format <- match.arg(format)
    
    if (format == "xlsx") {
        if (!requireNamespace("openxlsx", quietly = TRUE)) {
            stop("Package 'openxlsx' needed for Excel export. Please install it.",
                 call. = FALSE)
        }
        
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Table 1")
        
        # 写入数据
        openxlsx::writeData(wb, "Table 1", table1_result)
        
        # 设置SCI格式
        headerStyle <- openxlsx::createStyle(
            textDecoration = "bold",
            border = "bottom",
            borderStyle = "thin",
            fontSize = font_size,
            fontName = font_name,
            halign = "center",
            valign = "center",
            wrapText = TRUE
        )
        
        dataStyle <- openxlsx::createStyle(
            fontSize = font_size,
            fontName = font_name,
            halign = "center",
            valign = "center"
        )
        
        # 变量名列使用左对齐
        varStyle <- openxlsx::createStyle(
            fontSize = font_size,
            fontName = font_name,
            halign = "left",
            valign = "center"
        )
        
        # 应用样式
        openxlsx::addStyle(wb, "Table 1", headerStyle, 
                          rows = 1, cols = 1:ncol(table1_result))
        
        # 对数据列应用居中对齐
        openxlsx::addStyle(wb, "Table 1", dataStyle, 
                          rows = 2:(nrow(table1_result)+1), 
                          cols = 2:ncol(table1_result), 
                          gridExpand = TRUE)
        
        # 对变量名列应用左对齐
        openxlsx::addStyle(wb, "Table 1", varStyle,
                          rows = 2:(nrow(table1_result)+1),
                          cols = 1,
                          gridExpand = TRUE)
        
        # 设置列宽
        openxlsx::setColWidths(wb, "Table 1", 
                              cols = 1:ncol(table1_result), 
                              widths = "auto")
        
        # 保存
        openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
    } else {
        utils::write.csv(table1_result, filename, row.names = FALSE)
    }
    
    invisible(NULL)
}