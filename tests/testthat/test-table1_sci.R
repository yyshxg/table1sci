test_that("table1_sci works with basic input", {
  # Create sample data
  set.seed(123)
  n <- 100
  data <- data.frame(
    age = rnorm(n, mean = 50, sd = 10),
    sex = factor(sample(c("Male", "Female"), n, replace = TRUE)),
    bmi = rnorm(n, mean = 25, sd = 5),
    group = factor(sample(c("Treatment", "Control"), n, replace = TRUE))
  )
  
  # Test without grouping
  result1 <- table1_sci(data, vars = c("age", "sex", "bmi"))
  expect_s3_class(result1, "table1sci")
  expect_equal(nrow(result1), 3)
  expect_true("Overall" %in% names(result1))
  
  # Test with grouping
  result2 <- table1_sci(data, vars = c("age", "sex", "bmi"), group = "group")
  expect_s3_class(result2, "table1sci")
  expect_equal(nrow(result2), 3)
  expect_true(all(c("Treatment", "Control", "P-value") %in% names(result2)))
})

test_that("detect_var_type works correctly", {
  # Create sample data
  data <- data.frame(
    numeric_normal = rnorm(100),
    numeric_uniform = runif(100),
    categorical = factor(sample(c("A", "B", "C"), 100, replace = TRUE)),
    binary = sample(c(0, 1), 100, replace = TRUE)
  )
  
  # Test variable type detection
  types <- detect_var_type(data)
  expect_type(types, "list")
  expect_true(types$numeric_normal$type == "continuous")
  expect_true(types$categorical$type == "categorical")
  expect_true(types$binary$type == "categorical")
})

test_that("perform_test selects appropriate tests", {
  # Create sample data
  set.seed(123)
  n <- 100
  data <- data.frame(
    normal = rnorm(n),
    non_normal = exp(rnorm(n)),
    categorical = factor(sample(c("A", "B"), n, replace = TRUE)),
    group = factor(sample(c("Group1", "Group2"), n, replace = TRUE))
  )
  
  # Test continuous normal
  test1 <- perform_test(data, "normal", "group", "continuous", TRUE)
  expect_equal(test1$test_type, "t-test")
  
  # Test continuous non-normal
  test2 <- perform_test(data, "non_normal", "group", "continuous", FALSE)
  expect_equal(test2$test_type, "Mann-Whitney U test")
  
  # Test categorical
  test3 <- perform_test(data, "categorical", "group", "categorical")
  expect_true(test3$test_type %in% c("Chi-square test", "Fisher's exact test"))
}) 