# Load the necessary library
library(testthat)

# Start of tests
context("Testing op_remove_keypoints function")

# 1. SETUP: Create a comprehensive test data frame
test_df <- data.frame(
  # Keypoint 0: Control
  x0 = 1:10,
  y0 = 1:10,
  c0 = rep(1.0, 10),

  # Keypoint 1: For specific removal
  x1 = 1:10,
  y1 = 1:10,
  c1 = rep(0.9, 10),

  # Keypoint 2: Undetected (all c2 are 0)
  x2 = 1:10,
  y2 = 1:10,
  c2 = rep(0, 10),

  # Keypoint 3: Low mean confidence (mean is 0.1)
  x3 = 1:10,
  y3 = 1:10,
  c3 = rep(0.1, 10),

  # Keypoint 4: High missing data (40% zeros in y4)
  x4 = 1:10,
  y4 = c(rep(1, 6), rep(0, 4)),
  c4 = rep(0.8, 10),

  # Keypoint 5: For group-specific removal (low confidence for person B)
  x5 = 1:10,
  y5 = 1:10,
  c5 = c(rep(0.9, 5), rep(0.1, 5)),

  # Grouping variables
  person = rep(c("A", "B"), each = 5),
  region = rep("body", 10)
)

# 2. TESTS
test_that("removes specific keypoints correctly", {
  result <- op_remove_keypoints(test_df, remove_specific_keypoints = "1")

  expect_false("x1" %in% names(result))
  expect_false("y1" %in% names(result))
  expect_false("c1" %in% names(result))
  expect_true("x0" %in% names(result)) # Control column should remain
})

test_that("removes undetected keypoints correctly", {
  result <- op_remove_keypoints(test_df, remove_undetected_keypoints = TRUE)

  expect_false("x2" %in% names(result))
  expect_true("x0" %in% names(result))
})

# ... (the rest of the tests remain the same) ...

test_that("function handles edge cases gracefully", {
  # Empty data frame
  empty_df <- data.frame()
  expect_equal(op_remove_keypoints(empty_df), empty_df)

  # Data frame with no keypoint columns
  no_kp_df <- data.frame(a = 1:5, b = letters[1:5])
  expect_equal(op_remove_keypoints(no_kp_df), no_kp_df)
})
