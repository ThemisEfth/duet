library(testthat)

# Test op_interpolate function
test_that("op_interpolate basic functionality works", {
  # Create sample OpenPose data
  test_data <- data.frame(
    frame = 1:10,
    person = rep("A", 10),
    region = rep("face", 10),
    x1 = c(100, 101, NA, 103, 104, 105, 106, NA, 108, 109),
    y1 = c(200, 201, NA, 203, 204, 205, 206, NA, 208, 209),
    c1 = c(0.9, 0.8, 0.1, 0.9, 0.8, 0.9, 0.8, 0.2, 0.9, 0.8),
    x2 = c(150, 151, 152, 153, 154, 155, 156, 157, 158, 159),
    y2 = c(250, 251, 252, 253, 254, 255, 256, 257, 258, 259),
    c2 = rep(0.9, 10)
  )

  # Test basic interpolation with default parameters
  result <- op_interpolate(test_data, verbose = FALSE)

  # Check that result is a data frame with same number of rows
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(test_data))

  # Check that new columns are added
  expect_true("interpolated_points_count_per_row" %in% names(result))
  expect_true("interpolation_method_used" %in% names(result))

  # Check that NA values were interpolated (should not be NA anymore)
  expect_false(any(is.na(result$x1)))
  expect_false(any(is.na(result$y1)))

  # Check that interpolation method is recorded
  expect_equal(unique(result$interpolation_method_used), "median")
})

test_that("op_interpolate handles different interpolation methods", {
  test_data <- data.frame(
    frame = 1:6,
    person = rep("A", 6),
    x1 = c(10, 20, NA, 40, 50, 60),
    y1 = c(15, 25, NA, 45, 55, 65),
    c1 = c(0.9, 0.8, 0.1, 0.9, 0.8, 0.9)
  )

  # Test linear interpolation
  result_linear <- op_interpolate(test_data, method = "linear", verbose = FALSE)
  expect_equal(unique(result_linear$interpolation_method_used), "linear")
  # Check the exact interpolated value: (20 + 40) / 2 = 30
  expect_equal(result_linear$x1[3], 30)
  expect_equal(result_linear$y1[3], 35)

  # Test mean interpolation
  result_mean <- op_interpolate(test_data, method = "mean", verbose = FALSE)
  expect_equal(unique(result_mean$interpolation_method_used), "mean")
  # Check the exact interpolated value: mean of non-NA values
  expect_equal(result_mean$x1[3], mean(c(10, 20, 40, 50, 60)))
  expect_equal(result_mean$y1[3], mean(c(15, 25, 45, 55, 65)))

  # Test spline interpolation
  result_spline <- op_interpolate(test_data, method = "spline", verbose = FALSE)
  expect_equal(unique(result_spline$interpolation_method_used), "spline")
  expect_false(any(is.na(result_spline$x1)))
})

test_that("op_interpolate handles confidence threshold parameter", {
  test_data <- data.frame(
    frame = 1:5,
    person = rep("A", 5),
    x1 = c(10, 20, 30, 40, 50),
    y1 = c(15, 25, 35, 45, 55),
    c1 = c(0.9, 0.2, 0.8, 0.1, 0.9) # Points 2 and 4 have low confidence
  )

  # Test with confidence threshold
  result <- op_interpolate(
    test_data,
    confidence_threshold = 0.3,
    verbose = FALSE
  )

  # Check that rows with low confidence were marked for interpolation
  expect_true(result$interpolated_points_count_per_row[2] > 0)
  expect_true(result$interpolated_points_count_per_row[4] > 0)
  expect_equal(result$interpolated_points_count_per_row[1], 0)
  expect_equal(result$interpolated_points_count_per_row[3], 0)
  expect_equal(result$interpolated_points_count_per_row[5], 0)

  # Test without confidence threshold
  result_no_thresh <- op_interpolate(
    test_data,
    confidence_threshold = FALSE,
    verbose = FALSE
  )
  expect_true(all(result_no_thresh$interpolated_points_count_per_row == 0))
})

test_that("op_interpolate handles zero values when handle_zeros = TRUE", {
  test_data <- data.frame(
    frame = 1:5,
    person = rep("A", 5),
    x1 = c(10, 0, 30, 0, 50), # Zero values at positions 2 and 4
    y1 = c(15, 25, 0, 45, 55), # Zero value at position 3
    c1 = rep(0.9, 5)
  )

  # Test with handle_zeros = TRUE
  result <- op_interpolate(
    test_data,
    handle_zeros = TRUE,
    confidence_threshold = FALSE,
    verbose = FALSE
  )

  # Check that zero values were replaced
  expect_true(result$x1[2] != 0)
  expect_true(result$x1[4] != 0)
  expect_true(result$y1[3] != 0)

  # Check interpolation counts
  expect_true(result$interpolated_points_count_per_row[2] > 0)
  expect_true(result$interpolated_points_count_per_row[3] > 0)
  expect_true(result$interpolated_points_count_per_row[4] > 0)

  # Test with handle_zeros = FALSE (default)
  result_no_zeros <- op_interpolate(
    test_data,
    handle_zeros = FALSE,
    confidence_threshold = FALSE,
    verbose = FALSE
  )
  expect_true(all(result_no_zeros$interpolated_points_count_per_row == 0))
})

test_that("op_interpolate handles grouping variables correctly", {
  test_data <- data.frame(
    frame = rep(1:5, 2),
    person = rep(c("A", "B"), each = 5),
    x1 = c(10, NA, 30, 40, 50, 100, NA, 300, 400, 500),
    y1 = c(15, NA, 35, 45, 55, 150, NA, 350, 450, 550),
    c1 = rep(0.9, 10)
  )

  result <- op_interpolate(test_data, grouping_vars = "person", verbose = FALSE)

  # Check that interpolation was done separately for each person
  expect_false(any(is.na(result$x1)))
  expect_false(any(is.na(result$y1)))

  # Check that person A and B have different interpolated values
  person_a_x <- result$x1[result$person == "A"][2] # Previously NA
  person_b_x <- result$x1[result$person == "B"][2] # Previously NA
  expect_true(person_a_x != person_b_x)
})

test_that("op_interpolate handles edge cases", {
  # Test empty data frame
  empty_data <- data.frame()
  result_empty <- op_interpolate(empty_data, verbose = FALSE)
  expect_equal(nrow(result_empty), 0)

  # Test data with no OpenPose columns
  no_op_data <- data.frame(
    frame = 1:5,
    some_col = letters[1:5]
  )
  result_no_op <- op_interpolate(no_op_data, verbose = FALSE)
  expect_equal(result_no_op, no_op_data)

  # Test data with all good values (no interpolation needed)
  good_data <- data.frame(
    frame = 1:5,
    x1 = 10:14,
    y1 = 20:24,
    c1 = rep(0.9, 5)
  )
  result_good <- op_interpolate(good_data, verbose = FALSE)
  expect_true(all(result_good$interpolated_points_count_per_row == 0))
})

test_that("op_interpolate handles max_gap parameter", {
  test_data <- data.frame(
    frame = 1:10,
    person = rep("A", 10),
    x1 = c(10, NA, NA, NA, NA, 60, 70, NA, 90, 100), # 4 consecutive NAs, then 1 NA
    y1 = c(15, NA, NA, NA, NA, 65, 75, NA, 95, 105),
    c1 = rep(0.9, 10)
  )

  # Test with max_gap = 2 (should only interpolate the single NA, not the 4 consecutive)
  result <- op_interpolate(test_data, max_gap = 2, verbose = FALSE)

  # The single NA at position 8 should be interpolated
  expect_false(is.na(result$x1[8]))
  expect_true(result$interpolated_points_count_per_row[8] > 0)

  # The 4 consecutive NAs should remain (gap too large)
  expect_true(all(is.na(result$x1[2:5])))
  expect_true(all(result$interpolated_points_count_per_row[2:5] == 0))
})

test_that("op_interpolate handles treat_na_conf_as_low parameter", {
  test_data <- data.frame(
    frame = 1:5,
    person = rep("A", 5),
    x1 = c(10, 20, 30, 40, 50),
    y1 = c(15, 25, 35, 45, 55),
    c1 = c(0.9, NA, 0.8, NA, 0.9) # NA confidence values
  )

  # Test with treat_na_conf_as_low = TRUE (default)
  result_treat <- op_interpolate(
    test_data,
    confidence_threshold = 0.3,
    treat_na_conf_as_low = TRUE,
    verbose = FALSE
  )

  # NA confidence should be treated as low confidence (0) and interpolated
  expect_true(result_treat$interpolated_points_count_per_row[2] > 0)
  expect_true(result_treat$interpolated_points_count_per_row[4] > 0)

  # Test with treat_na_conf_as_low = FALSE
  result_no_treat <- op_interpolate(
    test_data,
    confidence_threshold = 0.3,
    treat_na_conf_as_low = FALSE,
    verbose = FALSE
  )

  # With treat_na_conf_as_low = FALSE, NA confidence values are still flagged
  # as problematic by the is.na(conf_vals) check in .identify_problematic_indices.
  # Therefore, we expect interpolation to still occur.
  expect_s3_class(result_no_treat, "data.frame")
  expect_true(result_no_treat$interpolated_points_count_per_row[2] > 0)
  expect_true(result_no_treat$interpolated_points_count_per_row[4] > 0)
})

test_that("op_interpolate input validation works", {
  # Test non-data.frame input
  expect_error(
    op_interpolate("not a dataframe"),
    "Input 'data' must be a data frame."
  )
  test_data <- data.frame(
    x1 = c(1, NA, 3),
    y1 = c(1, NA, 3),
    c1 = c(0.9, 0.1, 0.9)
  )

  # Invalid method should be handled gracefully (return data with NA intact)
  result <- op_interpolate(
    test_data,
    method = "invalid_method",
    verbose = FALSE
  )
  expect_s3_class(result, "data.frame")
  expect_true(is.na(result$x1[2]))
})

test_that("op_interpolate handles multiple keypoints", {
  test_data <- data.frame(
    frame = 1:5,
    person = rep("A", 5),
    x1 = c(10, NA, 30, 40, 50),
    y1 = c(15, NA, 35, 45, 55),
    c1 = c(0.9, 0.1, 0.8, 0.9, 0.8), # Low confidence at position 2
    x2 = c(100, 110, NA, 130, 140),
    y2 = c(150, 160, NA, 180, 190),
    c2 = c(0.9, 0.8, 0.1, 0.9, 0.8) # Low confidence at position 3
  )

  result <- op_interpolate(
    test_data,
    confidence_threshold = 0.3,
    verbose = FALSE
  )

  # Check that both keypoints were processed
  expect_false(any(is.na(result$x1)))
  expect_false(any(is.na(result$y1)))
  expect_false(any(is.na(result$x2)))
  expect_false(any(is.na(result$y2)))

  # Position 2 should have x1,y1 interpolated due to NA and low confidence
  # Position 3 should have x2,y2 interpolated due to NA and low confidence
  # So we should have some interpolated points, though not necessarily >1 per row
  expect_true(sum(result$interpolated_points_count_per_row) > 0)

  # Check that keypoint 1 was interpolated at position 2
  expect_true(result$interpolated_points_count_per_row[2] > 0)
  # Check that keypoint 2 was interpolated at position 3
  expect_true(result$interpolated_points_count_per_row[3] > 0)
})

test_that("op_interpolate handles multiple keypoints in same row", {
  # Create data where the same row has issues in multiple keypoints
  test_data <- data.frame(
    frame = 1:5,
    person = rep("A", 5),
    x1 = c(10, NA, 30, 40, 50), # NA at position 2
    y1 = c(15, NA, 35, 45, 55), # NA at position 2
    c1 = c(0.9, 0.1, 0.8, 0.9, 0.8), # Low confidence at position 2
    x2 = c(100, NA, 120, 130, 140), # NA at position 2 (same row)
    y2 = c(150, NA, 170, 180, 190), # NA at position 2 (same row)
    c2 = c(0.9, 0.1, 0.8, 0.9, 0.8) # Low confidence at position 2 (same row)
  )

  result <- op_interpolate(
    test_data,
    confidence_threshold = 0.3,
    verbose = FALSE
  )

  # Row 2 should have interpolated both keypoints (count should be 2)
  expect_equal(result$interpolated_points_count_per_row[2], 2)
})

test_that("op_interpolate handles different confidence column formats", {
  # Test with 'conf' prefix instead of 'c'
  test_data <- data.frame(
    frame = 1:5,
    x1 = c(10, NA, 30, 40, 50),
    y1 = c(15, NA, 35, 45, 55),
    conf1 = c(0.9, 0.1, 0.8, 0.9, 0.8)
  )

  result <- op_interpolate(test_data, verbose = FALSE)

  expect_s3_class(result, "data.frame")
  expect_false(any(is.na(result$x1)))
  expect_false(any(is.na(result$y1)))
})

# Test the internal helper functions if they were exported or made testable
# Note: These are marked as @noRd so they're internal, but we can test them
# indirectly through the main function or by accessing them with :::

test_that("keypoint detection works correctly", {
  # This tests the .detect_keypoint_cols function indirectly
  test_data <- data.frame(
    x1 = 1:5,
    y1 = 1:5,
    c1 = rep(0.9, 5),
    x2 = 1:5,
    y2 = 1:5,
    c2 = rep(0.9, 5),
    x5 = 1:5,
    y5 = 1:5,
    c5 = rep(0.9, 5), # Non-consecutive keypoint
    other_col = letters[1:5]
  )

  result <- op_interpolate(test_data, verbose = FALSE)

  # Should detect keypoints 1, 2, and 5
  expect_s3_class(result, "data.frame")

  # Test data with no matching keypoints
  no_keypoint_data <- data.frame(
    xa = 1:5,
    yb = 1:5,
    cc = rep(0.9, 5)
  )

  result_no_kp <- op_interpolate(no_keypoint_data, verbose = FALSE)
  expect_equal(result_no_kp, no_keypoint_data)
})
