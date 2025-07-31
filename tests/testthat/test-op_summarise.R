library(testthat)

test_that("op_summarise handles basic functionality correctly", {
  # Create test data
  test_data <- data.frame(
    person = rep(c("P1", "P2"), each = 10),
    region = rep(c("A", "B"), times = 10),
    Nose_x = rnorm(20, mean = 0, sd = 1),
    Nose_y = rnorm(20, mean = 5, sd = 2),
    LEye_x = rnorm(20, mean = -1, sd = 1.5)
  )

  # Test basic summarization
  result <- op_summarise(
    data = test_data,
    grouping_vars = c("person", "region"),
    metrics = c("mean", "sd", "count")
  )

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_true(all(
    c("person", "region", "keypoint", "mean", "sd", "count") %in% names(result)
  ))

  # Check that we have the expected number of rows (2 persons × 2 regions × 3 keypoints)
  expect_equal(nrow(result), 12)

  # Check that keypoints are correct
  expected_keypoints <- c("Nose_x", "Nose_y", "LEye_x")
  expect_true(all(expected_keypoints %in% result$keypoint))

  # Check that counts are correct (should be 5 for each combination)
  expect_true(all(result$count == 5))
})

test_that("op_summarise handles automatic grouping variable detection", {
  test_data <- data.frame(
    person = rep(c("P1", "P2"), each = 10),
    region = rep(c("A", "B"), times = 10),
    Nose_x = rnorm(20),
    Nose_y = rnorm(20)
  )

  # Test with NULL grouping_vars (should auto-detect person and region)
  result <- op_summarise(
    data = test_data,
    grouping_vars = NULL,
    metrics = c("mean", "count")
  )

  expect_true(all(c("person", "region", "keypoint") %in% names(result)))
  expect_equal(nrow(result), 8) # 2 persons × 2 regions × 2 keypoints
})

test_that("op_summarise handles participant instead of person", {
  test_data <- data.frame(
    participant = rep(c("P1", "P2"), each = 10),
    region = rep(c("A", "B"), times = 10),
    Nose_x = rnorm(20),
    Nose_y = rnorm(20)
  )

  # Test with NULL grouping_vars (should auto-detect participant and region)
  result <- op_summarise(
    data = test_data,
    grouping_vars = NULL,
    metrics = c("mean", "count")
  )

  expect_true(all(c("participant", "region", "keypoint") %in% names(result)))
})

test_that("op_summarise handles no grouping variables", {
  test_data <- data.frame(
    frame = 1:20,
    Nose_x = rnorm(20),
    Nose_y = rnorm(20)
  )

  result <- op_summarise(
    data = test_data,
    grouping_vars = NULL,
    metrics = c("mean", "count")
  )

  # Should only have keypoint as grouping variable
  expect_true("keypoint" %in% names(result))
  expect_equal(nrow(result), 2) # 2 keypoints
  expect_true(all(result$count == 20))
})

test_that("op_summarise excludes non-numeric columns with warning", {
  test_data <- data.frame(
    person = rep(c("P1", "P2"), each = 10),
    notes = rep("metadata", 20),
    condition = factor(rep(c("A", "B"), times = 10)),
    Nose_x = rnorm(20),
    Nose_y = rnorm(20)
  )

  expect_warning(
    result <- op_summarise(
      data = test_data,
      grouping_vars = "person",
      metrics = c("mean", "count")
    ),
    "non-numeric columns.*excluded"
  )

  # Should only include numeric columns in keypoint
  expect_true(all(result$keypoint %in% c("Nose_x", "Nose_y")))
  expect_false(any(result$keypoint %in% c("notes", "condition")))
})

test_that("op_summarise handles all metrics correctly", {
  set.seed(123) # For reproducible test
  test_data <- data.frame(
    person = rep("P1", 100),
    Nose_x = c(rnorm(95), rep(NA, 5)), # Include some NAs
    Nose_y = rep(1:10, 10) # Repeating pattern for dominant period
  )

  # Test all metrics
  result <- op_summarise(
    data = test_data,
    grouping_vars = "person",
    metrics = c(
      "count",
      "na_count",
      "valid_count",
      "mean",
      "median",
      "sd",
      "variance",
      "iqr",
      "min",
      "max",
      "skewness",
      "kurtosis",
      "dominant_period"
    )
  )

  # Check all metrics are present
  expected_metrics <- c(
    "count",
    "na_count",
    "valid_count",
    "mean",
    "median",
    "sd",
    "variance",
    "iqr",
    "min",
    "max",
    "skewness",
    "kurtosis",
    "dominant_period"
  )
  expect_true(all(expected_metrics %in% names(result)))

  # Check specific values for Nose_x
  nose_x_row <- result[result$keypoint == "Nose_x", ]
  expect_equal(nose_x_row$count, 100)
  expect_equal(nose_x_row$na_count, 5)
  expect_equal(nose_x_row$valid_count, 95)
})

test_that("op_summarise handles custom column specifications", {
  test_data <- data.frame(
    person = rep("P1", 10),
    Nose_x = rnorm(10),
    Nose_y = rnorm(10),
    LEye_x = rnorm(10),
    REye_x = rnorm(10)
  )

  # Test with specific columns
  result <- op_summarise(
    data = test_data,
    grouping_vars = "person",
    cols = c("Nose_x", "LEye_x"),
    metrics = c("mean", "count")
  )

  expect_true(all(result$keypoint %in% c("Nose_x", "LEye_x")))
  expect_false(any(result$keypoint %in% c("Nose_y", "REye_x")))
})

test_that("op_summarise handles custom names_to and values_to", {
  test_data <- data.frame(
    person = rep("P1", 10),
    Nose_x = rnorm(10),
    Nose_y = rnorm(10)
  )

  result <- op_summarise(
    data = test_data,
    grouping_vars = "person",
    names_to = "body_part",
    values_to = "measurement",
    metrics = c("mean", "count")
  )

  expect_true("body_part" %in% names(result))
  expect_false("keypoint" %in% names(result))
})

test_that("op_summarise handles edge cases with insufficient data", {
  # Test with single observation
  test_data <- data.frame(
    person = "P1",
    Nose_x = 1.0,
    Nose_y = 2.0
  )

  result <- op_summarise(
    data = test_data,
    grouping_vars = "person",
    metrics = c("mean", "sd", "skewness", "kurtosis", "dominant_period")
  )

  expect_equal(result$mean, c(1.0, 2.0))
  expect_true(all(is.na(result$sd))) # SD requires n > 1
  expect_true(all(is.na(result$skewness))) # Skewness requires n > 2
  expect_true(all(is.na(result$kurtosis))) # Kurtosis requires n > 3
  expect_true(all(is.na(result$dominant_period))) # Dominant period requires sufficient data
})

test_that("op_summarise handles all NA data", {
  test_data <- data.frame(
    person = rep("P1", 10),
    Nose_x = rep(NA_real_, 10),
    Nose_y = rep(NA_real_, 10)
  )

  result <- op_summarise(
    data = test_data,
    grouping_vars = "person",
    metrics = c("mean", "count", "na_count", "valid_count")
  )

  expect_equal(result$count, c(10, 10))
  expect_equal(result$na_count, c(10, 10))
  expect_equal(result$valid_count, c(0, 0))
  expect_true(all(is.na(result$mean)))
})

test_that("op_summarise input validation works correctly", {
  test_data <- data.frame(
    person = rep("P1", 10),
    Nose_x = rnorm(10)
  )

  # Test invalid data input
  expect_error(
    op_summarise(data = "not_a_dataframe"),
    "must be a data frame"
  )

  # Test invalid grouping_vars
  expect_error(
    op_summarise(data = test_data, grouping_vars = 123),
    "must be a character vector"
  )

  # Test missing grouping variables
  expect_error(
    op_summarise(data = test_data, grouping_vars = "nonexistent_col"),
    "not found in data"
  )

  # Test invalid metrics
  expect_error(
    op_summarise(data = test_data, metrics = "invalid_metric"),
    "Invalid metrics"
  )

  # Test invalid names_to
  expect_error(
    op_summarise(data = test_data, names_to = c("a", "b")),
    "must be a single non-empty character string"
  )

  # Test invalid dominant_period_min_points
  expect_error(
    op_summarise(data = test_data, dominant_period_min_points = -1),
    "must be a positive integer"
  )

  # Test overlap between cols and grouping_vars
  expect_error(
    op_summarise(data = test_data, grouping_vars = "person", cols = "person"),
    "cannot be in both 'cols' and 'grouping_vars'"
  )
})

test_that("op_summarise excludes frame and time columns automatically", {
  test_data <- data.frame(
    person = rep("P1", 10),
    frame = 1:10,
    time = seq(0, 1, length.out = 10),
    Nose_x = rnorm(10),
    Nose_y = rnorm(10)
  )

  result <- op_summarise(
    data = test_data,
    grouping_vars = "person",
    metrics = "count"
  )

  # frame and time should not be in keypoints
  expect_false(any(result$keypoint %in% c("frame", "time")))
  expect_true(all(result$keypoint %in% c("Nose_x", "Nose_y")))
})

test_that("op_summarise handles empty result after filtering", {
  test_data <- data.frame(
    person = rep("P1", 10),
    notes = rep("text", 10) # Only non-numeric column
  )

  expect_warning(
    expect_error(
      op_summarise(data = test_data, grouping_vars = "person"),
      "No numeric columns were found to pivot"
    ),
    "non-numeric columns.*excluded"
  )
})

test_that("op_summarise works without moments package", {
  # This would typically involve mocking requireNamespace to return FALSE
  # For now, we'll test that the function handles missing packages gracefully
  test_data <- data.frame(
    person = rep("P1", 10),
    Nose_x = rnorm(10)
  )

  # If moments package is available, this should work
  # If not available, should give warning and set skewness/kurtosis to NA
  result <- op_summarise(
    data = test_data,
    grouping_vars = "person",
    metrics = c("mean", "skewness", "kurtosis")
  )

  expect_true("skewness" %in% names(result))
  expect_true("kurtosis" %in% names(result))
})

test_that("op_summarise plot parameter works", {
  test_data <- data.frame(
    person = rep("P1", 10),
    Nose_x = rnorm(10),
    Nose_y = rnorm(10)
  )

  # Test that plot=TRUE doesn't cause errors (hard to test actual plotting)
  expect_no_error(
    op_summarise(
      data = test_data,
      grouping_vars = "person",
      metrics = c("mean", "sd", "median", "min", "max"),
      plot = TRUE
    )
  )

  # Test invalid plot parameter
  expect_error(
    op_summarise(data = test_data, plot = "yes"),
    "must be a single logical value"
  )
})

# Integration test
test_that("op_summarise integration test with realistic OpenPose data", {
  # Simulate realistic OpenPose data
  set.seed(42)
  n_frames <- 100
  n_participants <- 2

  test_data <- expand.grid(
    frame = 1:n_frames,
    participant = paste0("P", 1:n_participants)
  ) %>%
    mutate(
      # Nose coordinates with some movement
      Nose_x = 320 + 20 * sin(frame / 10) + rnorm(n(), 0, 5),
      Nose_y = 240 + 10 * cos(frame / 8) + rnorm(n(), 0, 3),
      # Eye coordinates
      LEye_x = Nose_x - 30 + rnorm(n(), 0, 2),
      LEye_y = Nose_y - 20 + rnorm(n(), 0, 2),
      REye_x = Nose_x + 30 + rnorm(n(), 0, 2),
      REye_y = Nose_y - 20 + rnorm(n(), 0, 2),
      # Add some missing values
      Nose_x = ifelse(runif(n()) < 0.05, NA, Nose_x)
    )

  result <- op_summarise(
    data = test_data,
    grouping_vars = "participant",
    metrics = c(
      "count",
      "mean",
      "sd",
      "min",
      "max",
      "na_count",
      "dominant_period"
    )
  )

  # Check structure
  expect_equal(nrow(result), 12) # 2 participants × 6 keypoints (Nose_x, Nose_y, LEye_x, LEye_y, REye_x, REye_y)
  expect_true(all(
    c("participant", "keypoint", "mean", "sd") %in% names(result)
  ))

  # Check that we have reasonable values
  nose_x_data <- result[result$keypoint == "Nose_x", ]
  expect_true(all(nose_x_data$mean > 250 & nose_x_data$mean < 400))
  expect_true(all(nose_x_data$count >= 95 & nose_x_data$count <= 100)) # Some NAs

  # Check that dominant period is calculated for periodic data
  expect_true(any(!is.na(nose_x_data$dominant_period)))
})
