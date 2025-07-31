library(testthat)

# Test suite for op_compute_motionenergy function
test_that("op_compute_motionenergy basic functionality works", {
  # Create sample test data
  sample_data <- data.frame(
    base_filename = rep("Test_File", 20),
    frame = rep(1:10, 2),
    region = "body",
    person = rep(c("left", "right"), each = 10),
    x0 = c(seq(100, 109, 1), seq(200, 209, 1)), # Linear motion
    y0 = c(seq(50, 59, 1), seq(150, 159, 1)),
    c0 = rep(0.9, 20),
    x1 = c(seq(120, 129, 1), seq(220, 229, 1)),
    y1 = c(seq(70, 79, 1), seq(170, 179, 1)),
    c1 = rep(0.8, 20),
    stringsAsFactors = FALSE
  )

  # Test basic functionality with default parameters
  result <- op_compute_motionenergy(sample_data)

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_true("motion_energy" %in% names(result))
  expect_true("frame" %in% names(result))
  expect_true(nrow(result) > 0)

  # Should have fewer rows than input due to frame differencing
  expect_lt(nrow(result), nrow(sample_data))
})

test_that("Auto-detection of ID columns works correctly", {
  sample_data <- data.frame(
    subject_id = "S001",
    session = "Session1",
    frame = 1:5,
    x0 = 1:5,
    y0 = 1:5,
    c0 = rep(0.9, 5),
    stringsAsFactors = FALSE
  )

  # Capture messages to verify auto-detection
  expect_message(
    result <- op_compute_motionenergy(sample_data),
    "Auto-detected ID columns"
  )

  expect_true("subject_id" %in% names(result))
  expect_true("session" %in% names(result))
})

test_that("Manual ID column specification works", {
  sample_data <- data.frame(
    id1 = "A",
    id2 = "B",
    frame = 1:5,
    x0 = 1:5,
    y0 = 1:5,
    stringsAsFactors = FALSE
  )

  result <- op_compute_motionenergy(sample_data, id_cols = c("id1", "id2"))

  expect_true("id1" %in% names(result))
  expect_true("id2" %in% names(result))
})

test_that("Different methods produce different results", {
  sample_data <- data.frame(
    frame = 1:5,
    x0 = c(0, 2, 0, 2, 0), # Creates movement
    y0 = c(0, 0, 2, 2, 0),
    stringsAsFactors = FALSE
  )

  result_abs <- op_compute_motionenergy(sample_data, method = "absolute")
  result_sq <- op_compute_motionenergy(sample_data, method = "squared")

  # Results should be different
  expect_false(identical(result_abs$motion_energy, result_sq$motion_energy))

  # Squared method should generally produce larger values for movements > 1
  expect_true(any(
    result_sq$motion_energy >= result_abs$motion_energy,
    na.rm = TRUE
  ))
})

test_that("Coordinate aggregation options work correctly", {
  sample_data <- data.frame(
    frame = 1:5,
    x0 = 1:5,
    y0 = 1:5,
    stringsAsFactors = FALSE
  )

  # Test with aggregate_coordinates = FALSE
  result_separate <- op_compute_motionenergy(
    sample_data,
    aggregate_coordinates = FALSE
  )
  expect_true("x_motion" %in% names(result_separate))
  expect_true("y_motion" %in% names(result_separate))
  expect_false("motion_energy" %in% names(result_separate))

  # Test with aggregate_coordinates = TRUE (default)
  result_combined <- op_compute_motionenergy(
    sample_data,
    aggregate_coordinates = TRUE
  )
  expect_true("motion_energy" %in% names(result_combined))
  expect_false("x_motion" %in% names(result_combined))
  expect_false("y_motion" %in% names(result_combined))
})

test_that("Keypoint aggregation options work correctly", {
  sample_data <- data.frame(
    frame = 1:5,
    x0 = 1:5,
    y0 = 1:5,
    x1 = 2:6,
    y1 = 2:6,
    stringsAsFactors = FALSE
  )

  # Test with aggregate_keypoints = FALSE
  result_per_kp <- op_compute_motionenergy(
    sample_data,
    aggregate_keypoints = FALSE
  )
  expect_true("keypoint" %in% names(result_per_kp))
  expect_true(length(unique(result_per_kp$keypoint)) > 1)

  # Test with aggregate_keypoints = TRUE (default)
  result_aggregated <- op_compute_motionenergy(
    sample_data,
    aggregate_keypoints = TRUE
  )
  expect_false("keypoint" %in% names(result_aggregated))
})

test_that("NA handling methods work correctly", {
  sample_data <- data.frame(
    frame = 1:5,
    x0 = c(1, NA, 3, 4, 5),
    y0 = c(1, 2, NA, 4, 5),
    stringsAsFactors = FALSE
  )

  # Test omit (default)
  result_omit <- op_compute_motionenergy(sample_data, na_action = "omit")
  expect_true(nrow(result_omit) <= 4) # Should have fewer rows due to NA removal

  # Test zero
  result_zero <- op_compute_motionenergy(sample_data, na_action = "zero")
  expect_true(nrow(result_zero) >= nrow(result_omit))

  # Test interpolate
  result_interp <- op_compute_motionenergy(
    sample_data,
    na_action = "interpolate"
  )
  expect_true(nrow(result_interp) >= nrow(result_omit))
})

test_that("rmea_format conversion works correctly", {
  sample_data <- data.frame(
    frame = rep(1:3, 4),
    region = rep(c("body", "face"), each = 6),
    person = rep(c("left", "right"), 6),
    x0 = 1:12,
    y0 = 1:12,
    stringsAsFactors = FALSE
  )

  result <- op_compute_motionenergy(sample_data, rmea_format = TRUE)

  # Should have columns for region*person combinations
  expect_true(any(grepl("\\*", names(result))))
  expect_true("frame" %in% names(result))

  # Should not have original region/person columns
  expect_false("region" %in% names(result))
  expect_false("person" %in% names(result))
})

test_that("Error handling works correctly", {
  # Test with non-data.frame input
  expect_error(
    op_compute_motionenergy("not_a_dataframe"),
    "data must be a data.frame"
  )

  # Test with missing frame column
  bad_data <- data.frame(x0 = 1:5, y0 = 1:5)
  expect_error(
    op_compute_motionenergy(bad_data),
    "Frame column 'frame' not found"
  )

  # Test with no coordinate columns
  bad_data2 <- data.frame(frame = 1:5, other_col = 1:5)
  expect_error(
    op_compute_motionenergy(bad_data2),
    "No coordinate columns found"
  )

  # Test rmea_format without required columns
  bad_data3 <- data.frame(frame = 1:5, x0 = 1:5, y0 = 1:5)
  expect_error(
    op_compute_motionenergy(bad_data3, rmea_format = TRUE),
    "rmea_format requires 'region' and 'person' columns"
  )

  # Test with non-existent ID columns
  sample_data <- data.frame(frame = 1:5, x0 = 1:5, y0 = 1:5)
  expect_error(
    op_compute_motionenergy(sample_data, id_cols = "nonexistent"),
    "Specified id_cols not found in data"
  )
})

test_that("Edge cases are handled properly", {
  # Test with single frame (should return empty result with warning)
  single_frame <- data.frame(
    frame = 1,
    x0 = 100,
    y0 = 50,
    stringsAsFactors = FALSE
  )

  expect_warning(
    result <- op_compute_motionenergy(single_frame),
    "No valid motion data calculated"
  )

  # Result should be empty data frame with proper structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true("motion_energy" %in% names(result))
  expect_true("frame" %in% names(result))

  # Test with identical coordinates (no motion)
  no_motion <- data.frame(
    frame = 1:5,
    x0 = rep(100, 5),
    y0 = rep(50, 5),
    stringsAsFactors = FALSE
  )

  result <- op_compute_motionenergy(no_motion)
  expect_s3_class(result, "data.frame")
  if (nrow(result) > 0) {
    expect_true(all(result$motion_energy == 0, na.rm = TRUE))
  }

  # Test with empty data frame
  empty_data <- data.frame(
    frame = integer(0),
    x0 = numeric(0),
    y0 = numeric(0),
    stringsAsFactors = FALSE
  )

  expect_warning(
    result_empty <- op_compute_motionenergy(empty_data),
    "No valid motion data calculated"
  )

  expect_s3_class(result_empty, "data.frame")
  expect_equal(nrow(result_empty), 0)

  # Test with only NA values
  all_na_data <- data.frame(
    frame = 1:3,
    x0 = c(NA, NA, NA),
    y0 = c(NA, NA, NA),
    stringsAsFactors = FALSE
  )

  # The expected warning is now consistent with other invalid data cases.
  expect_warning(
    result_na <- op_compute_motionenergy(all_na_data, na_action = "omit"),
    "No valid motion data calculated"
  )

  expect_s3_class(result_na, "data.frame")
  expect_equal(nrow(result_na), 0)

  # Test edge case: two frames with NA in between
  sparse_data <- data.frame(
    frame = 1:4,
    x0 = c(1, NA, NA, 4),
    y0 = c(1, NA, NA, 4),
    stringsAsFactors = FALSE
  )

  # With omit action, should have minimal data
  result_sparse_omit <- op_compute_motionenergy(sparse_data, na_action = "omit")
  expect_s3_class(result_sparse_omit, "data.frame")

  # With zero action, should treat NA as zero
  result_sparse_zero <- op_compute_motionenergy(sparse_data, na_action = "zero")
  expect_s3_class(result_sparse_zero, "data.frame")
  expect_true(nrow(result_sparse_zero) >= nrow(result_sparse_omit))
})

test_that("Multiple keypoints are processed correctly", {
  sample_data <- data.frame(
    frame = 1:5,
    x0 = 1:5, # Keypoint 0
    y0 = 1:5,
    x1 = 2:6, # Keypoint 1
    y1 = 3:7,
    x2 = 3:7, # Keypoint 2
    y2 = 4:8,
    stringsAsFactors = FALSE
  )

  # Test per-keypoint results
  result_per_kp <- op_compute_motionenergy(
    sample_data,
    aggregate_keypoints = FALSE
  )

  keypoints <- unique(result_per_kp$keypoint)
  expect_true(length(keypoints) == 3)
  expect_setequal(keypoints, c(0, 1, 2))

  # Test aggregated results
  result_agg <- op_compute_motionenergy(sample_data, aggregate_keypoints = TRUE)
  expect_false("keypoint" %in% names(result_agg))
  expect_true("motion_energy" %in% names(result_agg))
})

test_that("Frame ordering is maintained", {
  # Create data with non-sequential frame order
  sample_data <- data.frame(
    frame = c(3, 1, 4, 2, 5),
    x0 = c(30, 10, 40, 20, 50),
    y0 = c(30, 10, 40, 20, 50),
    stringsAsFactors = FALSE
  )

  result <- op_compute_motionenergy(sample_data)

  # Result should be ordered by frame
  if (nrow(result) > 1) {
    expect_true(all(diff(result$frame) > 0))
  }
})

test_that("Grouping variables work correctly", {
  sample_data <- data.frame(
    subject = rep(c("A", "B"), each = 10),
    frame = rep(1:5, 4),
    x0 = c(1:5, 1:5, 10:14, 10:14),
    y0 = c(1:5, 1:5, 10:14, 10:14),
    stringsAsFactors = FALSE
  )

  result <- op_compute_motionenergy(sample_data)

  # Should have separate calculations for each subject
  expect_true("subject" %in% names(result))
  expect_setequal(unique(result$subject), c("A", "B"))

  # Each subject should have motion calculations
  subjects <- unique(result$subject)
  for (subj in subjects) {
    subj_data <- result[result$subject == subj, ]
    expect_true(nrow(subj_data) > 0)
  }
})

test_that("Coordinate column detection works with various formats", {
  # Test with missing some coordinates
  sample_data <- data.frame(
    frame = 1:5,
    x0 = 1:5,
    y0 = 1:5,
    x2 = 3:7, # Skip x1, y1
    y2 = 4:8,
    x5 = 6:10, # Skip x3, y3, x4, y4
    y5 = 7:11,
    stringsAsFactors = FALSE
  )

  result <- op_compute_motionenergy(sample_data, aggregate_keypoints = FALSE)

  # Should process available keypoints
  keypoints <- unique(result$keypoint)
  expect_setequal(keypoints, c(0, 2, 5))
})

# Test mathematical correctness of motion calculation
test_that("Motion energy calculation is mathematically correct", {
  # Create simple test case with known movement
  sample_data <- data.frame(
    frame = 1:4,
    x0 = c(0, 3, 3, 0), # Movement: 3, 0, -3
    y0 = c(0, 0, 4, 4), # Movement: 0, 4, 0
    stringsAsFactors = FALSE
  )

  result <- op_compute_motionenergy(sample_data, method = "absolute")

  # Expected motion energies: sqrt(3^2 + 0^2), sqrt(0^2 + 4^2), sqrt(3^2 + 0^2)
  expected <- c(3, 4, 3)

  expect_equal(result$motion_energy, expected, tolerance = 1e-10)
})

# Test with confidence columns present
test_that("Confidence columns are ignored appropriately", {
  sample_data <- data.frame(
    frame = 1:5,
    x0 = 1:5,
    y0 = 1:5,
    c0 = rep(0.9, 5), # Confidence column should be ignored
    x1 = 2:6,
    y1 = 2:6,
    c1 = rep(0.8, 5),
    stringsAsFactors = FALSE
  )

  result <- op_compute_motionenergy(sample_data)

  # Should work without errors and not include confidence columns in output
  expect_false("c0" %in% names(result))
  expect_false("c1" %in% names(result))
  expect_true("motion_energy" %in% names(result))
})

test_that("Multiple keypoints are processed correctly", {
  sample_data <- data.frame(
    frame = 1:5,
    x0 = 1:5, # Keypoint 0
    y0 = 1:5,
    x1 = 2:6, # Keypoint 1
    y1 = 3:7,
    x2 = 3:7, # Keypoint 2
    y2 = 4:8,
    stringsAsFactors = FALSE
  )

  # Test per-keypoint results
  result_per_kp <- op_compute_motionenergy(
    sample_data,
    aggregate_keypoints = FALSE
  )

  keypoints <- unique(result_per_kp$keypoint)
  expect_true(length(keypoints) == 3)
  expect_setequal(keypoints, c(0, 1, 2))

  # Test aggregated results
  result_agg <- op_compute_motionenergy(sample_data, aggregate_keypoints = TRUE)
  expect_false("keypoint" %in% names(result_agg))
  expect_true("motion_energy" %in% names(result_agg))
})

test_that("Frame ordering is maintained", {
  # Create data with non-sequential frame order
  sample_data <- data.frame(
    frame = c(3, 1, 4, 2, 5),
    x0 = c(30, 10, 40, 20, 50),
    y0 = c(30, 10, 40, 20, 50),
    stringsAsFactors = FALSE
  )

  result <- op_compute_motionenergy(sample_data)

  # Result should be ordered by frame
  if (nrow(result) > 1) {
    expect_true(all(diff(result$frame) > 0))
  }
})

test_that("Grouping variables work correctly", {
  sample_data <- data.frame(
    subject = rep(c("A", "B"), each = 10),
    frame = rep(1:5, 4),
    x0 = c(1:5, 1:5, 10:14, 10:14),
    y0 = c(1:5, 1:5, 10:14, 10:14),
    stringsAsFactors = FALSE
  )

  result <- op_compute_motionenergy(sample_data)

  # Should have separate calculations for each subject
  expect_true("subject" %in% names(result))
  expect_setequal(unique(result$subject), c("A", "B"))

  # Each subject should have motion calculations
  subjects <- unique(result$subject)
  for (subj in subjects) {
    subj_data <- result[result$subject == subj, ]
    expect_true(nrow(subj_data) > 0)
  }
})

test_that("Coordinate column detection works with various formats", {
  # Test with missing some coordinates
  sample_data <- data.frame(
    frame = 1:5,
    x0 = 1:5,
    y0 = 1:5,
    x2 = 3:7, # Skip x1, y1
    y2 = 4:8,
    x5 = 6:10, # Skip x3, y3, x4, y4
    y5 = 7:11,
    stringsAsFactors = FALSE
  )

  result <- op_compute_motionenergy(sample_data, aggregate_keypoints = FALSE)

  # Should process available keypoints
  keypoints <- unique(result$keypoint)
  expect_setequal(keypoints, c(0, 2, 5))
})

# Test mathematical correctness of motion calculation
test_that("Motion energy calculation is mathematically correct", {
  # Create simple test case with known movement
  sample_data <- data.frame(
    frame = 1:4,
    x0 = c(0, 3, 3, 0), # Movement: 3, 0, -3
    y0 = c(0, 0, 4, 4), # Movement: 0, 4, 0
    stringsAsFactors = FALSE
  )

  result <- op_compute_motionenergy(sample_data, method = "absolute")

  # Expected motion energies: sqrt(3^2 + 0^2), sqrt(0^2 + 4^2), sqrt(3^2 + 0^2)
  expected <- c(3, 4, 3)

  expect_equal(result$motion_energy, expected, tolerance = 1e-10)
})

# Test with confidence columns present
test_that("Confidence columns are ignored appropriately", {
  sample_data <- data.frame(
    frame = 1:5,
    x0 = 1:5,
    y0 = 1:5,
    c0 = rep(0.9, 5), # Confidence column should be ignored
    x1 = 2:6,
    y1 = 2:6,
    c1 = rep(0.8, 5),
    stringsAsFactors = FALSE
  )

  result <- op_compute_motionenergy(sample_data)

  # Should work without errors and not include confidence columns in output
  expect_false("c0" %in% names(result))
  expect_false("c1" %in% names(result))
  expect_true("motion_energy" %in% names(result))
})
