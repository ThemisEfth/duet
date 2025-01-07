library(testthat)

test_that("op_compute_jerk calculates jerk correctly", {
  # Import the dataset
  sample_data_path <- system.file("extdata/csv_data/A-B_body_dyad_accel.csv", package = "duet")
  sample_data <- read.csv(sample_data_path)

  # Verify the structure of the dataset
  expect_true(any(grepl("^x", names(sample_data))), info = "No 'x' columns found in the dataset")
  expect_true(any(grepl("^y", names(sample_data))), info = "No 'y' columns found in the dataset")
  expect_true(is.data.frame(sample_data))

  # Test: Compute jerk with FPS provided
  jerk_data <- op_compute_jerk(
    data = sample_data,
    fps = 30,
    overwrite = FALSE,
    merge_xy = FALSE
  )
  # Verify that jerk columns for x and y are created
  expect_true(any(grepl("^j_x", names(jerk_data))), info = "Jerk columns for x were not created")
  expect_true(any(grepl("^j_y", names(jerk_data))), info = "Jerk columns for y were not created")

  # Verify jerk values for a sample column pair
  for (x_col in grep("^x", names(sample_data), value = TRUE)) {
    suffix <- sub("^x", "", x_col)
    y_col <- paste0("y", suffix)
    jerk_col_x <- paste0("j_", x_col)
    jerk_col_y <- paste0("j_", y_col)

    if (y_col %in% names(sample_data)) {
      # Expected jerk values for x and y
      velocity_x <- diff(c(NA, sample_data[[x_col]])) / (1 / 30)
      acceleration_x <- diff(c(NA, velocity_x)) / (1 / 30)
      expected_jerk_x <- diff(c(NA, acceleration_x)) / (1 / 30)

      velocity_y <- diff(c(NA, sample_data[[y_col]])) / (1 / 30)
      acceleration_y <- diff(c(NA, velocity_y)) / (1 / 30)
      expected_jerk_y <- diff(c(NA, acceleration_y)) / (1 / 30)

      expect_equal(jerk_data[[jerk_col_x]], expected_jerk_x, info = paste("Values in", jerk_col_x, "are incorrect"))
      expect_equal(jerk_data[[jerk_col_y]], expected_jerk_y, info = paste("Values in", jerk_col_y, "are incorrect"))
    }
  }

  # Test: Compute jerk with video duration provided
  jerk_data_duration <- op_compute_jerk(
    data = sample_data,
    video_duration = 10,
    overwrite = FALSE,
    merge_xy = FALSE
  )
  expect_true(any(grepl("^j_x", names(jerk_data_duration))), info = "Jerk columns for x were not created using video duration")
  expect_true(any(grepl("^j_y", names(jerk_data_duration))), info = "Jerk columns for y were not created using video duration")

  # Test: Overwrite original 'x' and 'y' columns
  jerk_data_overwrite <- op_compute_jerk(
    data = sample_data,
    fps = 30,
    overwrite = TRUE,
    merge_xy = FALSE
  )
  expect_false(any(grepl("^x", names(jerk_data_overwrite))), info = "'x' columns were not removed when overwrite = TRUE")
  expect_false(any(grepl("^y", names(jerk_data_overwrite))), info = "'y' columns were not removed when overwrite = TRUE")

  # Test: Merge 'x' and 'y' columns to calculate Euclidean jerk
  jerk_data_merged <- op_compute_jerk(
    data = sample_data,
    fps = 30,
    overwrite = FALSE,
    merge_xy = TRUE
  )
  expect_true(any(grepl("^j_", names(jerk_data_merged))), info = "Merged jerk columns were not created")

  # Test: Missing both fps and video duration
  expect_error(
    op_compute_jerk(
      data = sample_data,
      fps = NULL,
      video_duration = NULL,
      overwrite = FALSE,
      merge_xy = FALSE
    ),
    regexp = "Either fps or video_duration must be provided"
  )

  # Test: Incorrect column matching between 'x' and 'y'
  incomplete_data <- sample_data
  incomplete_data <- incomplete_data[, !grepl("^y", names(incomplete_data))] # Remove 'y' columns
  expect_error(
    op_compute_jerk(
      data = incomplete_data,
      fps = 30,
      overwrite = FALSE,
      merge_xy = TRUE
    ),
    "Error in merge_xy calculations"
  )
})
