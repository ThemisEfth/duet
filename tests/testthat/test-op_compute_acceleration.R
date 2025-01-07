library(testthat)

test_that("op_compute_acceleration calculates acceleration correctly", {
  # Import the dataset from inst/extdata/csv_data/A-B_body_dyad_velocity.csv
  sample_data_path <- system.file("extdata/csv_data/A-B_body_dyad_velocity.csv", package = "duet")
  sample_data <- read.csv(sample_data_path)

  # Verify the structure of the dataset
  expect_true(any(grepl("^x", names(sample_data))), info = "No 'x' columns found in the dataset")
  expect_true(any(grepl("^y", names(sample_data))), info = "No 'y' columns found in the dataset")
  expect_true(is.data.frame(sample_data))

  # Test: Compute acceleration with FPS provided
  acceleration_data <- op_compute_acceleration(
    data = sample_data,
    fps = 25,
    overwrite = FALSE,
    merge_xy = FALSE
  )
  expect_true(any(grepl("^a_x", names(acceleration_data))), info = "Acceleration columns for x were not created")
  expect_true(any(grepl("^a_y", names(acceleration_data))), info = "Acceleration columns for y were not created")

  # Test: Compute acceleration with video duration provided
  acceleration_data_duration <- op_compute_acceleration(
    data = sample_data,
    video_duration = 300,
    overwrite = FALSE,
    merge_xy = FALSE
  )
  expect_true(any(grepl("^a_x", names(acceleration_data_duration))), info = "Acceleration columns for x were not created using video duration")
  expect_true(any(grepl("^a_y", names(acceleration_data_duration))), info = "Acceleration columns for y were not created using video duration")

  # Test: Overwrite original 'x' and 'y' columns
  acceleration_data_overwrite <- op_compute_acceleration(
    data = sample_data,
    fps = 25,
    overwrite = TRUE,
    merge_xy = FALSE
  )
  expect_false(any(grepl("^x", names(acceleration_data_overwrite))), info = "'x' columns were not removed when overwrite = TRUE")
  expect_false(any(grepl("^y", names(acceleration_data_overwrite))), info = "'y' columns were not removed when overwrite = TRUE")

  # Test: Merge 'x' and 'y' columns to calculate Euclidean acceleration
  acceleration_data_merged <- op_compute_acceleration(
    data = sample_data,
    fps = 25,
    overwrite = FALSE,
    merge_xy = TRUE
  )
  expect_true(any(grepl("^a_", names(acceleration_data_merged))), info = "Merged acceleration columns were not created")

  # Test: Missing both fps and video duration
  expect_error(
    op_compute_acceleration(
      data = sample_data,
      fps = NULL,
      video_duration = NULL,
      overwrite = FALSE,
      merge_xy = FALSE
    ),
    regexp = "Either fps or video_duration must be provided"  # Ensure exact match
  )

  # Test: Incorrect column matching between 'x' and 'y'
  incomplete_data <- sample_data
  incomplete_data <- incomplete_data[, !grepl("^y", names(incomplete_data))] # Remove 'y' columns
  expect_error(
    op_compute_acceleration(
      data = incomplete_data,
      fps = 30,
      overwrite = FALSE,
      merge_xy = TRUE
    ),
    regexp = "Error in merge_xy calculations: Both x and y columns are required for merging"
  )
})
