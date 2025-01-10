library(testthat)

test_that("op_compute_velocity calculates velocity correctly", {
  # Import the dataset from inst/extdata/csv_data/A-B_body_dyad.csv
  sample_data_path <- system.file("extdata/csv_data/A-B_body_dyad.csv", package = "duet")
  sample_data <- read.csv(sample_data_path)

  # Verify the structure of the dataset
  expect_true(any(grepl("^x", names(sample_data))), info = "No 'x' columns found in the dataset")
  expect_true(any(grepl("^y", names(sample_data))), info = "No 'y' columns found in the dataset")
  expect_true(any(grepl("^c", names(sample_data))), info = "No 'c' columns found in the dataset")
  expect_true(is.data.frame(sample_data), info = "The imported dataset is not a data frame")

  # Test 1: Compute velocity with FPS provided
  velocity_data <- op_compute_velocity(
    data = sample_data,
    fps = 30,
    overwrite = FALSE,
    merge_xy = FALSE
  )
  expect_true(any(grepl("^v_x", names(velocity_data))), info = "Velocity columns for x were not created")
  expect_true(any(grepl("^v_y", names(velocity_data))), info = "Velocity columns for y were not created")
  expect_false(any(grepl("^c", names(velocity_data))), info = "'c' columns were not removed")

  # Test 2: Compute velocity with video duration provided
  velocity_data_duration <- op_compute_velocity(
    data = sample_data,
    video_duration = 10,
    overwrite = FALSE,
    merge_xy = FALSE
  )
  expect_true(any(grepl("^v_x", names(velocity_data_duration))), info = "Velocity columns for x were not created using video duration")
  expect_true(any(grepl("^v_y", names(velocity_data_duration))), info = "Velocity columns for y were not created using video duration")
  expect_false(any(grepl("^c", names(velocity_data_duration))), info = "'c' columns were not removed using video duration")

  # Test 3: Overwrite original 'x' and 'y' columns
  velocity_data_overwrite <- op_compute_velocity(
    data = sample_data,
    fps = 30,
    overwrite = TRUE,
    merge_xy = FALSE
  )
  expect_false(any(grepl("^x", names(velocity_data_overwrite))), info = "'x' columns were not removed when overwrite = TRUE")
  expect_false(any(grepl("^y", names(velocity_data_overwrite))), info = "'y' columns were not removed when overwrite = TRUE")

  # Test 4: Merge 'x' and 'y' columns to calculate Euclidean velocity
  velocity_data_merged <- op_compute_velocity(
    data = sample_data,
    fps = 30,
    overwrite = FALSE,
    merge_xy = TRUE
  )
  expect_true(any(grepl("^v_", names(velocity_data_merged))), info = "Merged velocity columns were not created")
  expect_false(any(grepl("^c", names(velocity_data_merged))), info = "'c' columns were not removed in merged mode")

  # Test 5: Missing both fps and video duration
  expect_error(
    op_compute_velocity(
      data = sample_data,
      fps = NULL,
      video_duration = NULL,
      overwrite = FALSE,
      merge_xy = FALSE
    ),
    "Either fps or video_duration must be provided",
    info = "The function did not throw an error when both fps and video_duration were missing"
  )

  # Test 6: Incorrect column matching between 'x' and 'y'
  incomplete_data <- sample_data
  incomplete_data <- incomplete_data[, !grepl("^y", names(incomplete_data))] # Remove 'y' columns
  expect_error(
    op_compute_velocity(
      data = incomplete_data,
      fps = 30,
      overwrite = FALSE,
      merge_xy = TRUE
    ),
    "Error in merge_xy calculations",
    info = "The function did not throw an error when 'y' columns were missing for merge_xy"
  )
})
