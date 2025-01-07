library(testthat)

test_that("op_animate_dyad generates a video correctly", {
  # Import the dataset from inst/extdata/csv_data/A-B_body_dyad.csv
  sample_data_path <- system.file("extdata/csv_data/A-B_body_dyad.csv", package = "duet")
  sample_data <- read.csv(sample_data_path)

  # Verify the structure of the dataset
  expect_true("frame" %in% names(sample_data), info = "'frame' column is missing in the dataset")
  expect_true("person" %in% names(sample_data), info = "'person' column is missing in the dataset")
  expect_true(any(grepl("^x", names(sample_data))), info = "No 'x' columns found in the dataset")
  expect_true(any(grepl("^y", names(sample_data))), info = "No 'y' columns found in the dataset")
  expect_true(is.data.frame(sample_data))

  # Define output path for the video
  output_video <- file.path(tempdir(), "test_dyad_animation.mp4")

  # Test: Generate a video with default settings
  expect_message(
    op_animate_dyad(
      data = sample_data,
      output_file = output_video,
      lines = TRUE,
      keylabels = FALSE,
      fps = 24,
      min_frame = 1,
      max_frame = 50,
      hide_labels = TRUE,
      left_color = "blue",
      right_color = "red",
      background_color = "white"
    ),
    "Video successfully created and saved"
  )

  # Verify that the output video was created
  expect_true(file.exists(output_video), info = "Output video was not created")

  # Test: Generate a video with keypoint labels and different background colour
  output_video_labels <- file.path(tempdir(), "test_dyad_animation_labels.mp4")
  expect_message(
    op_animate_dyad(
      data = sample_data,
      output_file = output_video_labels,
      lines = FALSE,
      keylabels = TRUE,
      label_type = "names",
      fps = 30,
      min_frame = 1,
      max_frame = 30,
      hide_labels = FALSE,
      left_color = "green",
      right_color = "purple",
      background_colour = "grey"
    ),
    "Video successfully created and saved"
  )

  # Verify that the output video with labels was created
  expect_true(file.exists(output_video_labels), info = "Output video with labels was not created")

  # Cleanup: Remove test video files
  unlink(output_video)
  unlink(output_video_labels)
})
