library(testthat)

test_that("op_plot_openpose generates plots for a specified frame", {
  # Import the dataset from inst/extdata/csv_data/A-B_body.csv
  sample_data_path <- system.file("extdata/csv_data/A-B_body_dyad.csv", package = "duet")
  sample_data <- read.csv(sample_data_path)

  # Verify the structure of the dataset
  expect_true("frame" %in% names(sample_data), info = "'frame' column is missing in the dataset")
  expect_true("person" %in% names(sample_data), info = "'person' column is missing in the dataset")
  expect_true(any(grepl("^x\\d+$", names(sample_data))), info = "No 'x' columns found in the dataset")
  expect_true(any(grepl("^y\\d+$", names(sample_data))), info = "No 'y' columns found in the dataset")
  expect_true(is.data.frame(sample_data))

  # Test: Generate a plot for a specific frame
  frame_to_plot <- sample_data$frame[1] # Use the first frame available
  expect_silent(
    op_plot_openpose(
      data = sample_data,
      frame_num = frame_to_plot,
      person = "both",
      lines = TRUE,
      keylabels = FALSE,
      label_type = "names",
      left_color = "blue",
      right_color = "red",
      background_color = "white",
      line_width = 2,
      point_size = 1.5,
      text_color = "black"
    )
  )

  # Test: Generate a plot with keypoint labels
  expect_silent(
    op_plot_openpose(
      data = sample_data,
      frame_num = frame_to_plot,
      person = "both",
      lines = TRUE,
      keylabels = TRUE,
      label_type = "numbers",
      left_color = "green",
      right_color = "purple",
      background_color = "grey90",
      line_width = 2,
      point_size = 2,
      text_color = "black"
    )
  )

  # Test: Generate a plot with hidden labels
  expect_silent(
    op_plot_openpose(
      data = sample_data,
      frame_num = frame_to_plot,
      person = "left",
      lines = TRUE,
      keylabels = TRUE,
      label_type = "names",
      hide_labels = TRUE,
      left_color = "blue",
      right_color = "red",
      background_color = "white",
      line_width = 2,
      point_size = 1.5,
      text_color = "black"
    )
  )

  # Test: Check for errors with an invalid frame
  expect_error(
    op_plot_openpose(
      data = sample_data,
      frame_num = -1, # Invalid frame number
      person = "both",
      lines = TRUE,
      keylabels = FALSE,
      label_type = "names",
      left_color = "blue",
      right_color = "red",
      background_color = "white",
      line_width = 2,
      point_size = 1.5,
      text_color = "black"
    ),
    "No data found for the specified frame."
  )
})
