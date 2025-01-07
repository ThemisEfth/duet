library(testthat)

test_that("op_plot_timeseries generates the expected plots", {
  # Load the sample dataset
  sample_data_path <- system.file("extdata/csv_data/A-B_body_dyad.csv", package = "duet")
  sample_data <- read.csv(sample_data_path)

  # Verify the structure of the dataset
  expect_true(all(c("frame", "x0", "y0", "x1", "y1", "person") %in% names(sample_data)))
  expect_true(is.data.frame(sample_data))

  # Test: Plot with overlay_axes = TRUE
  plot_overlay <- op_plot_timeseries(
    data = sample_data,
    keypoints = c("0", "1"),
    overlay_axes = TRUE,
    person = "both"
  )
  expect_s3_class(plot_overlay, "ggplot")

  # Test: Plot with overlay_axes = FALSE
  plot_separate <- op_plot_timeseries(
    data = sample_data,
    keypoints = c("0", "1"),
    overlay_axes = FALSE,
    person = "left"
  )
  expect_s3_class(plot_separate, "ggplot")

  # Test: Plot with free_y = FALSE
  plot_fixed_y <- op_plot_timeseries(
    data = sample_data,
    keypoints = c("0", "1"),
    overlay_axes = FALSE,
    free_y = FALSE,
    person = "right"
  )
  expect_s3_class(plot_fixed_y, "ggplot")

  # Test: Max facets limit exceeded
  expect_warning(
    op_plot_timeseries(
      data = sample_data,
      keypoints = c("0", "1", "2", "3", "4"),
      overlay_axes = FALSE,
      person = "both",
      max_facets = 2
    ),
    regexp = "Too many facets"
  )

  # Test: No keypoints specified (default behavior)
  plot_all_keypoints <- op_plot_timeseries(
    data = sample_data,
    overlay_axes = TRUE,
    person = "both",
    max_facets = 100 # Increase limit to allow plotting all keypoints
  )
  expect_s3_class(plot_all_keypoints, "ggplot")

  # Test: Invalid person argument
  expect_error(
    op_plot_timeseries(
      data = sample_data,
      keypoints = c("0", "1"),
      overlay_axes = TRUE,
      person = "invalid_person"
    ),
    regexp = "Invalid value for 'person'"
  )
})
