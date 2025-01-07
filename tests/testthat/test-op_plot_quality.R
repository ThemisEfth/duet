library(testthat)

test_that("op_plot_quality generates the expected plots", {
  # Import the dataset from inst/extdata/csv_data/A-B_body_dyad.csv
  sample_data_path <- system.file("extdata/csv_data/A-B_body_dyad.csv", package = "duet")
  sample_data <- read.csv(sample_data_path)

  # Verify the structure of the dataset
  required_columns <- c("base_filename", "region", "person")
  expect_true(all(required_columns %in% names(sample_data))) # Check for required columns
  expect_true(any(grepl("^c|c$", names(sample_data))))       # Check for confidence columns
  expect_true(is.data.frame(sample_data))

  # Test: Plot confidence ratings
  plot_confidence <- op_plot_quality(
    df = sample_data,
    plot_type = "confidence",
    threshold_line = 50
  )
  expect_s3_class(plot_confidence, "ggplot") # Check that a ggplot object is returned

  # Test: Plot completeness
  plot_completeness <- op_plot_quality(
    df = sample_data,
    plot_type = "completeness",
    threshold_line = 50
  )
  expect_s3_class(plot_completeness, "ggplot") # Check that a ggplot object is returned

  # Test: Plot both confidence and completeness
  plot_both <- op_plot_quality(
    df = sample_data,
    plot_type = "both",
    threshold_line = 50
  )
  expect_true(inherits(plot_both, "patchwork")) # Check that a patchwork object is returned

  # Test: Invalid plot_type
  expect_error(
    op_plot_quality(
      df = sample_data,
      plot_type = "invalid_type",
      threshold_line = 50
    ),
    "Invalid plot_type specified"
  )

  # Test: Missing required columns
  incomplete_data <- sample_data[, setdiff(names(sample_data), "region")]
  expect_error(
    op_plot_quality(
      df = incomplete_data,
      plot_type = "confidence",
      threshold_line = 50
    ),
    "df is missing the following required columns: region"
  )
})
