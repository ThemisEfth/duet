library(testthat)

test_that("op_create_csv processes JSON files correctly", {
  # Define paths
  input_path <- system.file("extdata/json_files", package = "duet")
  output_path <- tempdir()  # Temporary directory for output

  # Check input directory exists
  expect_true(file.exists(input_path))

  # Run the function
  op_create_csv(
    input_path = input_path,
    output_path = output_path,
    model = "body",
    include_filename = TRUE,
    include_labels = FALSE,
    frame_width = 1920,
    export_type = "dyad"
  )

  # Check if output directory exists
  expect_true(file.exists(output_path))

  # List generated CSV files
  csv_files <- list.files(output_path, pattern = ".csv", full.names = TRUE)

  # Assert that CSV files are generated
  expect_true(length(csv_files) > 0)

  # Check structure of a sample CSV file
  sample_file <- csv_files[1]
  expect_true(file.exists(sample_file))

  sample_data <- read.csv(sample_file)
  expect_true("region" %in% colnames(sample_data))
  expect_true("person" %in% colnames(sample_data))
  expect_true("frame" %in% colnames(sample_data))

  # Cleanup temporary output files
  file.remove(csv_files)
})
