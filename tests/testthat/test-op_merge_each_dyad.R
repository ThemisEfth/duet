library(testthat)

test_that("op_merge_each_dyad merges CSV files correctly", {
  # Create a temporary input base path and output base path
  input_base_path <- file.path(tempdir(), "input_dyad")
  output_base_path <- file.path(tempdir(), "output_dyad")

  # Create directories for the test
  dir.create(input_base_path, recursive = TRUE)
  dir.create(output_base_path, recursive = TRUE)

  # Create sample input data
  dyad_dir <- file.path(input_base_path, "dyad_1")
  dir.create(dyad_dir)

  # File 1: A_right.csv
  write.csv(
    data.frame(frame = 1:3, x1 = c(1, 2, 3), y1 = c(4, 5, 6), person = "A"),
    file = file.path(dyad_dir, "A_right.csv"),
    row.names = FALSE
  )

  # File 2: B_right.csv
  write.csv(
    data.frame(frame = 4:6, x1 = c(7, 8, 9), y1 = c(10, 11, 12), person = "B"),
    file = file.path(dyad_dir, "B_right.csv"),
    row.names = FALSE
  )

  # Run the merge function
  op_merge_each_dyad(input_base_path, output_base_path)

  # Check that the output directory contains the merged file
  merged_file <- file.path(output_base_path, "dyad_1_merged.csv")
  expect_true(file.exists(merged_file), info = "Merged file was not created")

  # Verify the content of the merged file
  merged_data <- read.csv(merged_file)
  expect_equal(nrow(merged_data), 6, info = "Merged file does not have the expected number of rows")
  expect_equal(ncol(merged_data), 4, info = "Merged file does not have the expected number of columns")
  expect_equal(merged_data$person, c("A", "A", "A", "B", "B", "B"), info = "Merged data does not have the correct 'person' column values")

  # Cleanup: Remove test directories and files
  unlink(input_base_path, recursive = TRUE)
  unlink(output_base_path, recursive = TRUE)
})
