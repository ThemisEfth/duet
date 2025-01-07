library(testthat)

test_that("op_remove_keypoints works correctly with provided data", {
  # Import the sample dataset
  sample_data_path <- system.file("extdata/csv_data/A-B_body_dyad.csv", package = "duet")
  sample_data <- read.csv(sample_data_path)

  # Debug: Print structure of the dataset
  print("Sample Data Structure:")
  print(str(sample_data))

  # Verify the structure of the dataset
  expect_true(all(c("person", "region", "c1", "c2", "x1", "x2", "y1", "y2") %in% names(sample_data)))
  expect_true(is.data.frame(sample_data))

  # Test: Remove specific keypoints
  filtered_data_specific <- op_remove_keypoints(
    sample_data,
    remove_specific_keypoints = c("c2"),
    apply_removal_equally = TRUE
  )
  # Debug: Print resulting dataset
  print("Filtered Data (Specific Keypoints Removed):")
  print(names(filtered_data_specific))

  expect_false("c2" %in% names(filtered_data_specific))
  expect_false("x2" %in% names(filtered_data_specific))
  expect_false("y2" %in% names(filtered_data_specific))
})
