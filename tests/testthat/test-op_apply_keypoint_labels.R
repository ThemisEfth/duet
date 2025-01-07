test_that("op_apply_keypoint_labels renames columns based on region", {
  # Import the dataset from inst/extdata/csv_data/A-B_body_dyad.csv
  sample_data_path <- system.file("extdata/csv_data/A-B_body_dyad.csv", package = "duet")
  sample_data <- read.csv(sample_data_path)

  # Check that the input data has the body region
  expect_true(any(sample_data$region == "body"), info = "The sample data does not include 'body' region.")

  # Apply the renaming function
  renamed_data <- op_apply_keypoint_labels(sample_data)

  # Define the expected body labels
  body_labels <- c("xNose", "yNose", "cNose", "xNeck", "yNeck", "cNeck", "xRShoulder", "yRShoulder", "cRShoulder")

  # Check if the renamed columns include the expected body labels
  expect_true(any(body_labels %in% names(renamed_data)), info = "Body keypoint labels were not applied correctly.")

  # Skip checks for face since the data does not include the 'face' region
  face_labels <- c("xFaceContour0", "yFaceContour0", "cFaceContour0")
  expect_false(any(face_labels %in% names(renamed_data)), info = "Face keypoint labels should not be present in the renamed data.")
})
