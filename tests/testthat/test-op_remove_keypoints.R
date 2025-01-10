test_op_remove_keypoints <- function() {
  # Import the dataset
  sample_data_path <- system.file("extdata/csv_data/A-B_body_dyad.csv", package = "duet")
  test_data <- read.csv(sample_data_path, stringsAsFactors = FALSE)

  # Debugging: Print the structure of the loaded data
  print(str(test_data))

  # Test 1: Remove specific keypoints (e.g., keypoint 2)
  result_specific <- op_remove_keypoints(
    df = test_data,
    remove_specific_keypoints = c("2"),
    remove_undetected_keypoints = FALSE,
    remove_keypoints_total_confidence = NULL,
    remove_keypoints_missing_data = NULL,
    apply_removal_equally = TRUE
  )

  # Ensure columns related to keypoint 2 are removed
  expected_removed <- c("x2", "y2", "c2")
  actual_columns <- names(result_specific)
  if (any(expected_removed %in% actual_columns)) {
    stop("Test 1 failed: Keypoint 2 columns were not removed correctly.")
  }

  # Test 2: Remove undetected keypoints (all-zero confidence)
  result_undetected <- op_remove_keypoints(
    df = test_data,
    remove_specific_keypoints = NULL,
    remove_undetected_keypoints = TRUE,
    remove_keypoints_total_confidence = NULL,
    remove_keypoints_missing_data = NULL,
    apply_removal_equally = TRUE
  )
  # Check that all-zero confidence keypoints are removed
  confidence_columns <- grep("^c", names(test_data), value = TRUE)
  removed_columns <- setdiff(confidence_columns, names(result_undetected))
  undetected_columns <- confidence_columns[sapply(confidence_columns, function(col) {
    all(test_data[[col]] == 0, na.rm = TRUE)
  })]
  if (!all(undetected_columns %in% removed_columns)) {
    print("Debug: Confidence columns not removed as expected:")
    print(undetected_columns)
    print("Remaining columns:")
    print(removed_columns)
    stop("Test 2 failed: Undetected keypoints were not removed correctly.")
  }

  # Test 3: Remove keypoints with low total confidence
  result_low_conf <- op_remove_keypoints(
    df = test_data,
    remove_specific_keypoints = NULL,
    remove_undetected_keypoints = FALSE,
    remove_keypoints_total_confidence = 0.5,
    remove_keypoints_missing_data = NULL,
    apply_removal_equally = TRUE
  )
  low_conf_keypoints <- grep("^c", names(test_data), value = TRUE)[sapply(grep("^c", names(test_data), value = TRUE), function(col) {
    mean(test_data[[col]], na.rm = TRUE) < 0.5
  })]
  removed_low_conf <- setdiff(names(test_data), names(result_low_conf))
  if (!all(low_conf_keypoints %in% removed_low_conf)) {
    print("Debug: Low confidence columns not removed as expected:")
    print(low_conf_keypoints)
    stop("Test 3 failed: Keypoints with low total confidence were not removed correctly.")
  }

  # Test 4: Remove keypoints with excessive missing/zero data
  result_high_missing <- op_remove_keypoints(
    df = test_data,
    remove_specific_keypoints = NULL,
    remove_undetected_keypoints = FALSE,
    remove_keypoints_total_confidence = NULL,
    remove_keypoints_missing_data = 0.5,
    apply_removal_equally = TRUE
  )
  high_missing_keypoints <- grep("^[xyzc]", names(test_data), value = TRUE)[sapply(grep("^[xyzc]", names(test_data), value = TRUE), function(col) {
    mean(test_data[[col]] == 0 | is.na(test_data[[col]])) > 0.5
  })]
  removed_high_missing <- setdiff(names(test_data), names(result_high_missing))
  if (!all(high_missing_keypoints %in% removed_high_missing)) {
    print("Debug: High missing columns not removed as expected:")
    print(high_missing_keypoints)
    stop("Test 4 failed: Keypoints with excessive missing/zero data were not removed correctly.")
  }

  # Test 5: Apply removal separately by person/region
  result_separate <- op_remove_keypoints(
    df = test_data,
    remove_specific_keypoints = NULL,
    remove_undetected_keypoints = TRUE,
    remove_keypoints_total_confidence = NULL,
    remove_keypoints_missing_data = NULL,
    apply_removal_equally = FALSE
  )
  # Re-check keypoints in each group
  unique_groups <- unique(test_data[, c("person", "region")])
  for (i in seq_len(nrow(unique_groups))) {
    person <- unique_groups[i, "person"]
    region <- unique_groups[i, "region"]
    subset <- test_data[test_data$person == person & test_data$region == region, ]
    undetected_cols <- grep("^c", names(subset), value = TRUE)[sapply(grep("^c", names(subset), value = TRUE), function(col) {
      all(subset[[col]] == 0, na.rm = TRUE)
    })]
    remaining_columns <- names(result_separate[result_separate$person == person & result_separate$region == region, ])
    if (any(undetected_cols %in% remaining_columns)) {
      print("Debug: Undetected columns in group:")
      print(undetected_cols)
      print("Remaining columns in group:")
      print(remaining_columns)
      stop(sprintf("Test 5 failed: Undetected keypoints not removed correctly for person %s, region %s.", person, region))
    }
  }

  # Output success message
  message("All tests passed successfully.")
}
