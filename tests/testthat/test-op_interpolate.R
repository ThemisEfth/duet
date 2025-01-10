# tests/testthat/test-op_interpolate.R

library(testthat)

# Test for op_interpolate function
test_op_interpolate <- function() {
  # Create a test dataset
  test_data <- data.frame(
    person = c(1, 1, 1, 2, 2, 2),
    region = c("A", "A", "A", "B", "B", "B"),
    x1 = c(NA, 2, 3, 5, NA, 7),
    y1 = c(1, NA, 3, NA, 6, 7),
    c1 = c(0.8, 0.2, 0.9, 0.7, 0.1, 0.9)
  )

  # Define expected output after interpolation
  expected_output <- data.frame(
    person = c(1, 1, 1, 2, 2, 2),
    region = c("A", "A", "A", "B", "B", "B"),
    x1 = c(1.6667, 2, 3, 5, 6, 7), # Interpolated values for x1
    y1 = c(1, 2, 3, 5.5, 6, 7),    # Interpolated values for y1
    c1 = c(0.8, 0.2, 0.9, 0.7, 0.1, 0.9)
  )

  # Run the function with a confidence threshold of 0.5
  result <- op_interpolate(
    data = test_data,
    confidence_threshold = 0.5,
    missing = TRUE,
    treat_na_conf_as_low = TRUE
  )

  # Round the results for comparison
  result$x1 <- round(result$x1, 4)
  result$y1 <- round(result$y1, 4)

  # Check if the result matches the expected output
  if (!all.equal(result, expected_output)) {
    stop("Test failed: The interpolated data does not match the expected output.")
  }

  message("Test passed: The function works as expected.")
}
