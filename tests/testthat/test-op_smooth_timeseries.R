library(testthat)

test_that("op_smooth_timeseries applies smoothing correctly with provided data", {
  # Import the sample dataset from inst/extdata/csv_data/A-B_body_dyad.csv
  sample_data_path <- system.file("extdata/csv_data/A-B_body_dyad.csv", package = "duet")
  sample_data <- read.csv(sample_data_path)

  # Verify the structure of the data
  expect_true(all(c("person", "frame", "x0", "y0") %in% names(sample_data)),
              info = "Missing required columns in the sample data")
  expect_true(is.data.frame(sample_data),
              info = "Sample data is not a dataframe")

  # Test the rollmean method
  smoothed_data_rollmean <- op_smooth_timeseries(sample_data, method = "rollmean", plot = FALSE)
  expect_true(all(!is.na(smoothed_data_rollmean$x0)),
              info = "NA values found in smoothed x0 column using rollmean")
  expect_true(all(!is.na(smoothed_data_rollmean$y0)),
              info = "NA values found in smoothed y0 column using rollmean")
  expect_equal(nrow(smoothed_data_rollmean), nrow(sample_data),
               info = "Row count mismatch after smoothing with rollmean")

  # Test the kza method
  smoothed_data_kza <- op_smooth_timeseries(sample_data, method = "kza", kza_k = 3, kza_m = 2, plot = FALSE)
  expect_true(all(!is.na(smoothed_data_kza$x0)),
              info = "NA values found in smoothed x0 column using kza")
  expect_true(all(!is.na(smoothed_data_kza$y0)),
              info = "NA values found in smoothed y0 column using kza")
  expect_equal(nrow(smoothed_data_kza), nrow(sample_data),
               info = "Row count mismatch after smoothing with kza")

  # Test the Savitzky-Golay filter
  smoothed_data_savitzky <- op_smooth_timeseries(sample_data, method = "savitzky", sg_window = 5, sg_order = 3, plot = FALSE)
  expect_true(all(!is.na(smoothed_data_savitzky$x0)),
              info = "NA values found in smoothed x0 column using Savitzky-Golay")
  expect_true(all(!is.na(smoothed_data_savitzky$y0)),
              info = "NA values found in smoothed y0 column using Savitzky-Golay")
  expect_equal(nrow(smoothed_data_savitzky), nrow(sample_data),
               info = "Row count mismatch after smoothing with Savitzky-Golay")

  # Test the Butterworth filter
  smoothed_data_butter <- op_smooth_timeseries(sample_data, method = "butterworth", butter_order = 3, butter_cutoff = 0.1, plot = FALSE)
  expect_true(all(!is.na(smoothed_data_butter$x0)),
              info = "NA values found in smoothed x0 column using Butterworth filter")
  expect_true(all(!is.na(smoothed_data_butter$y0)),
              info = "NA values found in smoothed y0 column using Butterworth filter")
  expect_equal(nrow(smoothed_data_butter), nrow(sample_data),
               info = "Row count mismatch after smoothing with Butterworth filter")

  # Test faceting by person and keypoints with plotting enabled
  expect_silent(op_smooth_timeseries(sample_data, method = "rollmean", plot = TRUE))
})
