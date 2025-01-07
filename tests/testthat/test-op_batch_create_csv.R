test_that("op_batch_create_csv processes provided example dyad correctly", {

  # Get the path to the example dataset included in the package
  sample_data_path <- system.file("extdata/json_files/", package = "duet")

  # Create a mock dyad directory
  input_base_path <- file.path(tempdir(), "json_files")
  dyad_dir <- file.path(input_base_path, "dyad_1")

  # Ensure the directory is fresh
  if (dir.exists(dyad_dir)) {
    unlink(dyad_dir, recursive = TRUE)
  }
  dir.create(dyad_dir, recursive = TRUE)

  # Copy all JSON files into the mock dyad directory
  file.copy(list.files(sample_data_path, full.names = TRUE), dyad_dir)

  output_base_path <- file.path(tempdir(), "output_csvs")

  # Ensure the output directory is fresh
  if (dir.exists(output_base_path)) {
    unlink(output_base_path, recursive = TRUE)
  }

  # Run the batch processing function
  op_batch_create_csv(
    input_base_path = input_base_path,
    output_base_path = output_base_path,
    include_filename = TRUE,
    include_labels = FALSE,
    overwrite = FALSE,
    frame_width = 1920,
    model = 'body'

  )

  # Verify the output directory and files
  dyad_output_path <- file.path(output_base_path, "dyad_1")

  # Check if the output directory exists
  expect_true(dir.exists(dyad_output_path), info = "Output directory was not created")

  # Check if CSV files were created
  csv_files <- list.files(dyad_output_path, pattern = "\\.csv$", full.names = TRUE)
  expect_true(length(csv_files) > 0, info = "No CSV files were created in the output directory")

  # Verify the structure of the generated CSV files
  for (csv_file in csv_files) {
    csv_data <- read.csv(csv_file)
    expect_true("frame" %in% names(csv_data), info = paste("Frame column missing in", csv_file))
    expect_true(any(grepl("^x\\d+", names(csv_data))), info = paste("Keypoint columns missing in", csv_file))
    expect_true(any(grepl("^y\\d+", names(csv_data))), info = paste("Keypoint columns missing in", csv_file))
  }

  # Cleanup: Remove temporary directories and files
  unlink(input_base_path, recursive = TRUE)
  unlink(output_base_path, recursive = TRUE)
})
