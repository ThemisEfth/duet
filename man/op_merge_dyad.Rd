% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/op_merge_dyad.R
\name{op_merge_dyad}
\alias{op_merge_dyad}
\title{Merge CSV files for each dyad}
\usage{
op_merge_dyad(input_base_path, output_base_path)
}
\arguments{
\item{input_base_path}{Character. The base path containing dyad directories with CSV files.}

\item{output_base_path}{Character. The base path where the merged CSV files will be saved.}
}
\value{
None. The function is called for its side effects.
}
\description{
This function merges all CSV files in each dyad directory within the specified input base path.
}
\examples{
# Load example data paths from the package
input_base_path <- system.file("extdata/csv_data/dyad_1", package = "duet")
output_base_path <- tempfile("merged_dyads")

# Ensure input files exist
input_files <- list.files(input_base_path, pattern = "\\\\.csv$", full.names = TRUE)
if (length(input_files) > 0) {
  # Merge CSV files for each dyad
  op_merge_dyad(input_base_path, output_base_path)

  # Check merged files
  merged_files <- list.files(output_base_path, pattern = "\\\\.csv$", full.names = TRUE)
  print(merged_files)

  # Read and display merged data
  if (length(merged_files) > 0) {
    merged_data <- read.csv(merged_files[1])
    print(merged_data)
  } else {
    message("No merged files were created.")
  }
} else {
  message("No input files found to process.")
}
}
