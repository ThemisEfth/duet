#' Merge CSV files for each dyad
#'
#' This function merges all CSV files in each dyad directory within the specified input base path.
#'
#' @param input_base_path Character. The base path containing dyad directories with CSV files.
#' @param output_base_path Character. The base path where the merged CSV files will be saved.
#'
#' @importFrom utils read.csv
#' @importFrom dplyr bind_rows
#'
#' @return None. The function is called for its side effects.
#' @export
#' @examples
#' # Load example data paths from the package
#' input_base_path <- system.file("extdata/csv_data/dyad_1", package = "duet")
#' output_base_path <- tempfile("merged_dyads")
#'
#' # Ensure input files exist
#' input_files <- list.files(input_base_path, pattern = "\\.csv$", full.names = TRUE)
#' if (length(input_files) > 0) {
#'   # Merge CSV files for each dyad
#'   op_merge_dyad(input_base_path, output_base_path)
#'
#'   # Check merged files
#'   merged_files <- list.files(output_base_path, pattern = "\\.csv$", full.names = TRUE)
#'   print(merged_files)
#'
#'   # Read and display merged data
#'   if (length(merged_files) > 0) {
#'     merged_data <- read.csv(merged_files[1])
#'     print(merged_data)
#'   } else {
#'     message("No merged files were created.")
#'   }
#' } else {
#'   message("No input files found to process.")
#' }
op_merge_dyad <- function(input_base_path, output_base_path) {

  if (!dir.exists(output_base_path)) {
    dir.create(output_base_path, recursive = TRUE)
  }

  dyad_dirs <- list.dirs(input_base_path, recursive = FALSE, full.names = TRUE)

  merge_dyad <- function(dyad_dir) {
    dyad_name <- basename(dyad_dir)
    output_file <- file.path(output_base_path, paste0(dyad_name, "_merged.csv"))

    csv_files <- list.files(dyad_dir, pattern = "\\.csv$", full.names = TRUE)

    if (length(csv_files) == 0) {
      warning(paste("No CSV files found in", dyad_dir))
      return(NULL)
    }

    merged_data <- do.call(bind_rows, lapply(csv_files, read.csv))

    write.csv(merged_data, output_file, row.names = FALSE)
  }

  lapply(dyad_dirs, merge_dyad)
}
