#' Merge all combined dyad CSV files into one
#'
#' This function merges all combined dyad CSV files from the specified input base path into a single CSV file.
#'
#' @param input_base_path Character. The base path containing the merged dyad CSV files.
#' @param output_file Character. The path to the output CSV file.
#' @importFrom utils read.csv
#' @importFrom dplyr bind_rows
#' @return None. The function is called for its side effects.
#' @export
#' @examples
#' \dontrun{
#' input_base_path <- "/path/to/OpenPose_Merged_CSV"
#' output_file <- "/path/to/All_Dyads_Merged.csv"
#' merge_all_dyads(input_base_path, output_file)
#' }

op_merge_all_dyads <- function(input_base_path, output_file) {

  merged_csv_files <- list.files(input_base_path, pattern = "_merged\\.csv$", full.names = TRUE)

  if (length(merged_csv_files) == 0) {
    stop("No merged CSV files found in the input base path.")
  }

  all_data <- do.call(bind_rows, lapply(merged_csv_files, read.csv))

  write.csv(all_data, output_file, row.names = FALSE)
}
