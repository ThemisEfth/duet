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
#' \dontrun{
#' input_base_path <- "/path/to/OpenPose_CSV"
#' output_base_path <- "/path/to/OpenPose_Merged_CSV"
#' merge_dyad_csvs(input_base_path, output_base_path)
#' }

op_merge_each_dyad <- function(input_base_path, output_base_path) {

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
