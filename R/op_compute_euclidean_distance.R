#' Compute Euclidean Distance
#'
#' This function calculates the Euclidean distance for each pair of columns that include
#' 'x' and 'y' (regardless of prefixes) and creates new columns with appropriate labels.
#'
#' @param data A data frame containing the columns to process.
#' @param overwrite Logical value indicating whether to remove original 'x' and 'y' columns.
#'
#' @return A data frame with Euclidean distance columns added.
#' @export
#'
#' @examples
#' # Create a sample dataframe
#' data <- data.frame(
#'   a_x = c(1, 2, 3),
#'   a_y = c(4, 5, 6),
#'   b_x = c(7, 8, 9),
#'   b_y = c(10, 11, 12)
#' )
#'
#' # Compute Euclidean distances without overwriting original columns
#' cleaned_data <- op_compute_euclidean_distance(data, overwrite = FALSE)
#' print(cleaned_data)
#'
#' # Compute Euclidean distances and overwrite original columns
#' cleaned_data_overwrite <- op_compute_euclidean_distance(data, overwrite = TRUE)
#' print(cleaned_data_overwrite)

op_compute_euclidean_distance <- function(data, overwrite = FALSE) {
  # Identify columns containing 'x' and 'y' regardless of prefixes
  x_columns <- grep("x", colnames(data), value = TRUE)
  y_columns <- grep("y", colnames(data), value = TRUE)

  # Extract common labels by removing prefixes
  common_labels <- intersect(sub("x.*$", "", x_columns), sub("y.*$", "", y_columns))

  # Compute Euclidean distance for each pair of 'x' and 'y' columns using base R
  for (label in common_labels) {
    x_col <- grep(paste0("^", label, "x"), colnames(data), value = TRUE)
    y_col <- grep(paste0("^", label, "y"), colnames(data), value = TRUE)
    distance_col <- paste0("distance_", label)

    if (length(x_col) == 1 && length(y_col) == 1) {
      data[[distance_col]] <- sqrt(data[[x_col]]^2 + data[[y_col]]^2)
    }
  }

  # Remove original 'x' and 'y' columns if overwrite is TRUE
  if (overwrite) {
    data <- data[, !colnames(data) %in% c(x_columns, y_columns)]
  }

  return(data)
}
