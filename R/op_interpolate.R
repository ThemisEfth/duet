#' Interpolate missing or low-confidence values in a dataset
#'
#' This function performs interpolation for x and y coordinate columns in a dataset
#' based on confidence thresholds. It groups the data by `person` and `region` and
#' uses spline interpolation to estimate missing or low-confidence values.
#'
#' @param data A data frame containing x, y, confidence columns, and grouping columns (`person`, `region`).
#' @param confidence_threshold A numeric value specifying the confidence threshold below which
#'   values will be interpolated.
#' @param missing Logical. If TRUE, interpolate missing values (`NA`) in addition to low-confidence values.
#'
#' @return A modified data frame with interpolated x and y values for low-confidence or missing rows.
#' @importFrom stats spline
#' @examples
#' # Example dataset
#' data <- data.frame(
#'   person = rep(1:2, each = 5),
#'   region = rep(1:2, each = 5),
#'   x1 = c(NA, 1, 2, 3, NA, 4, 5, 6, NA, 8),
#'   y1 = c(NA, 10, 20, 30, NA, 40, 50, 60, NA, 80),
#'   c1 = c(NA, 0.95, 0.8, 0.99, NA, 0.7, 0.6, 0.85, NA, 0.9)
#' )
#' op_interpolate(data, confidence_threshold = 0.9, missing = TRUE)
#'
#' @export
op_interpolate <- function(data, confidence_threshold, missing = TRUE) {
  # Ensure required columns are present
  required_columns <- c("person", "region")
  if (!all(required_columns %in% names(data))) {
    stop("The data must contain 'person' and 'region' columns.")
  }

  # Identify columns for x, y, and confidence
  x_cols <- grep("^x\\d+", names(data), value = TRUE)
  y_cols <- grep("^y\\d+", names(data), value = TRUE)
  conf_cols <- grep("^c\\d+", names(data), value = TRUE)

  if (length(x_cols) != length(y_cols) || length(x_cols) != length(conf_cols)) {
    stop("The number of x, y, and confidence columns must be the same.")
  }

  # Loop through each unique combination of person and region
  unique_groups <- unique(data[, c("person", "region")])

  for (group in seq_len(nrow(unique_groups))) {
    person_value <- unique_groups[group, "person"]
    region_value <- unique_groups[group, "region"]

    # Subset the data for this specific person and region
    group_data <- data[data$person == person_value & data$region == region_value, ]

    # Loop through each x, y, confidence set within this group
    for (i in seq_along(x_cols)) {
      x_col <- x_cols[i]
      y_col <- y_cols[i]
      conf_col <- conf_cols[i]

      # Define the condition for rows that need interpolation
      if (missing) {
        # Include NA values in the interpolation
        low_conf_idx <- which(group_data[[conf_col]] < confidence_threshold |
                                is.na(group_data[[x_col]]) | is.na(group_data[[y_col]]))
      } else {
        # Only interpolate based on confidence, ignoring NA values
        low_conf_idx <- which(group_data[[conf_col]] < confidence_threshold)
      }

      # Skip interpolation if no low-confidence or missing values
      if (length(low_conf_idx) == 0) next

      # Get non-missing indices
      x_values <- group_data[[x_col]]
      y_values <- group_data[[y_col]]
      non_missing_idx <- setdiff(seq_along(x_values), low_conf_idx)

      # Check if there are at least two non-missing values for interpolation
      if (length(non_missing_idx) < 2) {
        warning(sprintf("Not enough data to interpolate for person %s and region %s for %s and %s",
                        person_value, region_value, x_col, y_col))
        next
      }

      # Perform spline interpolation for missing/low-confidence values
      interpolated_x <- spline(non_missing_idx, x_values[non_missing_idx], xout = low_conf_idx)$y
      interpolated_y <- spline(non_missing_idx, y_values[non_missing_idx], xout = low_conf_idx)$y

      # Replace the low-confidence or missing values with interpolated ones
      data[data$person == person_value & data$region == region_value, x_col][low_conf_idx] <- interpolated_x
      data[data$person == person_value & data$region == region_value, y_col][low_conf_idx] <- interpolated_y
    }
  }

  return(data)
}
