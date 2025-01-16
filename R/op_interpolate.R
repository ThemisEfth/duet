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
#' @param treat_na_conf_as_low Logical. If TRUE, treat NA in the confidence column as low confidence.
#'
#' @return A modified data frame with interpolated x and y values for low-confidence or missing rows.
#'
#' @importFrom stats spline
#' @export
#' @examples
#' # Load example data from the package
#' data_path <- system.file("extdata/csv_data/A-B_body_dyad.csv", package = "duet")
#' data <- read.csv(data_path)
#'
#' # Interpolate missing or low-confidence values
#' result <- op_interpolate(
#'   data = data,
#'   confidence_threshold = 0.5,
#'   missing = TRUE,
#'   treat_na_conf_as_low = TRUE
#' )
#'
#' print(result)
op_interpolate <- function(data,
                           confidence_threshold,
                           missing = TRUE,
                           treat_na_conf_as_low = FALSE) {

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

    # Loop through each (x, y, confidence) triple
    for (i in seq_along(x_cols)) {
      x_col    <- x_cols[i]
      y_col    <- y_cols[i]
      conf_col <- conf_cols[i]

      x_values    <- group_data[[x_col]]
      y_values    <- group_data[[y_col]]
      conf_values <- group_data[[conf_col]]

      # Optionally treat NA confidence as low
      if (treat_na_conf_as_low) {
        conf_values[is.na(conf_values)] <- -Inf
      }

      # Indices needing interpolation
      if (missing) {
        # Interpolate if confidence < threshold OR actual x/y is NA
        low_conf_idx <- which(
          conf_values < confidence_threshold |
            is.na(x_values) | is.na(y_values)
        )
      } else {
        # Interpolate only if confidence < threshold
        low_conf_idx <- which(conf_values < confidence_threshold)
      }

      # If none to interpolate, move on
      if (length(low_conf_idx) == 0) {
        next
      }

      # Temporarily set low-confidence points to NA so spline() will replace them
      x_values[low_conf_idx] <- NA
      y_values[low_conf_idx] <- NA

      # Indices of good data points
      idx_seq       <- seq_along(x_values)
      good_data_idx <- setdiff(idx_seq, low_conf_idx)
      good_data_idx <- good_data_idx[!is.na(x_values[good_data_idx]) & !is.na(y_values[good_data_idx])]

      # Need at least 2 good points to spline
      if (length(good_data_idx) < 2) {
        warning(sprintf(
          "Not enough data to interpolate for person=%s, region=%s, x_col=%s, y_col=%s",
          person_value, region_value, x_col, y_col
        ))
        next
      }

      # Spline interpolation
      interpolated_x <- spline(x = good_data_idx,
                               y = x_values[good_data_idx],
                               xout = low_conf_idx)$y
      interpolated_y <- spline(x = good_data_idx,
                               y = y_values[good_data_idx],
                               xout = low_conf_idx)$y

      # Replace those NA / low-conf rows with interpolated values
      x_values[low_conf_idx] <- interpolated_x
      y_values[low_conf_idx] <- interpolated_y

      # Write back to the master data
      data[data$person == person_value & data$region == region_value, x_col] <- x_values
      data[data$person == person_value & data$region == region_value, y_col] <- y_values
    }
  }

  return(data)
}
