#' Compute Jerk
#'
#' This function calculates the jerk for each column that begins with 'x' and 'y'
#' and removes all columns that start with 'c'. It takes either the fps or the video
#' duration as input to compute the jerk.
#'
#' @param data A data frame containing the columns to process.
#' @param fps Frames per second, used to compute jerk.
#' @param video_duration Video duration in seconds, used to compute fps.
#' @param overwrite Logical value indicating whether to remove original 'x' and 'y' columns.
#' @param merge_xy Logical value indicating whether to merge x and y columns using Euclidean distance.
#'
#' @return A data frame with jerk columns added and 'c' columns removed.
#' @export
#'
op_compute_jerk <- function(data, fps = NULL, video_duration = NULL, overwrite = FALSE, merge_xy = FALSE) {

  # Determine fps if only video_duration is provided
  if (is.null(fps) && !is.null(video_duration)) {
    fps <- nrow(data) / video_duration
  } else if (is.null(fps) && is.null(video_duration)) {
    stop("Either fps or video_duration must be provided")
  }

  # Check if fps is still NULL
  if (is.null(fps)) {
    stop("fps could not be determined. Please provide either fps or video_duration.")
  }

  # Calculate frame duration
  frame_duration <- 1 / fps

  # Identify columns starting with 'x' and 'y'
  x_columns <- grep("^x", colnames(data), value = TRUE)
  y_columns <- grep("^y", colnames(data), value = TRUE)

  if (merge_xy) {
    # Ensure matching x and y columns based on suffix
    x_suffixes <- sub("^x", "", x_columns)
    y_suffixes <- sub("^y", "", y_columns)
    common_suffixes <- intersect(x_suffixes, y_suffixes)

    if (length(common_suffixes) == 0) {
      stop("Error in merge_xy calculations: Both x and y columns are required for merging, but no matching pairs were found.")
    }

    for (suffix in common_suffixes) {
      x_col <- paste0("x", suffix)
      y_col <- paste0("y", suffix)
      jerk_col <- paste0("j_", suffix)
      velocity_x <- diff(c(NA, data[[x_col]])) / frame_duration
      velocity_y <- diff(c(NA, data[[y_col]])) / frame_duration
      acceleration_x <- diff(c(NA, velocity_x)) / frame_duration
      acceleration_y <- diff(c(NA, velocity_y)) / frame_duration
      data[[jerk_col]] <- sqrt(diff(c(NA, acceleration_x))^2 + diff(c(NA, acceleration_y))^2) / frame_duration
    }
  } else {
    # Compute jerk for x and y columns separately
    for (col in x_columns) {
      jerk_col <- paste0("j_", col)
      velocity <- diff(c(NA, data[[col]])) / frame_duration
      acceleration <- diff(c(NA, velocity)) / frame_duration
      data[[jerk_col]] <- diff(c(NA, acceleration)) / frame_duration
    }

    for (col in y_columns) {
      jerk_col <- paste0("j_", col)
      velocity <- diff(c(NA, data[[col]])) / frame_duration
      acceleration <- diff(c(NA, velocity)) / frame_duration
      data[[jerk_col]] <- diff(c(NA, acceleration)) / frame_duration
    }
  }

  # Optionally remove original 'x' and 'y' columns after calculations
  if (overwrite) {
    data <- data[ , !(names(data) %in% c(x_columns, y_columns))]
  }

  # Remove columns starting with 'c'
  c_columns <- grep("^c", colnames(data), value = TRUE)
  data <- data[ , !(names(data) %in% c_columns)]

  return(data)
}
