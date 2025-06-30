#' Compute Velocity and Optionally Adjust Boundary Values
#'
#' This function calculates the velocity for each column that begins with 'x' and 'y'.
#' The first row of the data frame (containing initial NA velocities) is removed.
#' Optionally, the first and last calculated velocity values in the returned series
#' can be set to NA if they are suspected to be artifacts.
#' The function also removes all columns that start with 'c'.
#'
#' @param data A data frame containing the columns to process. Must have at least 2 rows
#'             if velocity calculation is expected.
#' @param fps Frames per second, used to compute velocity.
#' @param video_duration Video duration in seconds, used to compute fps.
#' @param overwrite Logical value indicating whether to remove original 'x' and 'y' columns
#'                  after velocity calculation. Default is FALSE.
#' @param merge_xy Logical value indicating whether to merge x and y columns using
#'                 Euclidean distance for velocity. If FALSE, velocity is computed
#'                 for x and y components separately. Default is FALSE.
#' @param boundary_velocity_treatment Character string specifying how to treat the first and
#'                                    last calculated velocity values in the output series.
#'                                    Options: "none" (default - no change),
#'                                    "set_na" (sets the first and last calculated velocities to NA).
#'
#' @return A data frame with velocity columns. If `boundary_velocity_treatment = "set_na"`,
#'         the first and last rows of the velocity columns will have NA values.
#'         The overall data frame will have one less row than the input due to the
#'         removal of the initial NA velocity row.
#' @export
#' @examples
#' # Create sample data
#' sample_data <- data.frame(
#'   frame = 1:10,
#'   x0 = cumsum(rnorm(10)), y0 = cumsum(rnorm(10)), c0 = rnorm(10),
#'   x1 = cumsum(rnorm(10)), y1 = cumsum(rnorm(10)), c1 = rnorm(10)
#' )
#'
#' # Compute velocity, default boundary treatment
#' result_default <- op_compute_velocity(
#'   data = sample_data,
#'   fps = 30
#' )
#' print(result_default) # Should have 9 rows
#'
#' # Compute velocity, set boundary velocities to NA
#' result_boundary_na <- op_compute_velocity(
#'   data = sample_data,
#'   fps = 30,
#'   boundary_velocity_treatment = "set_na"
#' )
#' print(result_boundary_na) # First and last velocity values should be NA
op_compute_velocity <- function(data, fps = NULL, video_duration = NULL,
                                overwrite = FALSE, merge_xy = FALSE,
                                boundary_velocity_treatment = "none") {

  if (!is.data.frame(data)) stop("Input 'data' must be a data frame.")

  original_nrow <- nrow(data)
  if (original_nrow == 0) {
    warning("Input data is empty. Returning empty data frame.")
    return(data)
  }

  if (is.null(fps) && !is.null(video_duration)) {
    if (original_nrow > 0 && video_duration > 0) {
      fps <- original_nrow / video_duration
    } else {
      stop("Cannot determine fps: nrow is 0 or video_duration is not positive.")
    }
  } else if (is.null(fps) && is.null(video_duration)) {
    stop("Either fps or video_duration must be provided.")
  }

  if (is.null(fps) || fps <= 0) {
    stop("fps could not be determined or is not positive. Please provide valid fps or video_duration.")
  }

  frame_duration <- 1 / fps
  velocity_cols_created <- FALSE
  newly_created_velocity_cols <- c()

  x_columns <- grep("^x", colnames(data), value = TRUE)
  y_columns <- grep("^y", colnames(data), value = TRUE)
  data_modified <- data

  if (original_nrow < 2 && boundary_velocity_treatment != "none") {
    warning("Data has fewer than 2 rows. Boundary velocity treatment might not be meaningful or applicable.")
  }
  if (original_nrow < 2 && velocity_cols_created) {
    warning("Data has fewer than 2 rows. Velocity calculation will result in all NAs.")
  }


  if (merge_xy) {
    x_suffixes <- sub("^x", "", x_columns)
    y_suffixes <- sub("^y", "", y_columns)
    common_suffixes <- intersect(x_suffixes, y_suffixes)

    if (length(common_suffixes) > 0) {
      velocity_cols_created <- TRUE
      for (suffix in common_suffixes) {
        x_col <- paste0("x", suffix)
        y_col <- paste0("y", suffix)
        velocity_col_name <- paste0("v_", suffix)
        newly_created_velocity_cols <- c(newly_created_velocity_cols, velocity_col_name)

        if (x_col %in% names(data_modified) && y_col %in% names(data_modified)) {
          dx <- diff(c(NA, data_modified[[x_col]]))
          dy <- diff(c(NA, data_modified[[y_col]]))
          data_modified[[velocity_col_name]] <- sqrt(dx^2 + dy^2) / frame_duration
        } else {
          warning(paste("Columns", x_col, "or", y_col, "not found for suffix", suffix))
        }
      }
    } else {
      if (length(x_columns) > 0 || length(y_columns) > 0) {
        warning("merge_xy is TRUE, but no matching x and y column pairs were found. No merged velocity columns created.")
      }
    }
  } else {
    if (length(x_columns) > 0) {
      velocity_cols_created <- TRUE
      for (col in x_columns) {
        velocity_col_name <- paste0("v_", col)
        newly_created_velocity_cols <- c(newly_created_velocity_cols, velocity_col_name)
        if (col %in% names(data_modified)) {
          data_modified[[velocity_col_name]] <- diff(c(NA, data_modified[[col]])) / frame_duration
        } else {
          warning(paste("Column", col, "not found."))
        }
      }
    }
    if (length(y_columns) > 0) {
      velocity_cols_created <- TRUE
      for (col in y_columns) {
        velocity_col_name <- paste0("v_", col)
        newly_created_velocity_cols <- c(newly_created_velocity_cols, velocity_col_name)
        if (col %in% names(data_modified)) {
          data_modified[[velocity_col_name]] <- diff(c(NA, data_modified[[col]])) / frame_duration
        } else {
          warning(paste("Column", col, "not found."))
        }
      }
    }
  }

  if (overwrite && velocity_cols_created) {
    cols_to_remove_overwrite <- intersect(c(x_columns, y_columns), names(data_modified))
    if(length(cols_to_remove_overwrite) > 0) {
      data_modified <- data_modified[, !(names(data_modified) %in% cols_to_remove_overwrite), drop = FALSE]
    }
  }

  c_columns <- grep("^c", colnames(data_modified), value = TRUE)
  if (length(c_columns) > 0) {
    data_modified <- data_modified[, !(names(data_modified) %in% c_columns), drop = FALSE]
  }

  if (velocity_cols_created && nrow(data_modified) > 0) {
    data_modified <- data_modified[-1, , drop = FALSE]

    if (nrow(data_modified) > 0 && boundary_velocity_treatment == "set_na") {
      cols_to_set_na <- intersect(newly_created_velocity_cols, names(data_modified))
      if (length(cols_to_set_na) > 0) {
        if (nrow(data_modified) >= 1) { # Set first calculated velocity to NA
          data_modified[1, cols_to_set_na] <- NA
        }
        # Only set last if there's more than one row after initial trimming
        if (nrow(data_modified) > 1) {
          data_modified[nrow(data_modified), cols_to_set_na] <- NA
        } else if (nrow(data_modified) == 1) {
          # If only one row remains, its first (and only) value was already set to NA above.
          # No distinct "last" row to set.
        }
      }
    }
  } else if (velocity_cols_created && nrow(data_modified) == 0 && original_nrow > 0) {
    warning("Velocity columns were intended, but data became empty after initial row removal (original data may have had only 1 row).")
  }

  return(data_modified)
}
