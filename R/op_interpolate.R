#' Interpolate missing or low-quality values in OpenPose time-series data
#'
#' @description
#' This function performs various interpolation methods for x and y coordinate columns
#' in OpenPose datasets based on confidence thresholds, missing values, or zero values.
#' It groups the data by specified grouping variables and uses the selected
#' interpolation method to estimate problematic values.
#'
#' The function is designed to be robust, automatically detecting the relevant
#' OpenPose columns (e.g., `x1, y1, c1`) and applying interpolation logic
#' to each keypoint within each specified group (e.g., for each person).
#'
#' @param data A data frame containing OpenPose keypoint data with x, y, and confidence columns.
#' @param method Character string specifying the interpolation method. Options include:
#'   "spline", "linear", "polynomial", "kalman", "locf" (last observation carried forward),
#'   "nocb" (next observation carried backward), "mean", "median". Default is "median".
#' @param confidence_threshold Numeric, NA, or FALSE. The confidence score below which
#'   data points are considered problematic and targeted for interpolation. If `NA`
#'   or `FALSE`, this check is skipped. Default is 0.3.
#' @param handle_missing Logical. If `TRUE` (default), `NA` values in coordinate
#'   columns will be targeted for interpolation.
#' @param handle_zeros Logical. If `TRUE`, coordinate values of exactly 0 will be
#'   targeted for interpolation. Default is `FALSE`.
#' @param treat_na_conf_as_low Logical. If `TRUE` (default), `NA` values in a
#'   confidence column are treated as having low confidence (i.e., 0).
#' @param grouping_vars Character vector of column names to group the data by before
#'   interpolation (e.g., `c("person", "region")`). Interpolation is performed
#'   independently for each group.
#' @param polynomial_degree Integer. The degree of the polynomial to use when
#'   `method = "polynomial"`. Default is 3.
#' @param max_gap Integer. The maximum number of consecutive problematic frames to
#'   interpolate. Gaps larger than this value will be ignored. Default is `Inf` (no limit).
#' @param smooth_factor Numeric. A smoothing factor for spline interpolation (currently unused,
#'   for future compatibility). Default is 0.
#' @param extrapolation Character string specifying how to handle values outside the
#'   range of good data. Not yet fully implemented. Default is "none".
#' @param verbose Logical. If `TRUE`, prints detailed messages about the process.
#'
#' @return A data frame identical in structure to the input `data`, but with
#'   problematic values replaced by interpolated estimates. Two new columns are added:
#'   `interpolated_points_count_per_row` and `interpolation_method_used`.
#'
#' @importFrom dplyr group_by do ungroup all_of across
#' @importFrom stats spline approx lm predict na.omit median ts
#' @importFrom zoo na.locf
#' @importFrom imputeTS na_kalman
#' @export
op_interpolate <- function(
  data,
  method = "median",
  confidence_threshold = 0.3,
  handle_missing = TRUE,
  handle_zeros = FALSE,
  treat_na_conf_as_low = TRUE,
  grouping_vars = c("person", "region"),
  polynomial_degree = 3,
  max_gap = Inf,
  smooth_factor = 0,
  extrapolation = "none",
  verbose = FALSE
) {
  # --- Input Validation ---
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }
  if (nrow(data) == 0) {
    if (verbose) {
      warning("Input data is empty, returning as is.")
    }
    return(data)
  }
  # (Add other validation checks for parameters here if desired)

  keypoint_info <- .detect_keypoint_cols(data)
  if (length(keypoint_info$x_cols) == 0) {
    if (verbose) {
      warning("No OpenPose keypoint columns detected. Returning data as is.")
    }
    return(data)
  }

  if (verbose) {
    cat(sprintf(
      "Detected %d keypoints. Method: '%s'. Confidence Threshold: %s.\n",
      length(keypoint_info$keypoint_ids),
      method,
      as.character(confidence_threshold)
    ))
  }

  # Determine actual grouping variables present in the data
  actual_grouping_vars <- grouping_vars[grouping_vars %in% names(data)]
  if (length(actual_grouping_vars) == 0 && verbose) {
    cat(
      "No valid grouping variables found. Processing data as a single group.\n"
    )
  }

  # Use dplyr's group_by() and do() for robust grouped operations.
  # This applies our helper function to each group and safely combines results.
  processed_data <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(actual_grouping_vars))) %>%
    dplyr::do({
      .interpolate_group(
        group_df = ., # The '.' refers to the data frame for the current group
        keypoint_info = keypoint_info,
        method = method,
        confidence_threshold = confidence_threshold,
        handle_missing = handle_missing,
        handle_zeros = handle_zeros,
        treat_na_conf_as_low = treat_na_conf_as_low,
        max_gap = max_gap,
        polynomial_degree = polynomial_degree,
        extrapolation = extrapolation,
        verbose = verbose
      )
    }) %>%
    dplyr::ungroup()

  processed_data$interpolation_method_used <- method

  if (verbose) {
    total_rows_changed <- sum(
      processed_data$interpolated_points_count_per_row > 0
    )
    cat(sprintf(
      "Finished interpolation. Total rows with at least one interpolated value: %d\n",
      total_rows_changed
    ))
  }

  return(processed_data)
}


# --- Internal Helper Functions ---

#' Process a single group of data for interpolation
#' @noRd
.interpolate_group <- function(
  group_df,
  keypoint_info,
  method,
  confidence_threshold,
  handle_missing,
  handle_zeros,
  treat_na_conf_as_low,
  max_gap,
  polynomial_degree,
  extrapolation,
  verbose
) {
  # Add a counter column for this group
  group_df$interpolated_points_count_per_row <- 0

  for (kp_idx in seq_along(keypoint_info$keypoint_ids)) {
    x_col <- keypoint_info$x_cols[kp_idx]
    y_col <- keypoint_info$y_cols[kp_idx]
    conf_col <- keypoint_info$conf_cols[kp_idx]

    # Extract data for the current keypoint
    x_vals <- group_df[[x_col]]
    y_vals <- group_df[[y_col]]
    conf_vals <- group_df[[conf_col]]

    if (treat_na_conf_as_low) {
      conf_vals[is.na(conf_vals)] <- 0
    }

    problematic_indices <- .identify_problematic_indices(
      x_vals,
      y_vals,
      conf_vals,
      confidence_threshold,
      handle_missing,
      handle_zeros
    )

    if (length(problematic_indices) == 0) {
      next
    }

    indices_to_interpolate <- .filter_indices_by_gap(
      problematic_indices,
      max_gap
    )

    if (length(indices_to_interpolate) == 0) {
      next
    }

    interpolation_result <- .interpolate_xy_series(
      x_vals,
      y_vals,
      indices_to_interpolate,
      method,
      polynomial_degree,
      extrapolation,
      verbose
    )

    if (!is.null(interpolation_result)) {
      # Safely update the data frame for this group
      group_df[[x_col]][interpolation_result$indices] <- interpolation_result$x
      group_df[[y_col]][interpolation_result$indices] <- interpolation_result$y

      # Increment the counter for each row that was changed
      group_df$interpolated_points_count_per_row[
        interpolation_result$indices
      ] <-
        group_df$interpolated_points_count_per_row[
          interpolation_result$indices
        ] +
        1
    }
  }
  return(group_df)
}

#' Detect OpenPose-formatted columns (x, y, confidence)
#'
#' This internal helper function scans the column names of a data frame
#' to find sets of x, y, and confidence columns typically produced by
#' OpenPose (e.g., x1, y1, c1, x2, y2, c2, ...).
#'
#' @param data A data frame.
#' @return A list containing:
#'   \itemize{
#'     \item \code{x_cols}: Character vector of x-coordinate column names.
#'     \item \code{y_cols}: Character vector of y-coordinate column names.
#'     \item \code{conf_cols}: Character vector of confidence column names.
#'     \item \code{keypoint_ids}: Character vector of the keypoint identifiers (e.g., "1", "2").
#'   }
#'   Returns empty vectors if no matching columns are found.
#' @noRd
#' @keywords internal
.detect_keypoint_cols <- function(data) {
  col_names <- names(data)
  x_cols <- sort(grep("^x\\d+$", col_names, value = TRUE))
  y_cols <- sort(grep("^y\\d+$", col_names, value = TRUE))
  conf_cols <- sort(grep(
    "^(c|conf)\\d+$",
    col_names,
    value = TRUE,
    ignore.case = TRUE
  ))

  if (length(x_cols) == 0 || length(y_cols) == 0 || length(conf_cols) == 0) {
    return(list(
      x_cols = character(0),
      y_cols = character(0),
      conf_cols = character(0),
      keypoint_ids = character(0)
    ))
  }

  get_id <- function(col_name) {
    sub("^(x|y|c|conf)", "", col_name, ignore.case = TRUE)
  }
  common_ids <- Reduce(
    intersect,
    list(get_id(x_cols), get_id(y_cols), get_id(conf_cols))
  )
  common_ids <- sort(unique(common_ids))

  if (length(common_ids) == 0) {
    return(list(
      x_cols = character(0),
      y_cols = character(0),
      conf_cols = character(0),
      keypoint_ids = character(0)
    ))
  }

  first_conf_col <- conf_cols[get_id(conf_cols) %in% common_ids][1]
  conf_prefix <- if (startsWith(first_conf_col, "conf")) "conf" else "c"

  return(list(
    x_cols = paste0("x", common_ids),
    y_cols = paste0("y", common_ids),
    conf_cols = paste0(conf_prefix, common_ids),
    keypoint_ids = common_ids
  ))
}

#' Identify indices of problematic data points
#' @noRd
.identify_problematic_indices <- function(
  x_vals,
  y_vals,
  conf_vals,
  threshold,
  handle_na,
  handle_0
) {
  # Start with a vector of all FALSE
  is_problematic <- logical(length(x_vals))
  if (handle_na) {
    is_problematic <- is_problematic | is.na(x_vals) | is.na(y_vals)
  }
  if (handle_0) {
    # Only flag non-NA zeros
    is_problematic <- is_problematic |
      ((!is.na(x_vals) & x_vals == 0) | (!is.na(y_vals) & y_vals == 0))
  }
  if (is.numeric(threshold) && !is.na(threshold)) {
    # Flag points where confidence is below threshold (treat NA confidence as 0)
    is_problematic <- is_problematic | is.na(conf_vals) | conf_vals < threshold
  }
  which(is_problematic)
}

#' Filter indices that belong to gaps larger than max_gap
#' @noRd
.filter_indices_by_gap <- function(problematic_indices, max_gap) {
  if (length(problematic_indices) == 0 || !is.finite(max_gap)) {
    return(problematic_indices)
  }
  # Split indices into groups of consecutive numbers
  gaps <- split(
    problematic_indices,
    cumsum(c(1, diff(problematic_indices) != 1))
  )
  # Keep only the gaps that are not too long
  indices_to_keep <- unlist(gaps[sapply(gaps, length) <= max_gap])
  return(indices_to_keep)
}

#' Perform the mathematical interpolation for a single x/y series
#' @noRd
.interpolate_xy_series <- function(
  x_vals,
  y_vals,
  indices_to_interp,
  method,
  poly_deg,
  extrap,
  verbose
) {
  # --- Method Validation ---
  valid_methods <- c(
    "spline",
    "linear",
    "polynomial",
    "kalman",
    "locf",
    "nocb",
    "mean",
    "median"
  )
  if (!method %in% valid_methods) {
    if (verbose) {
      warning(sprintf(
        "Invalid interpolation method '%s' provided. Skipping interpolation for this keypoint.",
        method
      ))
    }
    return(NULL)
  }
  n_pts <- length(x_vals)
  fit_indices <- setdiff(1:n_pts, indices_to_interp)
  fit_indices <- fit_indices[
    !is.na(x_vals[fit_indices]) & !is.na(y_vals[fit_indices])
  ]

  min_pts_needed <- switch(method, "spline" = 2, "polynomial" = poly_deg + 1, 1)
  if (length(fit_indices) < min_pts_needed) {
    return(NULL)
  }

  # Helper function to run the interpolation logic for one vector (x or y)
  run_interp <- function(values, fit_idx, interp_idx) {
    series_na <- values
    series_na[interp_idx] <- NA
    switch(
      method,
      "spline" = stats::spline(
        fit_idx,
        values[fit_idx],
        xout = interp_idx,
        method = "fmm"
      )$y,
      "linear" = stats::approx(
        fit_idx,
        values[fit_idx],
        xout = interp_idx,
        rule = 2
      )$y,
      "polynomial" = {
        model <- stats::lm(
          val ~ poly(idx, degree = min(poly_deg, length(fit_idx) - 1)),
          data.frame(val = values[fit_idx], idx = fit_idx)
        )
        stats::predict(model, newdata = data.frame(idx = interp_idx))
      },
      "kalman" = as.numeric(imputeTS::na_kalman(stats::ts(series_na)))[
        interp_idx
      ],
      "locf" = zoo::na.locf(series_na, na.rm = FALSE)[interp_idx],
      "nocb" = zoo::na.locf(series_na, na.rm = FALSE, fromLast = TRUE)[
        interp_idx
      ],
      "mean" = rep(mean(values[fit_idx], na.rm = TRUE), length(interp_idx)),
      "median" = rep(
        stats::median(values[fit_idx], na.rm = TRUE),
        length(interp_idx)
      )
    )
  }

  tryCatch(
    {
      interpolated_x <- run_interp(x_vals, fit_indices, indices_to_interp)
      interpolated_y <- run_interp(y_vals, fit_indices, indices_to_interp)
      return(list(
        x = interpolated_x,
        y = interpolated_y,
        indices = indices_to_interp
      ))
    },
    error = function(e) {
      if (verbose) {
        cat(sprintf("  Error during interpolation: %s. Skipping.\n", e$message))
      }
      return(NULL)
    }
  )
}
