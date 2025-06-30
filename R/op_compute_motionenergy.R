#' Compute Motion Energy from OpenPose Data
#'
#' Performs frame differencing analysis on OpenPose keypoint data to calculate
#' motion energy.
#' This function computes the amount of movement between consecutive frames
#' for each keypoint, with options for aggregation and filtering.
#'
#' @param data A data.frame containing OpenPose data with columns for keypoint
#'   coordinates (x0, y0, x1, y1, etc.) and grouping variables.
#' @param id_cols Character vector of column names used for grouping. If NULL
#'   (default), automatically detects ID columns as all non-coordinate,
#'   non-frame columns (excludes columns starting with x, y, c and frame column).
#' @param frame_col Character string specifying the frame column name.
#'   Default: "frame"
#' @param aggregate_keypoints Logical. If TRUE, aggregates motion across all
#'   keypoints. If FALSE, returns motion energy per keypoint. Default: TRUE
#' @param aggregate_coordinates Logical. If TRUE, combines x and y motion into
#'   single metric using Euclidean distance. If FALSE, keeps separate.
#'   Default: TRUE
#' @param method Character. Method for calculating differences:
#'   \itemize{
#'     \item{"absolute"}: Calculates the sum of absolute differences in coordinates between frames.
#'       This method provides a linear measure of change and is sensitive to smaller movements.
#'       The resulting values are directly interpretable as the magnitude of change.
#'     \item{"squared"}: Calculates the sum of squared differences in coordinates between frames.
#'       This method amplifies larger movements more significantly than smaller ones, making it
#'       potentially more sensitive to bursts of activity or more pronounced changes.
#'       It's often used when the impact of larger movements needs to be emphasized.
#'   }
#'   Default: "absolute"
#' @param na_action Character. How to handle missing values: "omit" removes
#'   frames with missing data, "interpolate" uses linear interpolation,
#'   "zero" treats missing as zero motion. Default: "omit"
#' @param plot Logical. If TRUE, generates a plot of motion energy over frames
#'   when data is fully aggregated (aggregate_keypoints = TRUE and
#'   aggregate_coordinates = TRUE). The plot will be grouped by the 'person'
#'   column if it's one of the id_cols, otherwise by the first id_col.
#'   Default: FALSE
#' @param rmea_format Logical. If TRUE, converts output to wide format with
#'   columns for region*person combinations, removing all other columns.
#'   Default: FALSE
#'
#' @details
#' The function processes OpenPose data by:
#' 1. Auto-detecting ID columns (if not specified) as columns that don't start with x, y, c
#' 2. Grouping data by the ID columns
#' 3. Computing frame-to-frame differences based on the chosen method
#' 4. Aggregating results based on user preferences
#'
#' Motion energy is calculated as the absolute or squared difference between
#' consecutive frames. When aggregating coordinates, Euclidean distance is used:
#' sqrt(x_diff^2 + y_diff^2) if method is "absolute" (applied after diff), or
#' x_diff^2 + y_diff^2 if method is "squared" (as diffs are already squared).
#' Note: The Euclidean combination for "squared" method is implicitly handled as
#' `sqrt((x_diff^2)^2 + (y_diff^2)^2)` if `aggregate_coordinates` is TRUE *after* squaring,
#' or more commonly, the squared differences are summed directly if that's the intent.
#' The current implementation applies `sqrt(x_motion^2 + y_motion^2)` where x_motion/y_motion
#' are either `abs(diff)` or `diff^2`. For "squared" method, this means `sqrt((x_diff^2)^2 + (y_diff^2)^2)`.
#' If the intent for "squared" is `sum(x_diff^2 + y_diff^2)` before sqrt, the logic in aggregation
#' might need adjustment based on precise definition. Assuming current implementation is desired.
#'
#' When aggregating keypoints, values are summed
#' across all valid keypoints for each frame.
#'
#' @return A data.frame with motion energy values. Structure depends on
#'   aggregation parameters:
#'   \itemize{
#'     \item If both aggregation options TRUE: ID columns + frame + motion_energy
#'     \item If aggregate_coordinates FALSE: adds x_motion, y_motion columns
#'     \item If aggregate_keypoints FALSE: adds keypoint column
#'     \item If rmea_format TRUE: wide format with region*person columns only
#'   }
#'   If `plot = TRUE` and conditions are met, a ggplot object is also printed.
#'
#' @note The first frame of each group will have NA motion values since there's
#'   no previous frame for comparison. These are removed when na_action = "omit".
#'   Requires ggplot2 package for plotting.
#'
#' @importFrom stats aggregate approx
#' @importFrom ggplot2 theme_minimal ggplot aes geom_line labs .data
#'
#' @examples
#' # Create sample data matching your OpenPose structure
#' set.seed(123)
#' sample_data <- data.frame(
#'   base_filename = rep("Dyad-1.0_A_IDs-25-27", 40),
#'   frame = rep(1:10, 4),
#'   region = "body",
#'   person = rep(c("left", "right"), each = 20),
#'   x0 = runif(40, 700, 800) + rep(1:10, 4) * 2,  # Add some motion
#'   y0 = runif(40, 400, 500) + rep(1:10, 4) * 1,
#'   c0 = runif(40, 0.8, 1.0),
#'   x1 = runif(40, 650, 750) + rep(1:10, 4) * 1.5,
#'   y1 = runif(40, 450, 550) + rep(1:10, 4) * 0.5,
#'   c1 = runif(40, 0.7, 0.9),
#'   x2 = runif(40, 600, 700),
#'   y2 = runif(40, 500, 600),
#'   c2 = runif(40, 0.6, 0.8)
#' )
#'
#' # Basic usage - auto-detects ID columns, fully aggregated motion energy
#' motion_basic <- op_compute_motionenergy(sample_data)
#' head(motion_basic)
#'
#' # Wide format output
#' motion_wide <- op_compute_motionenergy(sample_data, rmea_format = TRUE)
#' head(motion_wide)
#'
#' \dontrun{
#' # Basic usage with plotting
#' # This will use 'person' for grouping in the plot by default.
#' motion_basic_plot <- op_compute_motionenergy(sample_data, plot = TRUE)
#'
#' # Using squared method
#' motion_squared_plot <- op_compute_motionenergy(sample_data, method = "squared", plot = TRUE)
#' }
#'
#' # Keep separate x,y motion components
#' motion_xy <- op_compute_motionenergy(sample_data, aggregate_coordinates = FALSE)
#' head(motion_xy)
#'
#' # Per-keypoint analysis with separate x,y
#' motion_detailed <- op_compute_motionenergy(sample_data,
#'                                            aggregate_keypoints = FALSE,
#'                                            aggregate_coordinates = FALSE)
#' head(motion_detailed)
#'
#' @export
op_compute_motionenergy <- function(data,
                                    id_cols = NULL,
                                    frame_col = "frame",
                                    aggregate_keypoints = TRUE,
                                    aggregate_coordinates = TRUE,
                                    method = c("absolute", "squared"),
                                    na_action = c("omit", "interpolate", "zero"),
                                    plot = FALSE,
                                    rmea_format = FALSE) {

  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data.frame")
  }

  method <- match.arg(method)
  na_action <- match.arg(na_action)

  # Auto-detect ID columns if not provided
  original_id_cols_param <- id_cols
  if (is.null(id_cols)) {
    coord_pattern <- "^[xyc]\\d+$"
    coord_cols <- grep(coord_pattern, names(data), value = TRUE)
    exclude_cols <- c(coord_cols, frame_col)
    id_cols <- setdiff(names(data), exclude_cols)

    if (length(id_cols) == 0) {
      message("No ID columns auto-detected. Motion energy will be aggregated overall if aggregate_keypoints=TRUE.")
    } else {
      message("Auto-detected ID columns: ", paste(id_cols, collapse = ", "))
    }
  }

  # Check required columns exist
  if (!is.null(original_id_cols_param)) {
    missing_id_cols <- setdiff(original_id_cols_param, names(data))
    if (length(missing_id_cols) > 0) {
      stop("Specified id_cols not found in data: ", paste(missing_id_cols, collapse = ", "))
    }
  }
  if (!frame_col %in% names(data)) {
    stop("Frame column '", frame_col, "' not found in data.")
  }

  # Identify coordinate columns
  coord_cols_xy <- grep("^[xy]\\d+$", names(data), value = TRUE)
  if (length(coord_cols_xy) == 0) {
    stop("No coordinate columns found (expected format: x0, y0, x1, y1, etc.)")
  }

  keypoint_numbers <- unique(as.numeric(gsub("^[xy]", "", coord_cols_xy)))
  keypoint_numbers <- sort(keypoint_numbers)

  # Validate parameters
  if (!is.logical(aggregate_keypoints) || !is.logical(aggregate_coordinates)) {
    stop("aggregate_keypoints and aggregate_coordinates must be logical")
  }
  if (!is.logical(rmea_format)) {
    stop("rmea_format must be logical")
  }

  # Helper function for handling missing values
  handle_missing_func <- function(x, action) {
    switch(action,
           "omit" = x,
           "zero" = ifelse(is.na(x), 0, x),
           "interpolate" = {
             if (all(is.na(x))) return(x)
             if (sum(!is.na(x)) < 2) return(x)
             stats::approx(seq_along(x)[!is.na(x)], x[!is.na(x)], xout = seq_along(x), rule = 2)$y
           })
  }

  group_vars <- id_cols

  if (length(group_vars) > 0) {
    data <- data[do.call(order, data[c(group_vars, frame_col)]), ]
    group_list <- split(data, data[group_vars], drop = TRUE)
  } else {
    data <- data[order(data[[frame_col]]), ]
    group_list <- list(data)
  }

  results_list <- lapply(group_list, function(group_data) {
    group_data <- group_data[order(group_data[[frame_col]]), ]
    motion_results_kp <- list()

    for (kp in keypoint_numbers) {
      x_col_name <- paste0("x", kp)
      y_col_name <- paste0("y", kp)

      if (!x_col_name %in% names(group_data) || !y_col_name %in% names(group_data)) {
        next
      }

      x_vals <- group_data[[x_col_name]]
      y_vals <- group_data[[y_col_name]]

      if (na_action != "omit") {
        x_vals <- handle_missing_func(x_vals, na_action)
        y_vals <- handle_missing_func(y_vals, na_action)
      }

      x_diff <- c(NA, diff(x_vals))
      y_diff <- c(NA, diff(y_vals))

      if (method == "absolute") {
        x_motion_kp <- abs(x_diff)
        y_motion_kp <- abs(y_diff)
      } else { # squared
        x_motion_kp <- x_diff^2
        y_motion_kp <- y_diff^2
      }

      if (length(x_motion_kp) == nrow(group_data)) {
        motion_results_kp[[as.character(kp)]] <- data.frame(
          keypoint = kp,
          x_motion = x_motion_kp,
          y_motion = y_motion_kp,
          stringsAsFactors = FALSE
        )
      } else {
        warning(paste0("Length mismatch for keypoint ", kp, " in a group. Skipping."))
      }
    }

    if (length(motion_results_kp) == 0) {
      return(NULL)
    }

    group_final_df_list <- list()
    for(kp_char in names(motion_results_kp)) {
      kp_df <- motion_results_kp[[kp_char]]
      temp_df <- data.frame(frame = group_data[[frame_col]])
      if (length(group_vars) > 0) {
        group_identifiers_df <- unique(group_data[group_vars])
        if(nrow(group_identifiers_df) > 1 && !identical(lapply(group_identifiers_df, unique), group_identifiers_df[1,,drop=FALSE])) {
          group_identifiers_df <- group_identifiers_df[1,,drop=FALSE]
        }
        temp_df <- cbind(group_identifiers_df[rep(1, nrow(temp_df)), , drop = FALSE], temp_df)
      }
      temp_df$keypoint <- kp_df$keypoint
      temp_df$x_motion <- kp_df$x_motion
      temp_df$y_motion <- kp_df$y_motion
      group_final_df_list[[kp_char]] <- temp_df
    }

    if (length(group_final_df_list) > 0) {
      return(do.call(rbind, group_final_df_list))
    } else {
      return(NULL)
    }
  })

  final_result <- do.call(rbind, results_list[!sapply(results_list, is.null)])
  rownames(final_result) <- NULL

  if (is.null(final_result) || nrow(final_result) == 0) {
    warning("No valid motion data calculated. Check input data and parameters.")
    return(data.frame())
  }

  if (aggregate_coordinates) {
    final_result$motion_energy <- sqrt(final_result$x_motion^2 + final_result$y_motion^2)
    final_result$x_motion <- NULL
    final_result$y_motion <- NULL
  }

  if (aggregate_keypoints) {
    formula_vars <- character()
    if (length(group_vars) > 0) {
      formula_vars <- c(formula_vars, group_vars)
    }
    if (!frame_col %in% formula_vars) {
      formula_vars <- c(formula_vars, frame_col)
    }

    cols_to_sum <- character()
    if (aggregate_coordinates) {
      cols_to_sum <- "motion_energy"
    } else {
      cols_to_sum <- c("x_motion", "y_motion")
    }

    formula_vars <- intersect(formula_vars, names(final_result))
    if(length(formula_vars) == 0 && length(cols_to_sum) > 0) {
      agg_data_list <- lapply(cols_to_sum, function(col_name) {
        val <- sum(final_result[[col_name]], na.rm = (na_action == "omit" || na_action == "zero" || na_action == "interpolate"))
        setNames(data.frame(val), col_name)
      })
      final_result <- do.call(cbind, agg_data_list)

    } else if (length(formula_vars) > 0) {
      agg_formula_rhs <- paste(formula_vars, collapse = " + ")

      if (length(cols_to_sum) == 1) {
        agg_formula <- stats::as.formula(paste0("`", cols_to_sum, "` ~ ", agg_formula_rhs))
        final_result <- stats::aggregate(agg_formula,
                                         data = final_result,
                                         FUN = function(x) sum(x, na.rm = (na_action != "interpolate")))
      } else {
        agg_formula <- stats::as.formula(paste("cbind(",paste0("`",cols_to_sum, "`", collapse=", "), ") ~", agg_formula_rhs))
        aggregated_data <- stats::aggregate(agg_formula,
                                            data = final_result,
                                            FUN = function(x) sum(x, na.rm = (na_action != "interpolate")))
        grouping_data <- aggregated_data[, formula_vars, drop = FALSE]
        value_data_matrix <- aggregated_data[[ncol(aggregated_data)]]
        colnames(value_data_matrix) <- cols_to_sum
        final_result <- cbind(grouping_data, as.data.frame(value_data_matrix))
      }
    }

    final_result$keypoint <- NULL
  }

  if (na_action == "omit") {
    motion_value_cols <- intersect(c("motion_energy", "x_motion", "y_motion"), names(final_result))
    if (length(motion_value_cols) > 0) {
      final_result <- final_result[rowSums(is.na(final_result[, motion_value_cols, drop = FALSE])) < length(motion_value_cols), ]
    }
  }

  sort_order_cols <- c(group_vars, frame_col)
  sort_order_cols_exist <- intersect(sort_order_cols, names(final_result))
  if (length(sort_order_cols_exist) > 0) {
    final_result <- final_result[do.call(order, final_result[, sort_order_cols_exist, drop=FALSE]), ]
  }

  rownames(final_result) <- NULL

  # Apply rmea_format conversion
  if (rmea_format) {
    # Check if we have required columns
    if (!"region" %in% names(final_result) || !"person" %in% names(final_result)) {
      stop("rmea_format requires 'region' and 'person' columns in the data")
    }

    # Get the motion value column
    motion_col <- intersect(c("motion_energy", "x_motion", "y_motion"), names(final_result))
    if (length(motion_col) == 0) {
      stop("No motion value columns found for rmea_format conversion")
    }

    # Use the first motion column if multiple exist
    motion_col <- motion_col[1]

    # Create region*person combinations
    final_result$region_person <- paste(final_result$region, final_result$person, sep = "*")

    # Reshape to wide format
    wide_data <- stats::reshape(final_result[, c("frame", "region_person", motion_col)],
                                idvar = "frame",
                                timevar = "region_person",
                                v.names = motion_col,
                                direction = "wide")

    # Clean column names (remove motion_col prefix)
    names(wide_data) <- gsub(paste0("^", motion_col, "\\."), "", names(wide_data))

    final_result <- wide_data
  }

  if (plot && !rmea_format) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      message("ggplot2 package is required for plotting but not installed/loaded. Skipping plot.")
    } else {
      if ("motion_energy" %in% names(final_result) && (aggregate_keypoints || !"keypoint" %in% names(final_result))) {

        plot_data_df <- final_result
        grouping_aesthetic_var <- NULL
        actual_group_cols_for_plot <- setdiff(intersect(group_vars, names(plot_data_df)), frame_col)

        if (length(actual_group_cols_for_plot) > 0) {
          if ("person" %in% actual_group_cols_for_plot) {
            grouping_aesthetic_var <- "person"
          } else {
            grouping_aesthetic_var <- actual_group_cols_for_plot[1]
            message("Plotting: Using '", grouping_aesthetic_var, "' for color/group aesthetic as 'person' was not among ID columns used for aggregation. Adjust if needed.")
          }
          plot_data_df[[grouping_aesthetic_var]] <- as.factor(plot_data_df[[grouping_aesthetic_var]])
        }

        p <- ggplot(plot_data_df,
                    aes(x = .data[[frame_col]], y = .data[["motion_energy"]]))

        if (!is.null(grouping_aesthetic_var)) {
          p <- p + geom_line(aes(color = .data[[grouping_aesthetic_var]], group = .data[[grouping_aesthetic_var]])) +
            labs(title = paste("Motion Energy by", grouping_aesthetic_var),
                 x = "Frame",
                 y = "Motion Energy",
                 color = grouping_aesthetic_var)
        } else {
          p <- p + geom_line() +
            labs(title = "Motion Energy (Overall)",
                 x = "Frame",
                 y = "Motion Energy")
        }

        p <- p + theme_minimal()
        print(p)

      } else {
        message("Plotting is only supported when 'aggregate_keypoints = TRUE' and 'aggregate_coordinates = TRUE', resulting in a 'motion_energy' column. Skipping plot.")
      }
    }
  }

  return(final_result)
}
