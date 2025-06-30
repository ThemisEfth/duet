#' Summarise OpenPose Time-Series Data
#'
#' @description
#' This function takes time-series data (e.g., from OpenPose) in wide format,
#' reshapes it into long format, and calculates summary statistics for specified
#' metrics. It handles grouping, descriptive statistics, moments (skewness,
#' kurtosis), and dominant period estimation. When plot=TRUE, it generates
#' plots of the calculated summary statistics.
#'
#' @param data A data frame or tibble in wide format.
#' @param grouping_vars A character vector of column names in \code{data} that
#'   identify unique groups for summarisation (e.g., \code{c("person", "region")}).
#'   If \code{NULL} (default), the function will attempt to use "person" and/or "region"
#'   if they exist in \code{data} and have more than one unique level. Otherwise,
#'   no grouping is applied beyond the pivoted \code{names_to} column.
#' @param cols A character vector of column names to pivot from wide to long format.
#'   If \code{NULL} (default), all numeric columns not in \code{grouping_vars}
#'   (once determined) are pivoted.
#' @param names_to A character string specifying the name of the new column storing
#'   the names of pivoted columns. Default is \code{"keypoint"}.
#' @param values_to A character string specifying the name of the new column storing
#'   the numeric values from pivoted columns. Default is \code{"value"}.
#' @param metrics A character vector specifying which metrics to calculate.
#'   Available options: \code{"count"}, \code{"na_count"}, \code{"valid_count"},
#'   \code{"mean"}, \code{"median"}, \code{"sd"}, \code{"variance"}, \code{"iqr"},
#'   \code{"min"}, \code{"max"}, \code{"skewness"}, \code{"kurtosis"},
#'   \code{"dominant_period"}. Default calculates all metrics.
#' @param plot Logical indicating whether to generate summary plots of the
#'   calculated statistics. Default is \code{FALSE}.
#' @param dominant_period_min_points Integer specifying minimum number of non-NA,
#'   non-constant data points required for dominant period calculation. Default is 10L.
#' @param dominant_period_args List of additional arguments passed to
#'   \code{\link[stats]{spectrum}} for periodicity calculation. Default is \code{NULL}.
#'
#' @return A tibble with summary statistics. Each row corresponds to a unique
#'   combination of determined \code{grouping_vars} and values from \code{names_to}.
#'   Columns include grouping variables and requested metrics.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Validates input parameters.
#'   \item Determines grouping variables if not explicitly provided (checks for "person", "region").
#'   \item Identifies numeric columns to pivot, excluding non-numeric columns with a warning.
#'   \item Reshapes data from wide to long format using \code{\link[tidyr]{pivot_longer}}.
#'   \item Calculates requested summary statistics grouped by specified variables.
#'   \item Optionally generates visualization plots of the summary statistics.
#' }
#'
#' For dominant period calculation, the function uses \code{\link[stats]{spectrum}}
#' to find the peak in the power spectrum density. Skewness and kurtosis require
#' the \pkg{moments} package.
#'
#' @examples
#' # Create sample data with a non-numeric column
#' sample_data <- data.frame(
#'   frame = 1:100,
#'   participant = rep(c("P1", "P2"), each = 50),
#'   region = rep(c("A", "B"), times = 50),
#'   notes = "some_metadata", # This non-numeric column will be ignored
#'   Nose_x = rnorm(100),
#'   Nose_y = rnorm(100),
#'   LEye_x = rnorm(100),
#'   LEye_y = rnorm(100)
#' )
#'
#' # The function will now automatically ignore the 'notes' column and warn the user.
#' result_robust <- op_summarise(
#'    data = sample_data,
#'    grouping_vars = c("participant", "region"),
#'    metrics = c("mean", "sd"),
#'    plot = TRUE
#' )
#' print(result_robust)
#'
#' @export
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr all_of select across group_by summarise mutate n left_join distinct everything any_of
#' @importFrom ggplot2 ggplot aes geom_bar geom_errorbar geom_pointrange geom_hline facet_wrap facet_grid labs theme_bw theme element_text vars
#' @importFrom rlang .data := !! abort syms
#' @importFrom stats median sd var IQR spectrum na.fail as.formula

op_summarise <- function(data,
                         grouping_vars = NULL,
                         cols = NULL,
                         names_to = "keypoint",
                         values_to = "value",
                         metrics = c("count", "na_count", "valid_count", "mean",
                                     "median", "sd", "variance", "iqr", "min",
                                     "max", "skewness", "kurtosis", "dominant_period"),
                         plot = FALSE,
                         dominant_period_min_points = 10L,
                         dominant_period_args = NULL) {

  # Handle dynamic default for grouping_vars
  if (is.null(grouping_vars)) {
    potential_dynamic_groups <- character(0)
    if ("person" %in% names(data) && length(unique(na.omit(data$person))) > 1) {
      potential_dynamic_groups <- c(potential_dynamic_groups, "person")
    }
    if ("region" %in% names(data) && length(unique(na.omit(data$region))) > 1) {
      potential_dynamic_groups <- c(potential_dynamic_groups, "region")
    }
    if (!"person" %in% potential_dynamic_groups && "participant" %in% names(data) && length(unique(na.omit(data$participant))) > 1) {
      potential_dynamic_groups <- c(potential_dynamic_groups, "participant")
    }

    if (length(potential_dynamic_groups) > 0) {
      grouping_vars <- potential_dynamic_groups
    } else {
      grouping_vars <- character(0)
    }
  }

  .validate_inputs(data, grouping_vars, cols, names_to, values_to,
                   metrics, plot, dominant_period_min_points)

  cols_to_pivot <- .determine_pivot_columns(data, grouping_vars, cols)

  if (length(cols_to_pivot) == 0) {
    abort("No numeric columns were found to pivot. Please check the 'data' or 'cols' argument.")
  }

  # --- RESHAPE DATA TO LONG FORMAT (Inlined) ---
  data_long <- tryCatch({
    pivot_longer(
      data = data,
      cols = all_of(cols_to_pivot),
      names_to = names_to,
      values_to = values_to,
      values_drop_na = FALSE
    )
  }, error = function(e) {
    # Re-throw the error with a more informative message and preserve the original trace
    abort(
      message = paste(
        "Failed to reshape data.",
        "This can occur when trying to pivot columns with incompatible data types or due to an internal evaluation error.",
        "Please check the data types of your columns.",
        sep = "\n"
      ),
      parent = e # This preserves the original error and its traceback
    )
  })

  if (nrow(data_long) == 0L) {
    abort("Reshaping resulted in an empty data frame. This can happen if the pivot columns contain only NAs.")
  }

  summary_output <- .calculate_summary_statistics(
    data_long = data_long,
    group_vars = grouping_vars,
    variable_name = names_to,
    value_name = values_to,
    metrics = metrics,
    dominant_period_min_points = dominant_period_min_points,
    dominant_period_args = dominant_period_args
  )

  if (plot) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      warning("Package 'ggplot2' is required for plotting but not available. Skipping plots.", call. = FALSE)
    } else {
      .create_plots(summary_stats = summary_output,
                    group_vars = grouping_vars,
                    keypoint_col_name = names_to,
                    metrics_calculated = metrics)
    }
  }

  return(summary_output)
}


# Input validation helper (Unchanged)
.validate_inputs <- function(data, grouping_vars, cols, names_to, values_to,
                             metrics, plot, dominant_period_min_points) {

  if (!is.data.frame(data)) {
    abort("'data' must be a data frame or tibble.")
  }
  if (!is.character(grouping_vars)) {
    abort("'grouping_vars' must be a character vector (or character(0)).")
  }
  if (!is.null(cols) && !is.character(cols)) {
    abort("'cols' must be NULL or a character vector of column names.")
  }
  if (!is.character(names_to) || length(names_to) != 1L || nchar(names_to) == 0L) {
    abort("'names_to' must be a single non-empty character string.")
  }
  if (!is.character(values_to) || length(values_to) != 1L || nchar(values_to) == 0L) {
    abort("'values_to' must be a single non-empty character string.")
  }
  if (!is.character(metrics) || length(metrics) == 0L) {
    abort("'metrics' must be a non-empty character vector.")
  }
  available_metrics <- c("count", "na_count", "valid_count", "mean", "median",
                         "sd", "variance", "iqr", "min", "max", "skewness",
                         "kurtosis", "dominant_period")
  invalid_metrics <- setdiff(metrics, available_metrics)
  if (length(invalid_metrics) > 0L) {
    abort(paste0("Invalid metrics: ", paste(invalid_metrics, collapse = ", ")))
  }
  if (!is.logical(plot) || length(plot) != 1L) {
    abort("'plot' must be a single logical value.")
  }
  if (!is.numeric(dominant_period_min_points) ||
      length(dominant_period_min_points) != 1L ||
      dominant_period_min_points < 1L) {
    abort("'dominant_period_min_points' must be a positive integer.")
  }
  if (length(grouping_vars) > 0) {
    missing_vars <- setdiff(grouping_vars, names(data))
    if (length(missing_vars) > 0L) {
      abort(paste0("Grouping variables not found in data: ",
                   paste(missing_vars, collapse = ", ")))
    }
  }
  if (!is.null(cols)) {
    missing_cols <- setdiff(cols, names(data))
    if (length(missing_cols) > 0L) {
      abort(paste0("Columns not found in data: ",
                   paste(missing_cols, collapse = ", ")))
    }
    if (length(grouping_vars) > 0) {
      overlap <- intersect(cols, grouping_vars)
      if (length(overlap) > 0L) {
        abort(paste0("Columns cannot be in both 'cols' and 'grouping_vars': ",
                     paste(overlap, collapse = ", ")))
      }
    }
  }
}


# Determine columns to pivot, now with data type checking
.determine_pivot_columns <- function(data, grouping_vars, cols) {
  cols_to_exclude_always <- c("frame", "time")

  if (is.null(cols)) {
    candidate_cols <- setdiff(names(data), grouping_vars)
  } else {
    candidate_cols <- cols
  }

  if (length(candidate_cols) == 0) {
    return(character(0))
  }

  candidate_cols <- candidate_cols[!tolower(candidate_cols) %in% cols_to_exclude_always]

  if (length(candidate_cols) == 0) {
    return(character(0))
  }

  is_numeric_col <- sapply(data[candidate_cols], is.numeric)
  numeric_pivot_cols <- candidate_cols[is_numeric_col]

  non_numeric_cols <- candidate_cols[!is_numeric_col]

  if (length(non_numeric_cols) > 0) {
    warning(
      "The following non-numeric columns were found and will be excluded from pivoting and summarisation: ",
      paste(non_numeric_cols, collapse = ", "),
      call. = FALSE
    )
  }

  return(numeric_pivot_cols)
}


# Calculate summary statistics (Unchanged)
.calculate_summary_statistics <- function(data_long, group_vars, variable_name,
                                          value_name, metrics,
                                          dominant_period_min_points,
                                          dominant_period_args) {

  grouping_cols_for_summary <- unique(c(group_vars, variable_name))

  summary_stats <- data_long |>
    group_by(across(all_of(grouping_cols_for_summary))) |>
    summarise(
      count = n(),
      na_count = sum(is.na(.data[[value_name]])),
      valid_count = sum(!is.na(.data[[value_name]])),
      mean = if (sum(!is.na(.data[[value_name]])) > 0L) mean(.data[[value_name]], na.rm = TRUE) else NA_real_,
      median = if (sum(!is.na(.data[[value_name]])) > 0L) median(.data[[value_name]], na.rm = TRUE) else NA_real_,
      sd = if (sum(!is.na(.data[[value_name]])) > 1L) sd(.data[[value_name]], na.rm = TRUE) else NA_real_,
      variance = if (sum(!is.na(.data[[value_name]])) > 1L) var(.data[[value_name]], na.rm = TRUE) else NA_real_,
      iqr = if (sum(!is.na(.data[[value_name]])) > 1L) IQR(.data[[value_name]], na.rm = TRUE) else NA_real_,
      min = if (sum(!is.na(.data[[value_name]])) > 0L) min(.data[[value_name]], na.rm = TRUE) else NA_real_,
      max = if (sum(!is.na(.data[[value_name]])) > 0L) max(.data[[value_name]], na.rm = TRUE) else NA_real_,
      .groups = 'drop'
    )

  if (any(c("skewness", "kurtosis") %in% metrics)) {
    summary_stats <- .add_moments(summary_stats, data_long, grouping_cols_for_summary,
                                  value_name, metrics)
  }

  if ("dominant_period" %in% metrics) {
    summary_stats <- .add_dominant_period(summary_stats, data_long, grouping_cols_for_summary,
                                          value_name, dominant_period_min_points,
                                          dominant_period_args)
  }

  for (met in metrics) {
    if (!met %in% names(summary_stats)) {
      summary_stats[[met]] <- NA_real_
    }
  }

  final_cols_to_select <- c(grouping_cols_for_summary, intersect(metrics, names(summary_stats)))
  summary_stats <- select(summary_stats, all_of(final_cols_to_select))

  return(summary_stats)
}

# Add moment statistics
.add_moments <- function(summary_stats, data_long, grouping_vars, value_name, metrics) {
  if (!requireNamespace("moments", quietly = TRUE)) {
    warning("Package 'moments' not available. Skewness/kurtosis set to NA if requested.",
            call. = FALSE)
    if ("skewness" %in% metrics && !"skewness" %in% names(summary_stats)) summary_stats$skewness <- NA_real_
    if ("kurtosis" %in% metrics && !"kurtosis" %in% names(summary_stats)) summary_stats$kurtosis <- NA_real_
    return(summary_stats)
  }

  moment_stats <- data_long |>
    group_by(across(all_of(grouping_vars))) |>
    summarise(
      skewness = if ("skewness" %in% metrics && sum(!is.na(.data[[value_name]])) > 2L) {
        moments::skewness(.data[[value_name]], na.rm = TRUE)
      } else NA_real_,
      kurtosis = if ("kurtosis" %in% metrics && sum(!is.na(.data[[value_name]])) > 3L) {
        moments::kurtosis(.data[[value_name]], na.rm = TRUE) - 3
      } else NA_real_,
      .groups = 'drop'
    ) |>
    select(all_of(grouping_vars), any_of(c("skewness", "kurtosis")))

  cols_to_join_moments <- setdiff(names(moment_stats), grouping_vars)
  if (length(cols_to_join_moments) > 0) {
    summary_stats <- left_join(summary_stats, moment_stats, by = grouping_vars)
  } else {
    if ("skewness" %in% metrics && !"skewness" %in% names(summary_stats)) summary_stats$skewness <- NA_real_
    if ("kurtosis" %in% metrics && !"kurtosis" %in% names(summary_stats)) summary_stats$kurtosis <- NA_real_
  }

  return(summary_stats)
}

# Add dominant period calculation
.add_dominant_period <- function(summary_stats, data_long, grouping_vars, value_name,
                                 min_points, spectrum_args) {
  period_stats <- data_long |>
    group_by(across(all_of(grouping_vars))) |>
    summarise(
      dominant_period = .calculate_dominant_period(
        .data[[value_name]], min_points, spectrum_args
      ),
      .groups = 'drop'
    )

  if ("dominant_period" %in% names(period_stats)) {
    summary_stats <- left_join(summary_stats, period_stats, by = grouping_vars)
  } else if ("dominant_period" %in% summary_stats && !"dominant_period" %in% names(period_stats)) {
    summary_stats$dominant_period <- NA_real_
  }

  return(summary_stats)
}

# Calculate dominant period
.calculate_dominant_period <- function(x, min_points = 10L, spectrum_args = NULL) {
  x_clean <- x[!is.na(x)]
  n <- length(x_clean)
  if (n < min_points || length(unique(x_clean)) <= 1L) {
    return(NA_real_)
  }
  spec_result <- tryCatch({
    do.call(spectrum, c(list(x = x_clean, plot = FALSE, na.action = na.fail), spectrum_args))
  }, error = function(e) NULL)
  if (is.null(spec_result) || length(spec_result$spec) == 0L ||
      all(is.na(spec_result$spec))) {
    return(NA_real_)
  }
  max_idx <- which.max(spec_result$spec)
  if (length(max_idx) == 0L) {
    return(NA_real_)
  }
  dominant_freq <- spec_result$freq[max_idx]
  if (is.na(dominant_freq) || dominant_freq <= .Machine$double.eps) {
    return(NA_real_)
  }
  return(1 / dominant_freq)
}

# Create visualization plots of summary statistics
.create_plots <- function(summary_stats, group_vars, keypoint_col_name, metrics_calculated) {

  if (all(c("mean", "sd") %in% metrics_calculated) && all(c("mean", "sd") %in% names(summary_stats))) {
    p_mean_sd <- ggplot(summary_stats, aes(x = .data[[keypoint_col_name]], y = .data$mean)) +
      geom_bar(stat = "identity", aes(fill = .data[[keypoint_col_name]]), alpha = 0.7, show.legend = FALSE) +
      geom_errorbar(
        aes(ymin = .data$mean - .data$sd, ymax = .data$mean + .data$sd),
        width = 0.25
      ) +
      labs(
        title = "Mean +/- Standard Deviation by Keypoint",
        x = keypoint_col_name,
        y = "Mean Value"
      ) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        plot.title = element_text(size = 12, face = "bold")
      )

    if (length(group_vars) > 0 && all(group_vars %in% names(summary_stats))) { # Check group_vars exist in summary_stats
      facet_formula_str <- paste("~", paste(group_vars, collapse = " + "))
      p_mean_sd <- p_mean_sd + facet_wrap(as.formula(facet_formula_str), scales = "free_x")
    }
    print(p_mean_sd)
  }

  if (all(c("median", "min", "max") %in% metrics_calculated) && all(c("median", "min", "max") %in% names(summary_stats))) {
    p_median_range <- ggplot(summary_stats, aes(x = .data[[keypoint_col_name]], y = .data$median)) +
      geom_pointrange(
        aes(ymin = .data$min, ymax = .data$max, color = .data[[keypoint_col_name]]),
        fatten = 2
      ) +
      labs(
        title = "Median with Min-Max Range by Keypoint",
        x = keypoint_col_name,
        y = "Median Value"
      ) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        plot.title = element_text(size = 12, face = "bold"),
        legend.position = "none"
      )

    if (length(group_vars) > 0 && all(group_vars %in% names(summary_stats))) {
      facet_formula_str <- paste("~", paste(group_vars, collapse = " + "))
      p_median_range <- p_median_range + facet_wrap(as.formula(facet_formula_str), scales = "free_x")
    }
    print(p_median_range)
  }

  stats_to_plot_shape <- intersect(c("skewness", "kurtosis"), metrics_calculated)
  stats_present_in_summary <- intersect(stats_to_plot_shape, names(summary_stats))

  if (length(stats_present_in_summary) > 0) {
    cols_for_pivot_shape <- c(group_vars, keypoint_col_name, stats_present_in_summary)
    # Ensure only existing columns are selected before pivoting
    cols_for_pivot_shape <- cols_for_pivot_shape[cols_for_pivot_shape %in% names(summary_stats)]
    pivoted_shape_stats_cols <- setdiff(cols_for_pivot_shape, c(group_vars, keypoint_col_name))

    if(length(pivoted_shape_stats_cols) > 0) {
      summary_shape_long <- summary_stats |>
        select(all_of(cols_for_pivot_shape)) |>
        pivot_longer(
          cols = all_of(pivoted_shape_stats_cols),
          names_to = "shape_metric_type",
          values_to = "shape_metric_value",
          values_drop_na = FALSE
        )

      # Ensure shape_metric_type is a factor for consistent facet ordering
      summary_shape_long$shape_metric_type <- factor(summary_shape_long$shape_metric_type, levels = stats_present_in_summary)

      p_shape <- ggplot(summary_shape_long, aes(x = .data[[keypoint_col_name]], y = .data$shape_metric_value)) +
        geom_bar(stat = "identity", aes(fill = .data[[keypoint_col_name]]), show.legend = FALSE, alpha = 0.7) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
        facet_grid(rows = vars(.data$shape_metric_type),
                   cols = if(length(group_vars) > 0 && all(group_vars %in% names(summary_shape_long))) vars(!!!syms(group_vars)) else NULL,
                   scales = "free_y", switch="y") +
        labs(
          title = "Shape Statistics (Skewness & Kurtosis) by Keypoint",
          x = keypoint_col_name,
          y = "Value"
        ) +
        theme_bw() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
          plot.title = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(angle = 0, face="bold"),
          strip.placement = "outside"
        )
      print(p_shape)
    }
  }
}
