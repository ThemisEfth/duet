#' Plot Keypoints with Facet Wrap
#'
#' This function plots specified keypoints (or defaults) over time with facet wrapping.
#' It handles columns starting with x, y, v_, a_, or j_.
#' Color and linetype are used to distinguish overlaid persons or overlaid metric types.
#'
#' @param data Data frame containing the keypoint data. Must include a 'frame' column for the x-axis.
#' @param keypoints Character vector of keypoint identifiers to plot (e.g., "0", "1", "Nose").
#'                  If NULL (default), the first four available keypoint identifiers are used.
#' @param free_y Boolean indicating if the y-axis should be free in facet_wrap (default is TRUE).
#' @param overlay_axes Boolean indicating if different metric types (x, y, v_, a_, j_) for the
#'                     same keypoint ID should be overlaid in the same plot facet.
#'                     Default is FALSE.
#' @param person Character string specifying which person to plot. Options: "left", "right", or "both".
#'               Requires a 'person' column in `data`. Default is "both".
#' @param facet_by_person Boolean indicating if data for different persons (when `person = "both"`)
#'                        should be in separate facets (`TRUE`, default) or overlaid on the same
#'                        facets (`FALSE`).
#' @param max_facets Integer indicating the maximum number of facets allowed (default is 10).
#'                   If the total facets exceed this number, the function returns `NULL` with a warning.
#' @param x_axis Character string for the column to be used as the x-axis (time). Default is "frame".
#' @param verbose Logical, if TRUE, prints messages about default keypoint selection. Default is FALSE.
#'
#' @return A ggplot object or NULL if the maximum number of facets is exceeded or no data can be plotted.
#' @export
#'
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_line facet_wrap theme_classic theme labs scale_linetype_manual ggtitle
#' @importFrom dplyr mutate
#' @importFrom stats as.formula na.omit
#' @importFrom rlang sym .data
#' @importFrom tidyselect all_of
#' @importFrom utils head
#'
#' @examples
#' # Create sample data
#' sample_data <- data.frame(
#'   frame = 1:100,
#'   x0 = rnorm(100), y0 = rnorm(100), v_0 = rnorm(100, 5),
#'   x1 = rnorm(100, 2), y1 = rnorm(100, 2), a_1 = rnorm(100, 10),
#'   x2 = rnorm(100, -2), y2 = rnorm(100, -2), j_2 = rnorm(100, 1),
#'   x3 = rnorm(100, 1), y3 = rnorm(100, 1),
#'   person = rep(c("P1", "P2"), each = 50)
#' )
#'
#' \dontrun{
#' # Ex 1: Overlay axes, facet by person.
#' # Color by metric_type, linetype by metric_type.
#' op_plot_timeseries(data = sample_data, overlay_axes = TRUE, person = "both",
#'                    facet_by_person = TRUE)
#'
#' # Ex 2: Overlay axes, overlay persons.
#' # Color by person, linetype by person.
#' op_plot_timeseries(data = sample_data, overlay_axes = TRUE, person = "both",
#'                    facet_by_person = FALSE)
#' }
#'
op_plot_timeseries <- function(
  data,
  keypoints = NULL,
  free_y = TRUE,
  overlay_axes = FALSE,
  person = "both",
  facet_by_person = TRUE,
  max_facets = 10,
  x_axis = "frame",
  verbose = FALSE
) {
  # --- 1. Input Validation and Data Preparation ---
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }
  if (nrow(data) == 0) {
    warning("Input 'data' is empty. Returning NULL.")
    return(NULL)
  }
  if (!x_axis %in% names(data)) {
    stop(paste0("Specified x_axis column '", x_axis, "' not found in data."))
  }

  if (!person %in% c("left", "right", "both")) {
    stop(
      "Invalid value for 'person'. Allowed values are 'left', 'right', or 'both'."
    )
  }

  has_person_col <- "person" %in% names(data)
  person_data_filtered <- data

  if (person != "both") {
    if (has_person_col) {
      person_data_filtered <- data[data$person == person, ]
      if (nrow(person_data_filtered) == 0) {
        warning(paste(
          "No data remains after filtering for person =",
          person,
          ". Returning NULL."
        ))
        return(NULL)
      }
    } else {
      warning(paste0(
        "'person' column not found. Cannot filter by person '",
        person,
        "'. Proceeding with all data, person interactions will be disabled."
      ))
    }
  }

  person_facet_active <- person == "both" &&
    has_person_col &&
    length(unique(na.omit(person_data_filtered$person))) > 1 &&
    facet_by_person

  person_overlay_active <- person == "both" &&
    has_person_col &&
    length(unique(na.omit(person_data_filtered$person))) > 1 &&
    !facet_by_person

  # --- 2. Identify Plottable Columns and Keypoint IDs ---
  plottable_col_prefixes <- c("x", "y", "v_", "a_", "j_")
  plottable_cols_pattern <- paste0(
    "^(",
    paste(plottable_col_prefixes, collapse = "|"),
    ")"
  )
  plottable_cols <- names(person_data_filtered)[grepl(
    plottable_cols_pattern,
    names(person_data_filtered)
  )]

  if (length(plottable_cols) == 0) {
    stop(paste0(
      "No plottable columns (starting with ",
      paste(plottable_col_prefixes, collapse = ", "),
      ") found."
    ))
  }

  keypoint_ids_from_cols <- unique(sub(
    plottable_cols_pattern,
    "",
    plottable_cols
  ))
  keypoint_ids_from_cols <- sort(keypoint_ids_from_cols)

  if (is.null(keypoints)) {
    if (length(keypoint_ids_from_cols) > 0) {
      keypoints <- head(keypoint_ids_from_cols, 4)
      if (verbose) {
        message(paste(
          "No keypoints specified, defaulting to first",
          length(keypoints),
          "available:",
          paste(keypoints, collapse = ", ")
        ))
      }
    } else {
      stop("No keypoints could be automatically determined from column names.")
    }
  }
  if (length(keypoints) == 0) {
    warning("No keypoints selected for plotting. Returning NULL.")
    return(NULL)
  }

  # --- 3. Reshape Data for Plotting (Pivot Longer) ---
  cols_to_pivot_for_df <- character(0)
  for (kp_id_iter in keypoints) {
    for (prefix_iter in plottable_col_prefixes) {
      col_name_iter <- paste0(prefix_iter, kp_id_iter)
      if (col_name_iter %in% plottable_cols) {
        cols_to_pivot_for_df <- c(cols_to_pivot_for_df, col_name_iter)
      }
    }
  }
  cols_to_pivot_for_df <- unique(cols_to_pivot_for_df)

  if (length(cols_to_pivot_for_df) == 0) {
    warning(
      "No data columns found for the specified keypoints. Returning NULL."
    )
    return(NULL)
  }

  cols_for_pivot_df_subset <- c(x_axis, cols_to_pivot_for_df)
  if (has_person_col) {
    cols_for_pivot_df_subset <- c(cols_for_pivot_df_subset, "person")
  }

  pivot_data_subset_for_df <- person_data_filtered[,
    intersect(cols_for_pivot_df_subset, names(person_data_filtered)),
    drop = FALSE
  ]

  df_long <- pivot_longer(
    pivot_data_subset_for_df,
    cols = all_of(cols_to_pivot_for_df),
    names_to = c("metric_type", "keypoint_id"),
    names_pattern = paste0(
      "^(",
      paste(plottable_col_prefixes, collapse = "|"),
      ")(.*)"
    ),
    values_to = "values"
  )
  df_long <- df_long[df_long$keypoint_id %in% keypoints, ]

  if (is.null(df_long) || nrow(df_long) == 0) {
    warning(
      "No data available for plotting after reshaping and filtering. Returning NULL."
    )
    return(NULL)
  }

  if (has_person_col && "person" %in% names(df_long)) {
    df_long$person <- as.factor(df_long$person)
  }
  df_long$keypoint_id <- as.factor(df_long$keypoint_id)
  df_long$metric_type <- as.factor(df_long$metric_type)

  # --- 4. Facet Calculation and Max Facet Check ---
  facet_vars_for_calc <- "keypoint_id"
  if (!overlay_axes) {
    facet_vars_for_calc <- c(facet_vars_for_calc, "metric_type")
  }
  if (person_facet_active) {
    facet_vars_for_calc <- c(facet_vars_for_calc, "person")
  }

  facet_vars_for_calc <- intersect(facet_vars_for_calc, names(df_long))
  if (length(facet_vars_for_calc) == 0 && nrow(df_long) > 0) {
    warning(
      "No valid facet variables identified. Defaulting to 1 facet for calculation."
    )
    total_facets_to_plot <- 1
  } else if (length(facet_vars_for_calc) > 0) {
    total_facets_to_plot <- nrow(unique(df_long[,
      facet_vars_for_calc,
      drop = FALSE
    ]))
  } else {
    total_facets_to_plot <- 0
  }

  if (total_facets_to_plot == 0 && nrow(df_long) > 0) {
    warning(
      "Calculated 0 facets but data exists. Plotting may be incorrect. Defaulting to 1 facet for calculation."
    )
    total_facets_to_plot <- 1
  } else if (total_facets_to_plot == 0 && nrow(df_long) == 0) {
    warning("No data to plot, 0 facets. Returning NULL.")
    return(NULL)
  }

  if (total_facets_to_plot > max_facets) {
    warning(sprintf(
      "Too many facets (%d) to plot (max_facets = %d). Please adjust parameters or increase max_facets. Returning NULL.",
      total_facets_to_plot,
      max_facets
    ))
    return(NULL)
  }

  # --- 5. Construct Plot ---
  facet_formula_str <- paste("~", paste(facet_vars_for_calc, collapse = " + "))
  if (length(facet_vars_for_calc) == 0) {
    facet_formula_str <- "~ ."
  }

  base_aes <- aes(x = .data[[x_axis]], y = .data$values)
  active_legends <- list()

  # Determine color and linetype mappings
  if (person_overlay_active) {
    base_aes$colour <- sym("person")
    active_legends$colour <- "Person"
    base_aes$linetype <- sym("person")
    active_legends$linetype <- "Person"
  } else {
    # Persons are not being overlaid by color/linetype
    if (overlay_axes) {
      base_aes$colour <- sym("metric_type") # Color by metric_type
      active_legends$colour <- "Metric Type"
      base_aes$linetype <- sym("metric_type") # Also linetype by metric_type for redundancy
      active_legends$linetype <- "Metric Type"
    } else {
      # Not overlaying axes, not overlaying persons by color/linetype
      base_aes$colour <- sym("keypoint_id") # Color by keypoint_id
      active_legends$colour <- "Keypoint ID"
      # No explicit linetype mapping here, default solid line per facet
    }
  }

  p <- ggplot(df_long, base_aes) +
    geom_line(alpha = 0.8) +
    facet_wrap(
      as.formula(facet_formula_str),
      scales = ifelse(free_y, "free_y", "fixed")
    ) +
    theme_classic(base_size = 12) +
    labs(x = gsub("_", " ", x_axis), y = "Value") # Removed Hmisc::capitalize

  # Apply legend titles from active_legends
  if (length(active_legends) > 0) {
    p <- p + labs(!!!active_legends)
  }

  # Apply manual linetype scale if linetype was mapped
  if ("linetype" %in% names(active_legends)) {
    mapped_linetype_col <- NULL
    if (active_legends$linetype == "Person" && "person" %in% names(df_long)) {
      mapped_linetype_col <- df_long$person
    } else if (
      active_legends$linetype == "Metric Type" &&
        "metric_type" %in% names(df_long)
    ) {
      mapped_linetype_col <- df_long$metric_type
    }

    if (!is.null(mapped_linetype_col)) {
      num_levels <- length(levels(as.factor(mapped_linetype_col)))
      if (num_levels > 0) {
        p <- p +
          scale_linetype_manual(
            name = active_legends$linetype,
            values = 1:num_levels
          )
      }
    }
  }

  # General legend position
  if (length(active_legends) > 0) {
    p <- p + theme(legend.position = "bottom", legend.box = "vertical")
  } else {
    p <- p + theme(legend.position = "none") # Hide legend if no aesthetics mapped to it
  }

  title_str <- "Time Series of Keypoint Data"
  if (length(keypoints) <= 3) {
    title_str <- paste0(
      title_str,
      " for Keypoint(s): ",
      paste(keypoints, collapse = ", ")
    )
  }
  if (person != "both" && has_person_col) {
    title_str <- paste0(title_str, " (Person: ", person, ")")
  } else if (person_facet_active) {
    title_str <- paste0(title_str, " (Faceted by Person)")
  } else if (person_overlay_active) {
    title_str <- paste0(title_str, " (Persons Overlaid)")
  }

  p <- p + ggtitle(title_str)

  return(p)
}
