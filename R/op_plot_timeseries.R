#' Plot Keypoints with Facet Wrap
#'
#' This function plots the specified keypoints over time with facet wrapping.
#' It allows the user to overlay axes or separate them, and to filter the data by person.
#' If the number of facets exceeds the `max_facets` limit, a warning is issued, and the function will not return a plot.
#'
#' @param data Data frame containing the keypoint data.
#' @param keypoints Character vector of keypoints to include in the plot. When `overlay_axes = TRUE`, input keypoints as "0", "1", "2", etc. When `overlay_axes = FALSE`, input keypoints as "x0", "y0", "x1", "y1", etc.
#' @param free_y Boolean indicating if the y-axis should be free in facet_wrap (default is TRUE).
#' @param overlay_axes Boolean indicating if 'x' and 'y' columns should be overlaid in the same plot (default is FALSE).
#' @param person Character string specifying which person to plot ("left", "right", or "both", default is "both").
#' @param max_facets Integer indicating the maximum number of facets allowed (default is 10). If the total facets exceed this number, the function returns `NULL` with a warning.
#'
#' @return A ggplot object or NULL if the maximum number of facets is exceeded.
#' @export
#'
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_line facet_wrap theme_classic theme labs
#' @importFrom ggthemes scale_color_colorblind
#' @importFrom stats as.formula
#' @importFrom rlang sym .data
#' @importFrom tidyselect all_of
#'
#' @examples
#' # Example data frame
#' df <- data.frame(
#'   frame = 1:100,
#'   x0 = rnorm(100),
#'   y0 = rnorm(100),
#'   x1 = rnorm(100),
#'   y1 = rnorm(100),
#'   person = rep(c("left", "right"), each = 50)
#' )
#'
#' # Plot with overlay_axes = TRUE
#' op_plot_timeseries(data = df, keypoints = c("0", "1"), overlay_axes = TRUE, person = "both")
#'
#' # Plot with overlay_axes = FALSE
#' op_plot_timeseries(data = df, keypoints = c("0", "1"), overlay_axes = FALSE, person = "left")
op_plot_timeseries <- function(data, keypoints = NULL, free_y = TRUE, overlay_axes = FALSE, person = "both", max_facets = 10) {
  x_axis <- "frame"

  # Validate 'person' parameter
  if (!person %in% c("left", "right", "both")) {
    stop("Invalid value for 'person'. Allowed values are 'left', 'right', or 'both'.")
  }
  if (person != "both") {
    data <- data[data$person == person, ]
  }

  # Select numeric columns and ensure they exist in the data
  numeric_cols <- names(data)[grepl("^(x|y)", names(data))] # Include only x and y columns
  if (length(numeric_cols) == 0) {
    stop("No numeric columns found matching the expected patterns (x or y).")
  }

  # Reshape the data
  if (overlay_axes) {
    df_long <- pivot_longer(
      data[, c(x_axis, numeric_cols, "person"), drop = FALSE],
      cols = all_of(intersect(numeric_cols, names(data))),
      names_to = c("axis", "keypoint"),
      names_pattern = "(.)(.*)",
      values_to = "values"
    )
  } else {
    df_long <- pivot_longer(
      data[, c(x_axis, numeric_cols, "person"), drop = FALSE],
      cols = all_of(intersect(numeric_cols, names(data))),
      names_to = "keypoint",
      values_to = "values"
    )
  }

  # Adjust keypoints for consistency between overlay modes
  if (!is.null(keypoints)) {
    if (overlay_axes) {
      df_long <- df_long[df_long$keypoint %in% keypoints, ]
    } else {
      keypoints_expanded <- unlist(lapply(keypoints, function(kp) c(paste0("x", kp), paste0("y", kp))))
      df_long <- df_long[df_long$keypoint %in% keypoints_expanded, ]
    }
  }

  # Determine the number of facets
  num_keypoints <- length(unique(df_long$keypoint))
  num_persons <- if (person == "both") 2 else 1
  num_axes <- if (overlay_axes) 2 else 1
  total_facets <- num_keypoints * num_persons * num_axes

  # Check if the number of facets exceeds the maximum allowed
  if (total_facets > max_facets) {
    warning(sprintf("Too many facets (%d) to plot. Please specify fewer keypoints or set a higher max_facets limit.", total_facets))
    return(NULL)
  }

  # Determine facets
  facets <- "keypoint"
  if (person == "both") {
    facets <- paste(facets, "person", sep = " + ")
  }

  # Plot the data
  p <- ggplot(df_long, aes(x = .data[[x_axis]], y = values, color = if (overlay_axes) .data[["axis"]] else keypoint)) +
    geom_line() +
    facet_wrap(as.formula(paste("~", facets)), scales = ifelse(free_y, "free_y", "fixed")) +
    theme_classic(base_size = 15)

  # Apply color scheme based on overlay_axes parameter and adjust legend
  if (overlay_axes) {
    p <- p +
      scale_color_colorblind(name = "Axis") + # Set the legend title to "Axis"
      theme(legend.position = "bottom")      # Place the legend at the bottom
  } else {
    p <- p + theme(legend.position = 'none')
  }

  return(p)
}
