#' Plot Keypoints with Facet Wrap
#'
#' This function plots the specified keypoints over time with facet wrapping.
#' It allows the user to overlay axes or separate them, and to filter the data by person.
#'
#' @param data Data frame containing the keypoint data.
#' @param keypoints Character vector of keypoints to include in the plot. When `overlay_axes = TRUE`, input keypoints as "0", "1", "2", etc. When `overlay_axes = FALSE`, input keypoints as "x0", "y0", "x1", "y1", etc.
#' @param free_y Boolean indicating if the y-axis should be free in facet_wrap (default is TRUE).
#' @param overlay_axes Boolean indicating if 'x' and 'y' columns should be overlaid in the same plot (default is FALSE).
#' @param person Character string specifying which person to plot ("left", "right", or "both", default is "both").
#' @param max_facets Integer indicating the maximum number of facets allowed (default is 10).
#'
#' @return A ggplot object.
#' @export
#'
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes_string geom_line facet_wrap theme_classic theme
#' @importFrom ggthemes scale_color_colorblind
#' @importFrom stats as.formula
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

  # Ensure the 'person' parameter is treated as a string
  person <- as.character(person)

  # Filter data based on the 'person' parameter
  if (person != "both") {
    data <- data[data$person == person, ]
  }

  # Select columns that start with 'x' or 'y' only, and exclude 'frame' or other non-numeric columns
  numeric_cols <- data[, grepl("^(x|y)", names(data))]

  # Reshape the data
  if (overlay_axes) {
    df_long <- pivot_longer(
      data[c(x_axis, colnames(numeric_cols), "person")],
      cols = -c(x_axis, "person"),
      names_to = c("axis", "keypoint"),
      names_pattern = "(.)(.*)",
      values_to = "values"
    )
  } else {
    df_long <- pivot_longer(
      data[c(x_axis, colnames(numeric_cols), "person")],
      cols = -c(x_axis, "person"),
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
    warning(paste("Too many facets (", total_facets, ") to plot. Please specify fewer keypoints or set a higher max_facets limit.", sep = ""))
    return(NULL)
  }

  # Determine facets
  facets <- "keypoint"
  if (person == "both") {
    facets <- paste(facets, "person", sep = " + ")
  }

  # Plot the data
  p <- ggplot(df_long, aes_string(x = x_axis,
                                  y = "values",
                                  color = ifelse(overlay_axes, "axis", "keypoint"))) +
    geom_line() +
    facet_wrap(as.formula(paste("~", facets)), scales = ifelse(free_y, "free_y", "fixed")) +
    theme_classic(base_size = 15)

  # Apply color scheme based on overlay_axes parameter
  if (overlay_axes) {
    p <- p + scale_color_colorblind()
  } else {
    p <- p + theme(legend.position = 'none')
  }

  return(p)
}
