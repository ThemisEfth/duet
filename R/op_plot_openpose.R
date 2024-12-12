#' Plot OpenPose Data for a Specified Frame
#'
#' This function visualizes keypoints and their connections from OpenPose data for a specified frame.
#' The function allows customization of the plot, including the option to display labels, lines between keypoints,
#' and different colors for left and right persons.
#'
#' @param data A data frame containing OpenPose data. The data frame should include columns for the frame number,
#'   person identifier, and x/y coordinates for each keypoint.
#' @param frame_num A numeric value specifying the frame number to plot.
#' @param person A character string specifying which person to plot: "left", "right", or "both". Default is "both".
#' @param lines A logical value indicating whether to draw lines between keypoints. Default is TRUE.
#' @param keylabels A logical value indicating whether to display keypoint labels. Default is FALSE.
#' @param label_type A character string specifying the type of labels to display: "names" or "numbers". Default is "names".
#' @param hide_labels A logical value indicating whether to hide axis labels and plot titles. Default is FALSE.
#' @param left_color A character string specifying the color for the left person. Default is "blue".
#' @param right_color A character string specifying the color for the right person. Default is "red".
#' @param background_color A character string specifying the background color of the plot. Default is "white".
#' @param line_width A numeric value specifying the width of the lines between keypoints. Default is 2.
#' @param point_size A numeric value specifying the size of the keypoint markers. Default is 1.5.
#' @param text_color A character string specifying the color of the text (labels and titles). Default is "black".
#'
#' @importFrom graphics par text segments box points plot
#'
#' @examples
#' # Create a mock data frame with OpenPose data for demonstration
#' data <- data.frame(
#'   frame = rep(10, 4),
#'   person = rep(c("left", "right"), each = 2),
#'   x0 = c(100, 200, 300, 400),
#'   y0 = c(150, 250, 350, 450),
#'   x1 = c(110, 210, 310, 410),
#'   y1 = c(160, 260, 360, 460)
#' )
#'
#' # Plot the data for the specified frame
#' op_plot_openpose(data = data, frame_num = 10, person = "both", lines = TRUE, keylabels = TRUE,
#'                  label_type = "names", left_color = "green", right_color = "purple")
#'
#' @export
op_plot_openpose <- function(data, frame_num, person = c("both", "left", "right"),
                             lines = TRUE, keylabels = FALSE, label_type = c("names", "numbers"),
                             hide_labels = FALSE, left_color = "blue", right_color = "red",
                             background_color = "white", line_width = 2, point_size = 1.5,
                             text_color = "black") {

  # Argument validation
  stopifnot(is.data.frame(data),
            is.numeric(frame_num) && length(frame_num) == 1,
            person %in% c("both", "left", "right"),
            is.logical(lines) && length(lines) == 1,
            is.logical(keylabels) && length(keylabels) == 1,
            label_type %in% c("names", "numbers"),
            is.logical(hide_labels) && length(hide_labels) == 1)

  # Set the background color
  par(bg = background_color)

  # Filter the dataframe for the specified frame
  df_frame <- subset(data, data$frame == frame_num)
  if (nrow(df_frame) == 0) stop("No data found for the specified frame.")

  # Determine persons to plot
  persons <- if (person == "both") c("left", "right") else person

  # Setup the plot canvas
  plot(1, type = "n", xlab = if (hide_labels) "" else "X", ylab = if (hide_labels) "" else "Y",
       main = if (hide_labels) "" else paste("OpenPose Frame", frame_num),
       xlim = c(0, 1920), ylim = c(0, 1080), xaxs = "i", yaxs = "i",
       axes = !hide_labels, frame.plot = !hide_labels, col.main = text_color, col.lab = text_color, col.axis = text_color)

  for (p in persons) {
    # Filter by person
    df_person <- subset(df_frame, df_frame$person == p)
    if (nrow(df_person) == 0) next

    # Extract coordinates
    x_coords <- as.numeric(unlist(df_person[ , grep("^x\\d+$", names(df_person))]))
    y_coords <- as.numeric(unlist(df_person[ , grep("^y\\d+$", names(df_person))]))

    # Define joint labels and numbers
    joint_labels <- c("nose", "neck", "R_shoulder", "R_elbow", "R_wrist",
                      "L_shoulder", "L_elbow", "L_wrist", "mid_hip", "R_hip",
                      "R_knee", "R_ankle", "L_hip", "L_knee", "L_ankle",
                      "R_eye", "L_eye", "R_ear", "L_ear", "L_big_toe",
                      "L_small_toe", "L_heel", "R_big_toe", "R_small_toe", "R_heel")
    joint_numbers <- 0:24

    # Select labels
    labels <- if (match.arg(label_type) == "names") joint_labels else joint_numbers

    # Plot the points
    for (i in seq_along(x_coords)) {
      adjusted_point_size <- if (x_coords[i] == 0 && (1080 - y_coords[i]) == 1080) 0 else point_size
      color_to_use <- if (p == "left") left_color else right_color

      if (adjusted_point_size > 0) {
        points(x_coords[i], 1080 - y_coords[i], pch = 19, col = color_to_use, cex = adjusted_point_size)
      }
    }

    # Add joint labels if requested
    if (keylabels) {
      text(x_coords, 1080 - y_coords, labels = labels, pos = 3, cex = 0.7, offset = 0.5, col = text_color)
    }

    # Draw connections between joints if requested
    if (lines) {
      connections <- list(
        c(0, 1), c(1, 2), c(2, 3), c(3, 4), c(1, 5), c(5, 6), c(6, 7), c(1, 8),
        c(8, 9), c(9, 10), c(10, 11), c(8, 12), c(12, 13), c(13, 14), c(0, 15),
        c(15, 17), c(0, 16), c(16, 18), c(14, 19), c(19, 20), c(14, 21), c(11, 22),
        c(22, 23), c(11, 24)
      )

      for (conn in connections) {
        if (!is.na(x_coords[conn[1] + 1]) && !is.na(y_coords[conn[1] + 1]) &&
            !is.na(x_coords[conn[2] + 1]) && !is.na(y_coords[conn[2] + 1]) &&
            x_coords[conn[1] + 1] != 0 && y_coords[conn[1] + 1] != 0 &&
            x_coords[conn[2] + 1] != 0 && y_coords[conn[2] + 1] != 0) {
          segments(x_coords[conn[1] + 1], 1080 - y_coords[conn[1] + 1],
                   x_coords[conn[2] + 1], 1080 - y_coords[conn[2] + 1], col = color_to_use, lwd = line_width)
        }
      }
    }
  }

  # Remove the box if hide_labels is TRUE
  if (hide_labels) {
    box(which = "plot", lty = "blank")
  }
}
