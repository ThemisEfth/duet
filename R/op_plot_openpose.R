#' Plot OpenPose Data for a Specified Frame
#'
#' This function visualizes keypoints and their connections from OpenPose data for a specified frame.
#' The function allows customization of the plot, including the option to display labels, lines between keypoints,
#' and different colours for left and right persons.
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
#' @param background_colour A character string specifying the background colour of the plot (UK spelling). Default is NULL.
#' @param line_width A numeric value specifying the width of the lines between keypoints. Default is 2.
#' @param point_size A numeric value specifying the size of the keypoint markers. Default is 1.5.
#' @param text_color A character string specifying the color of the text (labels and titles). Default is "black".
#'
#' @return No return value, called for side effects (plotting to screen).
#'
#' @importFrom graphics par text segments box points plot
#'
#' @examples
#' # Path to example CSV file included with the package
#' file_path <- system.file("extdata/csv_data/A-B_body_dyad.csv", package = "duet")
#'
#' # Load the data
#' data <- read.csv(file_path)
#'
#' # Plot the data for the specified frame
#' op_plot_openpose(
#'   data = data,
#'   frame_num = 1,
#'   person = "both",
#'   lines = TRUE,
#'   keylabels = TRUE,
#'   label_type = "names",
#'   left_color = "blue",
#'   right_color = "red",
#'   background_colour = "grey90"
#' )
#'
#' @export
op_plot_openpose <- function(data, frame_num, person = c("both", "left", "right"),
                             lines = TRUE, keylabels = FALSE, label_type = c("names", "numbers"),
                             hide_labels = FALSE, left_color = "blue", right_color = "red",
                             background_color = "white", background_colour = NULL,
                             line_width = 2, point_size = 1.5, text_color = "black") {
  # Resolve spelling differences
  background_color <- if (!is.null(background_colour)) background_colour else background_color

  # Validate arguments
  stopifnot(
    is.data.frame(data),
    is.numeric(frame_num) && length(frame_num) == 1,
    person %in% c("both", "left", "right"),
    is.logical(lines),
    is.logical(keylabels),
    label_type %in% c("names", "numbers"),
    is.logical(hide_labels),
    is.character(left_color),
    is.character(right_color),
    is.character(background_color),
    is.numeric(line_width),
    is.numeric(point_size),
    is.character(text_color)
  )

  # Save original par settings and restore them on exit
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar), add = TRUE)

  # Set the background color of the plotting window
  par(bg = background_color)

  # Filter the dataframe for the specified frame
  df_frame <- subset(data, data$frame == frame_num)
  if (nrow(df_frame) == 0) stop("No data found for the specified frame.")

  # Determine persons to plot
  persons <- if (person == "both") c("left", "right") else person

  # Setup the plot canvas
  plot(
    1,
    type = "n",
    xlab = if (hide_labels) "" else "X",
    ylab = if (hide_labels) "" else "Y",
    main = if (hide_labels) "" else paste("OpenPose Frame", frame_num),
    xlim = c(0, 1920),
    ylim = c(0, 1080),
    xaxs = "i",
    yaxs = "i",
    axes = !hide_labels,
    frame.plot = !hide_labels,
    col.main = text_color,
    col.lab = text_color,
    col.axis = text_color
  )

  for (p in persons) {
    # Filter by person
    df_person <- subset(df_frame, df_frame$person == p)
    if (nrow(df_person) == 0) next

    # Extract coordinates
    x_coords <- as.numeric(unlist(df_person[, grep("^x\\d+$", names(df_person))]))
    y_coords <- as.numeric(unlist(df_person[, grep("^y\\d+$", names(df_person))]))

    # Plot the points
    for (i in seq_along(x_coords)) {
      color_to_use <- if (p == "left") left_color else right_color
      if (x_coords[i] > 0 && y_coords[i] > 0) {
        points(
          x_coords[i],
          1080 - y_coords[i],
          pch = 19,
          col = color_to_use,
          cex = point_size
        )
      }
    }
  }
}
