#' Animate OpenPose data for a dyad across a range of frames (Video)
#'
#' This function generates a video of the OpenPose data for both persons in a dyad across a specified range of frames.
#'
#' @param data A dataframe containing OpenPose data.
#' @param output_file A character string specifying the path and filename for the output video file.
#' @param lines A logical value indicating whether to draw lines connecting joints. Default is FALSE.
#' @param keylabels A logical value indicating whether to label keypoints. Default is FALSE.
#' @param label_type A character string specifying the type of labels to use: "names" or "numbers". Default is "names".
#' @param fps An integer specifying the frames per second for the video. Default is 24.
#' @param min_frame An optional integer specifying the minimum frame to include in the video. Default is the first frame in the data.
#' @param max_frame An optional integer specifying the maximum frame to include in the video. Default is the last frame in the data.
#' @param hide_labels A logical value indicating whether to hide the x and y axes, box, and title. Default is FALSE.
#' @param left_color A character string specifying the color to use for the left person. Default is "blue".
#' @param right_color A character string specifying the color to use for the right person. Default is "red".
#' @param background_color A character string specifying the background color of the plot. Default is "white". (US English)
#' @param background_colour A character string specifying the background colour of the plot. Default is "white". (UK English)
#'
#' @return No return value. This function generates a video file as a side effect, saved at the specified output path.
#' @importFrom grDevices dev.copy dev.off png
#' @importFrom graphics points
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export
#' @examples
#' \dontrun{
#' # Example OpenPose data
#' data <- data.frame(
#'   frame = rep(1:10, each = 2),
#'   person = rep(c("left", "right"), times = 10),
#'   x0 = runif(20, 0, 1920), y0 = runif(20, 0, 1080),
#'   x1 = runif(20, 0, 1920), y1 = runif(20, 0, 1080)
#' )
#'
#' # Output file path
#' output_file <- tempfile("output_video", fileext = ".mp4")
#'
#' # Generate video
#' op_animate_dyad(
#'   data = data,
#'   output_file = output_file,
#'   fps = 24,
#'   left_color = "blue",
#'   right_color = "red"
#' )
#' }
op_animate_dyad <- function(data, output_file, lines = FALSE, keylabels = FALSE, label_type = "names", fps = 24,
                            min_frame = NULL, max_frame = NULL, hide_labels = FALSE,
                            left_color = "blue", right_color = "red",
                            background_color = "white", background_colour = NULL) {

  # Resolve background color preference
  if (!is.null(background_colour)) {
    background_color <- background_colour
  }

  # Ensure the output file path is correct
  output_file <- normalizePath(output_file, mustWork = FALSE)

  # Check if FFmpeg is available
  ffmpeg_check <- system("ffmpeg -version", intern = TRUE, ignore.stderr = TRUE)
  if (length(ffmpeg_check) == 0) {
    stop("FFmpeg is not installed or not available in the system PATH.")
  }

  # Ensure the output directory is writable
  output_dir <- dirname(output_file)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  if (!file.access(output_dir, 2) == 0) {
    stop(paste("Cannot write to directory:", output_dir))
  }

  # Ensure the frame column is numeric
  if (!is.numeric(data$frame)) {
    data$frame <- as.numeric(as.character(data$frame))
  }

  # Determine the range of frames to include
  if (is.null(min_frame)) {
    min_frame <- min(data$frame, na.rm = TRUE)
  }
  if (is.null(max_frame)) {
    max_frame <- max(data$frame, na.rm = TRUE)
  }

  # Filter the dataframe to include only the specified frame range
  data_filtered <- subset(data, frame >= min_frame & frame <= max_frame)

  # Create a temporary directory for saving frames
  temp_dir <- file.path(tempdir(), "op_animate_dyad")
  dir.create(temp_dir, showWarnings = FALSE)

  # List to store the paths of the saved images
  frame_files <- vector("list", length(unique(data_filtered$frame)))

  # Progress bar
  pb <- txtProgressBar(min = 0, max = length(unique(data_filtered$frame)), style = 3)

  # Iterate over each frame in the filtered dataframe and save as an image
  frames <- unique(data_filtered$frame)
  for (i in seq_along(frames)) {
    frame_num <- frames[i]
    png_filename <- file.path(temp_dir, sprintf("frame_%04d.png", i))
    frame_files[[i]] <- png_filename

    # Plot for this frame with the new hide_labels and color parameters
    op_plot_openpose(
      data = data_filtered,
      frame_num = frame_num,
      person = "both",
      lines = lines,
      keylabels = keylabels,
      label_type = label_type,
      hide_labels = hide_labels,
      left_color = left_color,
      right_color = right_color,
      background_color = background_color
    )

    # Save the plot as an image
    suppressMessages({
      png(filename = png_filename, width = 1920, height = 1080)
      dev.off()
    })

    # Update progress bar
    setTxtProgressBar(pb, i)
  }

  close(pb)

  # Check if images were actually created
  if (length(frame_files) == 0 || !file.exists(frame_files[[1]])) {
    unlink(temp_dir, recursive = TRUE)
    stop("No frames were created; please check the input data and plotting process.")
  }

  # Generate the video using a system call to FFmpeg
  ffmpeg_cmd <- sprintf(
    'ffmpeg -y -loglevel error -framerate %d -i "%s" -c:v libx264 -r 30 -pix_fmt yuv420p "%s"',
    fps, file.path(temp_dir, "frame_%04d.png"), output_file
  )

  result <- system(ffmpeg_cmd, intern = TRUE)

  # Verify if the output file was successfully created
  if (!file.exists(output_file)) {
    unlink(temp_dir, recursive = TRUE)
    stop("FFmpeg failed to generate the video. Please check your input data and FFmpeg installation.")
  }

  # Clean up the temporary directory
  unlink(temp_dir, recursive = TRUE)

  # Success message
  message("Video successfully created and saved at: ", output_file)
}

# Declare global variables to avoid R CMD check notes
utils::globalVariables(c("x", "y", "frame", "person"))
