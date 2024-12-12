#' Smooth Time Series Data with Various Methods
#'
#' This function applies different smoothing techniques to time series data
#' for the selected columns (keypoints), including moving average, Kalman-Ziegler Adaptive (KZA),
#' Savitzky-Golay filter, and Butterworth filter. It can optionally plot the
#' smoothed data alongside the original data, with faceting based on the `person` and `keypoint` columns.
#'
#' @name op_smooth_timeseries
#' @param data A data frame containing the time series data. Must include `person`, `time`, and keypoints (e.g., `x0`, `y0`, etc.).
#' @param method The smoothing method to use. Options are "zoo" (moving average),
#'   "kza" (Kalman-Ziegler Adaptive), "savitzky" (Savitzky-Golay filter),
#'   and "butterworth" (Butterworth filter). Default is "zoo".
#' @param kza_k Window size for the KZA method. Default is 3.
#' @param kza_m Number of iterations for the KZA method. Default is 2.
#' @param rollmean_width Width of the moving average window for the zoo method. Default is 3.
#' @param sg_window Window size for the Savitzky-Golay filter. Default is 5.
#' @param sg_order Polynomial order for the Savitzky-Golay filter. Default is 3.
#' @param butter_order Order of the Butterworth filter. Default is 3.
#' @param butter_cutoff Cutoff frequency for the Butterworth filter. Default is 0.1.
#' @param side Character string indicating which side of the data to smooth.
#'   Options are "left", "right", or "both". Default is "both".
#' @param plot Logical, if TRUE, the function will generate a plot comparing the original
#'   and smoothed data. If FALSE, the function returns only the smoothed data frame without plotting. Default is TRUE.
#' @param keypoints Vector of keypoint column names (e.g., `x0`, `x1`) to be smoothed and included in the plot.
#'   If NULL, all keypoints beginning with `x` or `y` will be smoothed and plotted. Default is NULL.
#' @return A data frame with the smoothed time series data for the specified keypoints. If `plot = TRUE`, a plot is displayed comparing the original and smoothed data.
#' @importFrom zoo rollmean
#' @importFrom kza kza
#' @importFrom signal sgolayfilt butter filtfilt
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes geom_line facet_grid theme_classic labs
#' @export
# Declare global variables to avoid NOTE about undefined variables in ggplot2
utils::globalVariables(c("value", "type", "keypoint"))

op_smooth_timeseries <- function(data, method = "rollmean",
                                 kza_k = 3, kza_m = 2, rollmean_width = 3, sg_window = 5,
                                 sg_order = 3, butter_order = 3,
                                 butter_cutoff = 0.1, side = "both",
                                 plot = TRUE, keypoints = NULL) {

  smooth_column <- function(column) {
    if (method == "rollmean") {
      smoothed <- rollmean(column, k = rollmean_width, fill = 'extend')
    } else if (method == "kza") {
      smoothed <- kza(column, k = kza_k, m = kza_m)$kz
    } else if (method == "savitzky") {
      smoothed <- sgolayfilt(column, p = sg_order, n = sg_window)
    } else if (method == "butterworth") {
      butter_filter <- butter(butter_order, butter_cutoff)
      smoothed <- filtfilt(butter_filter, column)
    } else {
      stop("Invalid method. Choose either 'rollmean', 'kza', 'savitzky', or 'butterworth'.")
    }
    return(smoothed)
  }

  # Filter the data based on the side (person)
  if (side != "both") {
    data <- data[data$person == side, ]
  }

  smoothed_data <- data

  # If keypoints are provided, filter the data for those specific keypoints
  if (!is.null(keypoints)) {
    keypoints <- keypoints[keypoints %in% names(data)]
  } else {
    # Identify the keypoints (columns that begin with x and y)
    keypoints <- names(data)[grepl("^x\\d+$", names(data)) | grepl("^y\\d+$", names(data))]
  }

  # Loop over all specified keypoints and smooth them
  for (col in keypoints) {
    smoothed_data[[col]] <- smooth_column(data[[col]])
  }

  # If plot = FALSE, return the smoothed dataframe without plotting
  if (!plot) {
    return(smoothed_data)
  }

  # Reshape data to a long format for ggplot, including both raw and smoothed data
  data_long <- melt(data, id.vars = c("person", "frame"), measure.vars = keypoints)
  smoothed_data_long <- melt(smoothed_data, id.vars = c("person", "frame"), measure.vars = keypoints)

  # Combine raw and smoothed data for plotting
  data_long$type <- "Raw"
  smoothed_data_long$type <- "Smoothed"
  plot_data <- rbind(data_long, smoothed_data_long)

  # Extract the keypoint identifier from the column names
  plot_data$keypoint <- sub("^(x|y)(\\d+)$", "\\1\\2", plot_data$variable)

  # Plot raw and smoothed data with facets for each person and keypoint
  p <- ggplot(plot_data, aes(x = frame, y = value, color = type)) +
    geom_line(aes(linetype = type)) +
    facet_grid(person ~ keypoint) +  # Facet by both person and keypoint
    labs(title = "Raw vs Smoothed Data by Keypoint and Person", x = "Frame", y = "Value") +
    theme_classic()

  print(p)

  return(smoothed_data)
}
