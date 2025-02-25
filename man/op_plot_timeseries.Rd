% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/op_plot_timeseries.R
\name{op_plot_timeseries}
\alias{op_plot_timeseries}
\title{Plot Keypoints with Facet Wrap}
\usage{
op_plot_timeseries(
  data,
  keypoints = NULL,
  free_y = TRUE,
  overlay_axes = FALSE,
  person = "both",
  max_facets = 10
)
}
\arguments{
\item{data}{Data frame containing the keypoint data.}

\item{keypoints}{Character vector of keypoints to include in the plot. When \code{overlay_axes = TRUE}, input keypoints as "0", "1", "2", etc. When \code{overlay_axes = FALSE}, input keypoints as "x0", "y0", "x1", "y1", etc.}

\item{free_y}{Boolean indicating if the y-axis should be free in facet_wrap (default is TRUE).}

\item{overlay_axes}{Boolean indicating if 'x' and 'y' columns should be overlaid in the same plot (default is FALSE).}

\item{person}{Character string specifying which person to plot ("left", "right", or "both", default is "both").}

\item{max_facets}{Integer indicating the maximum number of facets allowed (default is 10). If the total facets exceed this number, the function returns \code{NULL} with a warning.}
}
\value{
A ggplot object or NULL if the maximum number of facets is exceeded.
}
\description{
This function plots the specified keypoints over time with facet wrapping.
It allows the user to overlay axes or separate them, and to filter the data by person.
If the number of facets exceeds the \code{max_facets} limit, a warning is issued, and the function will not return a plot.
}
\examples{
# Load example data from the package
data_path <- system.file("extdata/csv_data/dyad_1/A_body.csv", package = "duet")
data <- read.csv(data_path)

# Plot with overlay_axes = TRUE
op_plot_timeseries(data = data, keypoints = c("0", "1"), overlay_axes = TRUE, person = "both")

# Plot with overlay_axes = FALSE
op_plot_timeseries(data = data, keypoints = c("0", "1"), overlay_axes = FALSE, person = "left")
}
