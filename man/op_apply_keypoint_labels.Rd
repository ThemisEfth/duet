% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/op_apply_keypoint_labels.R
\name{op_apply_keypoint_labels}
\alias{op_apply_keypoint_labels}
\title{Rename Columns Based on Region}
\usage{
op_apply_keypoint_labels(df)
}
\arguments{
\item{df}{Dataframe with columns to be renamed.}
}
\value{
Dataframe with renamed columns.
}
\description{
This function renames columns of a dataframe based on the specified region.
}
\examples{
# Example dataframe
df <- data.frame(
  region = rep(c("body", "hand_left", "hand_right", "face"), each = 3),
  x0 = rnorm(12), y0 = rnorm(12), c0 = rnorm(12),
  x1 = rnorm(12), y1 = rnorm(12), c1 = rnorm(12)
)

# Apply keypoint labels
df_renamed <- op_apply_keypoint_labels(df)
print(df_renamed)
}
