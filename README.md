# Duet: An R Package for Dyadic Movement Analysis

## Overview

See the `demo/` folder for example data and a demonstration script.

`duet` is an R package designed for analysing dyadic movement data using OpenPose-generated keypoints. It provides functions to process, visualise, and compute various movement-based metrics, including velocity, acceleration, and jerkiness.

## Installation

To install the `duet` package, use the following command:

```r
install.packages("duet")
```

### Optional: Install Additional Dependencies

Some additional packages may be required for full functionality. You can install them using:

```r
required_packages <- c("tidyverse", "magick", "ggplot2", "cowplot", "rempsyc")

invisible(lapply(required_packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Package", pkg, "is not installed. Install it using install.packages('", pkg, "')", sep = ""))
  } else {
    library(pkg, character.only = TRUE)
  }
}))
```

## Load Required Packages

```r
library(tidyverse)
library(duet)
library(magick)
library(ggplot2)
library(cowplot)
library(rempsyc)
```

## Data Generation

### Generate Data for a Single Dyad

```r
op_create_csv(
  input_path = "./json/Dyad_1",
  output_path = "./Dyad",
  model = "body"
)
```

### Batch Generate Data for All Dyads

```r
op_batch_create_csv(
  input_base_path = "./json",
  output_base_path = "./dyad",
  model = "body"
)
```

## Import Data

```r
df <- read.csv("../data/dyad_16.csv")
```

## Data Quality Check

```r
df |> op_plot_openpose(frame_num = 2, person = "both", lines = TRUE)
```

## Apply Labels and Plot Data Quality

```r
df |>
  op_apply_keypoint_labels() |>
  op_plot_quality(plot_type = "completeness", threshold_line = 80) -> pl_completeness

pl_completeness

df |>
  op_plot_quality(plot_type = "confidence", threshold_line = 80) -> pl_conf

pl_conf

plot_grid(pl_completeness, pl_conf, ncol = 1, labels = "auto") -> pl_qual_both

df |>
  op_apply_keypoint_labels() |>
  op_plot_quality(plot_type = "both", threshold_line = 80) -> pl_qual

pl_qual

ggsave("../figures/Fig2_Quality.tiff", pl_qual_both, width = 6, height = 5, dpi = 300)
```

## Plot Time Series Data

```r
df |>
  op_plot_timeseries(keypoints = c("1", "2", "3"), free_y = TRUE, overlay = TRUE, person = "both", max_facets = 20) -> pl_ts

pl_ts

ggsave("./Fig3_Timeseries.tiff", pl_ts, width = 6, height = 5, dpi = 300)
```

## Apply Moving-Average Smoothing

```r
df |>
  op_smooth_timeseries(keypoints = c("x1"), method = "rollmean", rollmean_width = 100, plot = TRUE, side = "right")
```

## Compute Velocity

```r
df |>
  op_compute_velocity(merge_xy = TRUE, overwrite = TRUE, fps = 30) -> df_velocity

df_velocity |>
  filter(frame != 1) |>
  filter(frame < 6) |>
  select(person, frame, region, v_1, v_2, v_3, v_4) |>
  nice_table() -> tbl

flextable::save_as_docx(tbl, path = "./velocity.docx")
```

## Compute Acceleration

```r
df_velocity |>
  op_compute_acceleration(merge_xy = TRUE, overwrite = TRUE, fps = 30) -> df_acceleration
```

## Compute Jerkiness

```r
df_acceleration |>
  op_compute_acceleration(merge_xy = TRUE, overwrite = TRUE, fps = 30) -> df_jerkiness
```

## License

This package is licensed under the MIT License.

## Contributing

See the `demo/` folder for example data and a demonstration script.

If you’d like to contribute, please open an issue or submit a pull request on the [GitHub repository](https://github.com/ThemisEfth/duet).

## Acknowledgements

This package was developed to facilitate dyadic movement analysis using OpenPose-generated keypoints. If you use `duet` in your research, please consider citing the repository.

