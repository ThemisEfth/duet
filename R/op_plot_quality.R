#' Plot Data Quality (Confidence Ratings or Completeness)
#'
#' This function plots either the mean confidence ratings, the percentage of completeness (i.e., data present),
#' or both for the given dataframe. It can handle data for one or multiple persons and regions, creating separate panels for each.
#'
#' @param df A dataframe containing the confidence data, with columns for base_filename, region, person, and confidence values.
#' @param plot_type Character. Either "confidence" to plot the mean confidence rating, "completeness" to plot the percentage of completeness, or "both" to plot both.
#'                  Default is "confidence".
#' @param threshold_line Numeric. The value at which to draw a dashed horizontal line. Default is 50.
#' @return A ggplot object or a combined plot if "both" is selected.
#' @importFrom dplyr group_by summarise mutate %>% .data matches
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_bar facet_grid geom_hline labs theme_classic theme element_text scale_y_continuous position_dodge
#' @importFrom ggthemes scale_color_colorblind scale_fill_colorblind
#' @importFrom stats sd
#' @importFrom patchwork plot_layout
#' @export
#' @examples
#' # Example usage:
#' # df <- read.csv("path_to_your_data.csv")
#' # plot <- op_plot_data_quality(df, plot_type = "both", threshold_line = 75)
#' # print(plot)
op_plot_quality <- function(df, plot_type = "confidence", threshold_line = 50) {
  # Check if required columns exist
  required_columns <- c("base_filename", "region", "person")
  missing_columns <- setdiff(required_columns, colnames(df))
  if (length(missing_columns) > 0) {
    stop(
      paste(
        "df is missing the following required columns:",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Reshape data for confidence columns (assuming columns start with "c" or end with "c")
  df_confidence <- df %>%
    pivot_longer(
      cols = matches("^c|c$"),
      names_to = "keypoint",
      values_to = "confidence"
    ) %>%
    group_by(.data$base_filename, .data$region, .data$person, .data$keypoint) %>%
    summarise(
      confidence_m = mean(.data$confidence, na.rm = TRUE),
      confidence_sd = sd(.data$confidence, na.rm = TRUE),
      completeness = mean(.data$confidence > 0, na.rm = TRUE) * 100,  # Calculate completeness (opposite of missing)
      .groups = "drop"
    )

  # Plot mean confidence
  plot_confidence <- ggplot(df_confidence, aes(x = .data$keypoint, y = .data$confidence_m * 100)) +
    geom_bar(stat = "identity", position = position_dodge(), fill = "skyblue") +
    facet_grid(.data$person ~ .data$region, scales = "free_x") +
    theme_classic() +
    geom_hline(yintercept = threshold_line, linetype = "dashed", color = "blue") +  # User-defined threshold
    scale_color_colorblind() +
    scale_fill_colorblind() +
    labs(y = 'Confidence (%)', x = 'Keypoint') +
    scale_y_continuous(limits = c(0, 100)) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text.y = element_text(angle = 0)
    )

  # Plot completeness (opposite of missing data)
  plot_completeness <- ggplot(df_confidence, aes(x = .data$keypoint, y = .data$completeness)) +
    geom_bar(stat = "identity", position = position_dodge(), fill = "lightgreen") +
    facet_grid(.data$person ~ .data$region, scales = "free_x") +
    theme_classic() +
    geom_hline(yintercept = threshold_line, linetype = "dashed", color = "green") +  # User-defined threshold
    scale_color_colorblind() +
    scale_fill_colorblind() +
    labs(y = 'Completeness (%)', x = 'Keypoint') +
    scale_y_continuous(limits = c(0, 100)) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text.y = element_text(angle = 0)
    )

  # Plot based on user input
  if (plot_type == "confidence") {
    return(plot_confidence)
  } else if (plot_type == "completeness") {
    return(plot_completeness)
  } else if (plot_type == "both") {
    # Combine both plots using patchwork
    return(plot_confidence / plot_completeness)  # Vertical stacking
  } else {
    stop("Invalid plot_type specified. Choose either 'confidence', 'completeness', or 'both'.")
  }
}
