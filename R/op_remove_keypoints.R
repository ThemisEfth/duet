#' Remove Keypoints Based on Various Criteria
#'
#' This function removes keypoints and corresponding columns based on several criteria: user-specified keypoints,
#' low total confidence values over time, exceeding a threshold of missing/zero values, or if all data for a keypoint is missing (i.e., all zeros).
#' The user can choose to apply the removal equally across all persons or separately for each person.
#'
#' @param df A dataframe containing the confidence data (e.g., confidence columns prefixed with 'c', and corresponding x/y columns).
#' @param remove_undetected_keypoints Logical. TRUE to remove keypoints that have all zero confidence values, FALSE to keep them. Default is FALSE.
#' @param remove_specific_keypoints Character vector. A list of specific columns to be removed. Default is NULL.
#' @param remove_keypoints_total_confidence Numeric. A threshold for the mean of confidence values across all rows.
#'        Columns where the mean of confidence is below this threshold will be removed along with their corresponding x/y columns. Default is NULL.
#' @param remove_keypoints_missing_data Numeric. A threshold (between 0 and 1) for the percentage of zeros or missing data in each numeric column.
#'        Columns with missing data percentages exceeding this threshold will be removed. Default is NULL.
#' @param apply_removal_equally Logical. If TRUE, the columns to remove will be applied equally across all persons and regions.
#'        If FALSE, removals will be applied separately for each person and region. Default is TRUE.
#' @return The dataframe with keypoints and columns optionally removed based on the criteria specified.
#' @export
#' @examples
#' # Example dataframe
#' df <- data.frame(
#'   person = c(1, 1, 2, 2),
#'   region = c("face", "face", "face", "face"),
#'   c1 = c(0.9, 0.8, 0.7, 0.0),
#'   c2 = c(0.4, 0.3, 0.2, 0.0),
#'   x1 = c(1, 2, 3, 4),
#'   x2 = c(4, 5, 6, 7),
#'   y1 = c(7, 8, 9, 10),
#'   y2 = c(10, 11, 12, 13)
#' )
#'
#' # Example usage:
#' df_filtered <- op_remove_keypoints(df,
#'                                    remove_undetected_keypoints = TRUE,
#'                                    remove_specific_keypoints = c("c2"),
#'                                    remove_keypoints_total_confidence = 0.8,
#'                                    remove_keypoints_missing_data = 0.5,
#'                                    apply_removal_equally = TRUE)
#' print(df_filtered)
op_remove_keypoints <- function(df,
                                remove_undetected_keypoints = FALSE,
                                remove_specific_keypoints = NULL,
                                remove_keypoints_total_confidence = NULL,
                                remove_keypoints_missing_data = NULL,
                                apply_removal_equally = TRUE) {

  # Ensure logical inputs are either TRUE or FALSE, and not NULL
  remove_undetected_keypoints <- isTRUE(remove_undetected_keypoints)
  apply_removal_equally <- isTRUE(apply_removal_equally)

  # Initial number of columns before any removal
  initial_col_count <- ncol(df)

  # List of columns to remove across all groups (used if apply_removal_equally = TRUE)
  columns_to_remove <- c()

  # Loop through each unique combination of person and region
  unique_groups <- unique(df[, c("person", "region")])

  for (group in seq_len(nrow(unique_groups))) {
    person_value <- unique_groups[group, "person"]
    region_value <- unique_groups[group, "region"]

    # Subset the data for this specific person and region
    group_data <- df[df$person == person_value & df$region == region_value, ]

    # List of columns to remove for this group (used if apply_removal_equally = FALSE)
    group_columns_to_remove <- c()

    # 1. Remove specified columns, if provided
    if (!is.null(remove_specific_keypoints)) {
      remove_specific_keypoints <- remove_specific_keypoints[remove_specific_keypoints %in% names(group_data)]
      if (apply_removal_equally) {
        columns_to_remove <- unique(c(columns_to_remove, remove_specific_keypoints))
      } else {
        group_columns_to_remove <- unique(c(group_columns_to_remove, remove_specific_keypoints))
      }
    }

    # 2. Remove undetected keypoints (all-zero confidence) if remove_undetected_keypoints is TRUE
    if (remove_undetected_keypoints) {
      confidence_cols <- grep("^c", names(group_data), value = TRUE)
      undetected_keypoints <- confidence_cols[sapply(confidence_cols, function(col) all(group_data[[col]] == 0))]

      if (length(undetected_keypoints) > 0) {
        if (apply_removal_equally) {
          columns_to_remove <- unique(c(columns_to_remove, undetected_keypoints,
                                        gsub("^c", "x", undetected_keypoints),
                                        gsub("^c", "y", undetected_keypoints)))
        } else {
          group_columns_to_remove <- unique(c(group_columns_to_remove, undetected_keypoints,
                                              gsub("^c", "x", undetected_keypoints),
                                              gsub("^c", "y", undetected_keypoints)))
        }
      }
    }

    # 3. Remove columns based on total confidence threshold
    if (!is.null(remove_keypoints_total_confidence)) {
      confidence_cols <- grep("^c", names(group_data), value = TRUE)

      cols_to_remove <- sapply(confidence_cols, function(col) {
        mean_conf <- mean(group_data[[col]], na.rm = TRUE)
        return(mean_conf < remove_keypoints_total_confidence)
      })

      confidence_to_remove <- confidence_cols[cols_to_remove]
      if (length(confidence_to_remove) > 0) {
        if (apply_removal_equally) {
          columns_to_remove <- unique(c(columns_to_remove, confidence_to_remove,
                                        gsub("^c", "x", confidence_to_remove),
                                        gsub("^c", "y", confidence_to_remove)))
        } else {
          group_columns_to_remove <- unique(c(group_columns_to_remove, confidence_to_remove,
                                              gsub("^c", "x", confidence_to_remove),
                                              gsub("^c", "y", confidence_to_remove)))
        }
      }
    }

    # 4. Remove columns based on missing data percentage threshold (renamed to remove_keypoints_missing_data)
    if (!is.null(remove_keypoints_missing_data) && is.numeric(remove_keypoints_missing_data)) {
      zero_percentages <- sapply(group_data, function(x) if (is.numeric(x)) mean(x == 0, na.rm = TRUE) else NA)
      columns_to_check <- names(zero_percentages)[grepl("^[xyz]|^c", names(zero_percentages))]
      missing_data_cols <- columns_to_check[zero_percentages[columns_to_check] > remove_keypoints_missing_data]

      if (apply_removal_equally) {
        columns_to_remove <- unique(c(columns_to_remove, missing_data_cols))
      } else {
        group_columns_to_remove <- unique(c(group_columns_to_remove, missing_data_cols))
      }
    }

    # If not applying removals equally, remove columns for the current group
    if (!apply_removal_equally && length(group_columns_to_remove) > 0) {
      group_data <- group_data[, !names(group_data) %in% group_columns_to_remove]
      df[df$person == person_value & df$region == region_value, ] <- group_data
      message(paste(length(group_columns_to_remove), "columns removed for person", person_value, "region", region_value))
    }
  }

  # If applying removals equally, apply the column removals across the entire dataframe
  if (apply_removal_equally && length(columns_to_remove) > 0) {
    df <- df[, !names(df) %in% columns_to_remove]
    message(paste(length(columns_to_remove), "columns removed equally across all persons and regions:", paste(columns_to_remove, collapse = ", ")))
  }

  return(df)
}
