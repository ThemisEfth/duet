#' Remove Keypoints Based on Various Criteria
#'
#' This function removes keypoints and their corresponding columns based on several criteria:
#' user-specified keypoints, low total confidence values over time, exceeding a threshold of
#' missing/zero values, or if all data for a keypoint is missing (i.e., all zeros).
#'
#' @param df A data frame containing the data to process. Keypoint columns are expected to include
#'           x, y, and c (confidence) columns with corresponding indices.
#' @param remove_specific_keypoints Character vector. Specifies the keypoint indices (e.g., "1") to remove.
#'        This will automatically remove corresponding `x`, `y`, and `c` columns for those indices. Default is NULL.
#' @param remove_undetected_keypoints Logical. If TRUE, removes keypoints where all confidence values are zero across all rows. Default is FALSE.
#' @param remove_keypoints_total_confidence Numeric or FALSE. A threshold for the mean confidence values.
#'        Keypoints with a mean confidence below this threshold will be removed. If set to FALSE, behaves as NULL. Default is NULL.
#' @param remove_keypoints_missing_data Numeric or FALSE. A threshold (between 0 and 1) for the percentage
#'        of missing or zero values. Columns exceeding this threshold will be removed. If set to FALSE, behaves as NULL. Default is NULL.
#' @param apply_removal_equally Logical. If TRUE, the same columns will be removed across all rows of the dataset.
#'        If FALSE, removal criteria are applied separately for each combination of `person` and `region`. Default is TRUE.
#'
#' @return A data frame with specified keypoints and corresponding columns removed.
#' @export
op_remove_keypoints <- function(df,
                                remove_specific_keypoints = NULL,
                                remove_undetected_keypoints = FALSE,
                                remove_keypoints_total_confidence = NULL,
                                remove_keypoints_missing_data = NULL,
                                apply_removal_equally = TRUE) {
  # Treat FALSE as NULL for numeric thresholds
  remove_keypoints_total_confidence <- if (isFALSE(remove_keypoints_total_confidence)) NULL else remove_keypoints_total_confidence
  remove_keypoints_missing_data <- if (isFALSE(remove_keypoints_missing_data)) NULL else remove_keypoints_missing_data

  # Helper function to generate column names for a keypoint index
  get_keypoint_columns <- function(index) {
    paste0(c("x", "y", "c"), index)
  }

  # Helper function to apply removal logic to a subset of the data
  apply_removal <- function(subset_df) {
    # Initialize columns to remove
    columns_to_remove <- character()

    # Step 1: Remove specific keypoints
    if (!is.null(remove_specific_keypoints)) {
      specific_cols <- unlist(lapply(remove_specific_keypoints, get_keypoint_columns))
      specific_cols <- intersect(specific_cols, names(subset_df))
      columns_to_remove <- unique(c(columns_to_remove, specific_cols))
    }

    # Step 2: Remove undetected keypoints (all-zero confidence)
    if (remove_undetected_keypoints) {
      # Get all confidence columns
      confidence_cols <- grep("^c", names(subset_df), value = TRUE)

      # Identify confidence columns where all values are zero
      undetected_cols <- confidence_cols[sapply(confidence_cols, function(col) {
        all(subset_df[[col]] == 0)
      })]

      # Extract keypoint indices from the confidence column names
      undetected_indices <- gsub("^c", "", undetected_cols)

      # Get related columns (x, y, c) for each undetected keypoint index
      related_cols <- unlist(lapply(undetected_indices, get_keypoint_columns))

      # Add these columns to the list of columns to remove
      columns_to_remove <- unique(c(columns_to_remove, related_cols))
    }

    # Step 3: Remove keypoints with low total confidence
    if (!is.null(remove_keypoints_total_confidence)) {
      confidence_cols <- grep("^c", names(subset_df), value = TRUE)
      low_confidence_cols <- confidence_cols[sapply(confidence_cols, function(col) {
        mean(subset_df[[col]], na.rm = TRUE) < remove_keypoints_total_confidence
      })]
      related_cols <- unlist(lapply(gsub("^c", "", low_confidence_cols), get_keypoint_columns))
      columns_to_remove <- unique(c(columns_to_remove, related_cols))
    }

    # Step 4: Remove keypoints with excessive missing data
    if (!is.null(remove_keypoints_missing_data)) {
      numeric_cols <- grep("^[xyz]|^c", names(subset_df), value = TRUE)
      high_missing_cols <- numeric_cols[sapply(numeric_cols, function(col) {
        mean(subset_df[[col]] == 0 | is.na(subset_df[[col]])) > remove_keypoints_missing_data
      })]
      related_cols <- unlist(lapply(gsub("^[xyzc]", "", high_missing_cols), get_keypoint_columns))
      columns_to_remove <- unique(c(columns_to_remove, related_cols))
    }

    # Remove columns
    subset_df <- subset_df[, !names(subset_df) %in% columns_to_remove, drop = FALSE]
    return(subset_df)
  }

  # Apply removal equally across all rows or separately by person/region
  if (apply_removal_equally) {
    df <- apply_removal(df)
  } else {
    unique_groups <- unique(df[, c("person", "region")])
    for (i in seq_len(nrow(unique_groups))) {
      person <- unique_groups[i, "person"]
      region <- unique_groups[i, "region"]

      # Subset data for this group
      subset_df <- df[df$person == person & df$region == region, ]

      # Ensure column names align after processing subset
      updated_subset <- apply_removal(subset_df)

      # Update only existing columns in the main dataframe
      existing_columns <- intersect(names(updated_subset), names(df))
      df[df$person == person & df$region == region, existing_columns] <- updated_subset[, existing_columns]
    }
  }

  return(df)
}
