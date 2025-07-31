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
#' @examples
#' # Load example data from the package
#' data_path <- system.file("extdata/csv_data/dyad_1/A_body.csv", package = "duet")
#' df <- read.csv(data_path)
#'
#' # Remove keypoints based on various criteria
#' result <- op_remove_keypoints(
#'   df = df,
#'   remove_specific_keypoints = c("1", "2"), # Remove specific keypoints (e.g., keypoints 1 and 2)
#'   remove_undetected_keypoints = TRUE,      # Remove keypoints with all zero confidence
#'   remove_keypoints_total_confidence = 0.5, # Remove keypoints with mean confidence below 0.5
#'   remove_keypoints_missing_data = 0.2,     # Remove keypoints with >20% missing data
#'   apply_removal_equally = TRUE             # Apply removal equally across the dataset
#' )
#'
#' # Display the result
#' print(result)
op_remove_keypoints <- function(
  df,
  remove_specific_keypoints = NULL,
  remove_undetected_keypoints = FALSE,
  remove_keypoints_total_confidence = NULL,
  remove_keypoints_missing_data = NULL,
  apply_removal_equally = TRUE
) {
  # Treat FALSE as NULL for numeric thresholds
  remove_keypoints_total_confidence <- if (
    isFALSE(remove_keypoints_total_confidence)
  ) {
    NULL
  } else {
    remove_keypoints_total_confidence
  }
  remove_keypoints_missing_data <- if (isFALSE(remove_keypoints_missing_data)) {
    NULL
  } else {
    remove_keypoints_missing_data
  }

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
      specific_cols <- unlist(lapply(
        remove_specific_keypoints,
        get_keypoint_columns
      ))
      columns_to_remove <- unique(c(columns_to_remove, specific_cols))
    }

    # Step 2: Remove undetected keypoints (all-zero confidence)
    if (remove_undetected_keypoints) {
      confidence_cols <- grep("^c", names(subset_df), value = TRUE)
      # FIX: Added na.rm = TRUE to all()
      undetected_cols <- confidence_cols[sapply(confidence_cols, function(col) {
        all(subset_df[[col]] == 0, na.rm = TRUE)
      })]
      undetected_indices <- gsub("^c", "", undetected_cols)
      related_cols <- unlist(lapply(undetected_indices, get_keypoint_columns))
      columns_to_remove <- unique(c(columns_to_remove, related_cols))
    }

    # Step 3: Remove keypoints with low total confidence
    if (!is.null(remove_keypoints_total_confidence)) {
      confidence_cols <- grep("^c", names(subset_df), value = TRUE)
      low_confidence_cols <- confidence_cols[sapply(
        confidence_cols,
        function(col) {
          mean(subset_df[[col]], na.rm = TRUE) <
            remove_keypoints_total_confidence
        }
      )]
      related_cols <- unlist(lapply(
        gsub("^c", "", low_confidence_cols),
        get_keypoint_columns
      ))
      columns_to_remove <- unique(c(columns_to_remove, related_cols))
    }

    # Step 4: Remove keypoints with excessive missing data
    if (!is.null(remove_keypoints_missing_data)) {
      all_cols <- grep("^[xyc]", names(subset_df), value = TRUE)
      high_missing_cols <- all_cols[sapply(all_cols, function(col) {
        mean(subset_df[[col]] == 0 | is.na(subset_df[[col]])) >
          remove_keypoints_missing_data
      })]
      related_cols <- unlist(lapply(
        gsub("^[xyc]", "", high_missing_cols),
        get_keypoint_columns
      ))
      columns_to_remove <- unique(c(columns_to_remove, related_cols))
    }

    # Remove only the columns that actually exist in the subset
    columns_to_remove <- intersect(columns_to_remove, names(subset_df))
    subset_df <- subset_df[,
      !names(subset_df) %in% columns_to_remove,
      drop = FALSE
    ]
    return(subset_df)
  }

  # Apply removal equally across all rows or separately by person/region
  if (apply_removal_equally) {
    df <- apply_removal(df)
  } else {
    # FIX: Correctly implement split-apply-combine logic
    # Split the data frame into a list based on person and region
    group_list <- split(df, list(df$person, df$region), drop = TRUE)

    # Apply the removal function to each group in the list
    processed_list <- lapply(group_list, apply_removal)

    # Combine the processed data frames back together. This is complex in base R
    # because different groups may have different columns removed.
    # First, find the union of all column names from all processed groups.
    all_names <- unique(unlist(lapply(processed_list, names)))

    # Next, ensure each data frame in the list has this full set of columns,
    # filling missing ones with NA.
    standardised_list <- lapply(processed_list, function(sub_df) {
      missing_cols <- setdiff(all_names, names(sub_df))
      if (length(missing_cols) > 0) {
        sub_df[, missing_cols] <- NA
      }
      # Return the data frame with columns in a consistent order
      sub_df[, all_names]
    })

    # Now, safely combine all standardised data frames
    df <- do.call(rbind, standardised_list)
    rownames(df) <- NULL # Clean up row names from split-lapply-rbind
  }

  return(df)
}
