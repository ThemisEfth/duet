#' Process Multiple Dyads for Cross-Wavelet Coherence
#'
#' @description
#' A wrapper function that processes multiple dyads and/or regions in a dataset,
#' attaching unique identifiers to each result.
#'
#' @param data A data frame containing motion energy data for multiple dyads/regions.
#' @param process_by Character vector specifying what to process separately.
#'   Options: c("dyad"), c("region"), or c("dyad", "region"). Default is c("dyad").
#' @param unique_id_cols Character vector of column names to include as unique
#'   identifiers in the output. If NULL, uses the dyad column and region column.
#' @param parallel Logical, whether to use parallel processing (requires 'parallel' package).
#' @param ... Additional arguments passed to op_compute_coherence()
#'
#' @return A list with elements:
#' \item{results}{A list of results, one per dyad/region combination}
#' \item{summary_table}{A data frame combining all coherence summaries with unique IDs}
#' \item{processing_log}{A data frame with processing status for each combination}
#'
#' @export
op_compute_coherence_batch <- function(data,
                                       process_by = c("dyad"),
                                       unique_id_cols = NULL,
                                       parallel = FALSE,
                                       ...) {

  # Detect dyad column
  dyad_col <- .detect_dyad_column(data, list(...)$dyad_col)
  region_col <- ifelse(is.null(list(...)$region_col), "region", list(...)$region_col)

  # Set up unique ID columns
  if (is.null(unique_id_cols)) {
    unique_id_cols <- c(dyad_col)
    if ("region" %in% process_by && region_col %in% names(data)) {
      unique_id_cols <- c(unique_id_cols, region_col)
    }
  }

  # Create combinations to process
  combinations <- .create_processing_combinations(data, process_by, dyad_col, region_col)

  # Initialize results storage
  results <- list()
  processing_log <- data.frame(
    combination_id = character(0),
    dyad_id = character(0),
    region = character(0),
    status = character(0),
    error_message = character(0),
    stringsAsFactors = FALSE
  )

  # Process each combination
  process_function <- function(i) {
    combo <- combinations[i, ]
    combo_id <- .generate_combination_id(combo, process_by)

    tryCatch({
      # Extract unique ID values for this combination
      unique_ids <- .extract_unique_ids(data, combo, unique_id_cols, dyad_col, region_col)

      # Run the analysis
      result <- op_compute_coherence(
        data = data,
        dyad_id = combo$dyad_id,
        region = if("region" %in% process_by) combo$region else NULL,
        dyad_col = dyad_col,
        region_col = region_col,
        verbose = FALSE,  # Suppress individual messages for batch processing
        ...
      )

      # Attach unique identifiers to the result
      result <- .attach_unique_ids(result, unique_ids, combo_id)

      # Log success
      log_entry <- data.frame(
        combination_id = combo_id,
        dyad_id = combo$dyad_id,
        region = if("region" %in% names(combo)) combo$region else NA,
        status = "Success",
        error_message = NA,
        stringsAsFactors = FALSE
      )

      list(result = result, log = log_entry, combo_id = combo_id)

    }, error = function(e) {
      # Log error
      log_entry <- data.frame(
        combination_id = combo_id,
        dyad_id = combo$dyad_id,
        region = if("region" %in% names(combo)) combo$region else NA,
        status = "Error",
        error_message = as.character(e$message),
        stringsAsFactors = FALSE
      )

      list(result = NULL, log = log_entry, combo_id = combo_id)
    })
  }

  # Execute processing (parallel or sequential)
  if (parallel && requireNamespace("parallel", quietly = TRUE)) {
    cl <- parallel::makeCluster(parallel::detectCores() - 1)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    processed_results <- parallel::parLapply(cl, seq_len(nrow(combinations)), process_function)
  } else {
    processed_results <- lapply(seq_len(nrow(combinations)), process_function)
  }

  # Organize results
  for (proc_result in processed_results) {
    processing_log <- rbind(processing_log, proc_result$log)

    if (!is.null(proc_result$result)) {
      results[[proc_result$combo_id]] <- proc_result$result
    }
  }

  # Create summary table
  summary_table <- .create_summary_table(results)

  # Print summary
  n_success <- sum(processing_log$status == "Success")
  n_error <- sum(processing_log$status == "Error")
  message("Batch processing completed: ", n_success, " successful, ", n_error, " errors")

  if (n_error > 0) {
    message("Check processing_log for error details")
  }

  return(list(
    results = results,
    summary_table = summary_table,
    processing_log = processing_log
  ))
}

# Helper functions for batch processing
.create_processing_combinations <- function(data, process_by, dyad_col, region_col) {
  if ("dyad" %in% process_by && "region" %in% process_by) {
    # Process each dyad-region combination
    unique_combinations <- unique(data[, c(dyad_col, region_col)])
    names(unique_combinations) <- c("dyad_id", "region")
  } else if ("dyad" %in% process_by) {
    # Process each dyad (auto-detect region within each)
    unique_combinations <- data.frame(dyad_id = unique(data[[dyad_col]]))
  } else if ("region" %in% process_by) {
    # Process each region (auto-detect dyad within each)
    unique_combinations <- data.frame(region = unique(data[[region_col]]))
  }

  return(unique_combinations)
}

.generate_combination_id <- function(combo, process_by) {
  if ("dyad" %in% process_by && "region" %in% process_by) {
    paste0(combo$dyad_id, "__", combo$region)
  } else if ("dyad" %in% process_by) {
    as.character(combo$dyad_id)
  } else if ("region" %in% process_by) {
    as.character(combo$region)
  }
}

.extract_unique_ids <- function(data, combo, unique_id_cols, dyad_col, region_col) {
  # Filter data to this combination
  if ("dyad_id" %in% names(combo) && "region" %in% names(combo)) {
    combo_data <- data[data[[dyad_col]] == combo$dyad_id & data[[region_col]] == combo$region, ]
  } else if ("dyad_id" %in% names(combo)) {
    combo_data <- data[data[[dyad_col]] == combo$dyad_id, ]
  } else if ("region" %in% names(combo)) {
    combo_data <- data[data[[region_col]] == combo$region, ]
  }

  # Extract unique values for the specified columns
  unique_ids <- list()
  for (col in unique_id_cols) {
    if (col %in% names(combo_data)) {
      unique_vals <- unique(combo_data[[col]])
      if (length(unique_vals) == 1) {
        unique_ids[[col]] <- unique_vals[1]
      } else {
        unique_ids[[col]] <- unique_vals  # Keep all if multiple
      }
    }
  }

  return(unique_ids)
}

.attach_unique_ids <- function(result, unique_ids, combo_id) {
  # Add unique IDs to analysis_info
  result$analysis_info$unique_ids <- unique_ids
  result$analysis_info$combination_id <- combo_id

  # Add unique IDs to coherence_summary
  for (id_name in names(unique_ids)) {
    result$coherence_summary[[id_name]] <- unique_ids[[id_name]]
  }
  result$coherence_summary$combination_id <- combo_id

  return(result)
}

.create_summary_table <- function(results) {
  if (length(results) == 0) return(data.frame())

  # Extract coherence summaries and combine
  summary_list <- lapply(results, function(x) x$coherence_summary)

  # Use dplyr::bind_rows if available, otherwise rbind
  if (requireNamespace("dplyr", quietly = TRUE)) {
    dplyr::bind_rows(summary_list)
  } else {
    do.call(rbind, summary_list)
  }
}
