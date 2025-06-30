#' Compute Cross-Wavelet Coherence for Dyadic Motion Energy Data
#'
#' @description
#' This function computes cross-wavelet coherence between two individuals in a dyad
#' using motion energy data. It is designed to be robust, CRAN-compliant, and
#' user-friendly, with automatic detection of parameters and dynamic calculation
#' of frequency bands.
#'
#' @param data A data frame containing motion energy data.
#' @param dyad_id Character string for the dyad to analyze. If `NULL` (default),
#'   the function will proceed only if a single dyad is present in `data`.
#' @param region Character string for the body region to analyze. If `NULL`
#'   (default), proceeds only if a single region exists for the selected dyad.
#' @param person_ids A vector of two character strings for the persons in the
#'   dyad. If `NULL` (default), auto-detects the two persons.
#' @param dyad_col Character string for the dyad identifier column. Defaults to
#'   "base_filename" or "dyad_id" if found.
#' @param region_col Character string for the region column name (default: "region").
#' @param person_col Character string for the person column name (default: "person").
#' @param frame_col Character string for the frame/time column name (default: "frame").
#' @param motion_col Character string for the motion energy column name (default: "motion_energy").
#' @param freq_bands A named list of frequency bands in **Hertz (Hz)**. Each
#'   element is a numeric vector of length two specifying the lower and upper
#'   frequency bound (e.g., `list("slow_rhythm" = c(0.1, 0.5))`).
#' @param start_frame Integer, the starting frame for analysis (default: 1).
#' @param end_frame Integer, the ending frame for analysis. If `NULL` (default),
#'   uses all available frames.
#' @param param Numeric, the mother wavelet parameter for `biwavelet::wtc` (default: 8).
#' @param nrands Integer, the number of random simulations for significance
#'   testing (default: 1000).
#' @param plot_result Logical, if `TRUE`, generates a plot of the wavelet coherence.
#' @param return_raw Logical, if `TRUE`, includes the raw `wtc` object in the output.
#' @param verbose Logical, if `TRUE`, prints informative messages during execution.
#'
#' @return
#' A list containing:
#' \item{coherence_summary}{A data frame with `dyad_id` and coherence statistics for each frequency band.}
#' \item{analysis_info}{A list with metadata about the analysis.}
#' \item{wtc_object}{If `return_raw = TRUE`, the raw object from `biwavelet::wtc`.}
#'
#' @details
#' This function is a wrapper around `biwavelet::wtc` that simplifies its
#' application to dyadic motion data. It includes CRAN-compliant safety checks,
#' such as replacing `cat()` with `message()` and safely managing graphical
#' parameters with `on.exit()`.
#'
#' The key improvement is the dynamic calculation of frequency bands. You specify
#' bands in Hz, and the function identifies the corresponding indices from the
#' wavelet transform's scale/period results, making the analysis independent of
#' time series length and sampling rate.
#'
#' @examples
#' \dontrun{
#' # Create sample data
#' sample_data <- data.frame(
#'   frame = rep(1:100, 2),
#'   dyad_id = "D01",
#'   region = "body",
#'   person = rep(c("P1", "P2"), each = 100),
#'   motion_energy = c(rnorm(100), rnorm(100))
#' )
#'
#' # Define frequency bands in Hz
#' my_bands <- list(
#'   "slow" = c(0.1, 0.5), # 0.1 to 0.5 Hz
#'   "fast" = c(0.5, 1.0)  # 0.5 to 1.0 Hz
#' )
#'
#' # Run analysis (dyad_id and region are auto-detected)
#' result <- op_compute_coherence(
#'   data = sample_data,
#'   freq_bands = my_bands,
#'   plot_result = TRUE
#' )
#'
#' print(result$coherence_summary)
#' }
#'
#' @export
#' @importFrom biwavelet wtc
#' @importFrom graphics par plot
#' @importFrom stats sd median
#' @importFrom utils packageVersion
#' @importFrom dplyr bind_rows
op_compute_coherence <- function(data,
                                 dyad_id = NULL,
                                 region = NULL,
                                 person_ids = NULL,
                                 dyad_col = NULL,
                                 region_col = "region",
                                 person_col = "person",
                                 frame_col = "frame",
                                 motion_col = "motion_energy",
                                 freq_bands = list(
                                   "0.03-0.06Hz" = c(0.03125, 0.0625),
                                   "0.06-0.12Hz" = c(0.0625, 0.125),
                                   "0.12-0.25Hz" = c(0.125, 0.25),
                                   "0.25-0.5Hz" = c(0.25, 0.5),
                                   "0.5-1Hz" = c(0.5, 1),
                                   "1-2Hz" = c(1, 2),
                                   "2-4Hz" = c(2, 4)
                                 ),
                                 start_frame = 1,
                                 end_frame = NULL,
                                 param = 8,
                                 nrands = 1000,
                                 plot_result = FALSE,
                                 return_raw = FALSE,
                                 verbose = TRUE) {

  # Step 1: Validate inputs and filter data to the specific dyad/region
  prepared_data <- .resolve_and_filter_data(
    data = data, dyad_id = dyad_id, region = region, dyad_col = dyad_col,
    region_col = region_col, person_col = person_col, frame_col = frame_col,
    motion_col = motion_col, verbose = verbose
  )

  # Step 2: Prepare the two aligned and trimmed time series for the dyad
  ts_data <- .prepare_dyadic_timeseries(
    filtered_data = prepared_data$filtered_data,
    person_ids = person_ids, person_col = person_col,
    frame_col = frame_col, motion_col = motion_col, verbose = verbose,
    start_frame = start_frame, end_frame = end_frame
  )

  # Step 3: Compute wavelet coherence using the biwavelet package
  if (verbose) {
    message("Computing wavelet coherence (param = ", param, ", nrands = ", nrands, ")...")
  }
  wtc_result <- tryCatch({
    biwavelet::wtc(ts_data$t1, ts_data$t2, param = param, nrands = nrands)
  }, error = function(e) {
    stop("Error during wavelet coherence computation: ", e$message)
  })

  # Step 4: Summarise coherence across the specified frequency bands
  coherence_summary <- .summarise_coherence(
    wtc_result = wtc_result, freq_bands = freq_bands
  )

  # Add the dyad_id to the summary table to make it self-contained.
  # This places the dyad_id as the first column for clarity.
  if (nrow(coherence_summary) > 0) {
    coherence_summary <- data.frame(
      dyad_id = prepared_data$dyad_id,
      coherence_summary
    )
  }

  # Step 5: Plot the result if requested
  if (plot_result) {
    .plot_coherence(
      wtc_result = wtc_result, dyad_id = prepared_data$dyad_id, region = prepared_data$region,
      person_ids = ts_data$person_ids,
      start_frame = min(ts_data$analysis_frames),
      end_frame = max(ts_data$analysis_frames)
    )
  }

  # Step 6: Assemble and return the final results list
  analysis_info <- list(
    dyad_id = prepared_data$dyad_id,
    region = prepared_data$region,
    person_ids = ts_data$person_ids,
    dyad_col_used = prepared_data$dyad_col,
    region_col_used = region_col,
    person_col_used = person_col,
    frame_col_used = frame_col,
    motion_col_used = motion_col,
    start_frame = min(ts_data$analysis_frames),
    end_frame = max(ts_data$analysis_frames),
    total_frames_analyzed = length(ts_data$analysis_frames),
    total_common_frames_before_trim = length(ts_data$common_frames),
    param = param,
    nrands = nrands,
    analysis_date = format(Sys.time()),
    biwavelet_version = as.character(utils::packageVersion("biwavelet")),
    additional_ids = prepared_data$additional_ids
  )

  result <- list(coherence_summary = coherence_summary, analysis_info = analysis_info)
  if (return_raw) {
    result$wtc_object <- wtc_result
  }

  if (verbose) {
    message("Analysis completed successfully for ", prepared_data$dyad_id, " - ", prepared_data$region)
  }
  return(result)
}
