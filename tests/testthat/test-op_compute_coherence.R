library(testthat)

# Define a set of frequency bands that are valid for the short,
# low-sampling-rate time series used in these tests.
# With a time step of 1, the Nyquist frequency is 0.5 Hz.
safe_test_bands <- list(
  "band_1" = c(0.1, 0.2), # Corresponds to periods of 5-10 frames
  "band_2" = c(0.2, 0.45) # Corresponds to periods of ~2.2-5 frames
)

test_that("op_compute_coherence works with basic input", {
  # Create sample data
  set.seed(123)
  sample_data <- data.frame(
    frame = rep(1:100, 2),
    base_filename = "D01",
    region = "body",
    person = rep(c("P1", "P2"), each = 100),
    motion_energy = c(
      rnorm(100, mean = 0.5, sd = 0.2),
      rnorm(100, mean = 0.6, sd = 0.3)
    )
  )

  # Test basic functionality using valid frequency bands for this data
  result <- op_compute_coherence(
    data = sample_data,
    freq_bands = safe_test_bands, # Use safe bands
    verbose = FALSE
  )

  # Check structure of output
  expect_type(result, "list")
  expect_named(result, c("coherence_summary", "analysis_info"))
  expect_equal(nrow(result$coherence_summary), 2) # Check for 2 bands

  # Check coherence_summary structure
  expect_s3_class(result$coherence_summary, "data.frame")
  expected_cols <- c(
    "dyad_id",
    "frequency_band",
    "hz_low",
    "hz_high",
    "coherence",
    "coherence_sd",
    "coherence_median",
    "n_valid_values"
  )
  expect_named(result$coherence_summary, expected_cols)

  # Check analysis_info structure
  expect_type(result$analysis_info, "list")
  expect_equal(result$analysis_info$dyad_id, "D01")
  expect_equal(result$analysis_info$region, "body")
  expect_equal(result$analysis_info$person_ids, c("P1", "P2"))
  expect_equal(result$analysis_info$total_frames_analyzed, 100)
})

test_that("op_compute_coherence handles custom frequency bands", {
  set.seed(123)
  sample_data <- data.frame(
    frame = rep(1:50, 2),
    dyad_id = "test_dyad",
    region = "head",
    person = rep(c("A", "B"), each = 50),
    motion_energy = c(
      sin(1:50 * 0.1) + rnorm(50, sd = 0.1),
      cos(1:50 * 0.1) + rnorm(50, sd = 0.1)
    )
  )

  # Define custom bands that are valid for the data (N=50, max freq = 0.5 Hz)
  custom_bands <- list(
    "low" = c(0.1, 0.25),
    "mid" = c(0.25, 0.45)
  )

  result <- op_compute_coherence(
    data = sample_data,
    freq_bands = custom_bands,
    verbose = FALSE
  )

  expect_equal(nrow(result$coherence_summary), 2)
  expect_equal(result$coherence_summary$frequency_band, c("low", "mid"))
  expect_equal(result$coherence_summary$hz_low, c(0.1, 0.25))
  expect_equal(result$coherence_summary$hz_high, c(0.25, 0.45))
})

test_that("op_compute_coherence handles explicit parameters", {
  set.seed(123)
  sample_data <- data.frame(
    frame = rep(1:80, 2),
    session_id = "S1",
    region = "body",
    person = rep(c("P1", "P2"), each = 80),
    motion_energy = rnorm(160)
  )

  result <- op_compute_coherence(
    data = sample_data,
    dyad_id = "S1",
    region = "body",
    person_ids = c("P1", "P2"),
    dyad_col = "session_id",
    start_frame = 10,
    end_frame = 70,
    param = 6,
    nrands = 100,
    freq_bands = safe_test_bands, # Use safe bands
    verbose = FALSE
  )

  expect_equal(result$analysis_info$dyad_id, "S1")
  expect_equal(result$analysis_info$region, "body")
  expect_equal(result$analysis_info$total_frames_analyzed, 61)
  expect_equal(result$analysis_info$param, 6)
  expect_equal(nrow(result$coherence_summary), 2)
})

test_that("op_compute_coherence returns raw wtc object when requested", {
  set.seed(123)
  sample_data <- data.frame(
    frame = rep(1:50, 2),
    base_filename = "D01",
    region = "body",
    person = rep(c("P1", "P2"), each = 50),
    motion_energy = rnorm(100)
  )

  result <- op_compute_coherence(
    data = sample_data,
    return_raw = TRUE,
    freq_bands = safe_test_bands, # Use safe bands
    verbose = FALSE
  )

  expect_named(result, c("coherence_summary", "analysis_info", "wtc_object"))
  expect_s3_class(result$wtc_object, "biwavelet")
})

test_that("op_compute_coherence auto-detects dyad column", {
  set.seed(123)
  data1 <- data.frame(
    frame = rep(1:50, 2),
    base_filename = "dyad_01",
    region = "body",
    person = rep(c("P1", "P2"), each = 50),
    motion_energy = rnorm(100)
  )
  result1 <- op_compute_coherence(
    data1,
    freq_bands = safe_test_bands,
    verbose = FALSE
  )
  expect_equal(result1$analysis_info$dyad_col_used, "base_filename")

  data2 <- data.frame(
    frame = rep(1:50, 2),
    dyad_id = "dyad_02",
    region = "body",
    person = rep(c("P1", "P2"), each = 50),
    motion_energy = rnorm(100)
  )
  result2 <- op_compute_coherence(
    data2,
    freq_bands = safe_test_bands,
    verbose = FALSE
  )
  expect_equal(result2$analysis_info$dyad_col_used, "dyad_id")
})

# --- Error and Warning Tests (No changes needed for these) ---

test_that("op_compute_coherence handles missing columns error", {
  incomplete_data <- data.frame(
    frame = 1:50,
    dyad_id = "D01",
    person = rep(c("P1", "P2"), each = 25)
  )
  expect_error(
    op_compute_coherence(incomplete_data, verbose = FALSE),
    "The following required columns are missing"
  )
})

test_that("op_compute_coherence handles multiple dyads error", {
  multi_dyad_data <- data.frame(
    frame = rep(1:50, 4),
    base_filename = rep(c("D01", "D02"), each = 100),
    region = "body",
    person = rep(c("P1", "P2"), each = 50, times = 2),
    motion_energy = rnorm(200)
  )
  expect_error(
    op_compute_coherence(multi_dyad_data, verbose = FALSE),
    "dyad_id.*is missing and multiple dyads were found"
  )
})

test_that("op_compute_coherence handles multiple regions error", {
  multi_region_data <- data.frame(
    frame = rep(1:50, 4),
    base_filename = "D01",
    region = rep(c("body", "head"), each = 100),
    person = rep(c("P1", "P2"), each = 50, times = 2),
    motion_energy = rnorm(200)
  )
  expect_error(
    op_compute_coherence(multi_region_data, verbose = FALSE),
    "region.*is missing and multiple regions were found"
  )
})

test_that("op_compute_coherence handles non-existent dyad error", {
  sample_data <- data.frame(
    frame = rep(1:50, 2),
    base_filename = "D01",
    region = "body",
    person = rep(c("P1", "P2"), each = 50),
    motion_energy = rnorm(100)
  )
  expect_error(
    op_compute_coherence(sample_data, dyad_id = "D99", verbose = FALSE),
    "No data found for dyad_id 'D99'"
  )
})

test_that("op_compute_coherence handles insufficient persons error", {
  single_person_data <- data.frame(
    frame = 1:50,
    base_filename = "D01",
    region = "body",
    person = "P1",
    motion_energy = rnorm(50)
  )
  expect_error(
    op_compute_coherence(single_person_data, verbose = FALSE),
    "Exactly two person IDs are required"
  )
})

test_that("op_compute_coherence handles short time series warning", {
  short_data <- data.frame(
    frame = rep(1:20, 2),
    base_filename = "D01",
    region = "body",
    person = rep(c("P1", "P2"), each = 20),
    motion_energy = rnorm(40)
  )
  expect_warning(
    op_compute_coherence(short_data, verbose = FALSE),
    "Time series has fewer than 32 common frames"
  )
})

# --- Remaining Functional Tests (Updated) ---

test_that("op_compute_coherence handles frame filtering", {
  set.seed(123)
  sample_data <- data.frame(
    frame = rep(1:100, 2),
    base_filename = "D01",
    region = "body",
    person = rep(c("P1", "P2"), each = 100),
    motion_energy = rnorm(200)
  )

  result <- op_compute_coherence(
    data = sample_data,
    start_frame = 20,
    end_frame = 80,
    freq_bands = safe_test_bands, # Use safe bands
    verbose = FALSE
  )

  expect_equal(result$analysis_info$start_frame, 20)
  expect_equal(result$analysis_info$end_frame, 80)
  expect_equal(result$analysis_info$total_frames_analyzed, 61)
})

test_that("op_compute_coherence handles missing frames gracefully", {
  frames_p1 <- c(1:30, 40:70)
  frames_p2 <- c(5:45, 55:85)
  gapped_data <- data.frame(
    frame = c(frames_p1, frames_p2),
    base_filename = "D01",
    region = "body",
    person = c(rep("P1", length(frames_p1)), rep("P2", length(frames_p2))),
    motion_energy = rnorm(length(frames_p1) + length(frames_p2))
  )

  result <- op_compute_coherence(
    gapped_data,
    freq_bands = safe_test_bands,
    verbose = FALSE
  )
  common_frames <- intersect(frames_p1, frames_p2)
  expected_length <- max(common_frames) - min(common_frames) + 1

  expect_equal(result$analysis_info$total_frames_analyzed, expected_length)
})

test_that("op_compute_coherence validates person_ids parameter", {
  sample_data <- data.frame(
    frame = rep(1:50, 2),
    base_filename = "D01",
    region = "body",
    person = rep(c("Alice", "Bob"), each = 50),
    motion_energy = rnorm(100)
  )

  expect_error(
    op_compute_coherence(
      sample_data,
      person_ids = c("Charlie", "Dave"),
      verbose = FALSE
    ),
    "One or both specified person_ids not found"
  )

  result <- op_compute_coherence(
    sample_data,
    person_ids = c("Alice", "Bob"),
    freq_bands = safe_test_bands,
    verbose = FALSE
  )
  expect_equal(result$analysis_info$person_ids, c("Alice", "Bob"))
})

test_that("op_compute_coherence detects additional ID columns", {
  sample_data <- data.frame(
    frame = rep(1:50, 2),
    base_filename = "D01",
    region = "body",
    person = rep(c("P1", "P2"), each = 50),
    motion_energy = rnorm(100),
    session_id = "session_001",
    condition = "experimental",
    date = "2024-01-15"
  )

  result <- op_compute_coherence(
    sample_data,
    freq_bands = safe_test_bands,
    verbose = FALSE
  )
  expect_equal(result$analysis_info$additional_ids$session_id, "session_001")
  expect_equal(result$analysis_info$additional_ids$condition, "experimental")
  expect_equal(result$analysis_info$additional_ids$date, "2024-01-15")
})

test_that("op_compute_coherence coherence values are reasonable", {
  set.seed(123)
  time_vec <- 1:60
  signal_base <- sin(time_vec * 0.2) + 0.5 * sin(time_vec * 0.1)
  correlated_data <- data.frame(
    frame = rep(time_vec, 2),
    base_filename = "D01",
    region = "body",
    person = rep(c("P1", "P2"), each = 60),
    motion_energy = c(
      signal_base + rnorm(60, 0, 0.1),
      signal_base + rnorm(60, 0, 0.1)
    )
  )

  result <- op_compute_coherence(
    correlated_data,
    freq_bands = safe_test_bands,
    verbose = FALSE
  )

  expect_true(all(
    result$coherence_summary$coherence >= 0 &
      result$coherence_summary$coherence <= 1
  ))
  expect_true(any(result$coherence_summary$coherence > 0.1))
})

test_that("op_compute_coherence handles different column names", {
  custom_data <- data.frame(
    time_point = rep(1:50, 2),
    pair_identifier = "pair_01",
    body_region = "torso",
    participant = rep(c("A", "B"), each = 50),
    movement_energy = rnorm(100)
  )

  result <- op_compute_coherence(
    data = custom_data,
    dyad_col = "pair_identifier",
    region_col = "body_region",
    person_col = "participant",
    frame_col = "time_point",
    motion_col = "movement_energy",
    freq_bands = safe_test_bands,
    verbose = FALSE
  )

  expect_equal(result$analysis_info$dyad_id, "pair_01")
  expect_equal(result$analysis_info$region, "torso")
  expect_equal(result$analysis_info$person_ids, c("A", "B"))
})
