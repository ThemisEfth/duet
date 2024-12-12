#' Create CSV from JSON files
#'
#' This function reads JSON files from the specified directory, processes the pose keypoints,
#' and saves the results into CSV files.
#'
#' @param input_path Path to the directory containing JSON files.
#' @param output_path Path to the directory where CSV files will be saved. Defaults to the input path.
#' @param model The model to use: "all", "body", "hands", or "face". Defaults to "all".
#' @param include_filename Boolean indicating whether to include the base filename in column names. Defaults to FALSE.
#' @param include_labels Boolean indicating whether to rename columns based on region labels. Defaults to FALSE.
#' @param frame_width Width of the frame. Defaults to 1920.
#' @param export_type Type of export: "individual" to export separate CSV files for each person, "dyad" to export both persons' data into a single CSV file. Defaults to "individual".
#' @export
#' @importFrom rjson fromJSON
#' @importFrom utils write.csv
#' @importFrom stats setNames
#' @importFrom tools file_path_sans_ext
#' @importFrom parallel detectCores
#' @examples
#' \dontrun{
#' # Assume you have a directory with JSON files
#' input_path <- "path/to/json/files"
#' output_path <- "path/to/save/csv/files"
#'
#' # Create CSV files using the function
#' op_create_csv(
#'   input_path = input_path,
#'   output_path = output_path,
#'   model = "all",
#'   include_filename = TRUE,
#'   include_labels = TRUE,
#'   frame_width = 1920,
#'   export_type = "dyad"
#' )
#' }
op_create_csv <- function(input_path, output_path = input_path, model = "all", include_filename = FALSE, include_labels = FALSE, frame_width = 1920, export_type = "dyad") {
  if(missing(input_path)) stop('Argument "input_path" must be specified. Path to JSON (*.json) files, including trailing "/"', call. = FALSE)
  if(!file.exists(output_path)) dir.create(output_path)

  files <- list.files(path = input_path, full.names = TRUE, pattern = ".json")
  if(length(files) == 0) stop(paste("Couldn't find any JSON files in directory:", input_path), call. = FALSE)

  column_names_body <- c("region", "person", unlist(strsplit(sprintf("x%1$d-y%1$d-c%1$d", seq(0, 24)), "-")))
  column_names_hands <- c("region", "person", unlist(strsplit(sprintf("x%1$d-y%1$d-c%1$d", seq(0, 20)), "-")))
  column_names_face <- c("region", "person", unlist(strsplit(sprintf("x%1$d-y%1$d-c%1$d", seq(0, 69)), "-")))

  middle_of_screen <- frame_width / 2

  process_file <- function(f) {
    base_filename <- sub("_\\d+_keypoints$", "", tools::file_path_sans_ext(basename(f)))
    json_data <- rjson::fromJSON(file = f)
    if(length(json_data[["people"]]) < 2) stop(paste("File", f, "does not contain two people."), call. = FALSE)

    keypoints <- function(person, key) if(!is.null(person)) unlist(person[[key]]) else rep(-1, 75)
    person1 <- json_data[["people"]][[1]]
    person2 <- json_data[["people"]][[2]]
    person1_pose <- keypoints(person1, "pose_keypoints_2d")
    person2_pose <- keypoints(person2, "pose_keypoints_2d")

    person1_position <- mean(person1_pose[1:2])
    person2_position <- mean(person2_pose[1:2])

    if (person1_position <= person2_position) {
      right_person <- person2
      left_person <- person1
    } else {
      right_person <- person1
      left_person <- person2
    }

    append_data <- function(data_list, region, person, keypoints) {
      combined_key <- paste0(region, "_", person)
      person_value <- ifelse(grepl("left", person), "left", "right")
      data <- cbind(region = region, person = person_value, t(keypoints))
      data_list[[combined_key]] <- rbind(data_list[[combined_key]], data)
      return(data_list)
    }

    results <- list(base_filename = base_filename, data_list = list())
    models <- c("body", "hand_left", "hand_right", "face")
    keys <- c("pose_keypoints_2d", "hand_left_keypoints_2d", "hand_right_keypoints_2d", "face_keypoints_2d")

    for (i in seq_along(models)) {
      if (model == "all" || model == models[i]) {
        if (!is.null(left_person) && length(left_person[[keys[i]]]) > 0) {
          results$data_list <- append_data(results$data_list, models[i], "person_left", left_person[[keys[i]]])
          results$data_list <- append_data(results$data_list, models[i], "person_right", right_person[[keys[i]]])
        }
      }
    }

    return(results)
  }

  results <- lapply(files, process_file)

  combined_data_list <- list(
    body_person_left = data.frame(), body_person_right = data.frame(),
    hand_left_person_left = data.frame(), hand_right_person_left = data.frame(),
    hand_left_person_right = data.frame(), hand_right_person_right = data.frame(),
    face_person_left = data.frame(), face_person_right = data.frame()
  )

  for(result in results) {
    if (!is.null(result)) {
      for (key in names(result$data_list)) {
        combined_data_list[[key]] <- rbind(combined_data_list[[key]], result$data_list[[key]])
      }
      base_filename <- result$base_filename
    }
  }

  write_data <- function(data_list, region, person, column_names, base_filename, export_type) {
    if (export_type == "individual") {
      combined_region_person <- paste0(region, "_", person)
      if (!is.null(data_list[[combined_region_person]]) && nrow(data_list[[combined_region_person]]) > 0) {
        data <- data_list[[combined_region_person]]
        data <- setNames(data, column_names)
        data <- cbind(frame = 1:nrow(data), data)
        if (include_filename) data <- cbind(base_filename = base_filename, data)
        if (include_labels) data <- op_apply_keypoint_labels(data)
        write.csv(data, file = paste(output_path, "/", base_filename, "_", region, "_", person, ".csv", sep=""), row.names = FALSE)
      } else {
        message(paste("No data collected for", region, person))
      }
    } else if (export_type == "dyad") {
      combined_region_left <- paste0(region, "_person_left")
      combined_region_right <- paste0(region, "_person_right")
      if (!is.null(data_list[[combined_region_left]]) && nrow(data_list[[combined_region_left]]) > 0 &&
          !is.null(data_list[[combined_region_right]]) && nrow(data_list[[combined_region_right]]) > 0) {
        data_left <- data_list[[combined_region_left]]
        data_right <- data_list[[combined_region_right]]
        data_left <- cbind(frame = 1:nrow(data_left), setNames(data_left, column_names))
        data_right <- cbind(frame = 1:nrow(data_right), setNames(data_right, column_names))
        data <- rbind(data_left, data_right)
        if (include_filename) data <- cbind(base_filename = base_filename, data)
        if (include_labels) data <- op_apply_keypoint_labels(data)
        write.csv(data, file = paste(output_path, "/", base_filename, "_", region, "_dyad.csv", sep=""), row.names = FALSE)
      } else {
        message(paste("No data collected for dyad in", region))
      }
    }
  }

  for (region in c("body", "hand_left", "hand_right", "face")) {
    if (model == "all" || model == region) {
      column_names <- get(paste0("column_names_", region))
      if (export_type == "individual") {
        write_data(combined_data_list, region, "person_left", column_names, base_filename, export_type)
        write_data(combined_data_list, region, "person_right", column_names, base_filename, export_type)
      } else if (export_type == "dyad") {
        write_data(combined_data_list, region, "person_left", column_names, base_filename, export_type)
      }
    }
  }
}
