#' Rename Columns Based on Region
#'
#' This function renames columns of a dataframe based on the specified region.
#'
#' @param df Dataframe with columns to be renamed.
#' @return Dataframe with renamed columns.
#' @importFrom stringr str_replace_all
#' @export
#' @examples
#' # Example dataframe
#' df <- data.frame(
#'   region = rep(c("body", "hand_left", "hand_right", "face"), each = 3),
#'   x0 = rnorm(12), y0 = rnorm(12), c0 = rnorm(12),
#'   x1 = rnorm(12), y1 = rnorm(12), c1 = rnorm(12)
#' )
#'
#' # Apply keypoint labels
#' df_renamed <- op_apply_keypoint_labels(df)
#' print(df_renamed)
op_apply_keypoint_labels <- function(df) {
  # Define label sets for different regions
  label_sets <- list(
    body = c(
      "0" = "Nose", "1" = "Neck", "2" = "RShoulder", "3" = "RElbow", "4" = "RWrist",
      "5" = "LShoulder", "6" = "LElbow", "7" = "LWrist", "8" = "MidHip", "9" = "RHip",
      "10" = "RKnee", "11" = "RAnkle", "12" = "LHip", "13" = "LKnee", "14" = "LAnkle",
      "15" = "REye", "16" = "LEye", "17" = "REar", "18" = "LEar", "19" = "LBigToe",
      "20" = "LSmallToe", "21" = "LHeel", "22" = "RBigToe", "23" = "RSmallToe", "24" = "RHeel"
    ),
    hand_left = c(
      "0" = "LWrist", "1" = "LThumb1CMC", "2" = "LThumb2Knuckles", "3" = "LThumb3IP", "4" = "LThumb4FingerTip",
      "5" = "LIndex1Knuckles", "6" = "LIndex2PIP", "7" = "LIndex3DIP", "8" = "LIndex4FingerTip",
      "9" = "LMiddle1Knuckles", "10" = "LMiddle2PIP", "11" = "LMiddle3DIP", "12" = "LMiddle4FingerTip",
      "13" = "LRing1Knuckles", "14" = "LRing2PIP", "15" = "LRing3DIP", "16" = "LRing4FingerTip",
      "17" = "LPinky1Knuckles", "18" = "LPinky2PIP", "19" = "LPinky3DIP", "20" = "LPinky4FingerTip"
    ),
    hand_right = c(
      "0" = "RWrist", "1" = "RThumb1CMC", "2" = "RThumb2Knuckles", "3" = "RThumb3IP", "4" = "RThumb4FingerTip",
      "5" = "RIndex1Knuckles", "6" = "RIndex2PIP", "7" = "RIndex3DIP", "8" = "RIndex4FingerTip",
      "9" = "RMiddle1Knuckles", "10" = "RMiddle2PIP", "11" = "RMiddle3DIP", "12" = "RMiddle4FingerTip",
      "13" = "RRing1Knuckles", "14" = "RRing2PIP", "15" = "RRing3DIP", "16" = "RRing4FingerTip",
      "17" = "RPinky1Knuckles", "18" = "RPinky2PIP", "19" = "RPinky3DIP", "20" = "RPinky4FingerTip"
    ),
    face = c(
      "0" = "FaceContour0", "1" = "FaceContour1", "2" = "FaceContour2", "3" = "FaceContour3", "4" = "FaceContour4",
      "5" = "FaceContour5", "6" = "FaceContour6", "7" = "FaceContour7", "8" = "FaceContour8", "9" = "FaceContour9",
      "10" = "FaceContour10", "11" = "FaceContour11", "12" = "FaceContour12", "13" = "FaceContour13", "14" = "FaceContour14",
      "15" = "FaceContour15", "16" = "RightEyebrow0", "17" = "RightEyebrow1", "18" = "RightEyebrow2", "19" = "RightEyebrow3",
      "20" = "RightEyebrow4", "21" = "LeftEyebrow0", "22" = "LeftEyebrow1", "23" = "LeftEyebrow2",
      "24" = "LeftEyebrow3", "25" = "LeftEyebrow4", "26" = "NoseUpper0", "27" = "NoseUpper1", "28" = "NoseUpper2",
      "29" = "NoseUpper3", "30" = "NoseLower0", "31" = "NoseLower1", "32" = "NoseLower2", "33" = "NoseLower3",
      "34" = "NoseLower4", "35" = "RightEye0", "36" = "RightEye1", "37" = "RightEye2", "38" = "RightEye3",
      "39" = "RightEye4", "40" = "RightEye5", "41" = "LeftEye0", "42" = "LeftEye1", "43" = "LeftEye2",
      "44" = "LeftEye3", "45" = "LeftEye4", "46" = "LeftEye5", "47" = "MouthOuter0", "48" = "MouthOuter1",
      "49" = "MouthOuter2", "50" = "MouthOuter3", "51" = "MouthOuter4", "52" = "MouthOuter5", "53" = "MouthOuter6",
      "54" = "MouthOuter7", "55" = "MouthOuter8", "56" = "MouthOuter9", "57" = "MouthOuter10", "58" = "MouthOuter11",
      "59" = "MouthOuter12", "60" = "MouthOuter13", "61" = "MouthOuter14", "62" = "MouthOuter15", "63" = "MouthInner0",
      "64" = "MouthInner1", "65" = "MouthInner2", "66" = "MouthInner3", "67" = "MouthInner4", "68" = "MouthInner5",
      "69" = "MouthInner6", "70" = "MouthInner7"
    )
  )

  # Iterate over each region in the dataframe
  for (region in unique(df$region)) {
    label_set <- label_sets[[region]]
    if (is.null(label_set)) next

    cols_to_rename <- grepl("^x[0-9]+$|^y[0-9]+$|^c[0-9]+$", colnames(df))

    colnames(df)[cols_to_rename] <- sapply(colnames(df)[cols_to_rename], function(col_name) {
      index <- gsub("^[xyc]", "", col_name)
      prefix <- substr(col_name, 1, 1)
      if (index %in% names(label_set)) {
        paste0(prefix, label_set[index])
      } else {
        col_name
      }
    })
  }

  return(df)
}
