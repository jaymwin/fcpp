
#' Establish directories for upload, raw, and renamed folders
#'
#' Create paths to folders where 1) photos that need to be uploaded are stored,
#' 2) raw photos are stored, and 3) renamed photos are stored.
#'
#' @param year_for_photos breeding season year (e.g., 2020)
#'
#' @return
#' @export
#' @examples
#' setup_directories(year_for_photos)

setup_directories <- function(year_for_photos) {

  photos_to_upload <<- stringr::str_c(year_for_photos, '_nest_box_photos/photos_to_upload')
  raw_photo_location <<- stringr::str_c(year_for_photos, '_nest_box_photos/', year_for_photos, '_nest_box_photos_raw')
  renamed_photo_location <<- stringr::str_c(year_for_photos, '_nest_box_photos/', year_for_photos, '_nest_box_photos_renamed')

}
