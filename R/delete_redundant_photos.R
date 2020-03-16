
#' Delete Redundant Photos in the Upload Folder
#'
#' After ingesting and renaming new photos, go ahead and delete photos that are duplicates
#' (i.e., already in the research drive).
#'
#' @param directory_to_drive folder for research drive
#' @param photos_to_upload folder for photos from SD cards
#'
#' @return
#' @export
#'
#' @examples
#' delete_redundant_photos(directory_to_drive, photos_to_upload)

delete_redundant_photos <- function(directory_to_drive, photos_to_upload) {

  upload_jpgs_to_delete <- fs::dir_ls(stringr::str_c(directory_to_drive, photos_to_upload),
                                      glob = '*.JPG',
                                      recurse = TRUE)

  upload_jpgs_to_delete %>%
    dplyr::as_tibble() %>%
    dplyr::pull(value) %>%
    fs::file_delete()

  # try to delete the directories themselves (sometimes prevented though)
  upload_jpgs_to_delete <- fs::dir_ls(stringr::str_c(directory_to_drive, photos_to_upload))

  upload_jpgs_to_delete %>%
    dplyr::as_tibble() %>%
    dplyr::pull(value) %>%
    fs::file_delete()

}
