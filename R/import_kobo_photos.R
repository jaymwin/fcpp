
#' Import newly named kestrel banding photos to the research drive
#'
#' @param kobo_kestrel_data
#' @param directory_to_drive
#'
#' @return
#' @export
#'
#' @examples
#' import_kobo_photos(kobo_kestrel_data = '/Users/Jay/Desktop/kobo', directory_to_drive = 'Z:/HeathLab/American Kestrel projects/Full_Cycle_Phenology/Banding Photos/')


import_kobo_photos <- function(
  kobo_kestrel_data, # temporary location of kobo banding xlsx and photo zip folder
  directory_to_drive # research drive directory
) {

  # need for furrr functions
  future::plan(future::multiprocess)

  # find all of the photos you just renamed by banding info
  photos <- fs::dir_ls(path = stringr::str_c(kobo_kestrel_data, '/', 'renamed_images'), glob = '*.jpg')

  # create a tibble and new file paths (this is where photos will go on research drive)
  photos <- photos %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(
      banding_photos = stringr::str_c(directory_to_drive, basename(value))
    )

  # create function to move from current location (wherever you saved kobo data) to new location (the drive)
  move_photos <- purrr::as_mapper(~fs::file_copy(path = ..1,
                                                 new_path = ..2,
                                                 overwrite = FALSE)) # photos already in place not overwritten

  # now move
  photos %>%
    furrr::future_pmap_chr(move_photos)

}
