
#' Rename kobo kestrel photos by banding data
#'
#' @param kobo_kestrel_data
#'
#' @return
#' @export
#'
#' @examples
#' rename_kobo_photos(kobo_kestrel_data = '/Users/Jay/Desktop/kobo')
#'

rename_kobo_photos <- function(
  kobo_kestrel_data # temporary location of kobo banding xlsx and photo zip folder
) {

  # need this for furrr
  future::plan(future::multiprocess)


  # Find/unzip data ---------------------------------------------------------

  # find paths to excel and zip files downloaded from KoBo website (these should be together with this script in same folder)
  excel <- fs::dir_ls(path = kobo_kestrel_data, glob = '*.xlsx')
  zip <- fs::dir_ls(path = kobo_kestrel_data, glob = '*.zip')

  # unzip the photos
  unzip(zip, exdir = kobo_kestrel_data)

  # delete zip file now
  zip %>%
    fs::file_delete()


  # read in banding data and image paths and join -------------------------------------------

  dat <- readxl::read_excel(excel)

  # grab relevant columns (band number, transmitter ID if no band, and photo types)
  dat <- dat %>%
    dplyr::select(markerID, date, tailPhoto, frontPopsiclePhoto,
                  backPopsiclePhoto, frontWingPhoto, backWingPhoto, otherPhoto)

  # change format so photo columns become variable in column 'photo_type'
  dat <- dat %>%
    tidyr::gather(key = "photo_type", value = "fileName", 3:8) %>%
    dplyr::filter(!is.na(fileName)) %>% # get rid of NA's (missing or not taken photos)
    dplyr::arrange(markerID, photo_type) # organize by bird and photo type

  # create tibble of photos to be named
  rawPhotos <- fs::dir_ls(path = kobo_kestrel_data, glob = '*.jpg', recurse = TRUE) %>%
    dplyr::as_tibble() %>%
    dplyr::rename(path = value) %>%
    dplyr::mutate(fileName = basename(path))

  # join photos with banding data
  dat <- dat %>%
    dplyr::left_join(., rawPhotos) %>%
    dplyr::select(-fileName)


  # now loop through each bird and type of photo -------------------------

  # adapted from function for naming images and reading links: https://stackoverflow.com/questions/54262620/downloading-images-using-curl-library-in-a-loop-over-data-frame
  rename_photos <- purrr::as_mapper(~fs::file_move(path = ..4,
                                                   new_path = stringr::str_c(kobo_kestrel_data, '/', 'renamed_images', '/', ..1, "_", ..2, "_", ..3, ".jpg")))

  # create a folder to put renamed images in
  fs::dir_create(stringr::str_c(kobo_kestrel_data, '/', 'renamed_images'))

  # rename jpgs and put in 'renamed_images' folder using p(f)urrr::pmap_chr
  dat %>%
    furrr::future_pmap_chr(rename_photos)

}
