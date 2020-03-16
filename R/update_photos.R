
#' Ingest New Photos and Rename in the Research Drive
#'
#' Determine which photos are new, copy them to the raw folder, and then
#' rename them in the research drive. Code will not run if the proper folder
#' hierarchy is not in place.
#'
#' @param directory_to_drive folder for the research drive
#' @param year_for_photos breeding season year
#' @param photos_to_upload folder where SD card photos are added
#' @param raw_photo_location folder where new photos will be copied
#' @param renamed_photo_location folder where photos are renamed for annotation in ViXeN
#'
#' @return
#' @export
#'
#' @examples
#' update_photos(directory_to_drive, year_for_photos, photos_to_upload, raw_photo_location, renamed_photo_location)

update_photos <- function(
  directory_to_drive,
  year_for_photos,
  photos_to_upload,
  raw_photo_location,
  renamed_photo_location
) {

  # for parallel processing of p(f)urrr functions
  future::plan(future::multiprocess)

  # set some of the directories up ------------------------------------------

  # designate folder that new photos are uploaded to on the research drive
  upload_folder <- stringr::str_c(directory_to_drive, photos_to_upload)

  # designate folder where the 'raw' photos are; we'll add new photos only to this folder
  raw_folder <- stringr::str_c(directory_to_drive, raw_photo_location)

  # designate folder where the 'renamed' photos are; we'll add new photos only to this folder
  renamed_folder <- stringr::str_c(directory_to_drive, renamed_photo_location)

  # get rid of any TRANSMIT folders (these folders/images aren't needed)
  fs::dir_ls(upload_folder, recurse = TRUE, type = 'directory', glob = '*TRANSMIT') %>%
    fs::file_delete()

  # move nested image files out of subfolders into each parent folder (i.e., folder for a nest box)
  upload_list <- fs::dir_ls(upload_folder, recurse = TRUE, glob = '*.JPG') %>%
    dplyr::as_tibble() %>%
    dplyr::rename(full_path = value) %>%
    dplyr::mutate(
      state_folder = stringr::str_extract(full_path, "[A-Z]{2}"),
      box_folder = stringr::str_extract(full_path, "[A-Z]{2}[0-9]{2}"), # need to change start/end for drive folder
      image_name = basename(full_path),
      spy_subfolder = stringr::str_extract(full_path, "[0-9]{3}[DSCIM]{5}"), # spypoint images are within DSCIM
      reconyx_subfolder = stringr::str_extract(full_path, "[0-9]{3}[RECNX]{5}"), # reconyx have RECNX folders
      reconyx = stringr::str_detect(full_path, "RECNX"), # is this a reconyx camera
      spypoint = stringr::str_detect(full_path, "DSCIM"), # is this a spypoint camera?
      subfolders = dplyr::case_when(
        reconyx == TRUE | spypoint == TRUE ~ TRUE,
        TRUE ~ FALSE
      ), # is the subfolder structure correct?
      DCIM = stringr::str_detect(full_path, "DCIM"), # is there a DCIM folder that both spypoint and reconyx are within?
      state_present = stringr::str_detect(full_path, "/[A-Z]{2}/") # is there a state folder (like AK)?
    )

  # stop everything if photos are not properly structure in the drive
  no_state <- upload_list %>%
    dplyr::filter(state_present == FALSE)

  upload_list_subfolders <- upload_list %>%
    dplyr::filter(subfolders == FALSE)

  upload_list_DCIM <- upload_list %>%
    dplyr::filter(DCIM == FALSE)

  upload_list_box_names <- upload_list %>%
    dplyr::filter(is.na(box_folder))

  stopifnot(dim(no_state)[1] < 1) # break if no state folder (just box folders)
  stopifnot(dim(upload_list_subfolders)[1] < 1) # break if subfolders wrong
  stopifnot(dim(upload_list_DCIM)[1] < 1) # break if no DCIM folder
  stopifnot(dim(upload_list_box_names)[1] < 1) # break if box name is labeled incorrectly

  # now move photos out of subfolders into box folders ----------------------

  # this needs to be done in slightly different ways for spypoint and reconyx images so separate
  upload_list_spypoint <- upload_list %>%
    dplyr::filter(spypoint == TRUE)

  upload_list_reconyx <- upload_list %>%
    dplyr::filter(reconyx == TRUE)

  # function to move spypoint out of subfolders
  move_out_of_subfolders_spypoint <- purrr::as_mapper(~fs::file_move(path = ..1,
                                                                     new_path = stringr::str_c(upload_folder, '/', ..2, '/', ..3, '/', Sys.Date(), '_', ..4)))

  # function to move reconyx out of subfolders (must have subfolder name - 100RECNX for example - file names repeat)
  move_out_of_subfolders_reconyx <- purrr::as_mapper(~fs::file_move(path = ..1,
                                                                    new_path = stringr::str_c(upload_folder, '/', ..2, '/', ..3, '/', Sys.Date(), '_', ..6, '_', ..4)))

  # this moves them out of spypoint subfolders
  upload_list_spypoint %>%
    furrr::future_pmap_chr(move_out_of_subfolders_spypoint, .progress = TRUE)

  # this moves them out of reconyx subfolders
  upload_list_reconyx %>%
    furrr::future_pmap_chr(move_out_of_subfolders_reconyx, .progress = TRUE)

  # now, get rid of any DCIM folders (these folders/images are now empty and not needed anymore)
  fs::dir_ls(
    upload_folder,
    recurse = TRUE,
    type = 'directory',
    glob = '*DCIM'
  ) %>%
    fs::file_delete()

  # add new photos to raw folder by seeing whether metadata matches  --------

  # extract metadata for each image into a table (uploads)
  upload_photo_list <- fs::dir_ls(upload_folder, recurse = TRUE, glob = '*.JPG') %>%
    dplyr::as_tibble() %>%
    dplyr::transmute(
      path = as.character(value)
    ) %>%
    dplyr::pull(path)

  upload_photo_metadata <- exifr::read_exif(
    upload_photo_list,
    tags = c("FileName", "Directory", "DateTimeOriginal"
    )
  ) %>%
    dplyr::mutate(
      box_id = stringr::str_extract(Directory, "[A-Z]{2}[0-9]{2}"),
      datetime_photo = lubridate::ymd_hms(DateTimeOriginal),
      state = stringr::str_sub(box_id, start = 1, end = 2)
    ) %>%
    dplyr::select(
      state,
      box_id,
      datetime_photo,
      directory = Directory,
      file_name = FileName
    )

  focal_boxes <- upload_photo_metadata %>%
    dplyr::distinct(box_id) %>%
    dplyr::pull(box_id)

  # extract metadata for each image into a table (raw)
  raw_photo_list <- fs::dir_ls(raw_folder, recurse = TRUE, glob = '*.JPG') %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(
      box_id = stringr::str_extract(value, "[A-Z]{2}[0-9]{2}")
    ) %>%
    dplyr::filter(box_id %in% c(focal_boxes)) %>%
    dplyr::transmute(
      path = as.character(value)
    ) %>%
    dplyr::pull(path)

  # this is relevant if there are no photos yet in the raw folder;
  # if there are, copy over only new photos
  # otherwise, copy over all the photos to start with
  if(length(raw_photo_list) > 0) {

    raw_photo_metadata <- exifr::read_exif(
      raw_photo_list,
      tags = c("FileName", "Directory", "DateTimeOriginal")
    ) %>%
      dplyr::mutate(
        box_id = stringr::str_extract(Directory, "[A-Z]{2}[0-9]{2}"),
        datetime_photo = lubridate::ymd_hms(DateTimeOriginal),
        state = stringr::str_sub(box_id, start = 1, end = 2)
      ) %>%
      dplyr::select(
        state,
        box_id,
        datetime_photo,
        directory = Directory,
        file_name = FileName
      )

    # use anti_join to figure out which images are actually new in the upload folder
    photos_to_move <- upload_photo_metadata %>%
      dplyr::anti_join(., raw_photo_metadata, by = c('box_id', 'datetime_photo')) # filter out photos with timestamps that are not already in the raw photo folder

  } else {

    photos_to_move <- upload_photo_metadata

  }

  # now make new folders if necessary and move photos -----------------------

  # create new folders if necessary
  folders <- photos_to_move %>%
    dplyr::select(state, box_id) %>%
    dplyr::distinct()

  # functions to create state and box folders
  create_state_folders <- purrr::as_mapper(~dir.create(stringr::str_c(raw_folder, '/', ..1))) # dir_create doesn't work for some reason
  create_box_folders <- purrr::as_mapper(~dir.create(stringr::str_c(raw_folder, '/', ..1, '/', ..2))) # dir_create doesn't work for some reason

  # create them (ignore some errors if folders already in place)
  folders %>%
    dplyr::distinct(state) %>%
    furrr::future_pmap_chr(create_state_folders)

  folders %>%
    furrr::future_pmap_chr(create_box_folders)


  # move new photos into raw folder -----------------------------------------

  # create a function to add the new photos
  add_new_photos <- purrr::as_mapper(~fs::file_copy(path = stringr::str_c(..4, '/', ..5), # original photo path
                                                    new_path = stringr::str_c(raw_folder, '/', ..1, '/', ..2, '/', ..5)))

  # now actually move them
  photos_to_move %>%
    furrr::future_pmap_chr(add_new_photos, .progress = TRUE)

  # same photos need to be renamed based on their metadata
  photos_to_rename <- photos_to_move %>%
    dplyr::mutate(
      new_path = stringr::str_c(raw_folder, '/', state, '/', box_id, '/', file_name)
    ) %>%
    dplyr::pull(new_path)

  # grab metadata here
  photos_to_rename <- exifr::read_exif(
    photos_to_rename,
    tags = c("FileName", "Directory", "DateTimeOriginal")) %>%
    dplyr::mutate(
      box_id = stringr::str_extract(Directory, "[A-Z]{2}[0-9]{2}"),
      datetime_photo = lubridate::ymd_hms(DateTimeOriginal),
      state = stringr::str_sub(box_id, start = 1, end = 2)
    ) %>%
    dplyr::select(
      state,
      box_id,
      datetime_photo,
      directory = Directory,
      file_name = FileName
    )

  # change datetime format for naming
  photos_to_rename <- photos_to_rename %>%
    dplyr::mutate(
      datetime_photo = as.character(datetime_photo),
      datetime_photo = stringr::str_replace_all(datetime_photo, ':', '-'),
      datetime_photo = stringr::str_replace_all(datetime_photo, ' ', '__')
    )

  # create new folders if necessary
  folders <- photos_to_rename %>%
    dplyr::select(state, box_id) %>%
    dplyr::distinct()

  # functions to create state and box folders
  create_state_folders <- purrr::as_mapper(~dir.create(stringr::str_c(renamed_folder, '/', ..1))) # dir_create doesn't work for some reason
  create_box_folders <- purrr::as_mapper(~dir.create(stringr::str_c(renamed_folder, '/', ..1, '/', ..2))) # dir_create doesn't work for some reason

  # create them (ignore some errors if folders already in place)
  folders %>%
    dplyr::distinct(state) %>%
    furrr::future_pmap_chr(create_state_folders)

  folders %>%
    furrr::future_pmap_chr(create_box_folders)

  # function to move and rename photos
  rename_photos <- purrr::as_mapper(~fs::file_copy(path = stringr::str_c(..4, '/', ..5),
                                                   new_path = stringr::str_c(renamed_folder, '/', ..1, '/', ..2, '/', ..2, '__', ..3, '(1).JPG')))

  # renames photos like this: AK01__2019-05-23__09-00-26(1).JPG

  # do the renaming
  photos_to_rename %>%
    furrr::future_pmap_chr(rename_photos)

}
