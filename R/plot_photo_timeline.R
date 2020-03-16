
#' Plot a Timeline of Renamed Nest Box Photos
#'
#' After all of the photos have been renamed, plot a timeline of all of the photos
#' taken by nest box ID. Photos will be shown in the plot with a '|' mark.
#'
#' @param directory_to_drive folder for research drive
#' @param renamed_photo_location folder where renamed photos are stored
#' @param ggplot_location where ggplot will go
#'
#' @return
#' @export
#'
#' @examples
#' plot_photo_timeline(directory_to_drive, renamed_photo_location)

plot_photo_timeline <- function(directory_to_drive, renamed_photo_location, ggplot_location) {

  renamed_photos <- fs::dir_ls(
    path = stringr::str_c(directory_to_drive, renamed_photo_location),
    recurse = TRUE,
    glob = '*JPG'
  ) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(
      value = as.character(value)
    )

  renamed_photos <- renamed_photos %>%
    dplyr::mutate(
      state = stringr::str_extract(value, "[A-Z]{2}"),
      box = stringr::str_extract(value, "[A-Z]{2}[0-9]{2}"), # need to change start/end for drive folder
      image_name = basename(value),
      datetime = lubridate::ymd_hms(stringr::str_sub(image_name, start = 7, end = 26))
    ) %>%
    dplyr::select(-value)

  renamed_photos %>%
    dplyr::filter(datetime > '2019-01-01' & datetime < '2019-08-30') %>%
    ggplot2::ggplot(., ggplot2::aes(datetime, box, color = state)) +
    ggplot2::geom_point(pch = '|', size = 1) +
    ggplot2::scale_color_viridis_d(option = 'magma') +
    ggplot2::facet_wrap(~state, scales = 'free_y') +
    ggplot2::theme_dark() +
    ggplot2::theme(legend.position = 'none')
  ggplot2::ggsave(stringr::str_c(ggplot_location, '/', 'camera_timeline_', Sys.Date(), '.png'), width = 8.5, height = 7, units = 'in')

}
