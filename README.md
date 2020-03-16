
<!-- README.md is generated from README.Rmd. Please edit that file -->

fcpp
=============

This package contains functions for the Full Cycle Phenology Project. So far, functions are used for 1) repeatedly downloading SD card
images (from trail cameras inside American kestrel nest boxes), determining which photos are actually new to the research drive, copying
new photos into the drive, and renaming them by nest box ID and datetime; and 2) renaming banding photos from Kobo data form and importing them to the research drive.

Installing fcpp
------------------------

The fcpp package requires you to have R and RStudio installed.
After that, it’s simple to install and load the fcpp package.

    # If devtools package is not installed
    install.packages("devtools", dependencies = TRUE)

    # Now install and load fcpp
    devtools::install_github("jaymwin/fcpp")
    library(fcpp)

Using fcpp (for nest box photos)
-------------------

1. Add SD card images to the research drive with the proper folder hierarchy

Copy the 'DCIM' folder from each SD card into its respective nest box folder in the 'photos_to_upload' folder. Directories must be organized this way or the code will stop (for Spypoint and Reconyx cameras):

`photos_to_upload/AK/AK01/DCIM/100DSCIM/PICT0001.JPG`

`photos_to_upload/NM/NM18/DCIM/100RECNX/IMG_0001.JPG`

2.  Establish research drive directories

``` r
# if using a Mac this is the research drive path
macbook <- '/Volumes/Research/HeathLab/American Kestrel projects/Full_Cycle_Phenology/'

# for windows, this is the path
windows <- 'Z:/HeathLab/American Kestrel projects/Full_Cycle_Phenology/'

year_for_photos <- 2019 # year of nest box monitoring
directory_to_drive <- macbook # this is the folder that contains upload/raw/renamed folders
ggplot_location <- '/Users/Jay/Desktop' # path where you want to export the timeline plot

# this will create file paths to photo folders
setup_directories(year_for_photos)
```

3.  Ingest photos, determine which ones are new to the research drive,
    and rename them

``` r
# this will break if folder hierarchy is not correct
update_photos(
  directory_to_drive,
  year_for_photos,
  photos_to_upload,
  raw_photo_location,
  renamed_photo_location
)
```

4.  Plot a timeline of all the current renamed photos in the drive

``` r
plot_photo_timeline(directory_to_drive, renamed_photo_location, ggplot_location)  
```

5.  Finally, if everything worked then delete the photos that were
    already in the drive

``` r
delete_redundant_photos(directory_to_drive, photos_to_upload)
```

Using fcpp (for Kobo banding photos)
-------------------

1. Go to Kobo website and 1) download most recent xlsx file for the banding data, and 2) zip folder of banding images. Save them together in a temporary folder (desktop is fine).

2. Load 'fcpp' package, and rename all of the photos you just downloaded (something like this): 

`rename_kobo_photos(kobo_kestrel_data = '/Users/Jay/Desktop/kobo')`

3. Now import these renamed images to the research drive (this will only copy in new images to): 

`import_kobo_photos(
kobo_kestrel_data = '/Users/Jay/Desktop/kobo', 
directory_to_drive = 'Z:/HeathLab/American Kestrel projects/Full_Cycle_Phenology/Banding Photos/'
)`

4. Delete whatever is left on your desktop or wherever you saved the Kobo data.
