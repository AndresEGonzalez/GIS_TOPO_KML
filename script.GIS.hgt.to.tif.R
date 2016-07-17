# SCRIPT CONVERT ELEVATION DATA .HGT TO RASTER .TIF
# USE BEFORE THE -> script.contours_for_google_earth.R


### DOWNLOAD DATA DEM
# DEM digital elevation model in hgt-format downloaded from:
# http://viewfinderpanoramas.org/Coverage%20map%20viewfinderpanoramas_org3.htm# as done tutorial here:
# http://thebiobucket.blogspot.co.at/2013/06/use-r-to-bulk-download-digital.html
# splitted into tiles for easier handling and saved to tiff as done here:
# http://thebiobucket.blogspot.co.at/2014/03/use-gdal-from-r-console-to-split-raster.html


#### Convert *.hgt files into several raster *.tif
# from:  http://thebiobucket.blogspot.co.at/2014/03/use-gdal-from-r-console-to-split-raster.html
## get filesnames (assuming the datasets were downloaded already.
## please see http://thebiobucket.blogspot.co.at/2013/06/use-r-to-bulk-download-digital.html
## on how to download high-resolution DEMs)
## function for single file processing mind to replace the PATH to gdalinfo.exe!
## For linux download here: https://launchpad.net/ubuntu/saucy/amd64/libgdal-dev/1.9.0-3.1ubuntu1
## linux require "libgdal1" library

rm(list=ls())
setwd("~/Documents/1_WORKING/DATA/GIS_Database/HGT")
(files <- dir(pattern = ".hgt"))
## s = division applied to each side of raster, i.e. s = 2 gives 4 tiles, 3 gives 9, etc.
split_raster <- function(file, s = 2) {
  
  filename <- gsub(".hgt", "", file)
#   gdalinfo_str <- paste0("\"C:/OSGeo4W64/bin/gdalinfo.exe\" ", file)# Windows
  gdalinfo_str <- paste0("\"/usr/bin/gdalinfo\" ", file)# Linux: gdalinfo lists information about a raster dataset

  # pick size of each side
  x <- as.numeric(gsub("[^0-9]", "", unlist(strsplit(system(gdalinfo_str, intern = T)[3], ", "))))[1]
  y <- as.numeric(gsub("[^0-9]", "", unlist(strsplit(system(gdalinfo_str, intern = T)[3], ", "))))[2]
  
  # t is nr. of iterations per side
  t <- s - 1
  for (i in 0:t) {
    for (j in 0:t) {
      # [-srcwin xoff yoff xsize ysize] src_dataset dst_dataset
      srcwin_str <- paste("-srcwin ", i * x/s, j * y/s, x/s, y/s)
      gdal_str <- paste0("\"/usr/bin/gdal_translate\" ", srcwin_str, " ", "\"", file, "\" ", "\"", filename, "_", i, "_", j, ".tif\"")
      system(gdal_str)
    }
  }
}

## process all files and save to same directory
mapply(split_raster, files, 2) 
