# SCRIPT CREATE GEARTH KML FILES WITH CONTOURS LINES (.tif RASTER)
# USE AFTER THE -> script.GIS.hgt.to.tif.R

rm(list=ls())


# sudo apt-get install libgdal1-dev libproj-dev
# http://stackoverflow.com/questions/30424608/r-error-in-fetchkey-lazy-load-database
# http://stackoverflow.com/questions/15248815/rgdal-package-installation?lq=1
# http://stackoverflow.com/questions/30320583/unable-to-install-rgdal-in-ubuntu-14-04-undefined-to-reference-to-pj-ctx-fclos
# http://stackoverflow.com/questions/7852679/trouble-loading-rgdal-in-rstudio?rq=1
install.packages("rgdal", dependencies=T)
install.packages("raster", dependencies=T)
install.packages("maptools", dependencies=T)
install.packages("rgeos", dependencies=T)
install.packages("RCurl", dependencies=T)
install.packages("magrittr")
# remove.packages("rgdal")-
# install.packages("gdalUtils", dependencies=T)

library(rgdal)
library(raster)
library(maptools)
library(rgeos)
library(RCurl)#loading required package: bitops
library(plyr)

# dem in hgt-format downloaded from http://www.viewfinderpanoramas.org/dem3.html#alps
# as done here:
# http://thebiobucket.blogspot.co.at/2013/06/use-r-to-bulk-download-digital.html
# splitted into tiles for easier handling and saved to tiff as done here:
# http://thebiobucket.blogspot.co.at/2014/03/use-gdal-from-r-console-to-split-raster.html



setwd("~/Documents/1_WORKING/DATA/GIS_Database/DEM_2/SELECT_RASTER")
(filenames <- gsub(".tif", "", dir(pattern = ".tif")))

## funtion make_kml_contours
## arguments
## step: altitude in between contours, starting at 0 m
## simplify: 1-0, 1 is no generalization, 0 is straight line
## ftp: optional ftp uload

make_kml_contours <- function(filename, step = 25, simplify = 0.001, ftp = F)
  
{
  ## coerce into SpatialGridDataFrame
  dem <- readGDAL(paste0("~/Documents/1_WORKING/DATA/GIS_Database/DEM_2/SELECT_RASTER/", filename, ".tif"))
  # dem$band1[is.na(dem$band1)]<-0 #AGL
  ## make image object for contourLines function
  im <- as.image.SpatialGridDataFrame(dem)

  # check: summary(im$z)
  cl <- contourLines(im, levels = seq(0, max(im$z), step))#cero
  
  ## back convert to SpatialLinesDataFrame
  SLDF <- ContourLines2SLDF(cl)
  proj4string(SLDF) <- CRS("+proj=longlat +datum=WGS84")
  
  ## simplify
  simplSLDF <- gSimplify(SLDF, tol = simplify)

  ## view results
#    image(dem, col = gray.colors(20))
   # plot(simplSLDF, add = T)
  
  ## convert simplified SLDF to KML (btw, that's how to extract IDs unlist(lapply(slot(simplSLDF, 'lines'), function(x) slot(x, 'ID'))) )
  out <- sapply(slot(simplSLDF, "lines"), function(x) {
    # get meter level, by picking from sequence by ID: ID = 1 -> 1*step m, ID = 2, 2*step m, etc.
    # m <- seq(min(im$z), max(im$z), step)[as.numeric(gsub("C_", "", slot(x, "ID")))]#m <- seq(0, max(im$z), step)[as.numeric(gsub("C_", "", slot(x, "ID")))]
    # Round minimal isoline to 100' (or 194 -> 200)
    m <- seq(round_any(min(im$z), 100, f = ceiling), max(im$z), step)[as.numeric(gsub("C_", "", slot(x, "ID")))]
    # make thicker lines at 100 and 500 m Isolines, and color white 
    kmlLine(x, name = m, description = paste0(m, "m-Isoline"), col = "#FFFefb45", 
            lwd = ifelse(m%%100 == 0, ifelse(m%%500, 3, 1.25), 0.45))
  })
  
  # write KML
  tf <- tempfile()
  kmlFile <- file(tf, "w")
  cat(kmlLine(kmlname = filename, kmldescription = 
"<i>Contour lines by A. González based on script K. Cechini,  
see <a href=\"htp://gimoya.bplaced.net/terrain-overlays.blogspot.co.at\">Terrain-Overlays</a> for details</i>")$header,
      file = kmlFile, sep = "\n")# kmlname = "Contour Lines"-> filename
  cat(unlist(out["style", ]), file = kmlFile, sep = "\n")
  cat(unlist(out["content", ]), file = kmlFile, sep = "\n")
  cat(kmlLine()$footer, file = kmlFile, sep = "\n")
  close(kmlFile)
  
  kmlName <- paste0("TOPO_", filename, ".kml")
  file.copy(tf, kmlName, overwrite = FALSE)#file.copy -> file.rename error: https://github.com/wch/vtest/issues/14
 
  if (ftp == T) ftpUpload(kmlName, paste0('ftp://gimoya:password@gimoya.bplaced.net/Terrain-Overlays/downloads/', kmlName))
}

for (filename in filenames[1:length(filenames)])
{
  tryCatch(make_kml_contours(filename, step = 25, simplify = 0.000001, ftp = F),#0.000001
           error = function(e) message(paste0("\n..something happend with dataset ", filename, ":\n", e)))
  cat("File ", filename, " done!..\n")
}


