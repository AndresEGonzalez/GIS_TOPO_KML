# find pair wise distances most near to POI 
# working over large amount .tif raster images
# return raster file info more near to POI
# use center coords raster files to compare between POI 

rm(list=ls())

library(raster)
library(magrittr)#for pipe operator %>%

setwd("~/Documents/1_WORKING/DATA/GIS_Database/DEM_2/RASTER")
(tifnames <- gsub(".tif", ".tif", dir(pattern = ".tif")))

## make folder for output and set directory
dir.create("~/Documents/1_WORKING/DATA/GIS_Database/DEM_2/SELECT_RASTER")
#source file location 
current.folder <-"~/Documents/1_WORKING/DATA/GIS_Database/DEM_2/RASTER"



################################################
#function

#extract from all raster (.tif file) middle point coords to compare
raster.centro.coord <- function(file)
{
  r <- raster(file)
  x_centers <- xFromCol(r, col=ncol(r)/2)
  y_centers <- yFromRow(r, row = nrow(r)/2)
  (mid_p <- 
    data.frame(r@data@names, 
               X=as.numeric(x_centers), 
               Y=as.numeric(y_centers), stringsAsFactors = FALSE))
  
}

#all raster pass mid point coord to data frame
LS.to.df <- function(file)
{
  l.dat<-lapply(file, raster.centro.coord)
  xy.frame<-do.call(rbind,l.dat) %>% as.data.frame
}

xy.frame <-LS.to.df(tifnames)


#function calcule distance between geographic  points (all raster coord v/s POI)
dist.to.raster.centre <- function(POI,coords,data)
{
  raster.poi<-SpatialPointsDataFrame(coords, data)
  dist<-pointDistance(raster.poi, POI, lonlat = TRUE)#in meters
  
  (xy.frame[which.min(dist),])#show raster data for select
}

# encuentra la imagen raster mas cercana al POI de interes
POI <-c(-71.320965, -30.144305)#La serena c(-71.130271, -29.850007)#til-tilc(-71.254762, -33.264529)#c(-71.197299, -32.948736)#c(-70.749342, -33.250147)
# coords <- xy.frame[,c(2:3)]
# data xy.frame
dist.to.raster.centre(POI, xy.frame[,c(2:3)], xy.frame)

##########################################################
## Copy seleted file to SELECT_RASTER folder before created
select.file<-dist.to.raster.centre(POI, xy.frame[,c(2:3)], xy.frame)
file.name<-(paste(select.file[1,1],"tif", sep="."))

#copy file location
new.folder <-"~/Documents/1_WORKING/DATA/GIS_Database/DEM_2/SELECT_RASTER"

# copy the files to the new folder
file.copy(file.name, new.folder)
list.files(new.folder, ".tif")

