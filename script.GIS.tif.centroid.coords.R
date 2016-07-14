# find pair wise distances most near to POI 
# working over large amount .tif raster images
# return raster file info more near to POI
# use center coords raster files to compare between POI 

rm(list=ls())

library(raster)
library(magrittr)#for pipe operator %>%

setwd("~/Documents/1_WORKING/DATA/GIS_Database/DEM_2/RASTER")
(tifnames <- gsub(".tif", ".tif", dir(pattern = ".tif")))

################################ 
# raster to choice

raster.choice <- function(file,POI)
  {
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
  
  xy.frame <-LS.to.df(file)
  # return(xy.frame)
  
  #function calcule distance between geographic points (all raster coord v/s POI)
  dist.to.raster.centre <- function(POI)
  {
    raster.poi<-SpatialPointsDataFrame(xy.frame[,c(2:3)], xy.frame)
    dist<-pointDistance(raster.poi, POI, lonlat = TRUE)#in meters
    return(data.frame(xy.frame[which.min(dist),],POIx=POI[1],
                      POIy=POI[2],min(dist)))#show raster data for select
  }
  choice<-dist.to.raster.centre(POI)
  return(choice)
}

raster.choice(tifnames,c(-71.253619, -30.254160))






### working around
tifnames2 <- "S33W067_0_0.tif"
dput(tifnames)

r <- raster("S33W067_0_0.tif")
str(r)
head(r)
summary(r)

#extract vertices coord
xy <-xyFromCell(r, c(1, ncol(r), ncell(r)-ncol(r)+1, ncell(r)))
str(xy)
#convert spatial points
sp <- SpatialPoints(xy)

#get raster extention
ext<-extent(r)
#see raster values
getValues(r, row = 10)

#find mid point to raster
x_centers <- xFromCol(r, col=ncol(r)/2)
y_centers <- yFromRow(r, row = nrow(r)/2)

mid_p <- c(x_centers, y_centers, r@data@names)

# Plot raster
plot(r)
text(xy, c("xy1","xy2","xy3","xy4"))
text(sp, c("z1","z2","z3","z4"))

text(x_centers, y_centers, "centroid" )
plot(ext, col="red",add=TRUE)
image(r)


################################################
#functions

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
POI <-c(-71.254762, -33.264529)#c(-71.197299, -32.948736)#c(-70.749342, -33.250147)
# coords <- xy.frame[,c(2:3)]
# data xy.frame
dist.to.raster.centre(POI, xy.frame[,c(2:3)], xy.frame)

