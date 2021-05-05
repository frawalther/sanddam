library(sp)
library(sf)
library(rgdal)
library(raster)
library(mapview)
library(ncdf4)
library(rgeos)

#load study area
studyarea <- shapefile("~/01Master/MasterThesis/Pius/climatedata/extent_rough.shp") #rough extent - dont really need it, just for the plot
      # 
      # Kitui_county <- readOGR(dsn ="~/01Master/MasterThesis/Pius/geodata/Kitui_county", layer = "Kitui_county")
      # plot(Kitui_county, add = TRUE)

#snapped sand dam points
# SAND DAM POINTS 
KWest_snapped <- st_read(dsn = "~/01Master/MasterThesis/Pius/geodata", layer = "sd_KWest_snapped")
plot(st_geometry(KWest_snapped))

# Load in pre-processed land cover dataset/shapefile
#SD = sand dam ID (n=135)
#LC_proj = land cover class [value]
#class = LC class [name]
lc_int <- st_read(dsn = "~/01Master/MasterThesis/Pius/geodata", layer = 'lc_int')
crs(lc_int)

#Precipitation
#
#TAMSAT rainfall estimates 
tamsat_brick <- brick("~/01Master/MasterThesis/Pius/climatedata/TAMSAT/04-tamsatMonthly.v3.1-946684800-1609459200_-20.0_38.5_-2.2_-1.0.nc", varname="rfe") #or rfefilled
tamsat_brick <- crop(tamsat_brick, extent(studyarea), snap="out")

#subset 2014-2020
prec <- subset(tamsat_brick, which(getZ(tamsat_brick) >= '2014-01-31' & (getZ(tamsat_brick) <= '2020-12-31')))
crs(tamsat_brick)

#reproject
prec_proj <- projectRaster(prec, crs="+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

#zonal statistics 
# claculate the mean Precipitation for each area per time 

#zonalstats()
#install.packages("spatialEco") # my Rversion is too old
#remotes::install_github("jeffreyevans/spatialEco")
#install.packages("exactextractr")
library(spatialEco)
library(exactextractr)

zs_prec_mean <- zonal.stats(x=lc_int, y=prec_proj, stats = "mean") 
zs_prec_sd <- zonal.stats(x=lc_int, y=prec_proj, stats = "sd") 
zs_prec_min <- zonal.stats(x=lc_int, y=prec_proj, stats = "min") 
zs_prec_max <- zonal.stats(x=lc_int, y=prec_proj, stats = "max") 

