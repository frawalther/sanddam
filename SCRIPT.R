#Load in libraries

#remotes::install_github("mtalluto/rgrass7")
library(rgrass7)
library(raster)
library(dplyr)
library(rgdal)
library(rgeos)
library(sf)
library(watershed)

options(gisBase = "C:/PROGRA~1//QGIS3~1.16/apps/grass/grass76")

#Digital Elevation Model 
dem <- raster("~/01Master/MasterThesis/Pius/DEM/DEM_ext.tif", format="GTiff")

dem_proj <- projectRaster(dem, crs="+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", method='ngb') 

# hand digitized streams
streams <- readOGR(dsn="~/01Master/MasterThesis/Pius/DEM", layer="stream_dig")

execGRASS("r.carve", raster="dem_proj", vector="streams", output="dem_carved", width=30, depth=30)

#ERROR!: Grass commands do not run
#Error in if (get("SYS", envir = .GRASS_CACHE) == "WinNat" && nchar(WN_bat) ==  : 
#missing value where TRUE/FALSE needed
