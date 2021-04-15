# remove.packages(c("sp", "sf", "rgdal", "rgrass7", "watershed"))
# install.packages(c("rgdal", "sp", "sf"))
# remotes::install_github("rsbivand/rgrass7", dependencies = TRUE)
# remotes::install_github("flee-group/watershed", ref="main", dependencies=TRUE)

#Load in libraries
library(rgrass7)
library(raster)
library(dplyr)
library(rgdal)
library(rgeos)
library(sf)
library(watershed)
library(Matrix)

#options(gisBase = "C:/PROGRA~1//QGIS3~1.18/apps/grass/grass78")
options(gisBase = "C:/Program Files/GRASS GIS 7.8")

#########################################
############ TEST DATA ##################
#########################################
data(kamp_dem)
kamp = delineate(kamp_dem)
kamp_Tp = pixel_topology(kamp)
## Warning in .check_topology(res, warn = TRUE): Invalid topology; 1 nodes are
## downstream of more than two nodes.
kv = vectorise_stream(kamp[["stream"]], Tp=kamp_Tp)
## WARNING: Memory leak: 4 points are still in use
plot(kamp_dem, col=terrain.colors(20), axes = FALSE)
plot(st_geometry(kv), col='blue', add = TRUE)

kamp_Tr = reach_topology(kamp, kamp_Tp)



#############################
#Digital Elevation Model 
dem <- raster("~/01Master/MasterThesis/Pius/DEM/DEM_ext.tif", format="GTiff")
dem_proj <- projectRaster(dem, crs="+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", method='ngb') 

# hand digitized streams
streams <- readOGR(dsn="~/01Master/MasterThesis/Pius/DEM", layer="stream_dig")
#Carve DEM by manual edited streams 
watershed:::.start_grass(dem_proj, "dem_proj")
writeVECT(streams, "streams", driver="ESRI Shapefile")
execGRASS("r.carve", raster="dem_proj", vector="streams", output="dem_carved", width=30, depth=30)
  #WARNING: trying to divide by zero...no unique solution for
  #system...skipping...
dem_carved = raster(readRAST("dem_carved"))
  # Warning message:
  #   In showSRID(uprojargs, format = "PROJ", multiline = "NO", prefer_proj = prefer_proj) :
  #   Discarded datum unknown in Proj4 definition

#DELINEATE STREAM NETWORK
kitui = delineate(dem_carved, threshold=1e+03)
  # Warning messages:
  #   1: In delineate(dem_carved, threshold = 1e+06) :
  #   Small threshold; excessive computation time and memory usage are possible if threshold not increased
  # 2: In showSRID(SRS_string, format = "PROJ", multiline = "NO", prefer_proj = prefer_proj) :
  #   Discarded datum unknown in Proj4 definition
  # 3: In showSRID(SRS_string, format = "PROJ", multiline = "NO", prefer_proj = prefer_proj) :
  #   Discarded datum unknown in Proj4 definition
  # 4: In showSRID(SRS_string, format = "PROJ", multiline = "NO", prefer_proj = prefer_proj) :
  #   Discarded datum unknown in Proj4 definition
  # 5: In showSRID(SRS_string, format = "PROJ", multiline = "NO", prefer_proj = prefer_proj) :
  #   Discarded datum unknown in Proj4 definition

#memory.limit(size=30000)

kitui_Tp = pixel_topology(kitui)
kivec = vectorise_stream(kitui[["stream"]], Tp=kitui_Tp)
plot(dem_carved, col=terrain.colors(20), axes= FALSE)
plot(st_geometry(kivec), col='blue', add = TRUE)





