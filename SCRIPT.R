
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
#writeRaster(dem_proj, "~/01Master/MasterThesis/Pius/DEM/dem_proj.tif", format="GTiff", overwrite=T)
# hand digitized streams
streams <- readOGR(dsn="~/01Master/MasterThesis/Pius/DEM", layer="stream_dig")

#Hands on GRASS:
run <- FALSE
if (nchar(Sys.getenv("GISRC")) > 0 &&
    read.dcf(Sys.getenv("GISRC"))[1,"LOCATION_NAME"] == "nc_basic_spm_grass7") run <- TRUE
    oechoCmd <- get.echoCmdOption()
    set.echoCmdOption(TRUE)
if (run) {
  execGRASS("r.carve", raster="dem_proj", vector="streams", output="dem_carved", width=30, depth=30)
}
#ERROR: Raster map <dem_proj> not found





dem_carved <- raster("~/01Master/MasterThesis/Pius/DEM/dem_carved.tif")

#delineate stream network   

kitui = delineate(dem_carved,threshold= 1e+05)
#plot(dem_carved, col=terrain.colors(20), axes= FALSE)
#plot(kitui@stream, col="blue", add=T, legend=F)









