#remove.packages(c("sp", "sf", "rgdal", "rgrass7", "watershed"))
#install.packages(c("rgdal", "sp", "sf"))
# remotes::install_github("rsbivand/rgrass7", dependencies = TRUE)
# remotes::install_github("flee-group/watershed", ref="main", dependencies=TRUE)
# remotes::install_github("mtalluto/WatershedTools")

# Load in libraries
library(rgrass7)
library(raster)
library(dplyr)
library(rgdal)
library(rgeos)
library(sf)
library(watershed)
library(Matrix)
library(WatershedTools)

options(gisBase = "C:/Program Files/GRASS GIS 7.8")

# #########################################
# ############ TEST DATA ##################
# #########################################
# data(kamp_dem)
# kamp = delineate(kamp_dem)
# kamp_Tp = pixel_topology(kamp)
# ## Warning in .check_topology(res, warn = TRUE): Invalid topology; 1 nodes are
# ## downstream of more than two nodes.
# kv = vectorise_stream(kamp[["stream"]], Tp=kamp_Tp)
# ## WARNING: Memory leak: 4 points are still in use
# plot(kamp_dem, col=terrain.colors(20), axes = FALSE)
# plot(st_geometry(kv), col='blue', add = TRUE)
# 
# kamp_Tr = reach_topology(kamp, kamp_Tp)
# #########################################

# Load in Digital Elevation Model 
dem <- raster("~/01Master/MasterThesis/Pius/DEM/DEM_ext.tif", format="GTiff")
dem_proj <- projectRaster(dem, crs="+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", method='ngb') 

# Load in hand digitized streams
streams <- readOGR(dsn="~/01Master/MasterThesis/Pius/geodata", layer="stream_handdig")
crs(streams)
#plot(streams)

# Carve DEM with manual edited streams 
watershed:::.start_grass(dem_proj, "dem_proj")
writeVECT(streams, "streams", driver="ESRI Shapefile")
execGRASS("r.carve", raster="dem_proj", vector="streams", output="dem_carved", width=30, depth=30)
#WARNING: trying to divide by zero...no unique solution for
#system...skipping...
dem_carved = raster(readRAST("dem_carved"))
# Warning message:
#   In showSRID(uprojargs, format = "PROJ", multiline = "NO", prefer_proj = prefer_proj) :
#   Discarded datum unknown in Proj4 definition

writeRaster(dem_carved, "~/01Master/MasterThesis/Pius/DEM/d_carved.tif", format="GTiff", overwrite=T)
dem_carved <- raster("~/01Master/MasterThesis/Pius/DEM/d_carved.tif", format="GTiff")
crs(dem_carved) <- "+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

###########################
# DELINEATE STREAM NETWORK 
###########################

# Generate flow accumulation and flow direction
execGRASS("r.watershed", parameters = list(elevation="dem_carved", accumulation="accu", drainage="drain"), 
          flags="overwrite")

accu = raster(readRAST("accu"))
drain = raster(readRAST("drain"))

# Generate stream network (threshold: >= 200 accumulated cells)
execGRASS("r.stream.extract", parameters=list(elevation="dem_carved", accumulation="accu", threshold=200, 
                                              stream_raster="r_stream", stream_vector="v_stream"), 
          flags="overwrite") ##direction="fdir"
r_stream = raster(readRAST("r_stream"))
v_stream = readVECT("v_stream")

# SAND DAM POINTS 
sd <- read.csv("~/01Master/MasterThesis/Pius/sd_all_expl.csv", header=T)

sd <- SpatialPointsDataFrame(sd[,3:4], sd)
crs(sd) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
sd_proj <- spTransform(sd, crs("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

sd_snapped <- snapToStream(sd_proj, r_stream, buff=100)

#SAVE 
# sd_snapped <- st_as_sf(sd_snapped)
# st_write(sd_snapped, dsn = "~/01Master/MasterThesis/Pius/geodata", layer = 'sd_snapped', driver = 'ESRI Shapefile', delete_layer = T )
# # Warning message:
# #   In abbreviate_shapefile_names(obj) :
# #   Field names abbreviated for ESRI Shapefile driver
# sd_snapped <- st_read(dsn = "~/01Master/MasterThesis/Pius/geodata", layer = "sd_snapped")


#################################
##### Catchment Delineation #####
#################################

result <- list()
x<- sp::coordinates(sd_snapped)
for(i in 1:nrow(x)) {
  rgrass7::execGRASS("r.water.outlet", flags=c("overwrite", "quiet"), input = "drain", 
                     output = "catchment_areas", coordinates = x[i,])
  catchment_areas = raster(readRAST("catchment_areas"))
  rgrass7::execGRASS("r.to.vect", flags = c('overwrite', 'quiet'), input = "catchment_areas", 
                     output = "ca_vect", type = 'area', column='value')
  vect = sf::st_as_sf(rgrass7::readVECT("ca_vect", ignore.stderr = TRUE))
  result[[i]] = vect
}
#~173 WARNINGs: Vector map <ca_vect> already exists and will be overwritten 

ca_all <- do.call(rbind, result) 
ca_all <- cbind(ID = 1:nrow(ca_all), ca_all) 

plot(st_geometry(ca_all))
class(ca_all)

ca_all <- st_as_sf(ca_all)
ca_all <- ca_all %>%
  select(-cat, -value, -label)

#save ca_all (so I do not need to re-run it)

ca_all <- as(ca_all, "Spatial")
writeOGR(ca_all, dsn = "~/01Master/MasterThesis/Pius/geodata" , layer="ca_all" , driver = "ESRI Shapefile", overwrite_layer=T)

cas <- readOGR(dsn="~/01Master/MasterThesis/Pius/geodata", layer="ca_all")
cas <- st_as_sf(cas)

###############
### BUFFER ####
###############

#sand dam buffer [500m]
sd_snapped <- st_as_sf(sd_snapped)
sdbuf <- st_buffer(sd_snapped, 500)

#intersecting sand dam buffer with catchment areas
sd_ca <- st_intersection(sdbuf, cas)
sd_ca_int <- sd_ca[which(sd_ca$ID == sd_ca$ID.1),]

plot(st_geometry(sd_ca))
plot(st_geometry(sd_ca_int))

#river buffer [100m]
stream <- st_as_sf(v_stream) #or r_stream?
rivbuf <- st_buffer(stream, 100) #check distance decision again! 
rivbuf <- st_union(rivbuf) #CAUTION: breakdowns possible 

#intersect
sd_area <- st_intersection(sd_ca_int, rivbuf)
plot(st_geometry(sd_area))

st_write(sd_area, dsn = "~/01Master/MasterThesis/Pius/geodata", layer = 'sd_area', driver="ESRI Shapefile", delete_layer = TRUE) #check again

sd_area <- st_read(dsn = "~/01Master/MasterThesis/Pius/geodata", layer = 'sd_area')


#######################################################################################################
##### Land Cover ###############
################################

LC <- raster("~/01Master/MasterThesis/Pius/geodata/LC_extentrough.tif")
crs(LC) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

proj <- "+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

LC_proj <- projectRaster(LC, crs=proj, method='ngb') #slow 
writeRaster(LC_proj, "~/01Master/MasterThesis/Pius/geodata/LC_proj.tif", format="GTiff", overwrite=T)

LC_proj <- raster("~/01Master/MasterThesis/Pius/geodata/LC_proj.tif")

## crop and mask
LC_r <- crop(LC_proj, extent(sd_area))
LC_r <- mask(LC_r, sd_area)

unique(LC_r)
#[1] 1 2 3 4 5 8

#legend
#1 = Tree cover areas 
#2 = shrubs cover areas
#3 = Grassland 
#4 = Cropland
#5 = Vegetation aquatic or regularly flooded 
#8 = Built up areas 

#exclude: built up areas [8]

# lc of interest: cropland [4] and shrubs [2]

trees <-LC_r
values(trees)[values(trees) != 1] = NA
LC_trees <- rasterToPolygons(trees, dissolve = T, na.rm=T)
LC_trees <- st_as_sf(LC_trees)
trees_int <- st_intersection(LC_trees, sd_area)
trees_int <- trees_int[which(trees_int$ID == trees_int$ID_1),]
trees_int$class <- "treecover"
writeRaster(trees, "~/01Master/MasterThesis/Pius/geodata/trees.tif", format="GTiff", overwrite=T)
nrow(trees_int)
#10

shrubs<-LC_r
values(shrubs)[values(shrubs) != 2] = NA
LC_shrub <- rasterToPolygons(shrubs, dissolve = T, na.rm=T)
LC_shrub <- st_as_sf(LC_shrub)
shrub_int <- st_intersection(LC_shrub, sd_area)
shrub_int <- shrub_int[which(shrub_int$ID == shrub_int$ID_1),]
shrub_int$class <- "shrubs"
#writeRaster(shrubs, "~/01Master/MasterThesis/Pius/geodata/shrubs.tif", format="GTiff", overwrite=T)
nrow(shrub_int)
#171

grassland <-LC_r
values(grassland)[values(grassland ) != 3] = NA
LC_grass <- rasterToPolygons(grassland , dissolve = T, na.rm=T)
LC_grass <- st_as_sf(LC_grass)
grass_int <- st_intersection(LC_grass, sd_area)
grass_int <- grass_int[which(grass_int$ID == grass_int$ID_1),]
grass_int$class <- "grassland"
#writeRaster(grassland, "~/01Master/MasterThesis/Pius/geodata/grassland.tif", format="GTiff", overwrite=T)
nrow(grass_int)
#11

cropland<-LC_r
values(cropland)[values(cropland) != 4] = NA
LC_crop <- rasterToPolygons(cropland, dissolve = T, na.rm=T)
LC_crop <- st_as_sf(LC_crop)
crop_int <- st_intersection(LC_crop, sd_area)
crop_int <- crop_int[which(crop_int$ID == crop_int$ID_1),]
crop_int$class <- "cropland"
#writeRaster(cropland, "~/01Master/MasterThesis/Pius/geodata/Cropland.tif", format="GTiff", overwrite=T)
nrow(crop_int)
#173

aquaveg <-LC_r
values(aquaveg)[values(aquaveg) != 5] = NA
LC_aquaveg <- rasterToPolygons(aquaveg, dissolve = T, na.rm=T)
LC_aquaveg <- st_as_sf(LC_aquaveg)
aquaveg_int <- st_intersection(LC_aquaveg, sd_area)
aquaveg_int <- aquaveg_int[which(aquaveg_int$ID == aquaveg_int$ID_1),]
aquaveg_int$class <- "aquaveg"
writeRaster(aquaveg, "~/01Master/MasterThesis/Pius/geodata/aquaveg.tif", format="GTiff", overwrite=T)
nrow(aquaveg_int)
#17

#exclude classes: aquaveg (5), trees (1) and grass(3) due to low sample sizes 


#combine the remaining classes (cropland and shrubs) into one dataframe
lc_int <- rbind(crop_int, shrub_int)

#calculate area for each sf feature
lc_int$area_m2 <- st_area(lc_int) 

st_write(lc_int, dsn = "~/01Master/MasterThesis/Pius/geodata", layer = 'lc_int', driver="ESRI Shapefile", delete_layer = TRUE) #check again
