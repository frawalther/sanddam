#remove.packages(c("sp", "sf", "rgdal", "rgrass7", "watershed"))
#install.packages(c("rgdal", "sp", "sf"))
# remotes::install_github("rsbivand/rgrass7", dependencies = TRUE)
# remotes::install_github("flee-group/watershed", ref="main", dependencies=TRUE)
# remotes::install_github("mtalluto/WatershedTools")

#Load in libraries
library(rgrass7)
library(raster)
library(dplyr)
library(rgdal)
library(rgeos)
library(sf)
library(watershed)
library(Matrix)
library(WatershedTools)

#options(gisBase = "C:/PROGRA~1//QGIS3~1.18/apps/grass/grass78")
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

#writeRaster(dem_carved, "~/01Master/MasterThesis/Pius/DEM/d_carved.tif", format="GTiff")

    #DELINEATE STREAM NETWORK 
    #kitui = delineate(dem_carved, threshold=5e+04)# outlet=NA (default), set outlet=c(4704588, 2847762)
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
            
    
    #kitui_Tp = pixel_topology(kitui)
    #kivec = vectorise_stream(kitui[["stream"]], Tp=kitui_Tp) #streams arent captured nicely with delineate(); for more exact delienation: extract_stream() // r.stream.extract()
    #plot(dem_carved, col=terrain.colors(20), axes= FALSE)
    #plot(st_geometry(kivec), col='blue', add = TRUE)


# STREAMS: r.stream.extract // [extract_stream() fOES NOT EXIST YET]
execGRASS("r.watershed", parameters = list(elevation="dem_carved", accumulation="accu", drainage="drain"), 
          flags="overwrite")

accu = raster(readRAST("accu"))
drain = readVECT("v_stream")

execGRASS("r.stream.extract", parameters=list(elevation="dem_carved", accumulation="accu", threshold=200, stream_raster="r_stream", stream_vector="v_stream"), 
          flags="overwrite") ##direction="fdir"
r_stream = raster(readRAST("r_stream"))
v_stream = readVECT("v_stream")

#SAND DAM POINTS 
sd <- read.csv("~/01Master/MasterThesis/Pius/sd_all_explicit.csv", header=T)
KWest <- sd %>%
  filter(study == "Kitui West (Pius)")

sd_KWest <- SpatialPointsDataFrame(KWest[,3:4], KWest)
crs(sd_KWest) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
KWest_proj <- spTransform(sd_KWest, crs("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

        # writeVECT(KWest_proj, "KWest_proj", driver="ESRI Shapefile", v.in.ogr_flags="overwrite") 
        # execGRASS("r.stream.snap", parameters=list(input="KWest_proj", output="KWest_snap", stream_rast="r_stream", accumulation="accu", threshold=200, radius=1),
        #           flags="overwrite")
        # 
        # KWest_snap = readVECT("KWest_snap", type="point")
        
        # KWest_sd <- st_as_sf(KWest, coords= c("Point.X", "Point.Y"), crs=4326)
        # KWest_sd <- st_transform(KWest_sd, 32737)

KWest_snapped <- snapToStream(KWest_proj, r_stream, buff=100)

#################################
##### Catchment Delineation #####
#################################

#catchment(x=KWest_snapped, drainage=drain, output="sf") #area=T
    #Error in system2("grass74", args = c("--config path"), stdout = TRUE) : 
    #  '"grass74"' not found

result <- list()
x<- sp::coordinates(KWest_snapped)
for(i in 1:nrow(x)) {
  rgrass7::execGRASS("r.water.outlet", flags=c("overwrite", "quiet"), input = "drain", 
                     output = "catchment_areas", coordinates = x[i,])
  catchment_areas = raster(readRAST("catchment_areas"))
  rgrass7::execGRASS("r.to.vect", flags = c('overwrite', 'quiet'), input = "catchment_areas", 
                   output = "ca_vect", type = 'area', column='value')
  vect = sf::st_as_sf(rgrass7::readVECT("ca_vect", ignore.stderr = TRUE))
  result[[i]] = vect
}
#WARNINGs: Vector map <ca_vect> already exists and will be overwritten

ca_all <- do.call(rbind, result) 
ca_all <- cbind(ID = 1:nrow(ca_all), ca_all) 

plot(st_geometry(ca_all))
class(ca_all)


ca_all <- st_as_sf(ca_all)
ca_all <- ca_all %>%
  select(-cat, -value, -label)

#save ca_all (so I do not need to re-run it)

  # st_write(dsn = "~/01Master/MasterThesis/Pius/geodata/ca_all.shp", layer = 'ca_all')
  #   # Error in UseMethod("st_write") : 
  #   #   no applicable method for 'st_write' applied to an object of class "character"
  
  ca_all <- as(ca_all, "Spatial")
  writeOGR(ca_all, dsn = "~/01Master/MasterThesis/Pius/geodata" , layer="ca_all" , driver = "ESRI Shapefile", overwrite_layer=T)

cas <- readOGR(dsn="~/01Master/MasterThesis/Pius/geodata", layer="ca_all")
cas <- st_as_sf(cas)

###############
### BUFFER ####
###############

#sand dam buffer
KWest_snapped <- st_as_sf(KWest_snapped)
sdbuf <- st_buffer(KWest_snapped, 500)

#intersecting sand dam buffer with catchment areas
sd_ca <- st_intersection(sdbuf, cas)
sd_ca_int <- sd_ca[which(sd_ca$ID == sd_ca$ID.1),]

plot(st_geometry(sd_ca))
plot(st_geometry(sd_ca_int))

#river buffer
stream <- st_as_sf(v_stream) #or r_stream?
rivbuf <- st_buffer(stream, 100) #check distance decision again! 
rivbuf <- st_union(rivbuf) 

#intersect
sd_area <- st_intersection(sd_ca_int, rivbuf)
plot(st_geometry(sd_area))

st_write(sd_area, dsn = "~/01Master/MasterThesis/Pius/geodata", layer = 'sd_area', driver="ESRI Shapefile") #check again

sd_area <- st_read(dsn = "~/01Master/MasterThesis/Pius/geodata", layer = 'sd_area')
sd_area <- as(sd_area, "Spatial")


#######################################################################################################
##### Land Cover ###############
################################

LC <- raster("~/01Master/MasterThesis/Pius/geodata/LC_extentrough.tif")
crs(LC) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

proj <- "+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

LC_proj <- projectRaster(LC, crs=proj, method='ngb') #painly slow 
writeRaster(LC_proj, "~/01Master/MasterThesis/Pius/geodata/LC_proj.tif", format="GTiff", overwrite=T)

## crop and mask
LC_r <- crop(LC_proj, extent(sd_area))
LC_r <- mask(LC_r, sd_area)

#only cropland (4)
unique(LC_r)
#[1] 2 4 5 8

    cropland<-LC_r
    values(cropland)[values(cropland) != 4] = NA
    LC_crop <- rasterToPolygons(cropland, dissolve = T, na.rm=T)
    LC_crop <- st_as_sf(LC_crop)
    crop_int <- st_intersection(LC_crop, sd_area)
    crop_int <- crop_int[which(crop_int$ID == crop_int$ID_1),]
    crop_int$class <- "cropland"
    #writeRaster(cropland, "~/01Master/MasterThesis/Pius/geodata/Cropland.tif", format="GTiff", overwrite=T)
    
    shrubs<-LC_r
    values(shrubs)[values(shrubs) != 2] = NA
    LC_shrub <- rasterToPolygons(shrubs, dissolve = T, na.rm=T)
    LC_shrub <- st_as_sf(LC_shrub)
    shrub_int <- st_intersection(LC_shrub, sd_area)
    shrub_int <- shrub_int[which(shrub_int$ID == shrub_int$ID_1),]
    shrub_int$class <- "shrubs"
    #writeRaster(shrubs, "~/01Master/MasterThesis/Pius/geodata/shrubs.tif", format="GTiff", overwrite=T)
    
    veg_aqua<-LC_r
    values(veg_aqua)[values(veg_aqua) != 5] = NA
    LC_vega <- rasterToPolygons(veg_aqua, dissolve = T, na.rm=T)
    LC_vega <- st_as_sf(LC_vega)
    vega_int <- st_intersection(LC_vega, sd_area)
    vega_int <- vega_int[which(vega_int$ID == vega_int$ID_1),]
    vega_int$class <- "veg_aqua"
    #writeRaster(veg_aqua, "~/01Master/MasterThesis/Pius/geodata/veg_aqua.tif", format="GTiff", overwrite=T)
    #very small fraction! Does it fit to another lc?
    
    #exclude built up areas [8]
    #write in a loop?
    
#combine them into one dataframe
lc_int <- rbind(crop_int, shrub_int, vega_int)

#calculate area for each sf feature (basis to e.g. calculate overall mean EVI/SD)
lc_int$area_m2 <- st_area(lc_int)


