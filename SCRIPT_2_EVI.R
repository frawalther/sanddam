################################
#### VEGETATION EVI Data #######
################################

library(raster)
library(rgdal)
library(lubridate)
library(sf)
library(fasterize)

#background info:
  #https://philipperufin.github.io/gcg_eo/#session-03-vegetation-indices-data-transforms
  #evi <- 2.5 * ((nIR – red) / (nIR + 6 * red – 7.5 * blue + 10000))
  #reflectance values (EVI) in datasets are scaled by 10,000
  #INT2S data type

#open questions:
  #cloud cover: any cloud masking necessary? [NOT DONE]
  #https://philipperufin.github.io/gcg_eo/#session-02-data-quality-cloud-masking

EVI_list <- list.files("~/01Master/MasterThesis/Pius/NDVI_EVI", pattern='.tif$', recursive=T, full.names = T) #FW_EVI_2014-2020 
EVI_list #839 files

#extract date from filenames 
split_list = strsplit(EVI_list, split="_", fixed=TRUE)
split_col = unlist(lapply(split_list, "[[", 9)) #[9] = date of acquisition
#date <- lubridate::ymd(basename(split_col))

#Create reference raster to set extent
e <- readOGR(dsn="~/01Master/MasterThesis/Pius/climatedata", layer="extent_rough")
e_proj <- spTransform(e, crs("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

ref<-raster()
extent(ref) <- extent(e_proj)
res(ref) <- c(30,30)
crs(ref) <- "+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs"

###########################
###### ALL DATA ###########
###########################

        # #LAPPLY (Issue: running into memory shortage)
        # list_r <- lapply(EVI_list, raster)
        # r_proj <- lapply(list_r, projectRaster, to=ref, res=c(30,30), 
        #                  crs="+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs", 
        #                  method="bilinear")
        # #Error: cannot allocate vector of size N Mb
        # #There were 50 or more warnings (use warnings() to see the first 50)
        # 
        # ex_r <- lapply(list_r, extend, y=extent(ref), value=NA)
        # ##Error: cannot allocate vector of size N Mb
        # #crop_r <-lapply(list_r, crop, y=ref)
        
        #instead: Writing LOOP (run functions for each object (i) in list and respectively write Raster) 

#garbage collection 
gc()

tmp <- "~/01Master/MasterThesis/Pius/R/sand dam/outfiles/"
for(i in 1:length(EVI_list)) {
  r <-raster(EVI_list[[i]])
  r_proj <- projectRaster( from=r, to=ref, res=c(30,30), 
                           crs="+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs", 
                           method="bilinear")
  rc <- writeRaster(r_proj, paste(tmp, i, "EVI",split_col[[i]],".tif",sep="_"), overwrite=T)
  removeTmpFiles(0.1)
}
# executing loop function took ~5 hours

#Load outfiles
EVI_files <- list.files("C:/Users/franz/Documents/01Master/MasterThesis/Pius/R/sand dam/outfiles/", pattern='EVI', recursive=T, full.names = T)

#stack files 
EVI_rasters <- raster::stack(EVI_files)
  #plot(EVI_rasters$X_100_EVI_20190117_)

# Load in pre-processed LAND COVER dataset [shapefile]
    #SD/ ID / X / ID_1 = sand dam ID (n=135)
    #LC_proj = land cover class [value]
    #class = LC class [name]
lc_int <- st_read(dsn = "~/01Master/MasterThesis/Pius/geodata", layer = 'lc_int')

##########################
#### Zonal statistics ####
##########################

#zonalstats()
#install.packages("spatialEco") # my Rversion is too old
#remotes::install_github("jeffreyevans/spatialEco")
#install.packages("exactextractr")
library(spatialEco)
library(exactextractr)

zs <- zonal.stats(x=lc_int, y=EVI_rasters, stats = "mean") 

#redo same for standard deviation: need to write sd function for that within stats.zonal()? 
# is "sd" possible?
#zs_min <- zonal.stats(x=lc_int, y=EVI_rasters, stats = "min")
#zs_max <- zonal.stats(x=lc_int, y=EVI_rasters, stats = "max") 
  #very time consuming
    #start ca. 11.30
    #end: next day, 17:00

head(zs)
nrow(zs) #286

write.csv(zs, "zonalstats.csv")
#write.csv(zs_sd, "zs_sd.csv")


library(ggplot2)
library(dplyr)
library(tidyr)

zonalstats <- read.csv("~/01Master/MasterThesis/Pius/R/sand dam/zonalstats.csv", header=T)

zosta <- zonalstats %>%
  gather(key=filename, value= mean, -X) #X = 286 sd_areas/LC 

#data wrangling, extract date from filename 
split_zosta = strsplit(zosta$filename, split="_", fixed=TRUE)
split_z = unlist(lapply(split_zosta, "[[", 4))
zosta$date <- lubridate::ymd(basename(split_z))

#plot
zosta %>%
  na.omit() %>%
  filter(X == 1) %>% 
  ggplot(aes(x=date, y=mean)) + geom_point() 

#lots of zeros -> neglect them? =NA???

zosta %>%
  na.omit() %>%
  filter_if(., is.numeric, all_vars((.) != 0)) %>%
  ggplot(aes(x=date, y=mean)) + geom_point() 
filterX ==1 & mean > 0) %>% 
  
######################################################################
#Alternative to zonal.stats():
#zonal()
#

lc_int$num <- seq.int(nrow(lc_int))
lc_r <- fasterize(lc_int, ref, "num")

z <- zonal(t_rasters, lc_r, "mean", na.rm=F )
head(z)
nrow(z) #262

head(lc_int)
nrow(lc_int)
ncol(lc_int)
###################################################################### 


##################### Error: cannot allocate vector of size N Mb
### MEMORY ISSUE #### How to improve R performance? 
#####################

#garbage collection
# gc() 
#check memory limit
# memory.limit()  
# [1] 16182
#memory.size()
#rm()
#check RAM 
# library(benchmarkme)
# benchmarkme::get_ram()
#enable working on multiple cores in parallel [increases memory usage]
# library(doParallel)
# > detectCores()
# [1] 8
#mclapply() [only works for Mac - instead parLapply]
#numCores <- detectCores()-1


############################
#### SUBSAMPLE #############
############################

EVI_trial <- EVI_list[1:10]

gc()

tmp <- "~/01Master/MasterThesis/Pius/R/sand dam/t/"
for(i in 1:length(EVI_trial)) {
  r <-raster(EVI_trial[[i]])
  r_proj <- projectRaster( from=r, to=ref, res=c(30,30), 
                           crs="+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs", 
                           method="bilinear")
  rc <- writeRaster(r_proj, paste(tmp, "EVI", split_col[[i]], ".tif", sep="_"),overwrite=T)
  removeTmpFiles(0.1)
  }

EVI_t <- list.files("C:/Users/franz/Documents/01Master/MasterThesis/Pius/R/sand dam/t/", pattern='EVI', recursive=T, full.names = T)

# stack files 
t_rasters <- raster::stack(EVI_t)
plot(t_rasters$X_EVI_20140424_)

# brick
#EVI_brick <- brick(t_rasters)

# Load in pre-processed land cover dataset/shapefile
#SD = sand dam ID (n=135)
#LC_proj = land cover class [value]
#class = LC class [name]
lc_int <- st_read(dsn = "~/01Master/MasterThesis/Pius/geodata", layer = 'lc_int')


#### Zonal statistics ####

#zonalstats()
#install.packages("spatialEco") # my Rversion is too old
#remotes::install_github("jeffreyevans/spatialEco")
#install.packages("exactextractr")
library(spatialEco)
library(exactextractr)

zs <- zonal.stats(x=lc_int, y=t_rasters, stats = "mean")
head(zs)
nrow(zs) #286

save
#zonal()
lc_int$num <- seq.int(nrow(lc_int))
lc_r <- fasterize(lc_int, ref, "num")

z <- zonal(t_rasters, lc_r, "mean", na.rm=F )
head(z)
nrow(z) #262


#Looping approaches:

result <- list()
for(i in 1:length(EVI_t)) {
  r <- raster(EVI_t[[i]])
  z <- zonal(r, lc_r, "mean", na.rm=T)
  result[[i]] = z
}

result <- list()
for(i in 1:length(EVI_t)) {
  r <- raster(EVI_t[[i]])
  z <- zonal.stats(x=lc_int, y=r, stats = c("min", "mean", "max"))
  result[[i]] = z
}
warnings()
#need to remove NAs before!?


#################
?zonal
?extract()
?zonal.stats
?`zonal,RasterStackBrick,RasterLayer-method`


#comment: units???

library(rts)

install.packages("rts")

#############
### trash ###
#############

#brick
#EVI_brick <- brick(EVI_rasters)

# stackSave(EVI_rasters, "C:/Users/franz/Documents/01Master/MasterThesis/Pius/R/sand dam/EVIstack.stk")
# writeRaster(EVI_rasters, "C:/Users/franz/Documents/01Master/MasterThesis/Pius/R/sand dam/EVIstack.tif", format="GTiff")
#writeRaster takes very long, do I need to set datatype=INT2S?

# comments:
# executing loop function took ~5 hours
# lost important filenames (including date etc.)
# zu umständlich?

#s <- stackOpen("C:/Users/franz/Documents/01Master/MasterThesis/Pius/R/sand dam/EVIstack.stk")


