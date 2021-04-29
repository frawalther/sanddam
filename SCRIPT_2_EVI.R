################################
#### VEGETATION EVI Data #######
################################

library(raster)
library(rgdal)
library(lubridate)
library(sf)
library(fasterize)


EVI_list <- list.files("~/01Master/MasterThesis/Pius/NDVI_EVI", pattern='.tif$', recursive=T, full.names = T) #FW_EVI_2014-2020 #839
EVI_list

#Create reference raster to set extent
e <- readOGR(dsn="~/01Master/MasterThesis/Pius/climatedata", layer="extent_rough")
e_proj <- spTransform(e, crs("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

ref<-raster()
extent(ref) <- extent(e_proj)
res(ref) <- c(30,30)
crs(ref) <- "+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs"

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
# outpath <- "~/01Master/MasterThesis/Pius/NDVI_EVI/t"
# dir.create(outpath)
# outfiles <- paste0(outpath, EVI_list)
#outfiles <-list()
for(i in 1:length(EVI_list)) {
  r <-raster(EVI_list[[i]])
  r_proj <- projectRaster( from=r, to=ref, res=c(30,30), 
                           crs="+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs", 
                           method="bilinear")
  rc <- writeRaster(r_proj, paste("outfiles",i,".tif",sep=""), overwrite=T)
  removeTmpFiles(0.1)
}

EVI_files <- list.files("C:/Users/franz/Documents/01Master/MasterThesis/Pius/R/sand dam/", pattern='outfiles', recursive=T, full.names = T)

#stack files 
EVI_rasters <- raster::stack(EVI_files)
plot(EVI_rasters$outfiles1)

#brick
EVI_brick <- brick(EVI_rasters)

stackSave(EVI_rasters, "C:/Users/franz/Documents/01Master/MasterThesis/Pius/R/sand dam/EVIstack.stk")
writeRaster(EVI_rasters, "C:/Users/franz/Documents/01Master/MasterThesis/Pius/R/sand dam/EVIstack.tif", format="GTiff")


#comments:
# executing loop function took >2-3hours
#lost important filenames (nicluding date etc.)
# zu umständlich? 

s <- stackOpen("C:/Users/franz/Documents/01Master/MasterThesis/Pius/R/sand dam/EVIstack.stk")

plot(s$outfiles700)
head(s@z)


#extract date from filenames 
split_list = strsplit(EVI_list, split="_", fixed=TRUE)
split_col = unlist(lapply(split_list, "[[", 9))

date <- lubridate::ymd(basename(split_col))

###################################################################### 


#### Zonal statistics ####
?zonal


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

#Create reference raster to set extent
e <- readOGR(dsn="~/01Master/MasterThesis/Pius/climatedata", layer="extent_rough")
e_proj <- spTransform(e, crs("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

ref<-raster()
extent(ref) <- extent(e_proj)
res(ref) <- c(30,30)
crs(ref) <- "+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs"

gc()

#outfiles <-list()
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

#stack files 
t_rasters <- raster::stack(EVI_t)
plot(t_rasters$X_EVI_20140424_)

#brick
EVI_brick <- brick(t_rasters)

#comment: units???

library(rts)

install.packages("rts")

#Load in pre-processed land cover dataset/shapefile
#SD = sand dam ID (n=135)
#LC_proj = land cover class [value]
#class = LC class [name]

lc_int <- st_read(dsn = "~/01Master/MasterThesis/Pius/geodata", layer = 'lc_int')
head(lc_int)
lc_int$num <- seq.int(nrow(lc_int))
lc_r <- fasterize(lc_int, ref, "num")

result <- list()
for(i in 1:length(EVI_t)) {
  r <- raster(EVI_t[[i]])
  z <- zonal(r, lc_r, "mean", na.rm=T)
  result[[i]] = z
}
z <- zonal(t_rasters, lc_r, "mean", na.rm=F ) #na.rm=T or F
head(z)
nrow(z)
lc_r

?zonal
library(spatialEco)
?zonal.stats
