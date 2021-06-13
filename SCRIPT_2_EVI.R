################################
#### VEGETATION EVI Data #######
################################

library(raster)
library(rgdal)
library(lubridate)
library(sf)
library(fasterize)

library(exactextractr)
library(tidyr)
library(ggplot2)
library(dplyr)

#https://philipperufin.github.io/gcg_eo/#session-03-vegetation-indices-data-transforms
#evi <- 2.5 * ((nIR – red) / (nIR + 6 * red – 7.5 * blue + 10000))
#reflectance values (EVI) in datasets are scaled by 10,000
#INT2S data type

EVI_list <- list.files("~/01Master/MasterThesis/Pius/NDVI_EVI", pattern='.tif$', recursive=T, full.names = T) #FW_EVI_2014-2020 
EVI_list #839 files

#extract date from filenames 
split_list = strsplit(EVI_list, split="_", fixed=TRUE)
split_col = unlist(lapply(split_list, "[[", 9)) #[9] = date of acquisition

#Create reference raster to set extent
e <- readOGR(dsn="~/01Master/MasterThesis/Pius/climatedata", layer="extent_rough")
e_proj <- spTransform(e, crs("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

ref<-raster()
extent(ref) <- extent(e_proj)
res(ref) <- c(30,30)
crs(ref) <- "+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs"

#garbage collection 
gc()

#rasters have different extent -> Reproject rasters

tmp <- "~/01Master/MasterThesis/Pius/R/sand dam/outfiles/"
for(i in 1:length(EVI_list)) {
  r <-raster(EVI_list[[i]])
  r_proj <- projectRaster( from=r, to=ref, res=c(30,30), 
                           crs="+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs", 
                           method="bilinear")
  rc <- writeRaster(r_proj, paste(tmp, i, "EVI",split_col[[i]],".tif",sep="_"), overwrite=T)
  removeTmpFiles(0.1)
}

#Load reprojected rasters (outfiles)
EVI_files <- list.files("C:/Users/franz/Documents/01Master/MasterThesis/Pius/R/sand dam/outfiles/", 
                        pattern='EVI', recursive=T, full.names = T)

#Raster mosaic: merge rasters derived at same date; decrease amount of rasters that are getting stacked

split_evi = strsplit(EVI_files, split="_", fixed=TRUE)
split_e = unlist(lapply(split_evi, "[[", 4))

#set zeros to NA 
tmp <- "~/01Master/MasterThesis/Pius/R/sand dam/rasters_NA/"
for(i in 1:length(EVI_files)) {
  r <- raster(EVI_files[[i]])
  values(r)[values(r) == 0] = NA
  r_na <- writeRaster(r, paste(tmp, i, "_EVI_na_",split_e[[i]], ".tif", sep="")) #, overwrite=TRUE) #".tif",
  removeTmpFiles(0.1)
}

EVI_na_f <- list.files("C:/Users/franz/Documents/01Master/MasterThesis/Pius/R/sand dam/rasters_NA/", 
                       pattern='EVI', recursive=T, full.names = T)

#extract date from filenames 
split_evi = strsplit(EVI_na_f, split="_", fixed=TRUE)
split_e = unlist(lapply(split_evi, "[[", 5)) 
split = strsplit(split_e, split=".", fixed=T)
spli = unlist(lapply(split, "[[", 1))
dates_u <- unique(spli)  

#Load all rasters inside a list 
rlist <- list()
for (i in 1:length(EVI_na_f)) {
  r <- raster(EVI_na_f[[i]])
  rlist[[i]] = r
}

#Create loop for mosaic() rasters and write each output raster into a folder to not run into memory issues 
tmp <- "~/01Master/MasterThesis/Pius/R/sand dam/raster_mosaics/"
for (i in seq_along(dates_u)){
  idx <- which(spli %in% dates_u[i])
  if(length(idx) > 1){
    rlisttemp <- rlist[idx]
    rlisttemp$fun <- mean
    rmo <- do.call(mosaic, c(rlisttemp, na.rm=T))
    rm <- writeRaster(rmo, paste(tmp, i, "EVI", dates_u[[i]], ".tif", sep="_")) # , overwrite=T
  }else{
    rmo <- rlist[[idx]] #for non mosaicking images 
    rm <- writeRaster(rmo, paste(tmp, i, "EVI", dates_u[[i]], ".tif", sep="_")) # overwrite=T
  }
  #removeTmpFiles(0.1)
  rm(rmo)
  rm(rm)
  #gc()
}


#Load raster mosaics
Rasmo_files <- list.files("C:/Users/franz/Documents/01Master/MasterThesis/Pius/R/sand dam/raster_mosaics/", 
                          pattern='EVI', recursive=T, full.names = T)

# stack files 
EVI_stack <- raster::stack(Rasmo_files)
#plot(EVI_stack$X_1_EVI_20161031_)


# Load in pre-processed LAND COVER dataset/shapefile
lc_int <- st_read(dsn = "~/01Master/MasterThesis/Pius/geodata", layer = 'lc_int')
lc_int <- cbind(rn = rownames(lc_int), lc_int)

EVI_mean <-exactextractr::exact_extract(EVI_stack, lc_int, "mean")
#faster than extract() 
#more acurate than zonal(): https://isciences.gitlab.io/exactextractr/
#"raster pixels that are partially  covered by polygons are considered
#ignores NA:
# Undefined (NA) values are ignored in all of the named summary operations when they occur in the value raster. 
#   When they occur in the weighting raster, they cause the result of the summary operation to be NA

write.csv(EVI_mean, "EVI_mean_1.csv")

                #alternative zonal.stats() - dont know how to manually set mean, na.rm=T -> but does it base on exact_extract()?
                
                #start 18:10 (exactextractr::exact_extract())
                #end next day 10:50 ~17h
                
                #zonalstats()
                #install.packages("spatialEco") # my Rversion is too old
                #remotes::install_github("jeffreyevans/spatialEco")
                #install.packages("exactextractr")
                library(spatialEco)
                #library(exactextractr)
                EVI_mean_zs <- zonal.stats(x=lc_int, y=EVI_stack, stats = "mean")
                write.csv(EVI_mean_zs, "EVI_mean_2.csv")
                #beginn 11:00
                #end next day 17:30 ~30h
                
                #compare! how?

#calculate standard deviation
system.time({ EVI_sd <-exactextractr::exact_extract(EVI_stack, lc_int, "stdev") })
write.csv(EVI_sd, "EVI_sd.csv")


#Data wrangling: Dataframe/ data.table
EVI_mean <- read.csv("~/01Master/MasterThesis/Pius/R/sand dam/EVI_mean_1.csv", header=T)
zosta <- EVI_mean %>%
  gather(key=filename, value= EVI_mean, -X) #X = 286 sd_areas/LC

#extract date from filename
split_zosta = strsplit(zosta$filename, split="_", fixed=TRUE)
split_z = unlist(lapply(split_zosta, "[[", 4))
zosta$date <- lubridate::ymd(basename(split_z))

#plot
zosta %>%
  na.omit() %>%
  #  filter(X == 1) %>%
  ggplot(aes(x=date, y=EVI_mean)) + geom_point()

zosta[which.max(zosta$EVI_mean),]
#15067 -> how is this possible? [cloud mask error]
#all too high measures from a specific date: 2015-04-28 
#should I delete only >10000 or all from that specific date? 

df_EVI <- zosta %>%
  na.omit() %>%
  filter(date != "2015-04-28") 

df_EVI %>%
  ggplot(aes(x=date, y=EVI_mean)) + geom_point()

df_EVI$type <- "EVI"


EVI_sd <- read.csv("~/01Master/MasterThesis/Pius/R/sand dam/EVI_sd.csv", header=T)
evi_sd <- EVI_sd %>%
  gather(key=filename, value= EVI_sd, -X) #X = 286 sd_areas/LC

#extract date from filename
split_zosta = strsplit(evi_sd$filename, split="_", fixed=TRUE)
split_z = unlist(lapply(split_zosta, "[[", 4))
evi_sd$date <- lubridate::ymd(basename(split_z))

evi_sd %>%
  na.omit() %>%
  #  filter(X == 1) %>%
  ggplot(aes(x=date, y=EVI_sd)) + geom_point()


df_EVI_c <- merge(df_EVI, evi_sd, by=c("X", "date"))

df_EVI_c %>%
  #  na.omit() %>%
  #  filter(X == 1) %>%
  ggplot(aes(x=date, y=EVI_sd)) + geom_point()

df_EVI_c %>%
  filter(X==100) %>%
  ggplot(aes(x=date, y=EVI_mean, colour=X)) + 
  geom_errorbar(aes(ymin=EVI_mean-EVI_sd, ymax=EVI_mean+EVI_sd), width=.1, color="black") +
  geom_line() +
  geom_point() +
  ylab("EVI")

head(df_EVI_c)

#round dates down to months 
df_EVI_c$date <- as.Date(df_EVI_c$date)
df_EVI_c$year_month <- floor_date(df_EVI_c$date, "month") #round_date 

write.csv(df_EVI_c, "df_EVI.csv")
########################################################
#combine EVI dataset with LC_int data 

df_EVI_c <- read.csv("~/01Master/MasterThesis/Pius/R/sand dam/df_EVI.csv", header=T)
