library(sp)
library(sf)
library(rgdal)
library(raster)
library(mapview)
library(ncdf4)
library(rgeos)

library(tidyr)
library(ggplot2)
library(dplyr)
library(lubridate)

#load study area
studyarea <- shapefile("~/01Master/MasterThesis/Pius/climatedata/extent_rough.shp") #rough extent - for crop()

# Load in pre-processed land cover dataset/shapefile
#SD = sand dam ID (n=173)
#LC_proj = land cover class [value]
#class = LC class [name]
lc_int <- st_read(dsn = "~/01Master/MasterThesis/Pius/geodata", layer = 'lc_int')
crs(lc_int)

#Precipitation
# # # # # # #
#[MONTHLY]  #
# # # # # # #

#TAMSAT rainfall estimates 
tamsat_brick <- brick("~/01Master/MasterThesis/Pius/climatedata/TAMSAT/04-tamsatMonthly.v3.1-946684800-1609459200_-20.0_38.5_-2.2_-1.0.nc", varname="rfe") #or rfefilled
tamsat_brick <- crop(tamsat_brick, extent(studyarea), snap="out")

#subset 2014-2020
prec <- subset(tamsat_brick, which(getZ(tamsat_brick) >= '2014-01-31' & (getZ(tamsat_brick) <= '2020-12-31')))
crs(tamsat_brick)

#reproject
prec_proj <- projectRaster(prec, crs="+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

#zonal statistics 
# calculate the mean Precipitation for each area per time 

prec_mean <-exactextractr::exact_extract(prec_proj, lc_int, "mean")

write.csv(prec_mean, "prec_mean.csv")

###########
prec_mean_m <- read.csv("~/01Master/MasterThesis/Pius/R/sand dam/prec_mean.csv", header=T)

p_mean_m <- prec_mean_m %>%
  gather(key=filename, value= Precip_mean, -X) #X = 286 sd_areas/LC

#data wrangling, extract date from filename
split_prec = strsplit(p_mean_m$filename, split="X", fixed=TRUE)
split_p = unlist(lapply(split_prec, "[[", 2))
p_mean_m$date <- lubridate::ymd(basename(split_p))

p_mean_m$type <- "prec"

#plot
p_mean_m %>%
  #na.omit() %>%
  #  filter(X == 1) %>%
  ggplot(aes(x=date, y=Precip_mean)) + geom_point()+
  ylab("Precipitation (monthly)")

p_mean_m$date <- as.Date(p_mean_m$date)

p_mean_m %>% 
  #  filter (X == 1) %>%
  # filter (date >= "2015-01-01" & date <= "2015-12-31") %>%
  ggplot(aes(x=date, y=Precip_mean, colour=X)) + 
  geom_line() +
  geom_point()

head(p_mean_m)

#round dates down to months 
p_mean_m$date <- as.Date(p_mean_m$date)
p_mean_m$year_month <- floor_date(p_mean_m$date, "month") #round_date 

write.csv(p_mean_m, "prec_all.csv")

df_prec_m <- read.csv("~/01Master/MasterThesis/Pius/R/sand dam/prec_all.csv", header=T)

#merge EVI and precipitation data
df <- merge(df_EVI, df_prec_m, by=c("X", "year_month"))

#select only essential columns
subdf <- df %>%
  select(X, year_month, EVI_mean, Precip_mean)

#merge with info from lc_int
lc_int <- st_read(dsn = "~/01Master/MasterThesis/Pius/geodata", layer = 'lc_int')

lc_int <- cbind(rn = rownames(lc_int), lc_int)

sub_lcint <- lc_int %>%
  select(LC_proj, rn, ID, Point_X, Point_Y, Mnth_Cn, Yer_blt, Mnth_Us, Year_Us, Functin, Site, study, class, area_m2)
sub_lcint <- sub_lcint %>%
  rename(X = "rn")

df_c <- merge(df, sub_lcint, by="X")
class(df_c)


save(df_c, file = "df_c_all.RData") #write.csv() takes way more time and space 
load("df_c_all.RData")


#DATE
class(df_c$Yer_blt) 

df_c$Yer_blt <- as.Date(df_c$Yer_blt, "%Y")
df_c$Yer_blt <- lubridate::year(df_c$Yer_blt)

#incorpotate information about the Month of construction
df_c$Mnth_Cn <- as.integer(factor(df_c$Mnth_Cn, levels = month.name))

df_c$date_b <- lubridate::ymd(df_c$Yer_blt, truncated = 2L)

df_c$date_built <- if_else(df_c$Mnth_Cn != "NA",true=as.Date(with(df_c,paste(Yer_blt,Mnth_Cn,"01",sep="-")),"%Y-%m-%d"), 
                           false=df_c$date_b)

df_c <- df_c %>%
  mutate(d_built = if_else(is.na(date_built), date_b, date_built))

unique(df_c$date_built) #50
unique(df_c$date_b) #42
unique(df_c$d_built) #82 

#some results got lost? check again 

#Presence column
class(df_c$year_month)
df_c$year_month <- as.Date(df_c$year_month, "%Y-%m-%d")

df_cc<- df_c %>%
  mutate(presence = if_else(year_month %m+% years(1) >= d_built , 1, 0)) 
#1 year to fill up sand dam (partially) - can take up to 1-3 years (literature)
# stay with this decision of 1 year? 

#save 
save(df_cc, file = "df_cc_all.RData") #write.csv() takes way more time and space 
load("df_cc_all.RData")
head(df_cc)

# TIME: create column of discrete time steps (earliest date 01/01/2014 = 1)
df_cc <- df_cc %>%
  transform(timeorder=as.numeric(factor(year_month)))

class(df_cc$timeorder)
unique(df_cc$timeorder)
unique(df_cc$year_month)

min(df_cc$year_month)

# reshape LC_proj
# [4,2] -> [2,1] 2=cropland [4], 1 =shrubs [2]
df_cc <- df_cc %>%
  transform(LC_proj=as.numeric(factor(LC_proj)))

#rescale evi values 
df_cc$EVI_mean <- df_cc$EVI_mean / 10000

#boil down dataset to the most important columns as input for the model 
df_com <- df_cc %>%
  select(-geometry, -type.x, -type.y, -filename.x, -filename.y)

head(df_cc)
head(df_com)
unique(df_com$ID)
unique(df_com$X)

#save 
save(df_com, file = "df_com_all.RData") #write.csv() takes way more time and space 
load("df_com_all.RData")
head(df_com)
