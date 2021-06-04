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

#load study area
studyarea <- shapefile("~/01Master/MasterThesis/Pius/climatedata/extent_rough.shp") #rough extent - for crop()

# Load in pre-processed land cover dataset/shapefile
    #SD = sand dam ID (n=135)
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

      # #zonalstats()
      # #install.packages("spatialEco") # my Rversion is too old
      # #remotes::install_github("jeffreyevans/spatialEco")
      # #install.packages("exactextractr")
      # library(spatialEco)
      # library(exactextractr)
      # 
      # zs_prec_mean <- zonal.stats(x=lc_int, y=prec_proj, stats = "mean") 
      # zs_prec_sd <- zonal.stats(x=lc_int, y=prec_proj, stats = "sd") 
      # # zs_prec_min <- zonal.stats(x=lc_int, y=prec_proj, stats = "min") 
      # # zs_prec_max <- zonal.stats(x=lc_int, y=prec_proj, stats = "max") 
      # 
      # zs_prec_mean <- zonal.stats(x=lc_int, y=prec_proj, stats = "mean", na.rm=TRUE) 
      # #bring into neat datatable 


write.csv(prec_mean, "prec_mean.csv")

prec_sd <-exactextractr::exact_extract(prec_proj, lc_int, "stdev")
write.csv(prec_sd, "prec_sd.csv")

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

###
prec_sd_m <- read.csv("~/01Master/MasterThesis/Pius/R/sand dam/prec_sd.csv", header=T)
p_sd_m <- prec_sd_m %>%
  gather(key=filename, value= Precip_sd, -X) #X = 286 sd_areas/LC

#data wrangling, extract date from filename
split_prec = strsplit(p_sd_m$filename, split="X", fixed=TRUE)
split_p = unlist(lapply(split_prec, "[[", 2))
p_sd_m$date <- lubridate::ymd(basename(split_p))

#plot
p_sd_m %>%
  #na.omit() %>%
  #  filter(X == 1) %>%
  ggplot(aes(x=date, y=Precip_sd)) + geom_point()+
  ylab("Precipitation SD (monthly)")

df_prec_m <- merge(p_mean_m, p_sd_m, by= c("X", "date"))

df_prec_m$date <- as.Date(df_prec_m$date)

df_prec_m %>% 
#  filter (X == 1) %>%
# filter (date >= "2015-01-01" & date <= "2015-12-31") %>%
  ggplot(aes(x=date, y=Precip_mean, colour=X)) + 
  geom_errorbar(aes(ymin=Precip_mean-Precip_sd, ymax=Precip_mean+Precip_sd), width=.1) +
  geom_line() +
  geom_point()

head(df_prec_m)

#round dates down to months 
df_prec_m$date <- as.Date(df_prec_m$date)
df_prec_m$year_month <- floor_date(df_prec_m$date, "month") #round_date 

write.csv(df_prec_m, "df_prec_monthly.csv")

df_prec_m <- read.csv("~/01Master/MasterThesis/Pius/R/sand dam/df_prec_monthly.csv", header=T)

#merge EVI and precipitation data
df <- merge(df_EVI_c, df_prec_m, by=c("X", "year_month"))

#select only essential columns
subdf <- df %>%
  select(X, year_month, EVI_mean, EVI_sd, Precip_mean, Precip_sd)

#merge with info from lc_int
lc_int <- st_read(dsn = "~/01Master/MasterThesis/Pius/geodata", layer = 'lc_int')

lc_int <- cbind(rn = rownames(lc_int), lc_int)

sub_lcint <- lc_int %>%
  select(LC_proj, rn, ID, Point_X, Point_Y, Mnth_Cn, Yer_blt, Mnth_Us, Year_Us, Functin, Site, study, class, area_m2)
sub_lcint <- sub_lcint %>%
  rename(X = "rn")

df_c <- merge(df, sub_lcint, by="X")
class(df_c)


save(df_c, file = "df_c.RData") #write.csv() takes way more time and space 
load("df_c.RData")
        # 
        # library(data.table) 
        # fwrite(df, "df_1.csv", col.names = TRUE, sep="\t")
        # 
        # df_cc <- fread("~/01Master/MasterThesis/Pius/R/sand dam/df_1.csv", sep="\t")
        # head(df_cc)

#DATE

#Year
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
unique(df_c$date_b) #33
unique(df_date_built$d_built) #71 (without NAs)

#some results got lost? check again 

#Presence column
class(df_c$year_month)
df_c$year_month <- as.Date(df_c$year_month, "%Y-%m-%d")

df_c<- df_c %>%
  mutate(presence = if_else(year_month %m+% years(1) >= d_built , 1, 0)) 
#1 year to fill up sand dam (partially) - can take up to 1-3 years (literature)
# stay with this decision of 1 year? 



        # #Precipitation
        # # # # # # # #
        # #  [DAILY]  #
        # # # # # # # #
        # 
        # #TAMSAT rainfall estimates 
        # tamsat_brick_day <- brick("~/01Master/MasterThesis/Pius/climatedata/TAMSAT_daily/01-tamsatDaily.v3.1-944006400-1612137600_37.65_38.45_-2.2_-1.0.nc", varname="rfe") #or rfefilled
        # tamsat_brick_day <- crop(tamsat_brick_day, extent(studyarea), snap="out")
        # 
        # #subset 2014-2020
        # prec_day <- subset(tamsat_brick_day, which(getZ(tamsat_brick_day) >= '2014-01-01' & (getZ(tamsat_brick_day) <= '2020-12-31')))
        # crs(tamsat_brick_day)
        # 
        # #reproject
        # prec_proj_d <- projectRaster(prec_day, crs="+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
        # 
        # #zonal statistics 
        # # claculate the mean Precipitation for each area per time 
        # system.time({ prec_mean_day <-exactextractr::exact_extract(prec_proj_d, lc_int, "mean") })
        # write.csv(prec_mean_day, "prec_mean_daily.csv")
        # 
        # prec_sd_day <-exactextractr::exact_extract(prec_proj_d, lc_int, "stdev")
        # write.csv(prec_sd_day, "prec_sd_daily.csv")
        # 
        # prec_mean_day <- read.csv("~/01Master/MasterThesis/Pius/R/sand dam/prec_mean_daily.csv", header=T)
        # prec_sd_day <- read.csv("~/01Master/MasterThesis/Pius/R/sand dam/prec_sd_daily.csv", header=T)
        # 
        # p_mean_d <- prec_mean_day %>%
        #   gather(key=filename, value= Precip_mean, -X) #X = 286 sd_areas/LC
        # 
        # #data wrangling, extract date from filename
        # split_prec = strsplit(p_mean_d$filename, split="X", fixed=TRUE)
        # split_p = unlist(lapply(split_prec, "[[", 2))
        # p_mean_d$date <- lubridate::ymd(basename(split_p))
        # 
        # p_mean_d$type <- "prec"
        # 
        # #plot
        # p_mean_d %>%
        #   #na.omit() %>%
        #   #  filter(X == 1) %>%
        #   ggplot(aes(x=date, y=Precip_mean)) + geom_point()+
        #   ylab("Precipitation (daily)")
        # 
        # ###
        # prec_sd_d <- read.csv("~/01Master/MasterThesis/Pius/R/sand dam/prec_sd_daily.csv", header=T)
        # p_sd_d <- prec_sd_d %>%
        #   gather(key=filename, value= Precip_sd, -X) #X = 286 sd_areas/LC
        # 
        # #data wrangling, extract date from filename
        # split_prec = strsplit(p_sd_d$filename, split="X", fixed=TRUE)
        # split_p = unlist(lapply(split_prec, "[[", 2))
        # p_sd_d$date <- lubridate::ymd(basename(split_p))
        # 
        # #plot
        # p_sd_d %>%
        #   #na.omit() %>%
        #   #  filter(X == 1) %>%
        #   ggplot(aes(x=date, y=Precip_sd)) + geom_point()+
        #   ylab("Precipitation SD (daily)")
        # 
        # df_prec_d <- merge(p_mean_d, p_sd_d, by= c("X", "date"))
        # 
        # df_prec_d %>%
        #   #  filter (X == 1) %>%
        #   # filter (date >= "2015-01-01" & date <= "2015-12-31") %>%
        # ggplot(aes(x=date, y=Precip_mean, colour=X)) + 
        #   geom_errorbar(aes(ymin=Precip_mean-Precip_sd, ymax=Precip_mean+Precip_sd), width=.1) +
        #   geom_line() +
        #   geom_point()
        # 
        # head(df_prec_d)
        # write.csv(df_prec_d, "df_prec_daily.csv")
        # p_day <- read.csv("~/01Master/MasterThesis/Pius/R/sand dam/df_prec_daily.csv", header=T)
        # 
        # head(p_day)




##########

  # VERALTET???

    # # Load in pre-processed land cover dataset/shapefile
    # #SD = sand dam ID (n=135)
    # #LC_proj = land cover class [value]
    # #class = LC class [name]
    # lc_int <- st_read(dsn = "~/01Master/MasterThesis/Pius/geodata", layer = 'lc_int')
    # crs(lc_int)
    # lc_int <- cbind(rn = rownames(lc_int), lc_int)
    # 
    # sub_lcint <- lc_int %>%
    #   select(LC_proj, rn, ID, Point_X, Point_Y, Mnth_Cn, Yer_blt, Mnth_Us, Year_Us, Functin, Site, study, class, area_m2)
    # sub_lcint <- sub_lcint %>%
    #   rename(X = "rn")
    # 
    # df <- merge(df_EVI_c, sub_lcint, by="X")
    #     
    #     library(data.table) 
    #     fwrite(df, "df_1.csv", col.names = TRUE, sep="\t")
    #     
    #     df_cc <- fread("~/01Master/MasterThesis/Pius/R/sand dam/df_1.csv", sep="\t")
    #     head(df_cc)

      # 
      # 
      # 
      # # TRASH #
      # ######################
      # #############
      # df <- df %>%
      #   select(- c("filename.x", "filename.y"))
      # 
      # #plot mean precipitation and mean EVI 
      # sub <- df %>% 
      #   filter(evi_date >= "2015-01-01" & evi_date < "2016-01-01") #%>%
      #   ggplot() + geom_point(data=sub, aes(x=evi_date, y=EVI_mean, color="EVI")) 
      # subp <- df %>%
      #   filter(p_date >= "2015-01-01" & p_date < "2016-01-01")
      #   ggplot() + geom_point(data=subp,aes(x=p_date, y=Precip_mean, colour="Precipitation"))
      # 
      # df %>%
      #   filter( X == 1) %>%
      #   ggplot() + geom_point(aes(x=evi_date, y=EVI_mean, color="EVI")) +
      #   geom_point(aes(x=p_date, y=Precip_mean, colour="Precipitation")) +
      #   facet_wrap(~type,  ncol=1)
      # 
      # 
      # ggplot(df_EVI, aes(x=date, y=EVI_mean)) + geom_point() +
      #   geom_point(data=p_mean, colour='blue') + xlim(0,10)
      # 
      # #####################################################
      # # LONG FORMAT
      # 
      # #combine EVi and prec data
      # df_EVI <- df_EVI %>%
      #   rename(Value = "EVI_mean") #date = "evi_date",
      # 
      # p_mean <- p_mean %>%  
      #   rename(Value = "Precip_mean") #date = "p_date"
      # 
      # df_long <- rbind(df_EVI, p_mean)
      # head(df_long)
      # 
      # p <- df_long %>%
      #   filter( X == 100) %>%
      #   ggplot() + geom_line(aes(x=date, y=Value)) +
      #   facet_wrap(~type, scales="free_y", ncol=1)
      # 
      # 
      # #incorporate year sand dam was built
      # #add Sand Dam ID and Year.built somehow [incorporate 3 years delay?]
      # sub_lcint <- lc_int %>%
      #   select(LC_proj, X, ID, Point_X, Point_Y, Mnth_Cn, Yer_blt, Mnth_Us, Year_Us, Functin, Site, study, class, area_m2) #caution with area_m2 [CHECK]
      # 
      # df_explicit <- merge(df_long, sub_lcint, by="X")
      # 
      # df_explicit$Yer_blt <- as.Date(df_explicit$Yer_blt, "%Y")
      # 
      # plot <- df_explicit %>%
      #   filter( X == 2) %>%
      #   ggplot(aes(x=date, y=Value)) + geom_line() + geom_smooth(method="loess") +
      #   geom_vline(aes(xintercept = Yer_blt), data = df_explicit %>% filter(X==1), linetype="dotted", color="blue") +
      #   facet_wrap(~type, scales="free_y", ncol=1)

