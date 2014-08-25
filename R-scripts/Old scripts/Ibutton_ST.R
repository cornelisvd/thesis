library(sp)             # Used to create the coordinates for the spatial data frames.
library(rgdal)          # Used for the projection and writing of the KML % SHP files.
library(spacetime)
library(zoo)
library(xts)
library(lubridate)
library(raster)
library(gstat)
library(RColorBrewer)

# General data-loading

dir <- "/home/kees/ibuttons/"
country      <- "Honduras"
humddir      <- paste(dir, "Honduras/primera/humidity/", sep = "")  
files        <- dir(humddir, pattern = ".csv")

# Space - data

farmers.loc  <- read.csv(paste(dir, country, "/", "ibuttons.csv", sep = "")) # read the registration file
completeXY <- which(!is.na(farmers.loc$X | farmers.loc$Y))
farmers.loc <- na.omit(farmers.loc)
coordinates(farmers.loc) = ~X+Y
proj4string(farmers.loc) = "+proj=longlat +datum=WGS84"

# Summarize the total number of succesfully loaded iButtons out of the total

# Time - data

observations <- list()

for (i in completeXY) {
    
    value  <- subset(read.csv(paste(humddir, files[i], sep =""), skip = 19, 
                              header = TRUE), select = c("Date.Time", "Value")) 
    value$Date.Time <- strptime(value$Date.Time, "%m-%d-%y %I:%M:%S %p")
    valuechar <- as.character(value$Date.Time)
    value$Date.Time <- as.POSIXct(valuechar)
    value <- na.omit(value)                            
    value.zoo     <- zoo(value$Value, value$Date.Time)
    observations[[i]] <- value.zoo
}        

# Summarize the percentage of NA and which iButton has > xx% NA (can be removed)

rm(value.zoo, value,  humddir, dir, i, valuechar, files, country)

observations <- observations[!sapply(observations, is.null)] 
humidity <- do.call(merge, observations)
start.date  <- ceiling_date(start(humidity), unit = "hour")
end.date    <- ceiling_date(end(humidity), unit = "hour") 

# Provide information about the duration of the monitored growing season

season.data <- seq(from = start.date, to = end.date, by = as.difftime(1, units="hours"))
season  <- as.data.frame(season.data)
names(season) <- "index"
index  <- zoo(1, season.data)

humd.obs <- na.spline(merge.zoo(index, humidity), maxgap = length(completeXY))
humd.obs <- as.data.frame(merge.zoo(index, humd.obs, all=FALSE))
humd.obs[humd.obs < 0] <- 0 
humd.obs[humd.obs > 100] <- 100 # thresholds

# Provide the percentage of values that were set to NA or 100%    

humd.obs[1] <- season$index
humd.obs[2] <- NULL
colnames(humd.obs) <- c("Date", as.character(farmers.loc$CODE))

rm(completeXY, end.date, start.date, humidity, index, season.data)

# Space - time data merging

# transformation of the humidity or temperature

humd.ok <- humd.obs[, colMeans(is.na(humd.obs[2:ncol(humd.obs)])) <= .25]   # Remove columns with certain % NA
humd.ok$ID13 <- NULL # Not sure why this column is not dropped with the line above, as it is all NA
humd.ok <- na.omit(humd.ok)
humd.cor <- humd.ok[2:ncol(humd.ok)] # A certain statistical transformation can be applied here

# Data-frames will be merged and a spatio-temporal data-frame will be created

farmers.loc  <- farmers.loc[match(names(humd.cor), farmers.loc$CODE), ]
pts <- coordinates(farmers.loc[match(names(humd.cor), farmers.loc$CODE),])
pts <- SpatialPoints(pts, CRS("+proj=longlat +datum=WGS84"))
plot(farmers.loc)

humd.data <- stConstruct(humd.cor, space = list(values = 1:ncol(humd.cor)), 
                         time = humd.ok$Date, SpatialObj = pts, interval = TRUE)

plot(humd.data)

# Space - time interpolation

area <- raster(bbox(farmers.loc))
res(area) <- (1/10)
grd <- as(area, "SpatialPixels")
proj4string(grd) <- proj4string(humd.data)
n <- ceiling(nrow(humd.ok)/(24*7)) # number of interpolations that will be made of the season length
tgrd <- xts(1:n, seq(min(index(humd.data)), max(index(humd.data)), length = n))
pred.grd = STF(grd, tgrd)

v <- vgmST("separable", space = vgm(1, "Exp", 1000000), time = vgm(1, "Exp", 3600 * 24), sill=0.2)
humd.ST <- krigeST(values ~ 1, humd.data, pred.grd, v)

stplot(humd.ST, cuts = 10, col.regions = brewer.pal(10, "RdBu"))

rm(area, observations, humd.cor, pts, n, v, season, tgrd, pred.grd)
