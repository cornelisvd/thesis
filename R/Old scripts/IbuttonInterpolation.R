################################  Information  #################################
#                                                                              #
#                                                                              #    
#                                                                              #
#                                                                              #                                                        
#                                                                              #
#                                                                              #
################################################################################
#                                                                v.2013-06-11  #
#################################  Libraries  ##################################

    library(spacetime)       # Used to create (STFDF & EOF) time-space objects
    library(zoo)             # Used to create a large number of zoo-objects 
    library(lubridate)       # Used to round the time-object for the seasons
    library(splancs)         # Used to create a chull of the coordinates
    library(gstat)           # Used to create the variogram and kriging
    library(sp)              # Used to create spatial objects from X/Y values
    library(RColorBrewer)    # Used to create the color-scheme for the plots

############################## Function  variables  ############################ 

    # Temperature / humidity    ---  Input in the obs.dir (loading data)
    # Allowed NA % per sensor   ---  Input in Space - time data merging     
    # Season start / end date   ---  Space - time data merging and data plotting
    # Upper / lower threshold   ---  Input for threshold calculations

###############################  Loading data  #################################

rm(list = ls())                                           # Clear working space
    dir <- "/home/kees/ibuttons/Honduras/"                # Location of project
        obs.dir <- paste(dir, "primera/",                 # Change season
                              "humidity/", sep = "")      # Humidity/temperature
            files <- dir(obs.dir, pattern = ".csv")       # Files in folder

###############################  Space - data  #################################

farmers.loc  <- read.csv(paste(dir, "/", "ibuttons.csv", sep = "")) 
    total.ob <- nrow(farmers.loc)     
        complete.XY <- which(!is.na(farmers.loc$X | farmers.loc$Y))
            ok.xy <- length(complete.XY)
                farmers.loc <- na.omit(farmers.loc) 
                    coordinates(farmers.loc) = ~X+Y
                        proj4string(farmers.loc) = "+proj=longlat +datum=WGS84"

################################  Time - data  #################################

observations <- list()              # Empty list in which zoo-objects are looped 

    for (i in complete.XY) {
    
        value  <- subset(read.csv(paste(obs.dir, files[i], sep =""), skip = 19, 
                        header = TRUE), select = c("Date.Time", "Value")) 
            value$Date.Time <- strptime(value$Date.Time, "%m-%d-%y %I:%M:%S %p")
                valuechar <- as.character(value$Date.Time)
                    value$Date.Time <- as.POSIXct(valuechar)
        value <- na.omit(value)                            
            value.zoo     <- zoo(value$Value, value$Date.Time)
                observations[[i]] <- value.zoo
    }        
                         rm(value.zoo, value, obs.dir, dir, i, valuechar, files)

observations <- observations[!sapply(observations, is.null)] 
    obs.df <- do.call(merge, observations)
        start.date  <- ceiling_date(start(obs.df), unit = "hour")
            end.date    <- ceiling_date(end(obs.df), unit = "hour") 

##############################  Season data  ###################################

season.data <- seq(from = start.date, to = end.date,  # Difftime can be set to:
               by = as.difftime(1, units="hours"))    # mins, hours, days, weeks
    season  <- as.data.frame(season.data)
        names(season) <- "index"
            index  <- zoo(1, season.data)             # Period of measurement

spline.obs <- na.spline(merge.zoo(index, obs.df), maxgap = length(complete.XY))
    spline.obs <- as.data.frame(merge.zoo(index, spline.obs, all=FALSE))
        spline.obs[1] <- season$index
        spline.obs[2] <- NULL
            colnames(spline.obs) <- c("Date", as.character(farmers.loc$CODE))

obs.ok <- spline.obs[, colMeans(!is.na(spline.obs[2:ncol(spline.obs)])) >= .75]       
    obs.ok$ID13 <- NULL # This is an error, should be removed
        ok.col <-length(obs.ok[2:ncol(obs.ok)])
            tot.val <- sum(!is.na(spline.obs))
                bad.val <- length(which(spline.obs > 100 | (spline.obs < 0)))
                    na.val <- sum(is.na(spline.obs))
                        obs.ok <- na.omit(obs.ok)
                            spline.obs[spline.obs < 0] <- 0 
                               spline.obs[spline.obs > 100] <- 100 # thresholds
       
               rm(season.data, complete.XY, end.date, start.date, obs.df, index)                

#########################  Space - time data merging  ##########################

obs.cor <- obs.ok[2:ncol(obs.ok)]       # Apply a certain statistical correction

farmers.loc  <- farmers.loc[match(names(obs.cor), farmers.loc$CODE), ]
    pts <- coordinates(farmers.loc[match(names(obs.cor), farmers.loc$CODE),])
        pts <- SpatialPoints(pts, CRS("+proj=longlat +datum=WGS84"))
            ch <- chull(farmers.loc$X, farmers.loc$Y)
                ch <- c(ch, ch[1])
                    border <- cbind(farmers.loc$X[ch], farmers.loc$Y[ch]) 
                        xy.grid <- as.data.frame(gridpts(border, 1000)) 
                            gridded(xy.grid) = ~V1+V2
                                proj4string(xy.grid) <- proj4string(pts)

final.data <- stConstruct(obs.cor, space = list(values = 1:ncol(obs.cor)), 
                    time = obs.ok$Date, SpatialObj = pts, interval = TRUE)
       final.data = final.data[, "2013-06-25"] # Select a date or period
           
###############################  Data plotting  ################################

datum <- strsplit(as.character(obs.ok$Date), " ")
    datum.df <- data.frame(do.call(rbind, datum)) 
        hour <-  strsplit(as.character(datum.df$X2), ":")
            hour.df <- data.frame(do.call(rbind, hour)) 

obs.ok <- obs.ok[grep("2013-06-25", obs.ok$Date),]   # Same period as final.data

    meanobs   <- rowMeans(obs.ok[2:ncol(obs.ok)], na.rm = TRUE)  
        minobs    <- apply(obs.ok[2:ncol(obs.ok)], 1, min, na.rm = TRUE) 
            maxobs    <- apply(obs.ok[2:ncol(obs.ok)], 1, max, na.rm = TRUE)

obs.stat <- data.frame(obs.ok$Date, meanobs, minobs, maxobs) 
    hour <- list(obs.stat$obs.ok.Date)
        mean.obs <- aggregate(as.numeric(obs.stat$meanobs), by=hour, FUN = mean)
        min.obs <- aggregate(as.numeric(obs.stat$minobs),  by=hour, FUN = min)
        max.obs <- aggregate(as.numeric(obs.stat$maxobs),  by=hour, FUN = max) 
            clim.stat <- data.frame(hour, mean.obs$x, min.obs$x, max.obs$x) 
                colnames(clim.stat) <- c("Hour", "MeanOb", "MinOb", "MaxOb")

plot(clim.stat$Hour, clim.stat$MeanOb, type="l", col="black", main = 
         "Diurnal humidity range (% RH)", xlab = "Hour (local time)", 
            ylab = "Relative humidity", ylim=c(0,100))
                lines(clim.stat$Hour, clim.stat$MaxOb, type="l", col="red")
                    lines(clim.stat$Hour, clim.stat$MinOb, type="l", col="blue")
                        legend("bottomright", legend=c("Mean", "Max", "Min"), 
                            col=c('black','red','blue'), lty=1,lwd=1.5) 

#########################  Space - time interpolation  #########################

n <- 24 # number of interpolations of selected period
    tgrd <- seq(min(index(final.data)), max(index(final.data)), length=n)
        pred.grd = STF(xy.grid, tgrd)
            v <-  vgmST("separable", space = vgm(1.2, "Sph", 100000), 
                         time  = vgm(0.9, "Lin", n * 3600), 
                         sill  = 0.1)

final.ST <- krigeST(values ~ 1, final.data, pred.grd, v)
    final.EOF = EOF(final.ST)

##############################  Plotting of data  ##############################

    stplot(final.ST, cuts = 10, col.regions = brewer.pal(10, "RdBu"))
            
                    rm(list=objects(pattern="obs"), border, datum, datum.df, ch)
                    rm(pred.grd, pts, tgrd, v,hour.df, n, hour, xy.grid, season)

##############################  Quality indicators  ############################

cat(total.ob, "Farmers have particiated in the study. Out of the total", ok.xy, 
    "have included complete XY coordinates and", ok.col, "have < 25% NA.", 
    tot.val, "values can be used, while", bad.val, "are NA or outside limits.")

                           rm(total.ob, ok.xy, tot.val, bad.val, na.val, ok.col)

############################  Threshold calculations  ##########################


#####################################  End  ####################################