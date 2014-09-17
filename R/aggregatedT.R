##--------------------------------Load-sensors--------------------------------##
## Function to create a single zoo-object from the *.csv files of micro-sensors.
## This is the first step and requires the setting of the correct time - format,
## the time-unit can also be changed from 'hours' to other options (eg. 'mins').
## Main output of this code will be a corrected time-series of all used sensors.



##---------------------------------v.25-08-14---------------------------------##

# Load libraries & clear the workspace
rm (list = ls(all = TRUE))     
library(lubridate)
library(quantreg)
library(splines)
library(zoo)

# Set the working directory
setwd ("~/thesis/data/")   

# Add the folders with the sensor data
folder        <-  "Aquiares"
# period      <-  "primera" 
registry      <-  "iButtonIDs.csv"

# Select temperature and/or humidity 
temperature   <- TRUE
humidity      <- TRUE
units         <-  c("temperature", "humidity")

# Load (all) data from the sensors
load.sensors <- function (format = "%d/%m/%y %I:%M:%S %p",  # Date format
                          unit        = "hours",            # Time units
                          na.estimate = na.spline,          # Interpolation
                          sensor      = "iButton")          # Sensor type
    ## Information about how to adjust the date-time format can be found at: 
    ## https://stat.ethz.ch/R-manual/R-patched/library/base/html/strptime.html
{
    
    ## This code chunk can be adjusted to other sensors
    if (sensor  == "iButton") {
        rowskip <- 19 
        time    <- "Date.Time" 
        val     <- "Value" 
    }
    
    ## Reading of *.csv and file with coordinates
    reg      <- paste(folder, "/", registry, sep = "")
    loc     <<- read.csv(reg) 
    obs      <- nrow(loc)     
    com      <- which(!is.na(loc$X | loc$Y))
    ok.loc   <- length(com)
    loc      <- na.omit(loc) 
    
    ## Creation of two empty lists
    list.ok  <- list()
    list.cor <- list() 
    
    ## Create zoo-objects for observations
    for (i in units) {
        dir      <- paste(folder, "/", i, "/", sep = "")
        f        <- dir(dir, pattern = ".csv") 
        s        <- unlist(strsplit(f, "[.]"))
        m        <- match(s[1:length(s) %% 2 == 1], loc$Number) 
        nm      <<- s[1:length(s) %% 2 == 1]
        obs.zoo  <- list()   
        
        ### Load data of points with XY coordinates
        for (j in 1:length(m)) {
            v  <- subset(read.csv(paste(dir, f[j], sep =""), skip = rowskip, 
                                  header = TRUE), select = c(time, val)) 
            
            
            if (i == "temperature") {        
                load("~/thesis/data/Experiments/fit.T.Rdata")
                correction <- c()
                for (k in 1:length(v$Value)) {
                    if (v$Value[k] < fit$coefficients[1]) { 
                        correction[k] <- c(v$Value[k])
                    } else {
                        x <- suppressWarnings(predict(fit, 
                                                      data.frame(pipedata = v$Value[k])))
                        correction[k] <- x
                    }}}
            if (i == "humidity") {      
                load("~/thesis/data/Experiments/fit.H.Rdata")
                correction <- c()
                for (k in 1:length(v$Value)) {
                    if (v$Value[k] < fit$coefficients[1]) { 
                        correction[k] <- c(v$Value[k])
                    } else {
                        x <- suppressWarnings(predict(fit, 
                             data.frame(pipedata = v$Value[k])))
                        correction[k] <- x
                    }}}       
            
            v$Value       <- correction
            v$Date.Time   <- strptime(v$Date.Time, format)
            vc            <- as.character(v$Date.Time)
            v$Date.Time   <- as.POSIXct(vc) 
            #v             <- na.omit(v)                            
            v.zoo         <- zoo(v$Value, v$Date.Time)
            obs.zoo[[j]]  <- v.zoo
        }    
        
        ### Create season and add data
        obs.zoo  <- obs.zoo[!sapply(obs.zoo, is.null)] 
        obs.df   <- do.call(merge, obs.zoo)
        start    <- ceiling_date(start(obs.df), unit = "hour")
        end      <- ceiling_date(end(obs.df), unit = "hour") 
        season   <- seq(from = start, to = end, 
                        by = as.difftime(1, units = unit))  
        index    <- zoo(1, season) 
        seas    <<- index
        
        ### Interpolate points with NA values
        est.obs  <- na.estimate(merge.zoo(index, obs.df), maxgap = length(m))
        est.obs  <- as.data.frame(merge.zoo(index, est.obs, all=FALSE))   
        est.obs[est.obs > 100] <- 100
        est.obs[1] <- as.POSIXct(rownames(est.obs))
        est.obs[2] <- NULL
        names(est.obs) <- c("Date", as.character(s[1:length(s) %% 2 == 1]))
        list.ok[[i]]  <- est.obs     
    }    
    
    ## Add names to lists and add to global environment
    names(list.ok)  <- units
    list.ok        <<- list.ok
    obs.ok         <<- est.obs
    return(cat(length(m), "sensors have been loaded.")) 
    save(list.ok, file = "list.ok.Rdata") 
}

x   <- unlist(strsplit(as.character(obs.ok$Date), " "))
x   <- unlist(strsplit(as.character(x), ":"))
b <- x[seq(2, length(x), 4)]
obs.ok$Date <- b
meantemp <- data.frame(c(0:23))
for (i in 2:length(obs.ok)){
    x <- aggregate(obs.ok[i], by = list(obs.ok$Date), FUN = mean, na.rm=TRUE)
    meantemp <- cbind(meantemp, x[,2])
}
names(meantemp) <- c("Hour", names(obs.ok[2:length(obs.ok)]))

