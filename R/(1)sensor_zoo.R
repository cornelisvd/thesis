##--------------------------------Load-sensors--------------------------------##
## Function to create a single zoo-object from the *.csv files of micro-sensors.
## This is the first step and requires the setting of the correct time - format,
## the time-unit can also be changed from 'hours' to other options (eg. 'mins').
## Main output of this code will be a corrected time-series of all used sensors.
##---------------------------------v.25-08-14---------------------------------##

# Load libraries & clear the workspace
    require(zoo)
    require(lubridate)   
    rm (list = ls(all = TRUE)) 
    
# Set the working directory
    setwd ("~/thesis/data/")   
    
# Add the folders with the sensor data
    folder        <-  "Honduras"
    period        <-  "primera" 
    registry      <-  "ibuttons.csv"
    
# Select temperature and/or humidity 
    temperature   <- TRUE
    humidity      <- TRUE
    units         <-  c("temperature", "humidity")
    proj          <- "+proj=longlat +datum=WGS84"

# Load (all) data from the sensors
    load.sensors <- function (format = "%m-%d-%y %I:%M:%S %p",  ## Date format
                              unit        = "hours",            ## Time units
                              na.allowed  = 75,                 ## % NA allowed
                              na.estimate = na.spline,          ## Interpolation
                              sensor      = "iButton")          ## Sensor type
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
            dir <- paste(folder, "/", period, "/", i, "/", sep = "")
            f   <- dir(dir, pattern = ".csv") 
            obs.zoo  <- list()   
        
        ### Load data of points with XY coordinates
            for (j in com) {
                v  <- subset(read.csv(paste(dir, f[j], sep =""), skip = rowskip, 
                                      header = TRUE), select = c(time, val)) 
                v$Date.Time   <- strptime(v$Date.Time, format)
                vc            <- as.character(v$Date.Time)
                v$Date.Time   <- as.POSIXct(vc) 
                v             <- na.omit(v)                            
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
            est.obs  <- na.estimate(merge.zoo(index, obs.df), maxgap = length(com))
            est.obs  <- as.data.frame(merge.zoo(index, est.obs, all=FALSE))   
            est.obs[est.obs > 100] <- 100
            est.obs[1] <- as.POSIXct(rownames(est.obs))
            est.obs[2] <- NULL
            names <- unlist(strsplit(f[com], "[.]"))
            ID <- names[1:length(names) %% 2 == 1]
            names(est.obs) <- c("Date", ID)
            list.ok[[i]]  <- est.obs     
        }    
    
    ## Add names to lists and add to global environment
        names(list.ok)  <- units
        list.ok        <<- list.ok
        obs.ok         <<- est.obs
        return(cat(obs, "Farmers have particiated in the study. Out of the total", 
                   ok.loc, "have included complete XY coordinates."))
    }
       
        # Remove data with too little data
        # obs.ok <- est.obs[, colMeans(!is.na(est.obs[2:ncol(est.obs)])) 
        #                 >= (100-na.allowed)/100]
        # obs.ok$"013"   <- NULL # This is an error, should be removed
        # obs.cor       <- na.omit(obs.ok)
        #list.ok[[i]]  <- obs.ok
        # list.cor[[i]] <- obs.cor
        # names(list.cor) <- units
        # list.cor       <<- list.cor 