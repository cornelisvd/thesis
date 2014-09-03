##--------------------------------Experiment 1--------------------------------##






##---------------------------------v.01-09-14---------------------------------##

# Set the working directory
setwd ("~/thesis/data/Experiments/")   

# Load libraries & clear the workspace
rm (list = ls(all = TRUE))  ;   
source(".PackageInstall.R")

#
exp.time    <- function(exp     = "Exp1",
                        type    = "Reference",
                        rowskip =  19, 
                        time    = "Date.Time", 
                        val     = "Value",
                        format  = "%d/%m/%y %H:%M:%S",
                        time.un = "hours",
                        unit    = "Temp")
{
    
    ## 
    require(zoo)
    require(ggplot2)
    start    <- as.POSIXct("23/06/14 12:00:00", format = "%d/%m/%y %H:%M:%S")
    end      <- as.POSIXct("30/06/14 11:59:00", format = "%d/%m/%y %H:%M:%S")
    season   <<- seq(from = start, to = end, 
                     by = as.difftime(2, units = time.un))  
    index    <- zoo(1, season) 
    seas     <- index
    
    ##    
    type.dir <- dir(paste(exp,"/",type,"/",unit, sep =""), pattern =".csv")
    type.zoo <- list() 
    
    ##
    for (i in 1:length(type.dir)) {
        v  <- subset(read.csv(paste(exp, "/", type, "/", unit, "/", 
                                    type.dir[i], sep =""), skip = rowskip, 
                              header = TRUE), select = c(time, val)) 
        v$Date.Time <- strptime(v$Date.Time, format)
        vc <- as.character(v$Date.Time)
        v$Date.Time <- as.POSIXct(vc)
        v2 <- v[seq(sample(1:12,1), NROW(v), 12),] 
        v.zoo <- zoo(v2$Value, v2$Date.Time)
        type.zoo[[i]] <- v.zoo
    }  
    
    ##
    type.zoo <- type.zoo[!sapply(type.zoo, is.null)] 
    type.df  <<- do.call(merge, type.zoo)

    ## Interpolate points with NA values
    est.obs   <- na.approx(merge.zoo(index, type.df))
    est.obs   <- as.data.frame(merge.zoo(index, est.obs, all=FALSE))   
    est.obs  <<- est.obs[3:(length(type.dir)+2)]
}


exp.temp.r <- list()
exp.humd <- list()
exp.obs  <- c("Reference", "Type1", "Type2")

for (k in 1:length(exp.obs)) {
    exp.time(type = exp.obs[k], unit = "Temp")
    exp.temp.r[[k]] <- est.obs
}

for (k in 1:length(exp.obs)) {
    exp.time(type = exp.obs[k], unit = "Temp")
    exp.temp.rl[[k]] <- est.obs
}

names(exp.temp.r) <- exp.obs
random.hour <- as.matrix(exp.temp.r$Type2)
random.hour.l <- as.matrix(exp.temp.r$Type1)
layer <- exp.temp.r$Type2

layer <- exp.temp.r$Type1
plotlayers(ylabel = "degrees Celsius", ylimit = c(15, 30), 
           title = "8. Type 1 (lower resolution): cubic spline of temperature observations at hourly interval (random)") 
layer <- exp.temp.r$Type2
plotlayers(ylabel = "degrees Celsius", ylimit = c(15, 30), 
           title = "6. Type 2 (higher resolution): cubic spline of temperature observations at hourly interval (random)") 

