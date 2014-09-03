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
                            time.un = "mins",
                            unit    = "Temp")
    {
       
    ## 
        require(zoo)
        require(ggplot2)
        start    <- as.POSIXct("23/06/14 12:00:00", format = "%d/%m/%y %H:%M:%S")
        end      <- as.POSIXct("30/06/14 11:59:00", format = "%d/%m/%y %H:%M:%S")
        season   <<- seq(from = start, to = end, 
                        by = as.difftime(2, units = "hours"))  
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
        v <- na.omit(v)                            
        v.zoo <- zoo(v$Value, v$Date.Time)
        type.zoo[[i]] <- v.zoo
    }  
    
    ##
        type.zoo <- type.zoo[!sapply(type.zoo, is.null)] 
        type.df  <- do.call(merge, type.zoo)
          
    ## Interpolate points with NA values
        est.obs   <- na.spline(merge.zoo(index, type.df))
        est.obs   <- as.data.frame(merge.zoo(index, est.obs, all=FALSE))   
        est.obs  <<- est.obs[3:(length(type.dir)+2)]
    }
    
    
    exp.temp <- list()
    exp.humd <- list()
    exp.obs  <- c("Reference", "Type1", "Type2")
    
    for (k in 1:length(exp.obs)) {
        exp.time(type = exp.obs[k], unit = "Temp")
        exp.temp[[k]] <- est.obs
    }
    
    for (l in 1:length(exp.obs)) {
        exp.time(type = exp.obs[l], unit = "Humd")
        exp.humd[[l]] <- est.obs
    }      
    
    names(exp.temp) <- exp.obs
    names(exp.humd) <- exp.obs
  
    
    exp.temp.h <- list()
    exp.humd.h <- list()
    exp.obs  <- c("Reference", "Type1", "Type2")
    
    for (k in 1:length(exp.obs)) {
        exp.time(type = exp.obs[k], unit = "Temp", time.un = "hours")
        exp.temp.h[[k]] <- est.obs
    }
    
    for (l in 1:length(exp.obs)) {
        exp.time(type = exp.obs[l], unit = "Humd", time.un = "hours")
        exp.humd.h[[l]] <- est.obs
    }      
    
    names(exp.temp.h) <- exp.obs
    names(exp.humd.h) <- exp.obs
    
    
    
    
        rm(exp.obs, k, l)
    
    
    plotlayers <- function(ylabel, ylimit, title) {
    p <- ggplot(layer, aes(x=season, y=layer[,1])) + 
        geom_line(alpha = 0.02, size = 0.2) + 
        xlab("Date") + ylab(ylabel) + ylim(ylimit) + ggtitle(title) +
        theme(legend.position="none", panel.background = element_rect(fill = 'white'),
              panel.border = element_rect(color="black", size = 0.2, fill = NA),
              plot.title = element_text(vjust=1.8, face="bold"),
              axis.title.x=element_text(vjust=0.01))
         for (i in 2:length(layer)) {
        p <- p + geom_line(y= layer[,i], colour = "black", alpha = 0.2)
        }
    return(p)
    }
    
    
    histlayers <- function() {
        m <- ggplot(temp.min, aes(x=temp.min[,1])) +
        geom_density(aes(y = ..density..)) + xlim(c(15,32)) + 
            xlab("degrees Celsius") + ggtitle("Figure 1: Density plots of tempearature observations") +
        theme(panel.background = element_rect(fill = 'white'),
              panel.border = element_rect(color="black", size = 0.1, fill = NA),
              plot.title = element_text(vjust=1.8, face="bold"),
              axis.title.x=element_text(vjust=0.01)) +
            geom_density(data = temp.hour, aes(x = temp.hour[,1],
                           y = ..density..), binwidth = 0.5, linetype="dashed", colour = "grey25") +
              geom_density(data = temp.random, aes(x = temp.random[,1],
                             y = ..density..), binwidth = 0.5, colour = "grey50",
                           linetype="dotted") 
        return(m)
    }
       
    
    layer <- exp.temp$Type1
    plotlayers(ylabel = "degrees Celsius", ylimit = c(15, 30), 
               title = "1A. Type 1 (lower resolution) temperature observations") 
    layer <- exp.temp$Type2
    plotlayers(ylabel = "degrees Celsius", ylimit = c(15, 30),
               title = "1B. Type 2 (higher resolution) temperature observations") 
    layer <- exp.humd$Type1
    plotlayers(ylabel = "relative humidity (%)", ylimit = c(50, 100),
               title = "2A. Type 1 (lower resolution) humidity observations") 
    layer <- exp.humd$Type2
    plotlayers(ylabel = "relative humidity (%)", ylimit = c(50, 100),
               title = "2B. Type 2 (higher resolution) humidity observations") 
    layer <- exp.temp.h$Type2
    plotlayers(ylabel = "degrees Celsius", ylimit = c(15, 30), 
               title = "1. Type 2 (higher resolution) temperature observations at hourly interval (normal)") 
    
    layer <- exp.temp.h$Type1
    plotlayers(ylabel = "degrees Celsius", ylimit = c(15, 30), 
               title = "3. Type 1 (lower resolution) temperature observations at hourly interval (normal)") 
    
    type1.matrix.t <- as.matrix(exp.temp$Type1)
    type2.matrix.t <- as.matrix(exp.temp$Type2)
    type1.matrix.h <- as.matrix(exp.humd$Type1)
    type2.matrix.h <- as.matrix(exp.humd$Type2)
    temp.hour.mtrx <- as.matrix(exp.temp.h$Type2)
    temp.hour.mtrx.1 <- as.matrix(exp.temp.h$Type1)
    temp.min  <- as.data.frame(as.vector(type2.matrix.t))
    temp.hour <- as.data.frame(as.vector(temp.hour.mtrx))
    temp.random <- as.data.frame(as.vector(random.hour))
  
    histlayers() 
    
    layer <- exp.temp.h$Type1
    plotlayers(ylabel = "degrees Celsius", ylimit = c(15, 30), 
               title = "7. Type 1 (lower resolution): cubic spline of temperature observations at hourly interval (normal)") 
