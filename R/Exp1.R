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
                            unit    = "Temp")
    {
       
    ## 
        require(zoo)
        start    <- as.POSIXct("23/06/14 12:00:00", format = "%d/%m/%y %H:%M:%S")
        end      <- as.POSIXct("30/06/14 11:59:00", format = "%d/%m/%y %H:%M:%S")
        season   <<- seq(from = start, to = end, 
                        by = as.difftime(1, units = "mins"))  
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
        est.obs   <- na.approx(merge.zoo(index, type.df))
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
    
        rm(exp.obs, k, l)
    
    
    plotlayers <- function(ylabel, ylimit, title) {
    p <- ggplot(layer, aes(x=season, y=layer[,1])) + 
        geom_point(alpha = 0.02, size = 0.01) + 
        xlab("Date") + ylab(ylabel) + ylim(ylimit) + ggtitle(title) +
        theme(legend.position="none", panel.background = element_rect(fill = 'white'),
              panel.border = element_rect(color="black", size = 0.2, fill = NA),
              plot.title = element_text(vjust=1.8, face="bold"),
              axis.title.x=element_text(vjust=0.01))
         for (i in 2:length(layer)) {
        p <- p + geom_point(y= layer[,i], colour = "black", alpha = 0.02)
        }
    return(p)
    }
    
    layer <- exp.temp$Type1
    plotlayers(ylabel = "degrees Celsius", ylimit = c(15, 30), 
               title = "Type 1 (lower resolution) temperature observations") 
    layer <- exp.temp$Type2
    plotlayers(ylabel = "degrees Celsius", ylimit = c(15, 30),
               title = "Type 2 (higher resolution) temperature observations") 
    layer <- exp.humd$Type1
    plotlayers(ylabel = "relative humidity (%)", ylimit = c(50, 100),
               title = "Type 1 (lower resolution) humidity observations") 
    layer <- exp.humd$Type2
    plotlayers(ylabel = "relative humidity (%)", ylimit = c(50, 100),
               title = "Type 2 (higher resolution) humidity observations") 