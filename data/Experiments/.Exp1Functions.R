    exp.time    <- function(exp      = "Exp1",
                            type     = "Reference",
                            rowskip  =  19, 
                            time     = "Date.Time", 
                            val      = "Value",
                            format   = "%d/%m/%y %H:%M:%S",
                            time.un  = "mins",
                            time.dif = 1,
                            unit     = "Temp",
                            na.est   = na.approx)
    {
        
        ## 
        require(zoo)
        require(ggplot2)
        start    <- as.POSIXct("23/06/14 12:00:00", format = "%d/%m/%y %H:%M:%S")
        end      <- as.POSIXct("30/06/14 11:59:00", format = "%d/%m/%y %H:%M:%S")
        season   <<- seq(from = start, to = end, 
                         by = as.difftime(time.dif, units = time.un))  
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
        est.obs   <- na.est(merge.zoo(index, type.df))
        est.obs   <- as.data.frame(merge.zoo(index, est.obs, all=FALSE))   
        est.obs  <<- est.obs[3:(length(type.dir)+2)]
    }
    
    
    exp.temp <- list()
    exp.humd <- list()
    exp.obs  <- c("Reference", "Type1", "Type2")
    
    plotlayers <- function(ylabel, ylimit, title) {
        p <- ggplot(layer, aes(x=season, y=layer[,1])) + 
            geom_line(alpha = 0.02, size = 0.2) + 
            xlab("Date") + ylab(ylabel) + ylim(ylimit) + ggtitle(title) # +
  #          theme(legend.position="none", panel.background = element_rect(fill = 'white'),
  #                panel.border = element_rect(color="black", size = 0.2, fill = NA),
  #                plot.title = element_text(vjust=1.8, face="bold"),
  #                axis.title.x=element_text(vjust=0.01))
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
    
    exp.time2    <- function(exp     = "Exp1",
                             type    = "Reference",
                             rowskip =  19, 
                             time    = "Date.Time", 
                             val     = "Value",
                             format  = "%d/%m/%y %H:%M:%S",
                             time.un = "hours",
                             time.dif = 2,
                             unit    = "Temp",
                             na.est  = na.approx)
    {
        
        ## 
        require(zoo)
        require(ggplot2)
        start    <- as.POSIXct("23/06/14 12:00:00", format = "%d/%m/%y %H:%M:%S")
        end      <- as.POSIXct("30/06/14 11:59:00", format = "%d/%m/%y %H:%M:%S")
        season   <<- seq(from = start, to = end, 
                         by = as.difftime(time.dif, units = time.un))  
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
            v2 <- v[seq(sample(1:(12*time.dif),1), NROW(v), 12 * time.dif),] 
            v.zoo <- zoo(v2$Value, v2$Date.Time)
            type.zoo[[i]] <- v.zoo
        }  
        
        ##
        type.zoo <- type.zoo[!sapply(type.zoo, is.null)] 
        type.df  <<- do.call(merge, type.zoo)
        
        ## Interpolate points with NA values
        est.obs   <- na.est(merge.zoo(index, type.df))
        est.obs   <- as.data.frame(merge.zoo(index, est.obs, all=FALSE))   
        est.obs  <<- est.obs[3:(length(type.dir)+2)]
    }