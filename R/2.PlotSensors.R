##--------------------------------Plot-sensors--------------------------------##
## Function to create plots for all the micro-sensors that have been used in the
## project. This is the second function in which thresholds & limits can be set,
## while additional information will be added from the 'loc' file for this area. 
## A new folder will be created in the working directory for all of these plots.
##---------------------------------v.26-08-14---------------------------------##

# Load libraries
    library(ggplot2)             

# Load (all) data from the sensors
# plot.sensors <- function() {this does not yet work as a function at the moment
    
    ## The main for-loop creates humidity/temperature plots
        for (i in units) {
        ### This sets the humidity limits and thresholds
            if (i == "humidity") {
                lab <- "Relative Humidity   (% RH)" 
                lim <- c(0, 100)
                th <- 80 
                cl <- scale_colour_gradient2(high="darkblue")
        ### This sets the temperature limits and thresholds    
            } else {
                lab <- "Degrees Celsius (°C)" 
                lim <- c(0, 40)
                th <- 30
                cl <- scale_colour_gradient2(high="red")
            }
        
        ### Matching sensors and names for the plots
            obs.ok <- as.data.frame(list.ok[i])
            colnames(obs.ok)[1] <- "date"
            names <- unlist(strsplit(names(obs.ok), "[.]"))
            m <- match(names[1:length(names)], loc$Number) 
            m <- m[!is.na(m)]
            sl = c()
            sz = c()
        
        ### Creating lists of location & altitude
            for  (j in m) {
                l <- as.character(loc[j,]$LOCATION)
                z <- loc[j,]$Z
                sl <- c(sl, l)
                sz <- c(sz, z)
            }
        
        ### Setting up of new list and directory for plots
            # P <- list()
            nplot <- 2:length(obs.ok)
            dir.create(paste("plots(", folder, ")", sep = ""))
        
        ### The actual loop to create and save the ggplots
            for (j in nplot) {
                print(ggplot(obs.ok, aes(x=obs.ok$date, y=obs.ok[,j])) + 
                geom_point(aes(colour = obs.ok[,j]), alpha = 0.5) + 
                ylab(lab)+labs(title=paste("Sensor:",names(obs.ok[j]),
                "              Alt (m)   = ",sz[j-1],"            Place    =  ", 
                sl[j-1], sep="   ")) + ylim(lim) + xlab("Date") + cl +
                geom_hline(yintercept = th, colour = "red"))
                ggsave(paste(i, m[j-1], ".png", sep = ""),
                       path = paste("plots(", folder, ")", sep = ""))
        }  
        
    }
                       rm(cl, i, j, l, lab, lim, m, names, nplot, sl, sz, th, z)
# }    