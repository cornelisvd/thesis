##---------------------------------aggregate----------------------------------## 

    setwd ("~/thesis/data/")   
    #load("list.cor.Rdata")
    names(list.cor) <- c("temperature", "humidity")
    meanD <- mean(list.cor$temperature$Date)
    minD  <- min(list.cor$temperature$Date)
    maxD  <- max(list.cor$temperature$Date)
    Date  <- unlist(strsplit(as.character(meanD), " "))

##--------------------------------temperature---------------------------------##

    temp    <- list.cor$temperature
    t.cor   <- subset(temp, temp[,1] >= as.POSIXct("2014-08-06") & 
                          temp[,1] < as.POSIXct("2014-09-06"))
    h   <- unlist(strsplit(as.character(t.cor[,1]), " "))
    h   <- unlist(strsplit(as.character(h), ":"))
    b <- h[seq(2, length(h), 4)]
    t.cor[,1] <- b
        
##---------------------------------cleaning-----------------------------------##
    
    ## This is based on the 2.PlotSensors function and will remove the sensors
    ## that clearly have not been performing well. This is the first level of
    ## quality control, after this the data can be checked for certain outliers.
    
    t.rm  <- c("337F65", "337EB0", "337D07", "337B89", "337864", "33827E")
    t.cor <-  t.cor[ , -which(names(t.cor) %in% t.rm)]
    l <- length(t.cor)

##---------------------------------threshold----------------------------------##

## The possibility to remove values outside certain thresholds (e.g. max of 35C)
## Outliers are common during periods when the sensors will be placed or removed
## It will thus be more important to remove sufficient days from the final data.

##-----------------------------------mean-------------------------------------##
    
    meantemp <- data.frame(c(0:23))
            for (i in 2:l){
                x <- aggregate(t.cor[i], 
                               by = list(t.cor[,1]), 
                               FUN = mean, na.rm=TRUE)
                meantemp <- cbind(meantemp, x[,2])
            }
    names(meantemp) <- c("Hour", names(t.cor[2:l]))
        for (h in 0:23) {
            meantemp$Hour[h+1] <- paste(Date[1], " ", h, ":00:00", sep = "")
        }
    meantemp$Hour <- as.POSIXct(meantemp$Hour)

##------------------------------------min-------------------------------------##
    
    mintemp <- data.frame(c(0:23))
            for (i in 2:l){
                x <- aggregate(t.cor[i], 
                               by = list(t.cor$Date), 
                               FUN = min, na.rm=TRUE)
                mintemp <- cbind(mintemp, x[,2])
            }
    names(mintemp) <- c("Hour", names(t.cor[2:l]))
        for (h in 0:23) {
            mintemp$Hour[h+1] <- paste(Date[1], " ", h, ":00:00", sep = "")
        }
    mintemp$Hour <- as.POSIXct(mintemp$Hour)
    
##------------------------------------max-------------------------------------##

    maxtemp <- data.frame(c(0:23))
            for (i in 2:l){
                x <- aggregate(t.cor[i], 
                           by = list(t.cor$Date), 
                           FUN = max, na.rm=TRUE)
                maxtemp <- cbind(maxtemp, x[,2])
            }
    names(maxtemp) <- c("Hour", names(t.cor[2:l]))
        for (h in 0:23) {
            maxtemp$Hour[h+1] <- paste(Date[1], " ", h, ":00:00", sep = "")
        }
    maxtemp$Hour <- as.POSIXct(maxtemp$Hour)
    
##---------------------------------humidity-----------------------------------##
    
    humd    <- list.ok$humidity
    h.cor   <- subset(humd, humd$Date > as.POSIXct("2014-08-07") & 
                            humd$Date < as.POSIXct("2014-09-05"))
    h   <- unlist(strsplit(as.character(h.cor$Date), " "))
    h   <- unlist(strsplit(as.character(h), ":"))
    b <- h[seq(2, length(h), 4)]
    h.cor$Date <- b
    
##---------------------------------cleaning-----------------------------------##
    
    ## This is based on the 2.PlotSensors function and will remove the sensors
    ## that clearly have not been performing well. This is the first level of
    ## quality control, after this the data can be checked for certain outliers.
    
    h.rm  <- c("337F65", "337EB0", "337D07", "337B89", "337864", "33827E")
    h.cor <-  h.cor[ , -which(names(h.cor) %in% h.rm)]
    l <- length(h.cor)
    
##---------------------------------threshold----------------------------------##
    
## The possibility to remove values outside certain thresholds (e.g. max of 35C)
## Outliers are common during periods when the sensors will be placed or removed
## It will thus be more important to remove sufficient days from the final data.
    
##-----------------------------------mean-------------------------------------##
    
    meanhumd<- data.frame(c(0:23))
    for (i in 2:l){
        x <- aggregate(h.cor[i], 
                       by = list(h.cor$Date), 
                       FUN = mean, na.rm=TRUE)
        meanhumd <- cbind(meanhumd, x[,2])
    }
    names(meanhumd) <- c("Hour", names(t.cor[2:l]))
    for (h in 0:23) {
        meanhumd$Hour[h+1] <- paste(Date[1], " ", h, ":00:00", sep = "")
    }
    meanhumd$Hour <- as.POSIXct(meanhumd$Hour)
    
##------------------------------------max-------------------------------------##
    
    maxhumd <- data.frame(c(0:23))
    for (i in 2:l){
        x <- aggregate(h.cor[i], 
                       by = list(h.cor$Date), 
                       FUN = max, na.rm=TRUE)
        maxhumd <- cbind(maxhumd, x[,2])
    }
    names(maxhumd) <- c("Hour", names(t.cor[2:l]))
    for (h in 0:23) {
        maxhumd$Hour[h+1] <- paste(Date[1], " ", h, ":00:00", sep = "")
    }
    maxhumd$Hour <- as.POSIXct(maxhumd$Hour)
    
##------------------------------------min-------------------------------------##
    
    minhumd <- data.frame(c(0:23))
    for (i in 2:l){
        x <- aggregate(h.cor[i], 
                       by = list(h.cor$Date), 
                       FUN = min, na.rm=TRUE)
        minhumd <- cbind(minhumd, x[,2])
    }
    names(minhumd) <- c("Hour", names(t.cor[2:l]))
    for (h in 0:23) {
        minhumd$Hour[h+1] <- paste(Date[1], " ", h, ":00:00", sep = "")
    }
    minhumd$Hour <- as.POSIXct(minhumd$Hour)
    
##-----------------------------------lists------------------------------------##
    
    list.mean <- list(meantemp, meanhumd)
    list.min  <- list(mintemp, minhumd)
    list.max  <- list(maxtemp, maxhumd)
    names(list.mean) <- c("temperature", "humidity")
    save(list.mean, file = "list.mean.Rdata") 
    names(list.min)  <- c("temperature", "humidity")
    save(list.min, file = "list.min.Rdata") 
    names(list.max)  <- c("temperature", "humidity")
    save(list.max, file = "list.max.Rdata") 
    
##-------------------------------------end------------------------------------##