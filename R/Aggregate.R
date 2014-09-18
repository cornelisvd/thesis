##---------------------------------aggregate----------------------------------## 

    setwd ("~/thesis/data/")   
    load("list.ok.Rdata")
    meanD <- mean(list.ok$temperature$Date)
    minD  <- min(list.ok$temperature$Date)
    maxD  <- max(list.ok$temperature$Date)
    Date  <- unlist(strsplit(as.character(m), " "))

##--------------------------------temperature---------------------------------##

    temp    <- list.ok$temperature
    t.cor   <- subset(temp, temp$Date > as.POSIXct("2014-08-07") & 
                          temp$Date < as.POSIXct("2014-09-05"))
    h   <- unlist(strsplit(as.character(t.cor$Date), " "))
    h   <- unlist(strsplit(as.character(h), ":"))
    b <- h[seq(2, length(h), 4)]
    t.cor$Date <- b
    l <- length(t.cor)

##---------------------------------threshold----------------------------------##

## The possibility to remove values outside certain thresholds (e.g. max of 35C)
## Outliers are common during periods when the sensors will be placed or removed
## It will thus be more important to remove sufficient days from the final data.

##-----------------------------------mean-------------------------------------##
    
    meantemp <- data.frame(c(0:23))
            for (i in 2:l){
                x <- aggregate(t.cor[i], 
                               by = list(t.cor$Date), 
                               FUN = mean, na.rm=TRUE)
                meantemp <- cbind(meantemp, x[,2])
            }
    names(meantemp) <- c("Hour", names(t.cor[2:l]))
        for (h in 0:23) {
            meantemp$Hour[h+1] <- paste(Date[1], " ", h, ":00:00", sep = "")
        }

##------------------------------------max-------------------------------------##
    
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
    
##------------------------------------min-------------------------------------##

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
    
##---------------------------------humidity-----------------------------------##