

#plotsensor <- function{
                                                            require(ggplot2)
                                                            require(gridExtra)
msr <- "humidity"
plot = TRUE
                          
    if(plot == TRUE) {                                                            
                                                            
        if (msr == "humidity") {
            lab <- "Relative Humidity   (% RH)" 
            lim <- c(0, 100)
            th <- 80 # Coffee Berry Borer?
            cl <- scale_colour_gradient2(high="darkblue")
            }
    
        else {
            lab <- "Degrees Celsius (°C)" 
            lim <- c(0, 50)
            th <- 30
            cl <- scale_colour_gradient2(high="red")
            }
        
        m <- match(names(obs.ok), loc$CODE) 
        sl = c()
        sz = c()
        for  (i in m) {
            l <- as.character(loc[i,]$LOCATION)
            z <- loc[i,]$Z
            sl <- c(sl, l)
            sz <- c(sz, z)
        }
        P <- list()
            nplot <- 2:length(obs.ok)
                                                                          
    for (j in nplot) {
        P[[length(P) + 1]] <- print(ggplot(obs.ok, aes(x=obs.ok$Date, 
            y=obs.ok[,j])) + geom_point(aes(colour = obs.ok[,j]), alpha = 0.5) + 
            ylab(lab)+labs(title=paste("Sensor:",names(obs.ok[j]),
            "               Alt (m)   = ", sz[j], "            Place    =  ", 
            sl[j], sep="   ")) + ylim(lim) + xlab("Date") + cl +
            geom_hline(yintercept = th, colour = "red") + 
            geom_smooth(method = "auto", linetype="dashed", aes(group=1))) 
          
        }
                       rm(i, j, l, m, lab, lim, msr, nplot, plot, sl, sz, th, z)
    }
                                                            
#}                                                        