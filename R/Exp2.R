##--------------------------------Experiment 2--------------------------------##






##---------------------------------v.01-09-14---------------------------------##

# Set the working directory
setwd ("~/thesis/data/Experiments/")   

# Load libraries & clear the workspace
rm (list = ls(all = TRUE))  ;   
source(".PackageInstall.R")
source(".Exp2Functions.R")

##------------------------First test (station/pipes)--------------------------##

    exp.time3(type = "Station", unit = "Temp")
        test1.station.temp <- as.matrix(est.obs)
    exp.time3(type = "Station", unit = "Humd")
        test1.station.humd <- as.matrix(est.obs)
    exp.time3(type = "Pipes", unit = "Temp")
        test1.pipes.temp <- as.matrix(est.obs)
    exp.time3(type = "Pipes", unit = "Humd")
        test1.pipes.humd <- as.matrix(est.obs)
            t1.s.t <- rowMeans(test1.station.temp)
            t1.s.h <- rowMeans(test1.station.humd)
            t1.p.t <- rowMeans(test1.pipes.temp)
            t1.p.h <- rowMeans(test1.pipes.humd)
    test1 <- data.frame(t1.s.t, t1.s.h, t1.p.t, t1.p.h)

    plotlayers <- function() {
        p <- ggplot(test1, aes(x=season, y=test1[,1], color = "station temperature")) + 
            geom_line() + ggtitle("Temperature and humidity in pipes vs. station") + 
             ylab("Temperature (degrees Celsius) and relative humidity (%)") + xlab("Date") +
            theme(panel.background = element_rect(fill = 'white'),
                  panel.border = element_rect(color="black", size = 0.2, fill = NA),
                  plot.title = element_text(vjust=1.8, face="bold"),
                  axis.title.x=element_text(vjust=0.01)) +
                  geom_line(aes(y = test1[,2], color = "humidity (station)")) +
                  geom_line(aes(y = test1[,3], color = "temperature (pipes)"),
                            linetype="longdash") + ylim(c(0,100)) +
                  geom_line(aes(y = test1[,4], color = "humidity(pipes)"),
                            linetype="longdash") +
                 scale_color_manual(name  ="Legend", 
                           values=c("blue", "lightblue", "red", "orange")) 
                  return(p)
    }
    plotlayers()
##------------------------------------------------------------------------------------##