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
            geom_line() + ggtitle("Temperature and humidity in the pipes vs. the station") + 
             ylab("Temperature (degrees Celsius) and relative humidity (%)") + xlab("Date") +
            theme(panel.background = element_rect(fill = 'white'),
                  panel.border = element_rect(color="black", size = 0.2, fill = NA),
                  plot.title = element_text(vjust=1.8, face="bold"),
                  axis.title.x=element_text(vjust=0.01)) +
                  geom_line(aes(y = test1[,2], color = "humidity (station)")) +
                  geom_line(aes(y = test1[,3], color = "temperature (pipes)"),
                            linetype="longdash") + ylim(c(0,100)) +
                  geom_line(aes(y = test1[,4], color = "humidity (pipes)"),
                            linetype="longdash") +
                 scale_color_manual(name  ="Legend", 
                           values=c("blue", "lightblue", "red", "orange")) 
                  return(p)
    }
    plotlayers()
##------------------------------2nd test (large tubes)------------------------------------##

  exp.time4(exp = "Exp3", type = "Station", unit = "Temp")
    test2.station.temp <- as.matrix(est.obs)
  exp.time4(exp = "Exp3", type = "White", unit = "Temp")
    test2.white.temp <- as.matrix(est.obs)  
  exp.time4(exp = "Exp3", type = "Foil - no holes", unit = "Temp")
    test2.foilnh.temp <- as.matrix(est.obs)  
  exp.time4(exp = "Exp3", type = "Foil - with holes", unit = "Temp")
    test2.foilwh.temp <- as.matrix(est.obs)
  exp.time4(exp = "Exp3", type = "Holes", unit = "Temp")
    test2.holes.temp <- as.matrix(est.obs)
  exp.time4(exp = "Exp3", type = "Reflective tape", unit = "Temp")
    test2.reflect.temp <- as.matrix(est.obs)
      t2.s.t <- rowMeans(test2.station.temp)
      t2.w.t <- rowMeans(test2.white.temp)
      t2.n.t <- rowMeans(test2.foilnh.temp)
      t2.f.t <- rowMeans(test2.foilwh.temp)
      t2.h.t <- rowMeans(test2.holes.temp)
      t2.r.t <- rowMeans(test2.reflect.temp)
    
      test2 <- data.frame(t2.s.t, t2.w.t, t2.n.t, t2.f.t, t2.h.t, t2.r.t)

    plotlayers <- function() {
      p <- ggplot(test1, aes(x=season, y=test2[,1], color = "a. Station")) + 
        geom_line() + ggtitle("Temperature in different 50mm constructions (sunny day)") + 
        ylab("Temperature (degrees Celsius)") + xlab("Date") +
        theme(panel.background = element_rect(fill = 'white'),
              panel.border = element_rect(color="black", size = 0.2, fill = NA),
              plot.title = element_text(vjust=1.8, face="bold"),
              axis.title.x=element_text(vjust=0.01)) +
        geom_line(aes(y = test2[,2], color = "b. White paint")) +
        geom_line(aes(y = test2[,3], color = "c. Foil - with holes"),
                  linetype="longdash") + ylim(c(15,35)) +
        geom_line(aes(y = test2[,4], color = "d. Foil - no holes"),
                  linetype="longdash") +
        geom_line(aes(y = test2[,5], color = "e. Holes"),
                  linetype="longdash") +
        geom_line(aes(y = test2[,6], color = "f. Reflective tape"),
                  linetype="longdash") +
        scale_color_manual(name  ="Legend", 
                  values=c("red", "lightblue", "blue", "grey75", "green", "lightgreen")) 
      return(p)
    }
    plotlayers()

##------------------------------3rd test (large tubes)------------------------------------##

  exp.time4(exp = "Exp4", type = "Station", unit = "Temp", start.m  = "06/07/14 00:00:00",
            end.m    = "06/07/14 23:59:00")
    test3.station.temp <- as.matrix(est.obs)
  exp.time4(exp = "Exp4", type = "White", unit = "Temp", start.m  = "06/07/14 00:00:00",
            end.m    = "06/07/14 23:59:00")
    test3.white.temp <- as.matrix(est.obs)  
  exp.time4(exp = "Exp4", type = "Foil - no holes", unit = "Temp", start.m  = "06/07/14 00:00:00",
            end.m    = "06/07/14 23:59:00")
    test3.foilnh.temp <- as.matrix(est.obs)  
  exp.time4(exp = "Exp4", type = "Foil - with holes", unit = "Temp", start.m  = "06/07/14 00:00:00",
            end.m    = "06/07/14 23:59:00")
    test3.foilwh.temp <- as.matrix(est.obs)
  exp.time4(exp = "Exp4", type = "Holes", unit = "Temp", start.m  = "06/07/14 00:00:00",
            end.m    = "06/07/14 23:59:00")
    test3.holes.temp <- as.matrix(est.obs)
  exp.time4(exp = "Exp4", type = "Reflective tape", unit = "Temp",start.m  = "06/07/14 00:00:00",
            end.m    = "06/07/14 23:59:00")
    test3.reflect.temp <- as.matrix(est.obs)
      t3.s.t <- rowMeans(test3.station.temp)
      t3.w.t <- rowMeans(test3.white.temp)
      t3.n.t <- rowMeans(test3.foilnh.temp)
      t3.f.t <- rowMeans(test3.foilwh.temp)
      t3.h.t <- rowMeans(test3.holes.temp)
      t3.r.t <- rowMeans(test3.reflect.temp)
  
  test3 <- data.frame(t3.s.t, t3.w.t, t3.n.t, t3.f.t, t3.h.t, t3.r.t)
  
  plotlayers <- function() {
    p <- ggplot(test1, aes(x=season, y=test3[,1], color = "a. Station")) + 
      geom_line() + ggtitle("Temperature in different 50mm constructions (rainy day)") + 
      ylab("Temperature (degrees Celsius)") + xlab("Date") +
      theme(panel.background = element_rect(fill = 'white'),
            panel.border = element_rect(color="black", size = 0.2, fill = NA),
            plot.title = element_text(vjust=1.8, face="bold"),
            axis.title.x=element_text(vjust=0.01)) +
      geom_line(aes(y = test3[,2], color = "b. White paint")) +
      geom_line(aes(y = test3[,3], color = "c. Foil - with holes"),
                linetype="longdash") + ylim(c(15,35)) +
      geom_line(aes(y = test3[,4], color = "d. Foil - no holes"),
                linetype="longdash") +
      geom_line(aes(y = test3[,5], color = "e. Holes"),
                linetype="longdash") +
      geom_line(aes(y = test3[,6], color = "f. Reflective tape"),
                linetype="longdash") +
      scale_color_manual(name  ="Legend", 
                values=c("red", "lightblue", "blue", "grey75", "green", "lightgreen"))  
    return(p)
  }
  plotlayers()
                         
##------------------------------4th test (small tubes)------------------------------------##

exp.time4(exp = "Exp5", type = "Station", unit = "Temp", start.m  = "09/07/14 15:00:00",
          end.m    = "10/07/14 14:59:00")
test4.station.temp <- as.matrix(est.obs)
exp.time4(exp = "Exp5", type = "Holes", unit = "Temp", start.m  = "09/07/14 15:00:00",
          end.m    = "10/07/14 14:59:00")
test4.holes.temp <- as.matrix(est.obs)  
exp.time4(exp = "Exp5", type = "Holesfoil", unit = "Temp", start.m  = "09/07/14 15:00:00",
          end.m    = "10/07/14 14:59:00")
test4.foilwh.temp <- as.matrix(est.obs)  
exp.time4(exp = "Exp5", type = "Nothing", unit = "Temp", start.m  = "09/07/14 15:00:00",
          end.m    = "10/07/14 14:59:00")
test4.nothing.temp <- as.matrix(est.obs)
exp.time4(exp = "Exp5", type = "Foil", unit = "Temp", start.m  = "09/07/14 15:00:00",
          end.m    = "10/07/14 14:59:00")
test4.foil.temp <- as.matrix(est.obs)

t4.s.t  <- rowMeans(test4.station.temp)
t4.h.t  <- rowMeans(test4.holes.temp)
t4.fh.t <- rowMeans(test4.foilwh.temp)
t4.n.t  <- rowMeans(test4.nothing.temp)
t4.f.t  <- rowMeans(test4.foil.temp)




test4 <- data.frame(t4.s.t, t4.h.t, t4.fh.t, t4.n.t, t4.f.t)

plotlayers <- function() {
  p <- ggplot(test1, aes(x=season, y=test4[,1], color = "a. Station")) + 
    geom_line() + ggtitle("Temperature in different 25mm constructions (rainy day)") + 
    ylab("Temperature (degrees Celsius)") + xlab("Date") +
    theme(panel.background = element_rect(fill = 'white'),
          panel.border = element_rect(color="black", size = 0.2, fill = NA),
          plot.title = element_text(vjust=1.8, face="bold"),
          axis.title.x=element_text(vjust=0.01)) +
    geom_line(aes(y = test4[,2], color = "b. Holes"), 
              linetype="longdash", alpha = 0.9) +
    geom_line(aes(y = test4[,3], color = "c. Foil & holes"),
              , alpha = 0.9, linetype="dashed") + ylim(c(15,35)) +
    geom_line(aes(y = test4[,4], color = "d. Nothing"),
              linetype="longdash", alpha = 0.9) +
    geom_line(aes(y = test4[,5], color = "e. Foil"),
              linetype="dashed", alpha = 0.9) +
    scale_color_manual(name  ="Legend", 
                       values=c("red", "lightblue", "blue", "lightgreen", "green")) 
  return(p)
}
plotlayers()

##------------------------------5th test (small tubes)------------------------------------##

exp.time4(exp = "Exp6", type = "Station", unit = "Temp", start.m  = "15/07/14 00:00:00",
          end.m    = "15/07/14 23:59:00")
test5.station.temp <- as.matrix(est.obs)
exp.time4(exp = "Exp6", type = "Holes", unit = "Temp", start.m  = "15/07/14 00:00:00",
          end.m    = "15/07/14 23:59:00")
test5.holes.temp <- as.matrix(est.obs)  
exp.time4(exp = "Exp6", type = "Holesfoil", unit = "Temp", start.m  = "15/07/14 00:00:00",
          end.m    = "15/07/14 23:59:00")
test5.foilwh.temp <- as.matrix(est.obs)  
exp.time4(exp = "Exp6", type = "Nothing", unit = "Temp", start.m  = "15/07/14 00:00:00",
          end.m    = "15/07/14 23:59:00")
test5.nothing.temp <- as.matrix(est.obs)
exp.time4(exp = "Exp6", type = "Foil", unit = "Temp", start.m  = "15/07/14 00:00:00",
          end.m    = "15/07/14 23:59:00")
test5.foil.temp <- as.matrix(est.obs)

t5.s.t  <- rowMeans(test5.station.temp)
t5.h.t  <- rowMeans(test5.holes.temp)
t5.fh.t <- rowMeans(test5.foilwh.temp)
t5.n.t  <- rowMeans(test5.nothing.temp)
t5.f.t  <- rowMeans(test5.foil.temp)
x <- smooth.spline(t5.n.t, y = NULL, spar = 0.5)

test5 <- data.frame(t5.s.t, t5.h.t, t5.fh.t, t5.n.t, t5.f.t)

plotlayers <- function() {
  p <- ggplot(test5, aes(x=season, y=test5[,1], color = "a. Station")) + 
    geom_line() + ggtitle("Temperature in different 25mm constructions (rainy day)") + 
    ylab("Temperature (degrees Celsius)") + xlab("Date") +
    theme(panel.background = element_rect(fill = 'white'),
          panel.border = element_rect(color="black", size = 0.2, fill = NA),
          plot.title = element_text(vjust=1.8, face="bold"),
          axis.title.x=element_text(vjust=0.01)) +
    geom_line(aes(y = test5[,2], color = "b. Holes"), 
              linetype="longdash", alpha = 0.9) +
    geom_line(aes(y = test5[,3], color = "c. Foil & holes"),
              , alpha = 0.9, linetype="dashed") + ylim(c(15,35)) +
    geom_line(aes(y = test5[,4], color = "d. Nothing"),
              linetype="longdash", alpha = 0.9) +
    geom_line(aes(y = test5[,5], color = "e. Foil"),
              linetype="dashed", alpha = 0.9) +
    scale_color_manual(name  ="Legend", 
                       values=c("red", "lightblue", "blue", "lightgreen", "green")) 
  return(p)
}
plotlayers()

##----------------------------------------------------------------------------------##



