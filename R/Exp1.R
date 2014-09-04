##--------------------------------Experiment 1--------------------------------##






##---------------------------------v.01-09-14---------------------------------##

# Set the working directory
    setwd ("~/thesis/data/Experiments/")   

# Load libraries & clear the workspace
    rm (list = ls(all = TRUE))  ;   
    source(".PackageInstall.R")
    source(".Exp1Functions.R")
    
##------------------------------Minute linear----------------------------------##
       
    exp.temp <- list()
    exp.humd <- list(
        )
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
    
    type1.matrix.t <- as.matrix(exp.temp$Type1)
    type2.matrix.t <- as.matrix(exp.temp$Type2)
    type1.matrix.h <- as.matrix(exp.humd$Type1)
    type2.matrix.h <- as.matrix(exp.humd$Type2)
    
    vector.t1 <- as.vector(type1.matrix.t)
    summary.t1 <- summary(vector.t1)
    mean(rowSds(type1.matrix.t))
    max(rowDiffs(type1.matrix.t))
    
    vector.t2 <- as.vector(type2.matrix.t)
    vector.h2 <- as.vector(type2.matrix.h)
    correlat <- cor(vector.t2, vector.h2)
    summary.t2 <- summary(vector.t2)
    mean(rowSds(type2.matrix.t))
    max(rowDiffs(type2.matrix.t))
    
##----------------------------------Hour linear------------------------------------##
    
    exp.temp.h <- list()
    exp.humd.h <- list()
    exp.obs  <- c("Reference", "Type1", "Type2")
    
    for (k in 1:length(exp.obs)) {
        exp.time(type = exp.obs[k], unit = "Temp", time.un = "hours")
        exp.temp.h[[k]] <- est.obs
    }
    
    layer <- exp.humd.h$Type1
    plotlayers(ylabel = "relative humidity (%)", ylimit = c(50, 100),
               title = "2B. Type 2 (higher resolution) humidity observations") 
    
    for (l in 1:length(exp.obs)) {
        exp.time(type = exp.obs[l], unit = "Humd", time.un = "hours")
        exp.humd.h[[l]] <- est.obs
    }      
    
    names(exp.temp.h) <- exp.obs
    names(exp.humd.h) <- exp.obs
    
    type1.matrix.t.h <- as.matrix(exp.temp.h$Type1)
    type2.matrix.t.h <- as.matrix(exp.temp.h$Type2)
    type1.matrix.h.h <- as.matrix(exp.humd.h$Type1)
    type2.matrix.h.h <- as.matrix(exp.humd.h$Type2)
    
    vector.t1.h <- as.vector(type1.matrix.t.h)
    summary.t1.h <- summary(vector.t1.h)
    mean(rowSds(type1.matrix.t.h))
    max(rowDiffs(type1.matrix.t.h))
    
    vector.t2.h <- as.vector(type2.matrix.t.h)
    summary.t2.h <- summary(vector.t2.h)
    mean(rowSds(type2.matrix.t.h))
    max(rowDiffs(type2.matrix.t.h))
    
##----------------------------------Hour spline-----------------------------##
    
    exp.temp.h.s <- list()
    exp.humd.h.s <- list()
    exp.obs  <- c("Reference", "Type1", "Type2")
    
    for (k in 1:length(exp.obs)) {
        exp.time(type = exp.obs[k], unit = "Temp", time.un = "hours", 
                 na.est = na.spline)
        exp.temp.h.s[[k]] <- est.obs
    }
    
    for (l in 1:length(exp.obs)) {
        exp.time(type = exp.obs[l], unit = "Humd", time.un = "hours",
                 na.est = na.spline)
        exp.humd.h.s[[l]] <- est.obs
    }      
    
    names(exp.temp.h.s) <- exp.obs
    names(exp.humd.h.s) <- exp.obs
    
    type1.matrix.t.h.s <- as.matrix(exp.temp.h.s$Type1)
    type2.matrix.t.h.s <- as.matrix(exp.temp.h.s$Type2)
    type1.matrix.h.h.s <- as.matrix(exp.humd.h.s$Type1)
    type2.matrix.h.h.s <- as.matrix(exp.humd.h.s$Type2)
    
    vector.t1.h.s <- as.vector(type1.matrix.t.h.s)
    summary.t1.h.s <- summary(vector.t1.h.s)
    mean(rowSds(type1.matrix.t.h.s))
    max(rowDiffs(type1.matrix.t.h.s))
    
    vector.t2.h.s <- as.vector(type2.matrix.t.h.s)
    summary.t2.h.s <- summary(vector.t2.h.s)
    mean(rowSds(type2.matrix.t.h.s))
    max(rowDiffs(type2.matrix.t.h.s))

##--------------------------------2 Hour linear-------------------------------##
    
    exp.temp.2h <- list()
    exp.humd.2h <- list()
    exp.obs  <- c("Reference", "Type1", "Type2")
    
    for (k in 1:length(exp.obs)) {
        exp.time(type = exp.obs[k], unit = "Temp", time.un = "hours",
                 time.dif = 2)
        exp.temp.2h[[k]] <- est.obs
    }
    
    for (l in 1:length(exp.obs)) {
        exp.time(type = exp.obs[l], unit = "Humd", time.un = "hours",
                 time.dif = 2)
        exp.humd.2h[[l]] <- est.obs
    }      
    
    names(exp.temp.2h) <- exp.obs
    names(exp.humd.2h) <- exp.obs
    
    type1.matrix.t.2h <- as.matrix(exp.temp.2h$Type1)
    type2.matrix.t.2h <- as.matrix(exp.temp.2h$Type2)
    type1.matrix.h.2h <- as.matrix(exp.humd.2h$Type1)
    type2.matrix.h.2h <- as.matrix(exp.humd.2h$Type2)
    
    vector.t1.2h <- as.vector(type1.matrix.t.2h)
    summary.t1.2h <- summary(vector.t1.2h)
    mean(rowSds(type1.matrix.t.2h))
    max(rowDiffs(type1.matrix.t.2h))
    
    vector.t2.2h <- as.vector(type2.matrix.t.2h)
    summary.t2.2h <- summary(vector.t2.2h)
    mean(rowSds(type2.matrix.t.2h))
    max(rowDiffs(type2.matrix.t.2h))
    
##---------------------------------2 Hour spline----------------------------##
   
    exp.temp.2h.s <- list()
    exp.humd.2h.s <- list()
    exp.obs  <- c("Reference", "Type1", "Type2")
    
    for (k in 1:length(exp.obs)) {
        exp.time(type = exp.obs[k], unit = "Temp", time.un = "hours", 
                 na.est = na.spline, time.dif = 2)
        exp.temp.2h.s[[k]] <- est.obs
    }
    
    for (l in 1:length(exp.obs)) {
        exp.time(type = exp.obs[l], unit = "Humd", time.un = "hours",
                 na.est = na.spline, time.dif = 2)
        exp.humd.2h.s[[l]] <- est.obs
    }      
    
    names(exp.temp.2h.s) <- exp.obs
    names(exp.humd.2h.s) <- exp.obs
    
    type1.matrix.t.2h.s <- as.matrix(exp.temp.2h.s$Type1)
    type2.matrix.t.2h.s <- as.matrix(exp.temp.2h.s$Type2)
    type1.matrix.h.2h.s <- as.matrix(exp.humd.2h.s$Type1)
    type2.matrix.h.2h.s <- as.matrix(exp.humd.2h.s$Type2)
    
    vector.t1.2h.s <- as.vector(type1.matrix.t.2h.s)
    summary.t1.2h.s <- summary(vector.t1.2h.s)
    mean(rowSds(type1.matrix.t.2h.s))
    max(rowDiffs(type1.matrix.t.2h.s))
    
    vector.t2.2h.s <- as.vector(type2.matrix.t.2h.s)
    summary.t2.2h.s <- summary(vector.t2.2h.s)
    mean(rowSds(type2.matrix.t.2h.s))
    max(rowDiffs(type2.matrix.t.2h.s))

##-------------------------------Hour random linear--------------------------##
    
    exp.temp.r.h <- list()
    exp.humd.r.h <- list()
    exp.obs  <- c("Reference", "Type1", "Type2")
    
    for (k in 1:length(exp.obs)) {
        exp.time2(type = exp.obs[k], unit = "Temp", time.un = "hours",
                  time.dif = 1)
        exp.temp.r.h[[k]] <- est.obs
    }
    
    for (l in 1:length(exp.obs)) {
        exp.time2(type = exp.obs[l], unit = "Humd", time.un = "hours",
                  time.dif = 1)
        exp.humd.r.h[[l]] <- est.obs
    }      
    
    names(exp.temp.r.h) <- exp.obs
    names(exp.humd.r.h) <- exp.obs
    
    type1.matrix.r.t.h <- as.matrix(exp.temp.r.h$Type1)
    type2.matrix.r.t.h <- as.matrix(exp.temp.r.h$Type2)
    type1.matrix.r.h.h <- as.matrix(exp.humd.r.h$Type1)
    type2.matrix.r.h.h <- as.matrix(exp.humd.r.h$Type2)
    
    vector.t1.r.h <- as.vector(type1.matrix.r.t.h)
    summary.t1.r.h <- summary(vector.t1.r.h)
    mean(rowSds(type1.matrix.r.t.h))
    max(rowDiffs(type1.matrix.r.t.h))
    
    vector.t2.r.h <- as.vector(type2.matrix.r.t.h)
    summary.t2.r.h <- summary(vector.t2.r.h)
    mean(rowSds(type2.matrix.r.t.h))
    max(rowDiffs(type2.matrix.r.t.h))
    
##-------------------------------Hour random spline--------------------------##
    
    exp.temp.r.h.s <- list()
    exp.humd.r.h.s <- list()
    exp.obs  <- c("Reference", "Type1", "Type2")
    
    for (k in 1:length(exp.obs)) {
        exp.time2(type = exp.obs[k], unit = "Temp", time.un = "hours",
                  na.est = na.spline)
        exp.temp.r.h.s[[k]] <- est.obs
    }
    
    for (l in 1:length(exp.obs)) {
        exp.time2(type = exp.obs[l], unit = "Humd", time.un = "hours",
                  na.est = na.spline)
        exp.humd.r.h.s[[l]] <- est.obs
    }      
    
    names(exp.temp.r.h.s) <- exp.obs
    names(exp.humd.r.h.s) <- exp.obs
    
    type1.matrix.r.t.h.s <- as.matrix(exp.temp.r.h.s$Type1)
    type2.matrix.r.t.h.s <- as.matrix(exp.temp.r.h.s$Type2)
    type1.matrix.r.h.h.s <- as.matrix(exp.humd.r.h.s$Type1)
    type2.matrix.r.h.h.s <- as.matrix(exp.humd.r.h.s$Type2)
    
    vector.t1.r.h.s <- as.vector(type1.matrix.r.t.h.s)
    summary.t1.r.h.s <- summary(vector.t1.r.h.s)
    mean(rowSds(type1.matrix.r.t.h.s))
    max(rowDiffs(type1.matrix.r.t.h.s))
    
    vector.t2.r.h.s <- as.vector(type2.matrix.r.t.h.s)
    summary.t2.r.h.s <- summary(vector.t2.r.h.s)
    mean(rowSds(type2.matrix.r.t.h.s))
    max(rowDiffs(type2.matrix.r.t.h.s))
    
##-----------------------------2 Hour random linear--------------------------##
    
    exp.temp.r.2h <- list()
    exp.humd.r.2h <- list()
    exp.obs  <- c("Reference", "Type1", "Type2")
    
    for (k in 1:length(exp.obs)) {
        exp.time2(type = exp.obs[k], unit = "Temp", time.un = "hours",
                  time.dif = 2)
        exp.temp.r.2h[[k]] <- est.obs
    }
    
    for (l in 1:length(exp.obs)) {
        exp.time2(type = exp.obs[l], unit = "Humd", time.un = "hours",
                  time.dif = 2)
        exp.humd.r.2h[[l]] <- est.obs
    }      
    
    names(exp.temp.r.2h) <- exp.obs
    names(exp.humd.r.2h) <- exp.obs
    
    type1.matrix.r.t.2h <- as.matrix(exp.temp.r.2h$Type1)
    type2.matrix.r.t.2h <- as.matrix(exp.temp.r.2h$Type2)
    type1.matrix.r.h.2h <- as.matrix(exp.humd.r.2h$Type1)
    type2.matrix.r.h.2h <- as.matrix(exp.humd.r.2h$Type2)
    
    vector.t1.r.2h <- as.vector(type1.matrix.r.t.2h)
    summary.t1.r.2h <- summary(vector.t1.r.2h)
    mean(rowSds(type1.matrix.r.t.2h))
    max(rowDiffs(type1.matrix.r.t.2h))
    
    vector.t2.r.2h <- as.vector(type2.matrix.r.t.2h)
    summary.t2.r.2h <- summary(vector.t2.r.2h)
    mean(rowSds(type2.matrix.r.t.2h))
    max(rowDiffs(type2.matrix.r.t.2h))
    
    ##-------------------------------Hour random spline--------------------------##
    
    exp.temp.r.2h.s <- list()
    exp.humd.r.2h.s <- list()
    exp.obs  <- c("Reference", "Type1", "Type2")
    
    for (k in 1:length(exp.obs)) {
        exp.time2(type = exp.obs[k], unit = "Temp", time.un = "hours",
                  na.est = na.spline, time.dif = 2)
        exp.temp.r.2h.s[[k]] <- est.obs
    }
    
    for (l in 1:length(exp.obs)) {
        exp.time2(type = exp.obs[l], unit = "Humd", time.un = "hours",
                  na.est = na.spline, time.dif = 2)
        exp.humd.r.2h.s[[l]] <- est.obs
    }      
    
    names(exp.temp.r.2h.s) <- exp.obs
    names(exp.humd.r.2h.s) <- exp.obs
    
    type1.matrix.r.t.2h.s <- as.matrix(exp.temp.r.2h.s$Type1)
    type2.matrix.r.t.2h.s <- as.matrix(exp.temp.r.2h.s$Type2)
    type1.matrix.r.h.2h.s <- as.matrix(exp.humd.r.2h.s$Type1)
    type2.matrix.r.h.2h.s <- as.matrix(exp.humd.r.h.s$Type2)
    
    vector.t1.r.2h.s <- as.vector(type1.matrix.r.t.2h.s)
    summary.t1.r.2h.s <- summary(vector.t1.r.2h.s)
    mean(rowSds(type1.matrix.r.t.2h.s))
    max(rowDiffs(type1.matrix.r.t.2h.s))
    
    vector.t2.r.2h.s <- as.vector(type2.matrix.r.t.2h.s)
    summary.t2.r.2h.s <- summary(vector.t2.r.2h.s)
    mean(rowSds(type2.matrix.r.t.2h.s))
    max(rowDiffs(type2.matrix.r.t.2h.s))
    
##------------------------------------Density plotting----------------------------##
    
   
    minute.data <- data.frame(vector.t2)
    hour.data <- data.frame(vector.t2.h, vector.t2.h.s,  vector.t2.2h, vector.t2.2h.s,
                 vector.t2.r.h, vector.t2.r.h.s, vector.t2.r.2h, vector.t2.r.2h.s )

    histlayers <- function() {
        m <- ggplot(minute.data, aes(x=minute.data[,1])) +
            geom_density(aes(y = ..density..)) + xlim(c(15,32)) + xlab("degrees Celsius") +
            ggtitle("Figure 1: Density plots of tempearature observations") +
            theme(panel.background = element_rect(fill = 'white'),
                  panel.border = element_rect(color="black", size = 0.1, fill = NA),
                  plot.title = element_text(vjust=1.8, face="bold"),
                  axis.title.x=element_text(vjust=0.01)) +
            geom_density(data = hour.data, aes(x = hour.data[,1],
             y = ..density..), binwidth = 0.5, linetype="dashed") +
            geom_density(data = hour.data, aes(x = hour.data[,8],
             y = ..density..), binwidth = 0.5, linetype="dotted")      
        return(m)
    }
    
    x1 <- as.data.frame(type2.matrix.t)
    x2 <- as.data.frame(type2.matrix.r.t.h2)
    x3 <- as.data.frame(type2.matrix.r.t.2h.s)
    
    start    <- as.POSIXct("23/06/14 12:00:00", format = "%d/%m/%y %H:%M:%S")
    end      <- as.POSIXct("30/06/14 11:59:00", format = "%d/%m/%y %H:%M:%S")
    t.t1    <- seq(from = start, to = end, 
                     by = as.difftime(1, units = "mins"))  
    t.t2 <- seq(from = start, to = end, 
                     by = as.difftime(1, units = "hours")) 
    t.t3 <- seq(from = start, to = end, 
                by = as.difftime(2, units = "hours")) 
    

     ggplot(x1, aes(x=t.t1, y=x1[,1])) + 
            geom_line(data = x1, aes(x=t.t1, y=x1[,1]), colour = "grey90", linetype = "solid", size = 1) + 
            geom_line(data = x2, aes(x=t.t3, y=x2[,1]), colour = "grey50", linetype = "longdash") +
             xlab("Date") + ylab("degrees Celsius") + ylim(c(18,30)) + 
        ggtitle("Temperature extremes using a 2-hour interval (random start-point)") +
            geom_line(data = x3, aes(x=t.t3, y=x3[,1]), colour = "grey10", linetype = "dashed") +
        theme(legend.position="right", panel.background = element_rect(fill = 'white'),
              panel.border = element_rect(color="black", size = 0.2, fill = NA),
              plot.title = element_text(vjust=1.8, face="bold"),
              axis.title.x=element_text(vjust=0.01))
         
    
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