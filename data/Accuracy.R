load("RMSE_IDW.Rdata")
rmse <- c()
for (i in 1:24){
   err <- mean(RMSE_IDW[seq(i, length(RMSE_IDW), 24)])
   rmse <- c(rmse, err)
}

load("allresIDW.Rdata")

sensrmse <- c()
allres <- matrix(allresIDW, ncol=80)
for (n in 1:ncol(allres)){
    error <- mean(sqrt(allres[,n]^2))
    sensrmse <- c(sensrmse, error)
}

drmse <- c()
for (d in 1:31){
    d_rmse <- mean(RMSE_IDW[seq(((d-1)*24+1), (24*d), 1)])
    drmse <- c(drmse, d_rmse)
}

temp <- rowMeans(temp)
tempt <- c()
for (d in 1:31){
    mtemp <- max(temp[seq((d-1)*24, 24*d, 1)])
    tempt <- c(tempt, mtemp)
}

rsq2 <- c()
for (i in 1:24){
    err <- max(rsq[seq(i, length(rsq), 24)])
    rsq2 <- c(rsq2, err)
}

rel2 <- c()
for (i in 1:24){
    err <- rel[seq(i, nrow(rel), 24),]
    er2 <- colMeans(err)
    rel2 <- rbind(rel2, er2)
}

rel2 <- t(apply(rel2, 1, cumsum))
reldf <- as.data.frame(rel2)
reldf <- cbind(c(1:24), reldf)



ggplot(reldf, aes(reldf[,1], reldf[,7])) + 
    geom_area(aes(col="f. Radiation"), position = 'stack', fill=col[6]) +
    theme_set(theme_gray(base_size = 18)) +
    geom_area(aes(y = reldf[,6], col="e. Canopy"), fill=col[5]) +
    geom_area(aes(y = reldf[,5], col="d. LAI"), fill=col[4])+
    geom_area(aes(y = reldf[,4], col="c. Aspect"), fill=col[3]) +
    geom_area(aes(y = reldf[,3], col="b. Slope"), fill=col[2]) +
    geom_area(aes(y = reldf[,2], col="a. Altitude"), fill=col[1]) +
    coord_cartesian(ylim = c(0,1), xlim = c(1, 24)) +
    geom_line(y=rsq2, col="grey70", linetype = "longdash") +
    xlab("Hour of day") + ylab("Relative contribution to regression model") +
    geom_vline(xintercept = dl[1], colour="black", linetype="longdash") +
    geom_vline(xintercept = dl[2], colour="black", linetype="longdash") +
    scale_color_manual( name="Covariates",
                        values=c(col[1], col[2], col[3], col[4], col[5], col[6]))
    
    
    
+
    geom_area(aes(y = reldf[,1]), fill="purple") 
  
    geom_area(aes(colour = "red"), position = 'stack') +  
    geom_area(aes(colour = "red"), position = 'stack') +  


rsqdf <- data.frame(1:24, rsq2)
ggplot(rsqdf, aes(x=rsqdf[,1], y=rsqdf[,2])) + geom_line(col="blue", size=1.2) +
    geom_hline(yintercept = 0.5, colour="gray", linetype="longdash", size=0.1) +
    coord_cartesian(ylim = c(0,1), xlim = c(1, 24)) +
    xlab("Hour of day") + ylab("Mean model strength (R2)") +
    geom_hline(yintercept = 0.5, colour="black", linetype="longdash")


PLOT <- data.frame(tempt, rowMeans(cor.solT2, na.rm=TRUE))
ggplot(PLOT, aes(x=PLOT[,1], y=PLOT[,2])) + geom_point(size=4, col="red", alpha=0.5) +
    theme_set(theme_gray(base_size = 18)) +
    xlab("Maximum daily temperature") + ylab("Mean daily correlation of Ta with hourly radiation") +
    stat_smooth(method = "lm", formula = y ~ poly(x, 1))

xx <- data.frame(tempt, drmse)

ggplot(xx, aes(x=xx[,1], y=xx[,2])) + geom_point(size=4, col="red", alpha=0.5) +
   theme_set(theme_gray(base_size = 18)) +
    xlab("Maximum daily temperature (degrees Celsius)") + ylab("RMSE") +
    stat_smooth(method = "lm", formula = y ~ x)

RMS <- data.frame(1:24,rmse)
ggplot(RMS, aes(x=RMS[,1], y=RMS[,2])) + geom_line(size=2.5, col="red", alpha=0.5) +
    theme_set(theme_gray(base_size = 18)) + coord_cartesian(xlim = c(1, 24)) +
    xlab("Hour of day") + ylab("RMSE") 
