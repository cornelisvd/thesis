library(matrixStats)

ctrdataP <- c(rowMeans(test1.station.temp), 
             rowMeans(test2.station.temp), 
             rowMeans(test3.station.temp),
             rowMeans(test4.station.temp), 
             rowMeans(test5.station.temp))

controlP <- matrix(ctrdataP, ncol=5)

smpdataP <- c(rowMeans(test1.pipes.temp), 
             rowMeans(test2.white.temp), 
             rowMeans(test3.white.temp),
             rowMeans(test4.nothing.temp), 
             rowMeans(test5.nothing.temp))

sampleP <- matrix(smpdataP, ncol=5)
c <- as.vector(controlP[seq(1,nrow(controlP),60),])
s <- as.vector(sampleP[seq(1,nrow(sampleP),60),])

sampleP  <- sampleP[seq(1,nrow(controlP),60),]
controlP <- controlP[seq(1,nrow(controlP),60),]

poly <- as.data.frame(c)
poly <- cbind(poly, s)

ggplot(poly, aes(x=s, y=c)) + 
    geom_point(colour = "red", size = 5, alpha = 0.35) + 
    theme_set(theme_gray(base_size = 18)) + ylim(19,30) + xlim(19,30) +
    ylab("Temperature in the Stevenson shield") + xlab("Temperature in the PVC shield") +
    geom_smooth(method = "lm", formula = y ~ poly(x, 2), colour = "blue") 
+
+
    theme_set(theme_gray(base_size = 18)) +
    geom_line(aes(y=pipe$stationdata), colour = "blue", size = 1, alpha = 0.25) +
    ylab("Temperature (degrees Celsius") + xlab("Time (minutes)") +
    y
  

minimum <- c()
#for (i in 1:4) {

    cor      <- lm(c~s)#poly(s,2))
    test <- c()
    diffx <- c()
    for (n in 1:ncol(sampleP)){
        smp <- predict(cor, data.frame(s=sampleP[,n]))
        diffx <- append(diffx, sqrt((controlP[,n]-smp)^2))
        plot(smp,  type="b", ylim=c(20,30), ylab="Temperature", col = "grey")
        lines(controlP[,n])
    }
#   minimum <- c(minimum, mean(diffx))
#}