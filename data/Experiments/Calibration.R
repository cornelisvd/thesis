library(matrixStats)

morning <- 5.2

ctrdata <- c(rowMeans(test1.station.temp), 
             rowMeans(test2.station.temp), 
             rowMeans(test3.station.temp),
             rowMeans(test4.station.temp), 
             rowMeans(test5.station.temp))

control <- matrix(ctrdata, ncol=5)

smpdata <- c(rowMeans(test1.pipes.temp), 
             rowMeans(test2.white.temp), 
             rowMeans(test3.white.temp),
             rowMeans(test4.nothing.temp), 
             rowMeans(test5.nothing.temp))

sample <- matrix(smpdata, ncol=5)

contrlH <- control[seq(1,length(control[,n]),60),]
sampleH <- sample[seq(1,length(sample[,n]),60),]

strt <- c()
endt <- c()
    for (n in 1:ncol(sample)){
        check <- matrix(append(control[,n], sample[,n]), ncol=2)
        mor <- check[((morning - 2)*60):((morning + 2)*60),]
        evn <- check[((morning + 10)*60):nrow(check),]
        s <-  which.min(abs(rowDiffs(mor)))/60 + morning - 2
        e <-  which.min(abs(rowDiffs(evn)))/60 + morning + 10
        strt <- append(strt, s)    
        endt <- append(endt, e)
    }
start <- round(median(strt))
end   <- round(median(endt))

c3 <- c()
s3 <- c()
    for (n in 1:ncol(sample)){
        contrl1 <- contrlH[,n]
        sample1 <- sampleH[,n]
        c1 <- contrl1[start:end]
        s1 <- sample1[start:end]
        c2 <- diff(c1, lag = 1)
        s2 <- diff(s1,  lag = 1)
        c3 <- append(c3, c2)
        s3 <- append(s3, s2)
    }
cor <- lm(c3~0+s3)
s4 <- predict(cor, data.frame(s3=s3))

delt <- as.data.frame(s4)
delt <- cbind(delt, c3)

ggplot(delt, aes(x=s4, y=c3)) + 
    geom_point(colour = "red", size = 5, alpha = 0.35) + 
    theme_set(theme_gray(base_size = 18)) + ylim(-4,4) + xlim(-4,4) +
    ylab("Ta change (1 hour) in the Stevenson shield") + 
    xlab("Corrected Ta change (1 hour) in the PVC shield") +
    geom_smooth(method = "lm", formula = y ~ x, colour = "blue") 

minimum <- c()

#for (m in seq(0, 2, 0.01)){

    cor <- lm(c3~0+s3)
    diffx <- c()
    for (n in 1:ncol(sample)){
        contrl1 <- contrlH[,n]
        sample1 <- sampleH[,n]
        c1 <- contrl1[start:end]
        s1 <- sample1[start:end]
        c2 <- diff(c1, lag = 1)
        s2 <- diff(s1, lag = 1)
        s4 <- predict(cor, data.frame(s3=s2))
        sam <- c(sampleH[,n][start])
        for (i in 1:length(s4)){
            s <- sam[i] + 1.35*s4[i]
            sam <- append(sam, s)
        }
        s5 <- c(sampleH[,n][1:((start))]+0.22, 
                sam[2:(length(sam)-1)]+0.11, 
                sampleH[,n][(end):length(sample1)]+0.22)
        plot(s5,  type="b", ylim=c(20,30), ylab="Temperature", col = "grey")
        lines(control[,n][seq(1,length(control[,n]),60)])
        diffx <- c(diffx, sqrt((contrl1-s5)^2))    
    }
    
    minimum <- c(minimum, mean(diffx))
}

