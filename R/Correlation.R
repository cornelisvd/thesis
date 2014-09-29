## covariates
source(Aggregate.R)
meantemp[1] <- NULL # from list.mean
mintemp[1] <- NULL
maxtemp[1] <- NULL
meanhumd[1] <- NULL # from list.mean
minhumd[1] <- NULL
maxhumd[1] <- NULL
z <- as.data.frame(l) ## spatialaggregate function
alt <- as.numeric(z$Z)

cor.meanT <- c()
for (i in 1:nrow(meantemp)) {
    c <- cor(as.numeric(meantemp[i,]), alt)
    cor.meanT <- append(cor.meanT, c)
}

cor.minT <- c()
for (i in 1:nrow(mintemp)) {
    c <- cor(as.numeric(mintemp[i,]), alt)
    cor.minT <- append(cor.minT, c)
}

cor.maxT <- c()
for (i in 1:nrow(maxtemp)) {
    c <- cor(as.numeric(maxtemp[i,]), alt)
    cor.maxT <- append(cor.maxT, c)
}