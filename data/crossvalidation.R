# value is the data-frame that contains time in the first column and data in the
# other columns. pts is a SpatialPoints object that contains the coordinates of
# the data. The first data point is similar to the second (after time) column in
# the dataframe. These objects have also been used for the creation of the STFDF
#------------------------------------------------------------------------------#

setwd ("~/thesis/data/")
library(gstat)
library(spacetime)
library(raster)
source("/home/kees/thesis/R/RBTS.R")

load("pts.Rdata")
load("pred.grd.Rdata")
load("variogramST.Rdata")
load("value.Rdata")

# Leave one-out cross validation

    pred.LOO <- list()
    s <- 2:length(value) ## Create a sequence that does not include the time column.

## Create new STFDF objects for all-minus-one sensors (can take a long time...).
    for (i in s){
        st <- stConstruct(value, space = list(value = s[-i]), 
                          time = value$Hour, SpatialObj = pts[-i], 
                          interval = TRUE)
        stfdf.p <- krigeST(values ~ 1, st, prd.grd, v) 
        create.rast(obj = stfdf.p)
        pred.LOO[[i]] <- brickST
        }
    pred.LOO[[1]] <- NULL
    save(pred.LOO, file = "pred.LOO.max.Rdata")

## Extract the predicted value of the left-out sensor from the relevant object. 
    pred.df <- c() 
    for (p in 1:length(pts)) {
        e <- t(extract(pred.LOO[[p]], pts[p]))
        pred.df <- cbind(pred.df, e)
        }

## Create a dataframe of residuals (predicted - actual value) of all sensors.

    pred.res <- pred.df - value[2:length(value)]
    pred.rmse <- round(sqrt(mean(pred.res^2, na.rm=TRUE)),3)

dist <- c()
d <- gDistance(pts, byid=T)*111000
for (i in 1:ncol(d)){
    o <- mean(sort(d[,i])[2])
    dist <- c(dist,o)
}
xl <- as.data.frame(l)
   
