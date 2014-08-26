# R chunks

# Remove data with too little data
# obs.ok <- est.obs[, colMeans(!is.na(est.obs[2:ncol(est.obs)])) 
#                 >= (100-na.allowed)/100]
# obs.ok$"013"   <- NULL # This is an error, should be removed
# obs.cor       <- na.omit(obs.ok)
#list.ok[[i]]  <- obs.ok
# list.cor[[i]] <- obs.cor
# names(list.cor) <- units
# list.cor       <<- list.cor 
# na.allowed  = 75, ## % NA allowed
# proj          <- "+proj=longlat +datum=WGS84"