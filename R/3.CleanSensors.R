##------------------------------Quality control-------------------------------##
## This R-script can be used to automatically check the data to see whether this
## fits within the physical boundaries for the region, but the ID of sensors can
## also be removed manually from the lists in case the automatic control doesn't
## remove all 'bad'sensors. Automatic quality control will be implemented later.
##--------------------------------v.26-08-14----------------------------------##

# Automatically remove sensors that do not meet quality criteria

# Manually remove sensors that do not meet quality criteria
    clean.sensors <- function (temp_sensors = c("ID003", 
                                                "ID013"),
                               humd_sensors = c("ID003",
                                                "ID013")){
        
        lt <- list.ok$temperature
        lh <- list.ok$humidity
        lt <-  lt[ , -which(names(lt) %in% temp_sensors)]
        lh <-  lh[ , -which(names(lh) %in% humd_sensors)]
        lt <- na.omit(lt)
        lh <- na.omit(lh)
        list.cor <<- list(lt, lh)

    }