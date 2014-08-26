##------------------------------Quality control-------------------------------##
## Function to create a single zoo-object from the *.csv files of micro-sensors.
## This is the first step and requires the setting of the correct time - format,
## the time-unit can also be changed from 'hours' to other options (eg. 'mins').
## Main output of this code will be a corrected time-series of all used sensors.
##--------------------------------v.26-08-14----------------------------------##

# Remove sensors that do not meet quality criteria
    clean.sensors <- function (temp_sensors = c("ID003", 
                                                "ID013"),
                               humd_sensors = c("ID003",
                                                "ID013")){
        
        lt <- list.ok$temperature
        lh <- list.ok$humidity
        lt <-  lt[ , -which(names(lt) %in% temp_sensors)]
        lh <-  lh[ , -which(names(lh) %in% humd_sensors)]
        list.cor <<- list(lt, lh)

    }