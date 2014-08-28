##------------------------------Package install-------------------------------##
## This script can check for required packages and, when required, instal these.
##--------------------------------v.28-08-14----------------------------------##

# Function to install the required packages in the project
    pkgInstall <- function(pkg) {
        
        ## Add names of required packages here:
        
            required.packages <- c(     "ggplot2",                       
                                        "gstat",
                                        "lubridate",
                                        "raster",
                                        "rgeos",
                                        "sp",
                                        "spacetime",
                                        "splancs",
                                        "zoo"
                                   )
    
    ## Check/install the packages (from Sacha Epskamp at http://bit.ly/1pLPafb)
        for (pkg in required.packages)    
        {
            if (!require(pkg,character.only = TRUE))
            {
            install.packages(pkg,dep=TRUE)
            if(!require(pkg,character.only = TRUE)) stop("Package not found")
            }
        }    
    }     