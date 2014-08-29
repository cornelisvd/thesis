##

setwd("/home/kees/ibuttons/catie/")

sunMap   <-  function(dem     = raster("srtm_20_11.tif"), 
                      kml     = readOGR("Aquiares2.kml", "Aquiares2"), 
                      year    = 2014, 
                      month   = 7,  
                      day     = 24,
                      tmz     = -6,
                      horizon = 10,
                      weight  = 1) 
             {

require(raster)
require(solaR)
require(rgdal)
require(rgeos)
require(insol)
require(RColorBrewer)

    lat              <- (bbox(kml)[2,1] + bbox(kml)[2,2]) / 2
    lon              <- (bbox(kml)[1,1] + bbox(kml)[1,2]) / 2
    bb <- as.matrix(bbox(kml))
        bb[1] <- bb[1] - horizon / 111
        bb[2] <- bb[2] - horizon / 111 
        bb[3] <- bb[3] + horizon / 111
        bb[4] <- bb[4] + horizon / 111
    dem              <- crop(dem, bb)   
    dem <- disaggregate(dem, fact=10, method='bilinear')

    long             <- lon
    slope            <- terrain(dem, opt='slope')
    aspect           <- terrain(dem, opt='aspect')
    col              <- rev(brewer.pal(11,"RdYlBu"))
    dl               <- daylength(lat,lon,JDymd(year,month,day),tmz)

    sunPosition <- function(year, month, day, hour=0, min=0, sec=0,
                            lat, lon) {
    
        twopi <- 2 * pi
        deg2rad <- pi / 180
    
    # Get day of the year, e.g. Feb 1 = 32, Mar 1 = 61 on leap years
        month.days <- c(0,31,28,31,30,31,30,31,31,30,31,30)
        day <- day + cumsum(month.days)[month]
        leapdays <- year %% 4 == 0 & (year %% 400 == 0 | year %% 100 != 0) & 
            day >= 60 & !(month==2 & day==60)
        day[leapdays] <- day[leapdays] + 1
        
    # Get Julian date - 2400000
        hour <- hour + min / 60 + sec / 3600 # hour plus fraction
        delta <- year - 1949
        leap <- trunc(delta / 4) # former leapyears
        jd <- 32916.5 + delta * 365 + leap + day + hour / 24
    
    # The input to the Atronomer's almanach is the difference between
    # the Julian date and JD 2451545.0 (noon, 1 January 2000)
        time <- jd - 51545
    
    # Ecliptic coordinates
        
    # Mean longitude
        mnlong <- 280.460 + .9856474 * time
        mnlong <- mnlong %% 360
        mnlong[mnlong < 0] <- mnlong[mnlong < 0] + 360
        
    # Mean anomaly
        mnanom <- 357.528 + .9856003 * time
        mnanom <- mnanom %% 360
        mnanom[mnanom < 0] <- mnanom[mnanom < 0] + 360
        mnanom <- mnanom * deg2rad
    
    # Ecliptic longitude and obliquity of ecliptic
        eclong <- mnlong + 1.915 * sin(mnanom) + 0.020 * sin(2 * mnanom)
        eclong <- eclong %% 360
        eclong[eclong < 0] <- eclong[eclong < 0] + 360
        oblqec <- 23.439 - 0.0000004 * time
        eclong <- eclong * deg2rad
        oblqec <- oblqec * deg2rad
        
    # Celestial coordinates
    # Right ascension and declination
        num <- cos(oblqec) * sin(eclong)
        den <- cos(eclong)
        ra <- atan(num / den)
        ra[den < 0] <- ra[den < 0] + pi
        ra[den >= 0 & num < 0] <- ra[den >= 0 & num < 0] + twopi
        dec <- asin(sin(oblqec) * sin(eclong))
        
    # Local coordinates
    # Greenwich mean sidereal time
        gmst <- 6.697375 + .0657098242 * time + hour
        gmst <- gmst %% 24
        gmst[gmst < 0] <- gmst[gmst < 0] + 24.
        
    # Local mean sidereal time
        lmst <- gmst + long / 15.
        lmst <- lmst %% 24.
        lmst[lmst < 0] <- lmst[lmst < 0] + 24.
        lmst <- lmst * 15. * deg2rad
        
    # Hour angle
        ha <- lmst - ra
        ha[ha < -pi] <- ha[ha < -pi] + twopi
        ha[ha > pi] <- ha[ha > pi] - twopi
        
    # Latitude to radians
        lat <- lat * deg2rad
        
    # Azimuth and elevation
        el <- asin(sin(dec) * sin(lat) + cos(dec) * cos(lat) * cos(ha))
        az <- asin(-cos(dec) * sin(ha) / cos(el))
        
    # For logic and names, see Spencer, J.W. 1989. Solar Energy. 42(4):353
        cosAzPos <- (0 <= sin(dec) - sin(el) * sin(lat))
        sinAzNeg <- (sin(az) < 0)
        az[cosAzPos & sinAzNeg] <- az[cosAzPos & sinAzNeg] + twopi
        az[!cosAzPos] <- pi - az[!cosAzPos]
        
        el <- el / deg2rad
        az <- az / deg2rad
        lat <- lat / deg2rad
        
        return(c(el, az))
    } 
    t <- data.frame()
        for (i in 0:23)               {
            t <- c(t, sunPosition(year, month, day, i, 0, 0, lat, long))
    }
   
    r <- raster()
    s <- stack()
    l <- 1 : (length(t)/2)

    for (i in 1:length(l)) {
         if (i >= floor(dl[1]) && i <= ceiling(dl[2])) {
            ds <- doshade(dem, c((90-(t[[i*2-1]])), t[[i*2]]), sombra = dem)
            hs <- hillShade(slope, aspect, 90-t[[i*2-1]], t[[i*2]])
            dc <- crop(ds, kml)
            dm <- mask(dc, kml)
            hc <- crop(hs, kml)
            hm <- mask(hc, kml)
            rs <- (weight*dm+0.667*(hm+0.5))/(weight+1)
         } else {
            r <- setValues(dem, 0)
            r <- crop(r, kml)
            rs <- mask(r, kml)
            }
        s <- addLayer(s, rs)
    }
    names(s) <- paste("time: ", l, ":00", sep="")
    sunstack <<- s
    sun <- calc(s, sum)/dl[3]

    return(plot(sun))

}