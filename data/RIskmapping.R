daytime <- c()
for (i in 6:17){
    h <- seq(i, nlayers(temp), 24)
    daytime <- c(daytime, h)
}
d <- sort(daytime, decreasing = FALSE)
t <- 1:nlayers(temp)
n <- t[!(t %in% d)]

tempt <- temp
humdt <- humd

# Loop throught temperature data of the period to assess the thresholds

## Rust
rust <- list()
m1 <- c(0, 15, 0,  15, 18, 1,  18, 50, 0)
m2 <- c(0, 800, 0.75,  800, 900, 1,  900, 1500, 0.75)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rclmat2 <- matrix(m2, ncol=3, byrow=TRUE)
rc2 <- reclassify(dem, rclmat2)
emp <- setValues(dem, 0)
emp <- mask(emp, kml)

for (j in t){
    if (j %in% n) {
    temps <- reclassify(tempt[[j]],rclmat)
    temp2 <- temps * rc2
    rust[[j]] <- temp2
    } else {
    rust[[j]] <- emp    
    }
}
stackrust <- stack(rust)
rustmap <- calc(stackrust, sum)
rustmap <- rustmap/length(n)

## Berry disease
berryt <- list()
berryh <- list()
m3 <- c(0, 1000, 0,  1000, 2000, 1)
m4 <- c(0, 20, 0, 20, 22, 0.5,  22, 50, 0)
m5 <- c(0, 90, 0, 90, 110, 0.5)
rclmat3 <- matrix(m3, ncol=3, byrow=TRUE)
rclmat4 <- matrix(m4, ncol=3, byrow=TRUE)
rclmat5 <- matrix(m5, ncol=3, byrow=TRUE)
rc3 <- reclassify(dem, rclmat3)

for (j in t){
     temps <- reclassify(tempt[[j]],rclmat4)
     humds <- reclassify(humdt[[j]],rclmat5)
     total <- (temps + humds) * rc3
     berryt[[j]] <- total
}

berry <- stack(berryt)
berrymap <- calc(berry, mean)


## Berry borer
berryb <- list()
m6 <- c(0, 15, 0,  15, 23, 0.5, 23, 30, 1, 30, 32, 0.5, 32, 50, 0)
rclmat6 <- matrix(m6, ncol=3, byrow=TRUE)

for (j in t){
    temps <- reclassify(tempt[[j]],rclmat6)
    berryb[[j]] <- temps
}

berryst <- stack(berryb)
berrybmap <- calc(berryst, mean)


## Koleroga
kolere <- list()
m7 <- c(0, 27, 0, 27, 30, 1, 30, 50, 0)
m8 <- c(0, 95, 0.25, 95, 110, 0.75)
rclmat7 <- matrix(m7, ncol=3, byrow=TRUE)
rclmat8 <- matrix(m8, ncol=3, byrow=TRUE)

for (j in t){
    temps <- reclassify(tempt[[j]],rclmat7)
    humds <- reclassify(humdt[[j]],rclmat8)
    total <- temps * humds + 0.25
    kolere[[j]] <- temps
}

kolerst <- stack(kolere)
kolermap <- calc(kolerst, mean)

## American leafspot
leaf <- list()
m9 <- c(0, 23, 1, 23, 50, 0)
m10 <- c(0, 1100, 0.25, 1100, 1550, 0.75, 1550, 3000, 0.25)
rclmat9 <- matrix(m9, ncol=3, byrow=TRUE)
rclmat10 <- matrix(m10, ncol=3, byrow=TRUE)
rc10 <- reclassify(dem, rclmat10)

for (j in t){
    temps <- reclassify(tempt[[j]], rclmat9)
    temps <- temps*rc10 + 0.25
    leaf[[j]] <- temps
}

leafst <- stack(leaf)
leafmap <- calc(leafst, mean)

## Berry blotch
blotch <- list()
m11 <- c(0, 24, 0, 24, 26, 1, 26, 50, 0)
m12 <- c(0, 90, 0, 90, 110, 1)
rclmat11 <- matrix(m11, ncol=3, byrow=TRUE)
rclmat12 <- matrix(m12, ncol=3, byrow=TRUE)

for (j in t){
    if (j %in% n) {
        temps <- reclassify(tempt[[j]], rclmat11)
        humds <- reclassify(humdt[[j]], rclmat12)
        temp2 <- temps * humds
        blotch[[j]] <- temp2
    } else {
        blotch[[j]] <- emp    
    }
}

blotchst <- stack(blotch)
blotchmap <- calc(blotchst, sum)
blotchmap <- blotchmap/length(n)

## Stem borers
m13 <- c(0, 1000, 0.5, 1000, 3000, 1)
rclmat13 <- matrix(m13, ncol=3, byrow=TRUE)
rc13 <- reclassify(dem, rclmat13)
stemmap <- rc13


## Pink disease
pink <- list()
m14 <- c(0, 28, 0, 28, 50, 1)
m15 <- c(0, 90, 0.25, 90, 110, 0.75)
rclmat14 <- matrix(m14, ncol=3, byrow=TRUE)
rclmat15 <- matrix(m15, ncol=3, byrow=TRUE)

for (j in t){
    temps <- reclassify(tempt[[j]],rclmat14)
    humds <- reclassify(humdt[[j]],rclmat15)
    total <- temps * humds + 0.25
    pink[[j]] <- temps
}

pinkst <- stack(pink)
pinkmap <- calc(pinkst, mean)

risk <- stack(rustmap*0.3, berrymap*0.2, berrybmap*0.2, kolermap*0.1, 
              leafmap*0.05, blotchmap*0.05, stemmap*0.05, pinkmap*0.05)

risk2 <- stack(rustmap, berrymap, berrybmap, kolermap, 
              leafmap, blotchmap, stemmap, pinkmap)


days  <- c()
for(n in 1:(nlayers(berry)/24)){
    value <- c()
    for(h in 1:24){
    s <- sum(berry[[h*n]]@data@values, na.rm=TRUE)
    value <- c(value, s)
    }
    days <- c(days, sum(value))
}

    
    
    
    
   s <- sum(pinkst[[(n*24-23):(n*24)]]@data@values, na.rm=TRUE)
   print(s)
}
