library(matrixStats)
exp3.2 <- function(res = exp.temp$Type2){

    control_t2 <- as.matrix(res)
    stdiff <- c()
    n <- nrow(control_t2)/7
    dayC <- c()
    for (i in 1:7){
      begin <- n*(i-1)+1
      end <- i * n
      d <- matrix(control_t2[seq(begin, end, 1),], ncol=15)
      st <- mean(rowSds(d))
      diff <- max(abs(rowDiffs(d)))
      mn <- min(d)
      me <- mean(d)
      mx <- max(d)
      q <- quantile(d, probs = seq(0, 1, 0.05), na.rm = FALSE)
      comb <- c(mn, q[2], q[3], q[6], q[11], me, q[16], q[19], q[20], mx)
      dayC <- append(dayC, comb)
      stdiff <- append(stdiff, c(st, diff))
    }
    dayC <- matrix(dayC, nrow=10)
    stdiff <- matrix(stdiff, nrow=2)
    
    
    t1 <- type2.matrix.t.h
    t2 <- type2.matrix.t.h.s
    t3 <- type2.matrix.t.2h
    t4 <- type2.matrix.t.2h.s
    t5 <- type2.matrix.r.t.h
    t6 <- type2.matrix.r.t.h.s
    t7 <- type2.matrix.r.t.2h
    t8 <- type2.matrix.r.t.2h.s
    test <- list(t1, t2, t3, t4, t5, t6, t7, t8)
    
    
    check <- list()
    devt <- list()
    
    for (j in 1:length(test)){
    
    day <- test[[j]]
    stH <- c()
    dayH <- c()
    n <- nrow(day)/7
    for (i in 1:7){
      begin <- n*(i-1)+1
      end <- i * n
      d <- matrix(day[seq(begin, end, 1),], ncol=15)
      st <- mean(rowSds(d), na.rm=TRUE)
      diff <- max(rowDiffs(d), na.rm=TRUE)
      mn <- min(d, na.rm=TRUE)
      me <- mean(d, na.rm=TRUE)
      mx <- max(d, na.rm=TRUE)
      q <- quantile(d, probs = seq(0, 1, 0.05), na.rm = TRUE)
      comb <- c(mn, q[2], q[3], q[6], q[11], me, q[16], q[19], q[20], mx)
      dayH <- append(dayH, comb)
      stH <- append(stH, c(st, diff))
    }
    dayH <- matrix(dayH, nrow=10)
    stH <- matrix(stH, nrow=2)
    stH <- rowMeans(stH)
    diff1 <- rowMeans(dayH-dayC)
    check[[j]] <- diff1
    devt[[j]]  <- stH
    }
    check <<- check
    dect  <<- devt
}


