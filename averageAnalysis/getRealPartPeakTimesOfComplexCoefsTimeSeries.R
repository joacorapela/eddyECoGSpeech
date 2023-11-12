getRealPartPeakTimesOfComplexCoefsTimeSeries <-
 function(complexCoefs, times) {
    rePrev <- Re(complexCoefs[1])
    imPrev <- Im(complexCoefs[1])
    timePrev <- times[1]
    if(rePrev>0 && imPrev<0) {
        prevQ4 <- TRUE
    } else {
        prevQ4 <- FALSE
    }
    crossingTimes <- c()
    for(i in 2:length(complexCoefs)) {
        reCur <- Re(complexCoefs[i])
        imCur <- Im(complexCoefs[i])
        timeCur <- times[i]
        if(reCur>0 && imCur>0) {
            curQ1 <- TRUE
        } else {
            curQ1 <- FALSE
        }
        if(prevQ4 && curQ1) {
            arcCur <- atan(imCur/reCur)
            arcPrev <- atan(imPrev/rePrev)
            timePrev <- times[i-1]
            timeCur <- times[i]
            den <- (arcCur+arcPrev)
            crossingTimes <- c(crossingTimes, 
                                (arcCur*timeCur+arcPrev*timePrev)/den)
            prevQ4 <- FALSE
        } else {
            if(reCur>0 && imCur<0) {
                prevQ4 <- TRUE
            } else {
                prevQ4 <- FALSE
            }
        }
        rePrev <- reCur
        imPrev <- imCur
        timePrev <- timeCur
    }
    return(crossingTimes)
}
