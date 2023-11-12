
computePropSignTWsWithNegSpeedBtwAfterAndBeforeOverNSignTWsWithNegSpeed <- 
 function(cvsProductionTimingInfo, waveEvents, beforeLag, afterLag, 
                                   significance) {
    indicesWithNegSpeed <- which(waveEvents$pValue<significance & 
                                  waveEvents$speed<0)
    N <- nrow(cvsProductionTimingInfo)
    laggedCVSEndTimes <- cvsProductionTimingInfo$endTime[1:(N-1)]+afterLag
    laggedCVSStartTimes <- cvsProductionTimingInfo$startTime[2:N]+beforeLag
    indicesWithNegSpeedBtwAfterAndBefore <- c()
    for(indexWithNegSpeed in indicesWithNegSpeed) {
        weTime <- waveEvents$time[indexWithNegSpeed]
        whichRes <- which(laggedCVSEndTimes<=weTime &
                                             weTime<=laggedCVSStartTimes)
        if(length(whichRes)>0) {
            indicesWithNegSpeedBtwAfterAndBefore <- 
             c(indicesWithNegSpeedBtwAfterAndBefore, indexWithNegSpeed)
        }
    }
    propSignTWsWithNegSpeedBtwAfterAndBeforeOverNSignTWsWithNegSpeed <-
     length(indicesWithNegSpeedBtwAfterAndBefore)/
      length(indicesWithNegSpeed)
    nTWsWithNegSpeedBtwAfterAndBefore <- length(indicesWithNegSpeedBtwAfterAndBefore)
    return(list(proportion=propSignTWsWithNegSpeedBtwAfterAndBeforeOverNSignTWsWithNegSpeed,
                 total=nTWsWithNegSpeedBtwAfterAndBefore))
}
    
