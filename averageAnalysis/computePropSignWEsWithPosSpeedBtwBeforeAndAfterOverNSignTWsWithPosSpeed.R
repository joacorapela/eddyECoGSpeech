
computePropSignWEsWithPosSpeedBtwBeforeAndAfterOverNSignTWsWithPosSpeed <- 
 function(cvsProductionTimingInfo, waveEvents, beforeLag, afterLag, 
                                   significance) {
    indicesWithPosSpeed <- which(waveEvents$pValue<significance & 
                                  waveEvents$speed>0)
    laggedCVSStartTimes <- cvsProductionTimingInfo$startTime+beforeLag
    laggedCVSEndTimes <- cvsProductionTimingInfo$endTime+afterLag
    indicesWithPosSpeedBtwBeforeAndAfter <- c()
    for(indexWithPosSpeed in indicesWithPosSpeed) {
        weTime <- waveEvents$time[indexWithPosSpeed]
        whichRes <- which(laggedCVSStartTimes<=weTime &
                                               weTime<=laggedCVSEndTimes)
        if(length(whichRes)>0) {
            indicesWithPosSpeedBtwBeforeAndAfter <- 
             c(indicesWithPosSpeedBtwBeforeAndAfter, indexWithPosSpeed)
        }
    }
    propSignTWsWithPositiveSpeedBtwBeforeAndEndOverNSignTWsWithPosSpeed <-
     length(indicesWithPosSpeedBtwBeforeAndAfter)/
      length(indicesWithPosSpeed)
    nTWsWithPositivSpeedBtwBeforeAndEnd <- length(indicesWithPosSpeedBtwBeforeAndAfter)
    return(list(proportion=propSignTWsWithPositiveSpeedBtwBeforeAndEndOverNSignTWsWithPosSpeed,
                 total=nTWsWithPositivSpeedBtwBeforeAndEnd))
}
    
