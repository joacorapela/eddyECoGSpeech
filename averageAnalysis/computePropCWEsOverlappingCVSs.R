
computePropCWEsOverlappingCVSs <- function(cvsProductionTimingInfo, cwes) {

    cvsStarts <- cvsProductionTimingInfo$startTime
    cvsEnds <- cvsProductionTimingInfo$endTime
    indicesOverlapping <- c()
    for(index in 1:nrow(cwes)) {
        cweStart <- cwes$startTime[index]
        cweEnd <- cwes$endTime[index]
        if(cweOverlapsCVSs(cweStart=cweStart, 
                            cweEnd=cweEnd,
                            cvsStarts=cvsStarts, 
                            cvsEnds=cvsEnds)) {
            indicesOverlapping <- c(indicesOverlapping, index)
        }
    }
    propSignCWEsWithPositiveSpeedOverlapping <-
     length(indicesOverlapping)/nrow(cwes)
    nCWEsWithPositivSpeedOverlapping <- length(indicesOverlapping)
    return(list(proportion=propSignCWEsWithPositiveSpeedOverlapping,
                 total=nCWEsWithPositivSpeedOverlapping))
}
    
