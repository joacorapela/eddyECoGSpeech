
computePropEventsInSilence <- 
 function(cvsProductionTimingInfo, eventTimes, 
                                   prevCVSEndLag, followingCVSStartLag) {
    N <- length(cvsProductionTimingInfo$startTime)
    laggedPrevCVSEndTimes <- cvsProductionTimingInfo$endTime[1:(N-1)]+prevCVSEndLag
    laggedFollowingCVSStartTimes <- 
     cvsProductionTimingInfo$startTime[2:N]+followingCVSStartLag
    nEventsInSilence <- 0
    eventTimesInSilence <- c()
    for(eventTime in eventTimes) {
        whichRes <- which(laggedPrevCVSEndTimes<=eventTime &
                           eventTime<=laggedFollowingCVSStartTimes)
        if(length(whichRes)>0) {
            nEventsInSilence <- nEventsInSilence+1
            eventTimesInSilence <- c(eventTimesInSilence, eventTime)
        }
    }
    propEventsInSilence <- nEventsInSilence/length(eventTimes)
    return(list(proportion=propEventsInSilence, 
                 total=nEventsInSilence,
                 eventTimesInSilence=eventTimesInSilence))
}
    
