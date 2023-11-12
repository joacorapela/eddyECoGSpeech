
getLatenciesFromPrevPeak <- function(x, times) {
    peakTimes <- findPeaksOfTimeSeries(x=x, times=times)$times
    latenciesInSecs <- rep(NA, times=length(x))
    latenciesInRads <- rep(NA, times=length(x))
    for(i in 1:length(x)) {
        sampleTime <- times[i]
        prevPeaksIndices <- which(peakTimes<sampleTime)
        if(length(prevPeaksIndices)>0) {
            prevPeakIndex <- prevPeaksIndices[length(prevPeaksIndices)]
            prevPeakTime <- peakTimes[prevPeakIndex]
            latenciesInSecs[i] <- sampleTime-prevPeakTime
            if(prevPeakIndex<length(peakTimes)) {
                nextPeakTime <- peakTimes[prevPeakIndex+1]
                interPeakDistance <- nextPeakTime-prevPeakTime
            } else {
                prevPrevPeakTime <- peakTimes[prevPeakIndex-1]
                interPeakDistance <- prevPeakTime-prevPrevPeakTime
            }
            latenciesInRads[i] <- latenciesInSecs[i]/interPeakDistance*2*pi
        }
    }
    answer <- list(latenciesInSecs=latenciesInSecs,
                    latenciesInRads=latenciesInRads,
                    peakTimes=peakTimes)
    return(answer)
}
