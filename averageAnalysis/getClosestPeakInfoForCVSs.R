getClosestPeakInfoForCVSs <- function(x, times, 
                                          cvsStartTimes, 
                                          lowpassCutoffHz,
                                          lowpassTransitionWidthHz,
                                          lowpassRipple,
                                          sampleRate,
                                          plotLowpass=FALSE) {
    res <- getPeakInfo(x=x, times=times, 
                           lowpassCutoffHz=lowpassCutoffHz,
                           lowpassTransitionWidthHz=lowpassTransitionWidthHz,
                           lowpassRipple=lowpassRipple,
                           sampleRate=sampleRate,
                           plotLowpass=plotLowpass)
    
    peakTimes <- res$peakTimes
    peakValues <- res$peakValues
    lowpassedX <- res$lowpassedX
    closestPeakIndices <- rep(NA, times=length(cvsStartTimes))
    closestPeakTimes <- rep(NA, times=length(cvsStartTimes))
    closestPeakValues <- rep(NA, times=length(cvsStartTimes))
    delaysPeakAndCVS <- rep(NA, times=length(cvsStartTimes))
    for(i in 1:length(cvsStartTimes)) {
        cvsStartTime <- cvsStartTimes[i]
        minIndex <- which.min(abs(peakTimes-cvsStartTime))
        closestPeakIndices[i] <- minIndex
        closestPeakTimes[i] <- peakTimes[minIndex]
        closestPeakValues[i] <- peakValues[minIndex]
        delaysPeakAndCVS[i] <- peakTimes[minIndex]-cvsStartTime
    }
    answer <- list(cvsStartTimes=cvsStartTimes, 
                    closestPeakIndices=closestPeakIndices, 
                    closestPeakTimes=closestPeakTimes, 
                    closestPeakValues=closestPeakValues,
                    delaysPeakAndCVS=delaysPeakAndCVS,
                    lowpassed=lowpassed,
                    peakTimes=peakTimes,
                    peakValues=peakValues)
    return(answer)
}
