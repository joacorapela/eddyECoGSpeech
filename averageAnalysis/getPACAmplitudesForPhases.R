getPACAmplitudesForPhases <- function(phases, phaseBinsBreaks, 
                                              amplitudesBinnedByPhase) {
    pacAmplitudes <- rep(NA, length(phases))
    for(i in 1:length(phases)) {
        largerBinBreaks <- which(phases[i]<phaseBinsBreaks)
        if(length(largerBinBreaks>0)) {
            phaseBin <- largerBinBreaks[1]
        } else {
            phaseBin <- length(phaseBinsBreaks)+1
        }
        pacAmplitudes[i] <- mean(amplitudesBinnedByPhase[[phaseBin]])
    }
    return(pacAmplitudes)
}
