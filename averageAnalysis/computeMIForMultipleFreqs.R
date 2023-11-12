
computeMIForMultipleFreqs <- function(timeFreqs, times, freqs,
                                                 freqsForPhases, 
                                                 freqsForAmplitudes,
                                                 nBins=18) {
    mis <- matrix(NA, nrow=length(freqsForAmplitudes),
                      ncol=length(freqsForPhases))
    phaseBinsBreaks <- seq(from=-pi, to=pi, length.out=nBins+1)
    # the breaks should not include pi or -pi 
    phaseBinsBreaks <- phaseBinsBreaks[2:nBins] 
    #
    for(i in 1:length(freqsForAmplitudes)) {
        freqForAmplitudes <- freqsForAmplitudes[i]
        for(j in 1:length(freqsForPhases)) {
            freqForPhases <- freqsForPhases[j]
            show(sprintf("Processing (%.02f, %.02f)", 
                         freqForAmplitudes, freqForPhases))
            res <- binTrialsAmplitudesByPhaseFromTimeFreqs(
                    timeFreqs=timeFreqs,
                    freqs=freqs,                 
                    freqForPhases=freqForPhases,
                    freqForAmplitudes=freqForAmplitudes,
                    phaseBinsBreaks=phaseBinsBreaks)
                amplitudesBinnedByPhase <- res$allAmplitudesBinnedByPhase
            mi <- computeModulationIndex(amplitudesBinnedByPhase=
                                          amplitudesBinnedByPhase)
            mis[i, j] <- mi
        }
    }
    return(list(mis=mis, freqsForAmplitudes=freqsForAmplitudes,
                         freqsForPhases=freqsForPhases))
}
