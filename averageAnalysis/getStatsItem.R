getStatsItem <- function(itcInfo, pacInfo,
                                  itcWinMinTime, itcWinMaxTime, itcWinFreq) {
    getMaxITC <- function(itcInfo, itcWinMinTime, itcWinMaxTime, itcWinFreq) {
        itcWinTimeIndices <- which(itcWinMinTime<=itcInfo$times & 
                                 itcInfo$times<=itcWinMaxTime)
        itcWinFreqIndex <- which.min(abs(itcInfo$freqs-itcWinFreq))
        itcWinITCValues <- itcInfo$maskedITC[itcWinTimeIndices, itcWinFreqIndex]
        maxITC <- max(itcWinITCValues)
        return(maxITC)
    }
    getPhaseAtHighestMeanAmplitude <- function(amplitudesBinnedByPhase,
                                                phaseBinsCenters) {
        meanAmplitudes <- array(NA, dim=length(amplitudesBinnedByPhase))
        for(i in 1:length(amplitudesBinnedByPhase)) {
            meanAmplitudes[i] <- mean(amplitudesBinnedByPhase[[i]])
        }
        indexMaxMeanAmplitude <- which.max(meanAmplitudes)
        return(phaseBinsCenters[indexMaxMeanAmplitude])
    }

    maxITC <- getMaxITC(itcInfo=itcInfo, itcWinMinTime=itcWinMinTime,
                                         itcWinMaxTime=itcWinMaxTime, 
                                         itcWinFreq=itcWinFreq)
    mi <- computeModulationIndex(amplitudesBinnedByPhase=
                                  pacInfo$allAmplitudesBinnedByPhase)
    phaseBinsCenters <- getPhaseBinsCenters(phaseBinsBreaks=
                                             pacInfo$phaseBinsBreaks)
    phaseAtHighestMeanAmplitude <- getPhaseAtHighestMeanAmplitude(
                                    amplitudesBinnedByPhase=
                                     pacInfo$allAmplitudesBinnedByPhase,
                                    phaseBinsCenters=phaseBinsCenters)
    return(data.frame(maxITC=maxITC, 
                       mi=mi,
                       phaseAtHighestMeanAmplitude=phaseAtHighestMeanAmplitude))
}
