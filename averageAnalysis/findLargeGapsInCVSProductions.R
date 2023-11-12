
findLargeGapsInCVSProductions <- function(gapThreshold, 
                                           ecogFilename, 
                                           transcriptionFilename,
                                           transcriptionSampleRate,
                                           epochFromTime,
                                           epochToTime,
                                           decimateFactor,
                                           minSeparation) {
    res <- getNonOverlappingEpochs(ecogFilename=ecogFilename, 
                                    transcriptionFilename=
                                     transcriptionFilename, 
                                    transcriptionSampleRate=
                                     transcriptionSampleRate,
                                    epochFromTime=epochFromTime,
                                    epochToTime=epochToTime,
                                    decimateFactor=decimateFactor,
                                    minSeparation=minSeparation)
    ecogSampleRate <- res$srate
    res <- getEpochsSamples(transcriptionFilename=transcriptionFilename,
                             transcriptionSampleRate=
                              transcriptionSampleRate,
                             ecogSampleRate=ecogSampleRate)
    epochSamples <- res$samples
    epochTimes <- epochSamples/ecogSampleRate
    previousTime <- epochTimes[1]
    largeGaps <- list()
    for(i in 2:length(epochTimes)) {
        if(epochTimes[i]-epochTimes[i-1]>gapThreshold) {
            largeGaps <- c(largeGaps, list(c(epochTimes[i-1], epochTimes[i])))
        }
    }
    return(largeGaps)
}

