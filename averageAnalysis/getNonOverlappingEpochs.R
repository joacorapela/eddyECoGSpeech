
getNonOverlappingEpochs <- function(ecogData, ecogSampleRate,
                                              transcriptionFilename, 
                                              transcriptionSampleRate,
                                              epochFromTime,
                                              epochToTime,
                                              minSeparation) {
    res <- getInfoCVSsInitiations(transcriptionFilename=transcriptionFilename,
                                   transcriptionSampleRate=
                                    transcriptionSampleRate,
                                   ecogSampleRate=ecogSampleRate)
    epochSamples <- res$samples

    nonOverlappingIndices <-
     getNonOverlappingTimesIndices(times=epochSamples/ecogSampleRate, 
                                    minSeparation=minSeparation)
    noEpochSamples <- epochSamples[nonOverlappingIndices]

    epochs <- buildEpochs(data=ecogData, epochsSamples=noEpochSamples, 
                                         fromTime=epochFromTime, 
                                         toTime=epochToTime,
                                         sampleRate=ecogSampleRate)
    return(list(epochs=epochs, epochingTimes=noEpochSamples/ecogSampleRate,
                               srate=ecogSampleRate))
}
