computePhaseDifferencesAtFreq <- function(freq, groupNumbers, elecNumbers, 
                                                timeFreqsFilenamePattern) {
    electrodes <- c()
    show(sprintf("Processing group %d, elec %d", 
                 groupNumbers[1], elecNumbers[1]))
    electrodes <- c(electrodes, getAbsoluteElectrodeNumber(groupNumber=
                                                             groupNumbers[1], 
                                                            elecNumber=
                                                             elecNumbers[1]))
    timeFreqsFilename <- sprintf(timeFreqsFilenamePattern, groupNumbers[1],
                                                           elecNumbers[1])
    res <- getElectrodePhasesAtFreq(timeFreqsFilename=timeFreqsFilename, 
                                     freq=freq)
    times <- res$times
    trials <- res$trials
    referencePhases <- res$phasesAtFreq

    # phaseDifferences \in nElec-1 x times x trials
    phaseDifferences <- array(NA, dim=c(length(elecNumbers)-1,
                                         nrow(referencePhases),
                                         ncol(referencePhases)))
    for(i in 2:length(elecNumbers)) {
        show(sprintf("Processing group %d, elec %d", 
                     groupNumbers[i], elecNumbers[i]))
        electrodes <- c(electrodes, 
                         getAbsoluteElectrodeNumber(groupNumber=
                                                      groupNumbers[1], 
                                                     elecNumber=
                                                      elecNumbers[1]))
        timeFreqsFilename <- sprintf(timeFreqsFilenamePattern, groupNumbers[i],
                                                               elecNumbers[i])
        elecPhases <- 
         getElectrodePhasesAtFreq(timeFreqsFilename=timeFreqsFilename, 
                                          freq=freq)$phasesAtFreq
        phaseDifferences[i-1,,] <- elecPhases-referencePhases
    }
    return(list(electrodes=electrodes, times=times, trials=trials,
                                       phaseDifferences=phaseDifferences))
}

getElectrodePhasesAtFreq <- function(timeFreqsFilename, freq) {
    res <- get(load(timeFreqsFilename))
    timeFreqs <- res$mwt # timeFreqs \in times x freqs x trials
    times <- res$times
    freqs <- res$freqs
    nTrials <- dim(timeFreqs)[3]
    freqIndex <- which.min(abs(freqs-freq))
    phasesAtFreq <- array(NA, dim=c(length(times), nTrials))
    for(i in 1:nTrials) {
        phasesAtFreq[, i] = Arg(timeFreqs[, freqIndex, i])
    }
    trials <- 1:nTrials
    return(list(times=times, trials=trials, phasesAtFreq=phasesAtFreq))
}

