
source("doLoadSources.R")

processAll <- function() {
    freq <- 1/1.62
    elecNumbers <- 1:16
    groupNumbers <- rep(3, times=length(elecNumbers))
    timeFreqsFilenamePattern <- "results/EC2_B105/mwtWav%d%d.RData"
    phaseDiferencesFilenamePattern <-
     "results/EC2_B105/phaseDifferencesAtFreq%.02fFromE%dToE%d.RData"

    fromElectrode <- getAbsoluteElectrodeNumber(groupNumber=groupNumbers[1], 
                                                 elecNumber=elecNumbers[1])
    toElectrode <- getAbsoluteElectrodeNumber(groupNumber=last(groupNumbers),
                                                 elecNumber=last(elecNumbers))
    phaseDiferencesFilename <- sprintf(phaseDiferencesFilenamePattern, freq,
                                        fromElectrode, toElectrode)
    phaseDifferencesAtFreq <- 
     computePhaseDifferencesAtFreq(freq=freq, groupNumbers=groupNumbers, 
                                              elecNumbers=elecNumbers, 
                                              timeFreqsFilenamePattern=
                                               timeFreqsFilenamePattern)
    save(phaseDifferencesAtFreq, file=phaseDiferencesFilename)
}

processAll()

rm(processAll)

