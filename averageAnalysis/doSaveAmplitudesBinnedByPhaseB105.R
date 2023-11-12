
source("doLoadSources.R")

processAll <- function() {
    # groupNumbers <- 3
    # elecNumbers <- 36
    # freqForAmplitudes <- 100.0

    sessionLabel <- "EC2_B105"
    elecNumbers <- c(1:160, 162:256)
    # elecNumbers <- c(162:256)
    freqForAmplitudes <- 100.0
    # fromTime <- 340.0
    # toTime <- 400.0
    fromTime <- 240.0
    toTime <- 300.0
    freqForPhases <- 2/1.62
    nBins <- 18
    phaseBinsBreaks <- seq(from=-pi, to=pi, length.out=nBins+1)
    # the breaks should not include pi or -pi 
    phaseBinsBreaks <- phaseBinsBreaks[2:nBins] 
    #
    timeFreqsFilenamePattern <- "results/%s/mwtWav%d%d.RData"
    resultsFilenamePattern <- "results/%s/amplitudesBinnedByPhaseWav%d%dTimeFrom%.02fTimeTo%.02f_freqPhase%.2f_freqAmp%.2f.RData"

    for(elecNumber in elecNumbers) {
        show(sprintf("Processing electrode %d", elecNumber))
        res <- getGroupAndElecNumber(elecNumber=elecNumber)
        groupNumber <- res$groupNumber
        elecNumberInGroup <- res$elecNumber
        timeFreqsFilename <- sprintf(timeFreqsFilenamePattern, sessionLabel,
                                      groupNumber, elecNumberInGroup)
        if(file.exists(timeFreqsFilename)) {
            res <- get(load(timeFreqsFilename))
            timeFreqs <- res$mwt
            freqs <- res$freqs
            epochingTimes <- res$epochingTimes
            selectedEpochs <- which(fromTime<=epochingTimes & epochingTimes<=toTime)
            selectedTimeFreqs <- timeFreqs[,,selectedEpochs]
            resultsFilename <- sprintf(resultsFilenamePattern, 
                                        sessionLabel,
                                        groupNumber, 
                                        elecNumberInGroup,
                                        fromTime, toTime,
                                        freqForPhases,
                                        freqForAmplitudes)
            res <- binTrialsAmplitudesByPhaseFromTimeFreqs(
                    timeFreqs=selectedTimeFreqs,
                    freqs=freqs,                 
                    freqForPhases=freqForPhases,
                    freqForAmplitudes=freqForAmplitudes,
                    phaseBinsBreaks=phaseBinsBreaks)
            save(res, file=resultsFilename)
        }
    }
}

processAll()

rm(processAll)
