
source("doLoadSources.R")

getCWEs <- function(wes, significance, speedChangeThrs=NA) {
    cwes <- c()
    cweStarted <- FALSE
    cweSpeedChangeHist <- c()
    allCWEsSpeedChangeHist <- c()
    for(i in 1:nrow(wes)) {
        if(!cweStarted && wes$pValues[i]<significance) {
            # start new cwe since there is no current one
            cweStarted <- TRUE
            cweStartTime <- wes$times[i]
            cweInitialSpeed <- wes$speeds[i]
            allCWEsSpeedChangeHist <- c(allCWEsSpeedChangeHist, cweSpeedChangeHist)
            cweSpeedChangeHist <- c()
        } else {
            if(cweStarted) {
                if(wes$pValues[i]>=significance) {
                    # stop current cwe due to loss of significance
                    cwes <- rbind(cwes, c(cweStartTime, wes$times[i-1],
                                                        wes$speeds[i-1]))
                    cweStarted <- FALSE
                } else {
                    speedChange <- wes$speeds[i]-wes$speeds[i-1]
                    # if we have enough speedChange values test to see if the
                    # current speed changed from previous ones
                    if(!is.na(speedChangeThrs[1]) &&
                        (speedChange<speedChangeThrs[1] || 
                          speedChangeThrs[2]<speedChange)) {
                        # stop current cwe due to a large change of speed and, 
                        # because wes$pValues[i]<significance, start a new cwe
                        cwes <- rbind(cwes, c(cweStartTime, wes$times[i-1], wes$speeds[i-1]))
                        cweStartTime <- wes$times[i]
                        cweInitialSpeed <- wes$speeds[i]
                        allCWEsSpeedChangeHist <- c(allCWEsSpeedChangeHist, cweSpeedChangeHist)
                        cweSpeedChangeHist <- c()
                    } else {
                        cweSpeedChangeHist <- c(cweSpeedChangeHist, speedChange)
                    }
                }
            }
        }
    }
    cwes <- data.frame(startTime=cwes[,1], endTime=cwes[,2], initialSpeed=cwes[,3])
    answer <- list(cwes=cwes, speedChangeHist=allCWEsSpeedChangeHist)
    return(answer)
}

processAll <- function() {

    sessionName <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    saveFromTime <- 0
    saveToTime <- 700
    saveDT <- .05
    elecNumbers <- 136:141
    significance <- .01
    probsQuantilesSpeedChangeThrs <- c(.001, .999)
    waveEventsFilenamePattern <- 
     "results/%s/waveEventsFilteredFrom%.02fTo%.02fOrder%02dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.%s"
    contiguousWaveEventsFilenamePattern <- 
     "results/%s/contriguousWaveEventsFilteredFrom%.02fTo%.02fOrder%02dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.%s"

    waveEventsFilename <- sprintf(waveEventsFilenamePattern, sessionName, lowCutoff, highCutoff, order, saveFromTime, saveToTime, saveDT, min(elecNumbers), max(elecNumbers), "RData")
    contiguousWaveEventsFilename <- sprintf(contiguousWaveEventsFilenamePattern, sessionName, lowCutoff, highCutoff, order, saveFromTime, saveToTime, saveDT, min(elecNumbers), max(elecNumbers), "RData")
    wes <- get(load(file=waveEventsFilename))
    res <- getCWEs(wes=wes, significance=significance, speedChangeThrs=NA)
    quantilesSpeedChangeHist <- quantile(res$speedChangeHist,
                                          probs=probsQuantilesSpeedChangeThrs)
browser()
    res <- getCWEs(wes=wes, significance=significance,
                            speedChangeThrs=quantilesSpeedChangeHist)
    cwes <- res$cwes
    save(file=contiguousWaveEventsFilename, cwes=cwes)
    browser()
}
    
processAll()

rm(processAll)

