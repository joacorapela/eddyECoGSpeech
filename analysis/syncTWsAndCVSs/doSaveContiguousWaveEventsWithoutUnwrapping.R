source("doLoadSources.R")

processAll <- function() {

    sessionName <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    zScore <- FALSE
    saveFromTime <- 0
    saveToTime <- 700
    saveDT <- .1
    minCWEDuration <- .35
    # elecNumbers <- c(70, 71, 72, 88, 104, 120)
    # elecNumbers <- c(54, 70, 71, 72, 88, 104, 120, 121, 
                     # 138, 139, 140, 141, 158, 174,
                     # 154, 155, 156, 157, 173,
                                    # 172)
    # elecNumbers <- c(95, 110, 109, 108, 107, 123)
    # elecNumbers <- 135:142
    elecNumbers <- 135:142
    # elecNumbers <- seq(from=141, to=136, by=-1)
    # elecNumbers <- c(70, 71, 72, 88, 104, 120)
    significance <- .01
    rThreshold <- .85
    waveEventsFilenamePattern <- 
     "results/%s/waveEventsWithoutPhaseUnwrappingDatacubeFilteredFrom%.02fTo%.02fOrder%02dZScored%dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.%s"
    contiguousWaveEventsFilenamePattern <- 
     "results/%s/contiguousWaveEventsWithoutPhaseUnwrappingDatacubeSignificance%.02frTheshold%.02fFilteredFrom%.02fTo%.02fOrder%02dZScored%dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.%s"
    waveEventsFilename <- sprintf(waveEventsFilenamePattern, sessionName, lowCutoff, highCutoff, order, zScore, saveFromTime, saveToTime, saveDT, elecNumbers[1], elecNumbers[length(elecNumbers)], "RData")
    contiguousWaveEventsFilename <- sprintf(contiguousWaveEventsFilenamePattern, sessionName, significance, rThreshold, lowCutoff, highCutoff, order, zScore, saveFromTime, saveToTime, saveDT, elecNumbers[1], elecNumbers[length(elecNumbers)], "RData")
    wes <- get(load(file=waveEventsFilename))
    minNumberWEs <- round(minCWEDuration/saveDT)
    cwes <- getCWEs(wes=wes, significance=significance, rThreshold=rThreshold, minNumberWEs=minNumberWEs, speedChangeThrs=NA)
    save(file=contiguousWaveEventsFilename, cwes=cwes)
    browser()
}
    
processAll()

rm(processAll)

