
source("doLoadSources.R")
require(boot)

processAll <- function() {
    sessionName <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    zScore <- TRUE
    twTemporalFreq <- 0.62
    elecNumbers <- c(141, 140, 139, 138, 137, 136)
    distancesFromRefElec <- c(0, 4, 8, 12, 16, 20)*1e-3
    saveFromTime <- 0
    saveToTime <- 700
    saveDT <- .1
    nResamples <- 1000
    htFilenamePattern <- "results/%s/htFilteredFrom%.02fTo%.02fOrder%02dZScored%dWav%d%d.RData"
    waveEventsFilenamePattern <- "results/%s/waveEventsFilteredFrom%.02fTo%.02fOrder%02dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.%s"

    res <- getGroupAndElecNumber(elecNumber=elecNumbers[1])
    groupNumber <- res$groupNumber
    elecNumber <- res$elecNumber
    htFilename <- sprintf(htFilenamePattern,
                           sessionName,
                           lowCutoff, highCutoff, order, zScore,
                           groupNumber, elecNumber)
    if(file.exists(htFilename)) {
        res <- get(load(htFilename))
        times <- ((1:length(res$ht))-1)/res$ecogSampleRate
        refElecPhases <- Arg(res$ht)
    } else {
        stop(sprintf("%s not found", htFilename))
    }

    saveFromSample <- which.min(abs(times-saveFromTime))
    saveToSample <- which.min(abs(times-saveToTime))
    saveBySamples <- as.integer(saveDT*res$ecogSampleRate)
    saveSamples <- seq(from=saveFromSample, to=saveToSample, by=saveBySamples)
    timesToSave <- times[saveSamples]

    phaseDiffs <- c()
    distancesFromRefElecSubset <- c()
    for(i in 1:length(elecNumbers)) {
        elecNumber <- elecNumbers[i]
        show(sprintf("Processing electrode %d", elecNumber))
        res <- getGroupAndElecNumber(elecNumber=elecNumber)
        groupNumber <- res$groupNumber
        elecNumberInGroup <- res$elecNumber

        htFilename <- sprintf(htFilenamePattern,
                               sessionName,
                               lowCutoff, highCutoff, order, zScore,
                               groupNumber, elecNumberInGroup)
        if(file.exists(htFilename)) {
            res <- get(load(htFilename))
            targetElecPhases <- Arg(res$ht)
            phaseDiffs <- cbind(phaseDiffs, targetElecPhases-refElecPhases)
            distancesFromRefElecSubset <- c(distancesFromRefElecSubset, 
                                             distancesFromRefElec[i])
        } else {
            warning(sprintf("%s not found", htFilename))
        }
    }
    phaseDiffsToSave <- phaseDiffs[saveSamples,]
    stats <- getWaveEventsStats(distancesFromRefElec=distancesFromRefElec, 
                                 phaseDiffs=phaseDiffsToSave, twTemporalFreq=twTemporalFreq)
    wes <- cbind(data.frame(times=timesToSave), stats)
    waveEventsRDataFilename <- sprintf(waveEventsFilenamePattern, sessionName, lowCutoff, highCutoff, order, saveFromTime, saveToTime, saveDT, min(elecNumbers), max(elecNumbers), "RData")
    save(wes, file=waveEventsRDataFilename)
    waveEventsCSVFilename <- sprintf(waveEventsFilenamePattern, sessionName, lowCutoff, highCutoff, order, saveFromTime, saveToTime, saveDT, min(elecNumbers), max(elecNumbers), "csv")
    write.csv(x=wes, file=waveEventsCSVFilename, quote=FALSE, row.names=FALSE)

    browser()
}

processAll()
