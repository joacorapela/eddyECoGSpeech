
source("doLoadSources.R")

processAll <- function() {
    prevCVSEndLag <- -0.2
    followingCVSStartLag <- 0.2
    sessionName <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    saveFromTime <- 0
    saveToTime <- 700
    saveDT <- .05
    elecNumbers <- 136:141
    significance <- .01
    minCWEDuration <- 0.005
    cwesStartTime <- 30 # seconds
    transcriptionSampleRate <- 1e7
    transcriptionFilename <-
     "../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final_completed.lab"
    contiguousWaveEventsFilenamePattern <- 
     "results/%s/contriguousWaveEventsFilteredFrom%.02fTo%.02fOrder%02dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.%s"
    resultsFilenamePattern <- 
     "results/%s/propOfEndTimesInSlienceOfCWEsWithNegSpeed.RData"

    resultsFilename <- sprintf(resultsFilenamePattern, sessionName)
    infoInit <- getInfoCVSsInitiations(transcriptionFilename=
                                         transcriptionFilename,
                                        transcriptionSampleRate=
                                         transcriptionSampleRate,
                                        ecogSampleRate=transcriptionSampleRate)
    infoTerm <- getInfoCVSsTerminations(transcriptionFilename=
                                          transcriptionFilename,
                                         transcriptionSampleRate=
                                          transcriptionSampleRate,
                                         ecogSampleRate=
                                          transcriptionSampleRate)
    cvsProductionTimingInfo <- data.frame(startTime=infoInit$time,
                                           endTime=infoTerm$time)
    contiguousWaveEventsFilename <- sprintf(contiguousWaveEventsFilenamePattern, sessionName, lowCutoff, highCutoff, order, saveFromTime, saveToTime, saveDT, min(elecNumbers), max(elecNumbers), "RData")
    cwes <- get(load(file=contiguousWaveEventsFilename))
    cwes <- cwes[cwes[,2]>cwesStartTime,]
    cwes <- cwes[cwes[,2]-cwes[,1]>minCWEDuration,] # only look at CWEs longer than minCWEDuration
    cwes <- cwes[which(cwes[,3]<0),] # only look at CWEs with negative speeds
    results <- computePropEventsInSilence(cvsProductionTimingInfo=
                                            cvsProductionTimingInfo, 
                                           eventTimes=cwes[,2],
                                           prevCVSEndLag=prevCVSEndLag,
                                           followingCVSStartLag=
                                            followingCVSStartLag)
    save(file=resultsFilename, results)

    show(sprintf("proportion=%.02f, total=%d", results$proportion, results$total))
    browser()
}

processAll()

rm(processAll)

