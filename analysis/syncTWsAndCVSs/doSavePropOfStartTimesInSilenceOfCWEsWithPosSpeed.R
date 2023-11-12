
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
    saveDT <- .1
    elecNumbers <- 136:141
    significance <- .01
    cwesStartTime <- 30 # seconds
    transcriptionSampleRate <- 1e7
    transcriptionFilename <-
     "../../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final_completed.lab"
    contiguousWaveEventsFilenamePattern <- 
     "results/%s/contiguousWaveEventsWith1DPhaseUnwrappingFilteredFrom%.02fTo%.02fOrder%02dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.%s"
    resultsFilenamePattern <- 
     "results/%s/propOfStartTimesInSilenceOfCWEsWithPosSpeedFilteredFrom%.02fTo%.02fOrder%02dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.%s"

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
    contiguousWaveEventsFilename <- sprintf(contiguousWaveEventsFilenamePattern, sessionName, lowCutoff, highCutoff, order, saveFromTime, saveToTime, saveDT, elecNumbers[1], elecNumbers[length(elecNumbers)], "RData")
    cwes <- get(load(file=contiguousWaveEventsFilename))
    cwes <- cwes[cwes[,2]>cwesStartTime,]
    cwes <- cwes[which(cwes[,3]>0),] # only look at CWEs with positive speeds
    results <- computePropEventsInSilence(cvsProductionTimingInfo=
                                            cvsProductionTimingInfo, 
                                           eventTimes=cwes[,1],
                                           prevCVSEndLag=prevCVSEndLag,
                                           followingCVSStartLag=
                                            followingCVSStartLag)
    resultsFilename <- sprintf(resultsFilenamePattern, sessionName, lowCutoff, highCutoff, order, saveFromTime, saveToTime, saveDT, elecNumbers[1], elecNumbers[length(elecNumbers)], "RData")
    save(file=resultsFilename, results)

    show(sprintf("proportion=%.02f, total=%d", results$proportion, results$total))
    browser()
}

processAll()

rm(processAll)

