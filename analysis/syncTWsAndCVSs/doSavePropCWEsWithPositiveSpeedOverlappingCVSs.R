
source("doLoadSources.R")

processAll <- function() {
    sessionName <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    saveFromTime <- 0
    saveToTime <- 700
    saveDT <- .1
    elecNumbers <- 136:141
    significance <- .01
    minCWEDuration <- 0.36
    cwesStartTime <- 30 # seconds
    transcriptionSampleRate <- 1e7
    transcriptionFilename <-
     "../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final_completed.lab"
    contiguousWaveEventsFilenamePattern <- 
     "results/%s/contiguousWaveEventsFromElec%03dToElec%03dSign%.02f.RData"
    resultsFilenamePattern <- 
     "results/%s/propCWEsWithPosSpeedOverlappingCVSs.RData"

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
    contiguousWaveEventsFilename <- sprintf(contiguousWaveEventsFilenamePattern,
                                             sessionName,
                                             elecNumbers[1], 
                                             elecNumbers[length(elecNumbers)],
                                             significance)
    cwes <- get(load(file=contiguousWaveEventsFilename))
    cwes <- cwes[cwes[,2]>cwesStartTime,]
    cwes <- cwes[cwes[,2]-cwes[,1]+saveDT>minCWEDuration,] # only look at CWEs longer than minCWEDuration
    cwes <- cwes[which(cwes[,3]>0),] # only look at CWEs with positive speeds
    results <- computePropCWEsOverlappingCVSs(cvsProductionTimingInfo=cvsProductionTimingInfo, cwes=cwes)
    save(file=resultsFilename, results)

    show(sprintf("proportion=%.02f, total=%.02f", results$proportion, results$total))
    browser()
}

processAll()

rm(processAll)

