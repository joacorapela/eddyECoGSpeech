
source("doLoadSources.R")

processAll <- function() {
    cwesWithPositiveSpeed <- FALSE
    sessionName <- "EC2_B105"
    elecNumbers <- 135:142
    significance <- 0.01
    rThreshold <- .85
    nBins <- 10
    minCWEDuration <- 0.36
    cwesStartTime <- 30 # seconds
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    zScore <- FALSE
    saveFromTime <- 0
    saveToTime <- 700
    saveDT <- .1
    transcriptionSampleRate <- 1e7
    transcriptionFilename <-
     "../../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final_completed.lab"
    rTranscriptionFilename <-
     "../../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final_completed_randomized.lab"
    contiguousWaveEventsFilenamePattern <- 
     "results/%s/contiguousWaveEventsWithoutPhaseUnwrappingDatacubeSignificance%.02frTheshold%.02fFilteredFrom%.02fTo%.02fOrder%02dZScored%dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.%s"
    statsCWEsFilenamePattern <-
     "results/%s/statsContiguousWaveEventsWithoutPhaseUnwrappingDatacubeSignificance%.02frTheshold%.02fFilteredFrom%.02fTo%.02fOrder%02dZScored%dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.%s"

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
    rInfoInit<- 
     getInfoCVSsInitiations(transcriptionFilename=
                              rTranscriptionFilename,
                             transcriptionSampleRate=transcriptionSampleRate,
                             ecogSampleRate=transcriptionSampleRate)
    rInfoTerm<- 
     getInfoCVSsTerminations(transcriptionFilename=
                              rTranscriptionFilename,
                             transcriptionSampleRate=transcriptionSampleRate,
                             ecogSampleRate=transcriptionSampleRate)
    contiguousWaveEventsFilename <- sprintf(contiguousWaveEventsFilenamePattern, sessionName, significance, rThreshold, lowCutoff, highCutoff, order, zScore, saveFromTime, saveToTime, saveDT, elecNumbers[1], elecNumbers[length(elecNumbers)], "RData")
    cwes <- get(load(file=contiguousWaveEventsFilename))
    nSigCWEs <- nrow(cwes)
    cwes <- cwes[cwes[,2]>cwesStartTime,]
    nCWEsLaterCWEsStartTime <- nrow(cwes)
    cwes <- cwes[cwes[,2]-cwes[,1]+saveDT>minCWEDuration,] # only look at CWEs longer than minCWEDuration
    nCWEsLargerMinCWEDuration <- nrow(cwes)
    posSpeedCWEs <- cwes[which(cwes[,3]>0),] # only look at CWEs with pos speeds
    nCWEsWithPosSpeed <- nrow(posSpeedCWEs)
    negSpeedCWEs <- cwes[which(cwes[,3]<0),] # only look at CWEs with neg speeds
    nCWEsWithNegSpeed <- nrow(negSpeedCWEs)

    cwes <- negSpeedCWEs 
    cvsStarts <- infoInit$time
    cvsEnds <- infoTerm$time
    latencies <- array(data=NA, dim=length(nrow(cwes)))
    for(i in 1:nrow(cwes)) {
        cweStart <- cwes[i, 1]
        cweEnd <- cwes[i, 2]
        if(cweOverlapsCVSs(cweStart=cweStart, 
                            cweEnd=cweEnd,
                            cvsStarts=cvsStarts, 
                            cvsEnds=cvsEnds)) {
            index <- which.min(abs(cvsStarts-cweStart))
            latency <- cweStart-cvsStarts[index]
            latencies[i] <- latency
        }
    }
    propCWEsWithNegSpeedOverlapingCVSs <- length(latencies)/nrow(cwes)

    cwes <- posSpeedCWEs 
    cvsStarts <- infoInit$time
    cvsEnds <- infoTerm$time
    latencies <- array(data=NA, dim=length(nrow(cwes)))
    for(i in 1:nrow(cwes)) {
        cweStart <- cwes[i, 1]
        cweEnd <- cwes[i, 2]
        if(cweOverlapsCVSs(cweStart=cweStart, 
                            cweEnd=cweEnd,
                            cvsStarts=cvsStarts, 
                            cvsEnds=cvsEnds)) {
            index <- which.min(abs(cvsStarts-cweStart))
            latency <- cweStart-cvsStarts[index]
            latencies[i] <- latency
        }
    }
    propCWEsWithPosSpeedOverlapingCVSs <- length(latencies)/nrow(cwes)

    statsCWEsFilename <- sprintf(statsCWEsFilenamePattern, sessionName, significance, rThreshold, lowCutoff, highCutoff, order, zScore, saveFromTime, saveToTime, saveDT, elecNumbers[1], elecNumbers[length(elecNumbers)], "txt")

    con <- file(description=statsCWEsFilename, open="w")
    writeLines(text=sprintf("Number of significant CWEs: %d", nSigCWEs), con=con)
    writeLines(text=sprintf("Number of significant CWEs after %d seconds: %d", cwesStartTime, nCWEsLaterCWEsStartTime), con=con)
    writeLines(text=sprintf("Number of significant CWEs after %d seconds and lasting more than %.02f seconds: %d ", cwesStartTime, minCWEDuration, nCWEsLargerMinCWEDuration), con=con)
    writeLines(text=sprintf("Number of significant CWEs after %d seconds and lasting more than %.02f seconds with positive speed: %d", cwesStartTime, minCWEDuration, nCWEsWithPosSpeed), con=con)
    writeLines(text=sprintf("Number of significant CWEs after %d seconds and lasting more than %.02f seconds with negitive speed: %d", cwesStartTime, minCWEDuration, nCWEsWithNegSpeed), con=con)
    writeLines(text=sprintf("Proportion of CWEs with negative speed overlapping CVS: %.02f", propCWEsWithNegSpeedOverlapingCVSs), con=con)
    writeLines(text=sprintf("Proportion of CWEs with positive speed overlapping CVS: %.02f", propCWEsWithPosSpeedOverlapingCVSs), con=con)
    close(con)

    browser()
}

processAll()

rm(processAll)
