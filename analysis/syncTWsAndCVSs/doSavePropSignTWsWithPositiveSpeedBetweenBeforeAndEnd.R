
source("doLoadSources.R")

reportResults <- function(indicesBtwBeforeAndEnd,
                           indicesWithPositiveSpeedBtwPreviousAndEnd,
                           propSignTWsWithPositiveSpeedBtwBeforeAndEnd,
                           out) {
    writeLines(text=sprintf("number of significant wave events between before and end: %d", length(indicesBtwBeforeAndEnd)), con=out)
    writeLines(text=sprintf("number of significant wave events between before and end with speed>0: %d", length(indicesWithPositiveSpeedBtwPreviousAndEnd)), con=out)
    writeLines(text=sprintf("proportion of significant wave events between before and end with speed>0: %.04f", propSignTWsWithPositiveSpeedBtwBeforeAndEnd), con=out)
}

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
    beforePercentage <- .3
    waveEvents <- 
    transcriptionSampleRate <- 1e7
    transcriptionFilename <-
     "../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final.lab"
    waveEventsFilenamePattern <- 
     "results/%s/waveEventsFilteredFrom%.02fTo%.02fOrder%02dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.%s"
    resultsFilenamePattern <-
     "results/%s/propOfSignTWsWithPositiveSpeedBetweenBeforeAndEnd.txt"

    waveEventsFilename <- sprintf(waveEventsFilenamePattern, sessionName, lowCutoff, highCutoff, order, saveFromTime, saveToTime, saveDT, elecNumbers[1], elecNumbers[length(elecNumbers)], "RData")
    waveEvents <- get(load(file=waveEventsFilename))
    infoInit <- getInfoCVSsInitiations(transcriptionFilename=
                                         transcriptionFilename,
                                        transcriptionSampleRate=
                                         transcriptionSampleRate,
                                        ecogSampleRate=transcriptionSampleRate)
    infoCVTrans <- getInfoCVSsCVTransitions(transcriptionFilename=
                                              transcriptionFilename,
                                             transcriptionSampleRate=
                                              transcriptionSampleRate,
                                             ecogSampleRate=
                                              transcriptionSampleRate)
    infoTerm <- getInfoCVSsTerminations(transcriptionFilename=
                                          transcriptionFilename,
                                         transcriptionSampleRate=
                                          transcriptionSampleRate,
                                         ecogSampleRate=
                                          transcriptionSampleRate)
    cvsProductionTimingInfo <- data.frame(startTime=infoInit$time,
                                           transitionTime=infoCVTrans$time,
                                           endTime=infoTerm$time)
    cvsProductionTimingInfo$beforeTime <- cvsProductionTimingInfo$startTime-beforePercentage*(cvsProductionTimingInfo$endTime-cvsProductionTimingInfo$startTime)
    resultsFilename <- sprintf(resultsFilenamePattern, sessionName)
    indicesBtwBeforeAndEnd <- c()
    for(i in 1:nrow(cvsProductionTimingInfo)) {
        beforeTime <- cvsProductionTimingInfo[i, "beforeTime"]
        endTime <- cvsProductionTimingInfo[i, "endTime"]
        indicesBtwBeforeAndEnd <- c(indicesBtwBeforeAndEnd,
                                     which(waveEvents$pValue<significance &
                                            beforeTime<=waveEvents$time &
                                            waveEvents$time<endTime))
    }
    indicesWithPositiveSpeedBtwPreviousAndEnd <-
     which(waveEvents[indicesBtwBeforeAndEnd,]$pValue<significance & 
            waveEvents[indicesBtwBeforeAndEnd,]$speed>0)
    propSignTWsWithPositiveSpeedBtwBeforeAndEnd <-
     length(indicesWithPositiveSpeedBtwPreviousAndEnd)/
      length(indicesBtwBeforeAndEnd)

    out <- file(resultsFilename, "w")
    reportResults(indicesBtwBeforeAndEnd=indicesBtwBeforeAndEnd,
           indicesWithPositiveSpeedBtwPreviousAndEnd=
            indicesWithPositiveSpeedBtwPreviousAndEnd,
           propSignTWsWithPositiveSpeedBtwBeforeAndEnd=
            propSignTWsWithPositiveSpeedBtwBeforeAndEnd,
           out=out)
    close(out)

    out <- stdout()
    reportResults(indicesBtwBeforeAndEnd=indicesBtwBeforeAndEnd,
           indicesWithPositiveSpeedBtwPreviousAndEnd=
            indicesWithPositiveSpeedBtwPreviousAndEnd,
           propSignTWsWithPositiveSpeedBtwBeforeAndEnd=
            propSignTWsWithPositiveSpeedBtwBeforeAndEnd,
           out=out)

    browser()
}

processAll()

rm(processAll)

