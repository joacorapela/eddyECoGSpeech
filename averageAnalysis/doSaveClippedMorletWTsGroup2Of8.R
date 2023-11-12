
source("doLoadSources.R")

processAll <- function() {
    saveGroupNumber <- 2
    saveNumberOfGroups <- 8
    totalNumberOfElectrodes <- 256
    sessionLabel <- "EC2_B105"
    transcriptionSampleRate <- 1e7
    clipFromTime <- -0.5
    clipToTime <- 0.6
    epochFromTime <- clipFromTime-10.0
    epochToTime <- clipToTime+10.0
    fmin <- .1
    nvoice <- 10
    w0 <- 3/2*pi
    minSeparation <- 0
    decimateFactor <- 4
    ecogFilenamePattern <- "../data/rData/%s/RawHTK/Wav%d%d.bin"
    resultsFilenamePattern <- "results/%s/mwtWav%d%d.RData"
    transcriptionFilenamePattern <- "../data/transcriptionFiles/%s/%s_transcription_final_completed.lab"

    saveGroupSize <- totalNumberOfElectrodes/saveNumberOfGroups
    elecNumbers <- (saveGroupNumber-1)*saveGroupSize+1:saveGroupSize
    transcriptionFilename <- sprintf(transcriptionFilenamePattern, 
                                      sessionLabel, sessionLabel)
    for(elecNumber in elecNumbers) {
        show(sprintf("Processing electrode %d", elecNumber))
        res <- getGroupAndElecNumber(elecNumber=elecNumber)
        ecogFilename <- sprintf(ecogFilenamePattern, sessionLabel,
                                                     res$groupNumber, 
                                                     res$elecNumber)
        if(file.exists(ecogFilename)) {
            resultsFilename <- sprintf(resultsFilenamePattern, 
                                        sessionLabel, 
                                        res$groupNumber,
                                        res$elecNumber)
            saveClippedMorletWTs(
             ecogFilename=ecogFilename,
             resultsFilename=resultsFilename,
             transcriptionFilename=transcriptionFilename,
             transcriptionSampleRate=transcriptionSampleRate,
             epochFromTime=epochFromTime,
             epochToTime=epochToTime,
             clipFromTime=clipFromTime,
             clipToTime=clipToTime,
             fmin=fmin,
             nvoice=nvoice,
             w0=w0,
             minSeparation=minSeparation,
             decimateFactor=decimateFactor)
        }
    }
}

processAll()

rm(processAll)
