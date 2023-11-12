
source("doLoadSources.R")

processAll <- function() {
    elecNumbers <- 1:256
    # elecNumbers <- c(3)
    sessionLabel <- "EC2_B105"
    transcriptionSampleRate <- 1e7
    epochFromTime <- -10.0
    epochToTime <- 10.0
    minSeparation <- 0
    spans <- NULL
    detrend <- FALSE
    # log <- "dB"
    log <- "no"
    desiredFrameRate <- 50
    # desiredFrameRate <- 3052
    decimateFactor <- 4
    nResamples <- 2000
    conf <- .95
    ecogFilenamePattern <- "../data/rData/%s/RawHTK/Wav%d%d.bin"
    transcriptionFilenamePattern <- "../data/transcriptionFiles/%s/%s_transcription_final_completed.lab"
    resultsFilenamePattern <- "results/%s/averagedPSDElec%dEpochFromTime%.02fToTime%.02f.RData"

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
                                        elecNumber, 
                                        epochFromTime,
                                        epochToTime)
            saveAveragedPSD(
             ecogFilename=ecogFilename,
             resultsFilename=resultsFilename,
             transcriptionFilename=transcriptionFilename,
             transcriptionSampleRate=transcriptionSampleRate,
             epochFromTime=epochFromTime,
             epochToTime=epochToTime,
             desiredFrameRate=desiredFrameRate,
             minSeparation=minSeparation,
             spans=spans, detrend=detrend, log=log,
             nResamples=nResamples, conf=conf)
        }
    }
}

processAll()

rm(processAll)
