
source("doLoadSources.R")

processAll <- function() {
    # elecNumbers <- c(136)
    elecNumbers <- c(3)
    sessionLabel <- "EC2_B105"
    transcriptionSampleRate <- 1e7
    spans <- c(41) # it should be odd
    detrend <- FALSE
    log <- "dB"
    desiredFrameRate <- 30
    # desiredFrameRate <- 3052
    decimateFactor <- 4
    nResamples <- 2000
    conf <- .95
    ecogFilenamePattern <- "../data/rData/%s/RawHTK/Wav%d%d.bin"
    transcriptionFilenamePattern <- "../data/transcriptionFiles/%s/%s_transcription_final_completed.lab"
    resultsFilenamePattern <- "results/%s/unaveragedPSDElec%d.RData"

    transcriptionFilename <- sprintf(transcriptionFilenamePattern, 
                                      sessionLabel, sessionLabel)
    for(elecNumber in elecNumbers) {
        show(sprintf("Processing electrode %d", elecNumber))
        res <- getGroupAndElecNumber(elecNumber=elecNumber)
        ecogFilename <- sprintf(ecogFilenamePattern, sessionLabel,
                                                     res$groupNumber, 
                                                     res$elecNumber)
        if(file.exists(ecogFilename)) {
            resultsFilename <- sprintf(resultsFilenamePattern, sessionLabel, 
                                                               elecNumber)
            saveUnaveragedPSD(ecogFilename=ecogFilename, 
                               resultsFilename=resultsFilename,
                               desiredFrameRate=desiredFrameRate,
                               spans=spans, detrend=detrend, log=log)
        }
    }
}

processAll()

rm(processAll)
