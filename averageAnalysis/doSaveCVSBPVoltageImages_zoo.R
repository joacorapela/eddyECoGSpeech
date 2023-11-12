
source("doLoadSources.R")

processAll <- function() {
    cvs <- "zoo"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    sessionLabel <- "EC2_B105"
    elecNumbers <- 1:256
    fromTimeAfterCVSInitiation <- 0 # seconds
    maxToTimeAfterCVSInitiation <- 1 # seconds
    pauseTimeBTWCVSs <- 0.5 # seconds
    pauseValueBTWCVSs <- 0
    blurSigma <- .75
    # blurSigma <- 0
    titlePattern <- "%d:%02d"
    nrow <- 16
    ncol <- 16
    desiredFrameRate <- 25
    transcriptionSampleRate <- 1e7
    transcriptionFilename <-
     "../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final_completed.lab"
    bandpassedFilenamePattern <- "results/%s/bandpassedFilteredFrom%.02fTo%.02fOrder%02dWav%d%d.RData"
    figDirnamePattern <- "videos/%s/voltagesCVS%sElec%03d-%03dFR%.02fTimeFrom%.02fTo%.02fHtFilteredFrom%.02fTo%0.2fOrder%02dBlurSigma%.02f"
    figFilenamePattern <- "voltagesIndex%06d.png"

    saveCVSBPVoltageImages(cvs=cvs, 
                            sessionLabel=sessionLabel,
                            transcriptionSampleRate=transcriptionSampleRate,
                            transcriptionFilename=transcriptionFilename,
                            bandpassedFilenamePattern=bandpassedFilenamePattern,
                            figDirnamePattern=figDirnamePattern, 
                            figFilenamePattern=figFilenamePattern,
                            elecNumbers=elecNumbers,
                            lowCutoff=lowCutoff, highCutoff=highCutoff, 
                            order=order,
                            fromTimeAfterCVSInitiation=
                             fromTimeAfterCVSInitiation, 
                            maxToTimeAfterCVSInitiation=
                             maxToTimeAfterCVSInitiation,
                            pauseTimeBTWCVSs=pauseTimeBTWCVSs,
                            pauseValueBTWCVSs=pauseValueBTWCVSs,
                            desiredFrameRate=desiredFrameRate,
                            blurSigma=blurSigma,
                            titlePattern=titlePattern, 
                            nrow=nrow, ncol=ncol)
}

processAll()

rm(processAll)
