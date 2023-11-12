
source("doLoadSources.R")

processAll <- function() {
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    sessionLabel <- "EC2_B105"
    elecNumbers <- 1:256
    peaksLowpassCutoff <- 0.8
    peaksLowpassTransitionWidth <- 0.2
    duration <- 697
    fromTime <- 0
    nrow <- 16
    ncol <- 16
    peaksLowpassRipple <- 0.1
    transcriptionSampleRate <- 1e7
    plotLowpass <- FALSE
    titlePattern <- "%d:%02d"
    bandpassedFilenamePattern <- "results/%s/bandpassedFilteredFrom%.02fTo%.02fOrder%02dWav%d%d.RData"
    transcriptionFilenamePattern <- "../data/transcriptionFiles/%s/%s_transcription_final_completed.lab"
    figDirnamePattern <- "videos/%s/phasesAtCVSPresentationsElec%03d-%03dFrom%03dTo%03dHtFilteredFrom%.02fTo%0.2fOrder%02d"
    figFilenamePattern <- "phaseIndex%06d.png"

    toTime <- fromTime + duration
    transcriptionFilename <- sprintf(transcriptionFilenamePattern, sessionLabel, sessionLabel)
    saveVoltagePhaseAtCVSPresentationsImagesGGPlot2(sessionLabel=sessionLabel,
                     bandpassedFilenamePattern=bandpassedFilenamePattern, 
                     figDirnamePattern=figDirnamePattern, 
                     figFilenamePattern=figFilenamePattern,
                     elecNumbers=elecNumbers,
                     lowCutoff=lowCutoff, highCutoff=highCutoff, order=order,
                     fromTime=fromTime, toTime=toTime,
                     transcriptionFilename=transcriptionFilename,
                     transcriptionSampleRate=transcriptionSampleRate,
                     peaksLowpassCutoff=peaksLowpassCutoff,
                     peaksLowpassTransitionWidth=peaksLowpassTransitionWidth,
                     peaksLowpassRipple=peaksLowpassRipple,
                     titlePattern=titlePattern, 
                     plotLowpass=plotLowpass,
                     nrow=nrow, ncol=ncol)
}

processAll()

rm(processAll)
