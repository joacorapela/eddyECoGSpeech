
source("doLoadSources.R")

processAll <- function() {
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    zScore <- FALSE
    sessionLabel <- "EC2_B105"
    titlePattern <- "%d:%02d"
    # elecNumbers <- 162:256
    # elecNumbers <- c(37:39, 50:54, 67:70, 85:86, 101:103)
    elecNumbers <- 1:256
    fromTime <- 340
    duration <- 60
    nrow <- 16
    ncol <- 16
    desiredFrameRate <- 25
    htFilenamePattern <- "results/%s/htFilteredFrom%.02fTo%.02fOrder%02dZScored%dWav%d%d.RData"
    figDirnamePattern <- "videos/%s/phasesElec%03d-%03dFPS%.02fFrom%03dTo%03dHtFilteredFrom%.02fTo%0.2fOrder%02dZScored%d"
    figFilenamePattern <- "phaseIndex%06d.png"

    toTime <- fromTime + duration
    savePhaseImagesGGPlot2(sessionLabel=sessionLabel,
                               htFilenamePattern=htFilenamePattern, 
                               figDirnamePattern=figDirnamePattern, 
                               figFilenamePattern=figFilenamePattern,
                               elecNumbers=elecNumbers,
                               lowCutoff=lowCutoff, highCutoff=highCutoff, 
                               order=order,
                               zScore=zScore,
                               fromTime=fromTime, toTime=toTime,
                               desiredFrameRate=desiredFrameRate,
                               titlePattern=titlePattern, 
                               nrow=nrow, ncol=ncol)
}

processAll()

rm(processAll)
