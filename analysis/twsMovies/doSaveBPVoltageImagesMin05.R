
source("doLoadSources.R")

processAll <- function() {
    minute <- 5

    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    sessionLabel <- "EC2_B105"
    elecNumbers <- 1:256
    fromTime <- (minute-1)*60
    duration <- 60
    blurSigma <- .75
    titlePattern <- "%d:%02d"
    nrow <- 16
    ncol <- 16
    desiredFrameRate <- 25
    bandpassedFilenamePattern <- "results/%s/bandpassedFilteredFrom%.02fTo%.02fOrder%02dWav%d%d.RData"
    figDirnamePattern <- "videos/%s/voltagesElec%03d-%03dFR%.02fFrom%03dTo%03dHtFilteredFrom%.02fTo%0.2fOrder%02dBlurSigma%.02f"
    figFilenamePattern <- "voltagesIndex%06d.png"

    toTime <- fromTime + duration
    saveBPVoltageImagesGGPlot2(sessionLabel=sessionLabel,
                     bandpassedFilenamePattern=bandpassedFilenamePattern, 
                     figDirnamePattern=figDirnamePattern, 
                     figFilenamePattern=figFilenamePattern,
                     elecNumbers=elecNumbers,
                     lowCutoff=lowCutoff, highCutoff=highCutoff, order=order,
                     fromTime=fromTime, toTime=toTime,
                     desiredFrameRate=desiredFrameRate,
                     blurSigma=blurSigma,
                     titlePattern=titlePattern, 
                     nrow=nrow, ncol=ncol)
}

processAll()

rm(processAll)
