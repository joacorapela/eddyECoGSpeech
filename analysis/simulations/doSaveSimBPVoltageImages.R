
source("doLoadSources.R")

processAll <- function() {
    lowCutoff <- 12.0
    highCutoff <- 15.0
    order <- 2
    sessionLabel <- "simulatedPlaneTWs"
    elecNumbers <- 1:256
    fromTime <- 0
    duration <- 1
    titlePattern <- "%d:%02d"
    nrow <- 16
    ncol <- 16
    desiredFrameRate <- 1000
    bandpassedFilenamePattern <- "../data/simulated/%s/results/bandpassedFilteredFrom%.02fTo%.02fOrder%02dWav%d%d.RData"
    figDirnamePattern <- "videos/%s/voltagesElec%03d-%03dFR%.02fFrom%03dTo%03dHtFilteredFrom%.02fTo%0.2fOrder%02d"
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
                     titlePattern=titlePattern, 
                     nrow=nrow, ncol=ncol)
}

processAll()

rm(processAll)
