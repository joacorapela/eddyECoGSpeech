
source("doLoadSources.R")

processAll <- function() {
#     lowCutoff <- 0.6
#     highCutoff <- 1.2
#     sessionLabel <- "EC2_B1"
    lowCutoff <- 0.4
    highCutoff <- 1.4
    order <- 3
    sessionLabel <- "EC2_B105"
    titlePattern <- "%d:%02d"
    elecNumbers <- 1:256
    xMin <- 7
    xMax <- 14
    yMin <- 5
    yMax <- 13
#     fromTime <- 130
    fromTime <- 210
    duration <- 60
    nrow <- 16
    ncol <- 16
    desiredFrameRate <- 25
    htFilenamePattern <- "results/%s/htFilteredFrom%.02fTo%.02fOrder%02dWav%d%d.RData"
    figDirnamePattern <- "videos/%s/phases%.02ffpsTimeFrom%03dTo%03dHtFilteredFrom%.02fTo%0.2fOrder%02d_squareXMin%02dXMax%02dYMin%02dYMax%02d"
    figFilenamePattern <- "cosPhaseIndex%06d.png"

    toTime <- fromTime + duration
    saveCosPhaseImagesWithSquare(sessionLabel=sessionLabel,
                     htFilenamePattern=htFilenamePattern, 
                     figDirnamePattern=figDirnamePattern, 
                     figFilenamePattern=figFilenamePattern,
                     elecNumbers=elecNumbers,
                     xMin=xMin, xMax=xMax, yMin=yMin, yMax=yMax,
                     lowCutoff=lowCutoff, highCutoff=highCutoff, order=order,
                     fromTime=fromTime, toTime=toTime,
                     desiredFrameRate=desiredFrameRate,
                     titlePattern=titlePattern, 
                     nrow=nrow, ncol=ncol)
}

processAll()

rm(processAll)
