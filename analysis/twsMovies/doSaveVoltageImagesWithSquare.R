
source("doLoadSources.R")

processAll <- function() {
    sessionLabel <- "EC2_B105"
    titlePattern <- "%d:%02d"
    elecNumbers <- 135:144
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
    voltagesFilenamePattern <- "../data/rData/%s/RawHTK/Wav%d%d.bin"
    figDirnamePattern <- "videos/%s/voltages135-144FPS%.02fFrom%03dTo%03d_squareXMin%02dXMax%02dYMin%02dYMax%02d"
    figFilenamePattern <- "voltagesIndex%06d.png"

    toTime <- fromTime + duration
    saveVoltageImagesWithSquare(sessionLabel=sessionLabel,
                     voltagesFilenamePattern=voltagesFilenamePattern, 
                     figDirnamePattern=figDirnamePattern, 
                     figFilenamePattern=figFilenamePattern,
                     elecNumbers=elecNumbers,
                     xMin=xMin, xMax=xMax, yMin=yMin, yMax=yMax,
                     fromTime=fromTime, toTime=toTime,
                     desiredFrameRate=desiredFrameRate,
                     titlePattern=titlePattern, 
                     nrow=nrow, ncol=ncol)
}

processAll()

rm(processAll)
