
source("doLoadSources.R")

processAll <- function() {
    sessionLabel <- "EC2_B105"
    lowCutoff <- 1.0
    highCutoff <- 1.4
    order <- 3
    duration <- 697
    elecNumbers <- 1:256
    xMin <- 7
    xMax <- 14
    yMin <- 5
    yMax <- 13
    fromTime <- 0
    nrow <- 16
    ncol <- 16
    desiredFrameRate <- 381
    htFilenamePattern <- "results/%s/htFilteredFrom%.02fTo%.02fOrder%02dWav%d%d.RData"
    resultsFilenamePattern <- "results/%s/meanCosPhaseInfo%.02ffpsFromFreq%.02fToFreq%.02fOrder%02dFromTime%03dToTime%03d_squareXMin%02dXMax%02dYMin%02dYMax%02d.RData"

    toTime <- fromTime + duration
    results <- getMeansCosPhaseInSquare(sessionLabel=sessionLabel,
                                         htFilenamePattern=htFilenamePattern, 
                                         elecNumbers=elecNumbers,
                                         xMin=xMin, xMax=xMax, 
                                         yMin=yMin, yMax=yMax,
                                         lowCutoff=lowCutoff, 
                                         highCutoff=highCutoff,
                                         order=order,
                                         fromTime=fromTime, toTime=toTime,
                                         desiredFrameRate=desiredFrameRate,
                                         nrow=nrow, ncol=ncol)
    resultsFilename <- sprintf(resultsFilenamePattern, sessionLabel, results$frameRate, lowCutoff, highCutoff, order, fromTime, toTime, xMin, xMax, yMin, yMax)
    save(results, file=resultsFilename)
}

processAll()

rm(processAll)
