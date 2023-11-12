
source("doLoadSources.R")

processAll <- function() {
    lowCutoff <- 0.4
    highCutoff <- 0.8
    sessionLabel <- "EC2_B105"
    titlePattern <- "%d:%02d"
    elecNumbers <- 1:256
    fromTime <- 430
    duration <- 60
    nrow <- 16
    ncol <- 16
    desiredFrameRate <- 381
    width <- 6
    height <- 6
    htFilenamePattern <- "results/%s/htFilteredFrom%.02fTo%.02fWav%d%d.RData"
    figDirnamePattern <- "movies/%s/amplitudes%.02ffpsFrom%03dTo%03dHtFilteredFrom%.02fTo%0.2f"
    figFilenamePattern <- "amplitudeIndex%06d.png"

    toTime <- fromTime + duration
    saveAmplitudeImages(sessionLabel=sessionLabel,
                     htFilenamePattern=htFilenamePattern, 
                     figDirnamePattern=figDirnamePattern, 
                     figFilenamePattern=figFilenamePattern,
                     elecNumbers=elecNumbers,
                     lowCutoff=lowCutoff, highCutoff=highCutoff,
                     fromTime=fromTime, toTime=toTime,
                     desiredFrameRate=desiredFrameRate,
                     titlePattern=titlePattern, 
                     nrow=nrow, ncol=ncol,
                     width=width, height=height)
}

processAll()

rm(processAll)
