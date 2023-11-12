
source("doLoadSources.R")

processAll <- function() {
#     lowCutoff <- 0.6
#     highCutoff <- 1.2
#     sessionLabel <- "EC2_B1"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    sessionLabel <- "EC2_B105"
    titlePattern <- "%d:%02d"
    elecNumbers <- 1:256
#     fromTime <- 130
    fromTime <- 390
    duration <- 60
    nrow <- 16
    ncol <- 16
    desiredFrameRate <- 25
    htFilenamePattern <- "results/%s/htFilteredFrom%.02fTo%.02fWav%d%d.RData"
    figDirnamePattern <- "movies/%s/phases%.02ffpsFrom%03dTo%03d_test"
    figFilenamePattern <- "phaseHtFilteredFrom%.02fTo%0.2fIndex%06d.png"

    toTime <- fromTime + duration
    saveCosPhaseImages(sessionLabel=sessionLabel,
                        htFilenamePattern=htFilenamePattern, 
                        figDirnamePattern=figDirnamePattern, 
                        figFilenamePattern=figFilenamePattern,
                        elecNumbers=elecNumbers,
                        lowCutoff=lowCutoff, highCutoff=highCutoff,
                        fromTime=fromTime, toTime=toTime,
                        desiredFrameRate=desiredFrameRate,
                        titlePattern=titlePattern, 
                        nrow=nrow, ncol=ncol)
}

processAll()

rm(processAll)
