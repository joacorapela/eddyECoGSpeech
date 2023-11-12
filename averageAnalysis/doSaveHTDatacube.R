
source("doLoadSources.R")

processAll <- function() {
    sessionLabel <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    zScore <- TRUE
    elecNumbers <- 1:256
    fromTime <- 0
    toTime <- 700
    nrow <- 16
    ncol <- 16
    htFilenamePattern <- "results/%s/htFilteredFrom%.02fTo%.02fOrder%02dZScored%dWav%d%d.RData"
    htDatacubeFilenamePattern <- "results/%s/htDatacubeFilteredFrom%.02fTo%.02fOrder%02dZScored%dFromTime%.02fTo%.02f.%s"

    htDatacube <- buildHTDatacube(elecNumbers=elecNumbers, 
                                   fromTime=fromTime, toTime=toTime, 
                                   htFilenamePattern=htFilenamePattern, 
                                   sessionLabel=sessionLabel, 
                                   lowCutoff=lowCutoff, highCutoff=highCutoff,
                                   order=order, zScore=zScore,
                                   nrow=nrow, ncol=ncol)
    htDatacubeFilename <- sprintf(htDatacubeFilenamePattern, 
                                   sessionLabel,
                                   lowCutoff, highCutoff, order, zScore,
                                   fromTime, toTime, "RData")
    save(htDatacube, file=htDatacubeFilename)
}

processAll()
