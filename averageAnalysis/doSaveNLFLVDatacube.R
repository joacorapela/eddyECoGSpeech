
source("doLoadSources.R")

processAll <- function() {
    sessionLabel <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    elecNumbers <- 1:256
    fromTime <- 0
    toTime <- 700
    desiredSampleRate <- 256
    nrow <- 16
    ncol <- 16
    nlflvFilenamePattern <- "results/%s/nlflvFilteredFrom%.02fTo%.02fOrder%02dWav%d%d.RData"
    nlflvDatacubeFilenamePattern <- "results/%s/nlflvDatacubeFilteredFrom%.02fTo%.02fOrder%02dFromTime%.02fTo%.02f.RData"

    nlflvDatacube <- buildNLFLVDatacube(elecNumbers=elecNumbers, 
                                   fromTime=fromTime, toTime=toTime, 
                                   nlflvFilenamePattern=nlflvFilenamePattern, 
                                   sessionLabel=sessionLabel, 
                                   lowCutoff=lowCutoff, highCutoff=highCutoff,
                                   order=order, 
                                   desiredSampleRate=desiredSampleRate,
                                   nrow=nrow, ncol=ncol)
    nlflvDatacubeFilename <- sprintf(nlflvDatacubeFilenamePattern, sessionLabel, lowCutoff, highCutoff, order, fromTime, toTime)
    save(nlflvDatacube, file=nlflvDatacubeFilename)
}

processAll()
