
source("doLoadSources.R")

processAll <- function() {
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    # lowCutoff <- 1.0
    # highCutoff <- 1.4
    # order <- 3
    zScore <- TRUE
    sessionLabel <- "EC2_B105"
    elecNumbers <- 1:256
    fromTime <- 00
    duration <- 700
    pixelSpacing <- 1
    nrow <- 16
    ncol <- 16
    desiredSampleRate <- 25
    htFilenamePattern <- "results/%s/htFilteredFrom%.02fTo%.02fOrder%02dZScored%dWav%d%d.RData"
    gradientsFilenamePattern <- "results/%s/gradientsHTFilteredFrom%.02fTo%.02fOrder%02dZScored%dFPS%.02fElecFrom%03dTo%03dTimeFrom%.02fTo%.02f.RData"
    ifsFilenamePattern <- "results/%s/ifsHTFilteredFrom%.02fTo%.02fOrder%02dZScored%dFPS%.02fElecFrom%03dTo%03dTimeFrom%.02fTo%.02f.RData"

    toTime <- fromTime + duration
    htDatacube <- buildHTDatacube(elecNumbers=elecNumbers, 
                                   fromTime=fromTime, toTime=toTime, 
                                   htFilenamePattern=htFilenamePattern, 
                                   sessionLabel=sessionLabel, 
                                   lowCutoff=lowCutoff, highCutoff=highCutoff,
                                   order=order, zScore=zScore,
                                   desiredSampleRate=desiredSampleRate,
                                   nrow=nrow, ncol=ncol)
    htsArray <- htDatacube$htsArray
    htsArray[is.na(htsArray)] <- 0.0

    resIFs <- computeInstantaneousFrequencyForDatacube(xph=htsArray, Fs=htDatacube$actualSampleRate)
    resIFsWithTimes <- c(list(times=htDatacube$timesToSave, sampleRate=htDatacube$actualSampleRate), resIFs)
    ifsFilename <- sprintf(ifsFilenamePattern, 
                            sessionLabel, lowCutoff, highCutoff, order, zScore,
                            htDatacube$actualSampleRate, 
                            min(elecNumbers), max(elecNumbers), 
                            fromTime, toTime)
    save(resIFsWithTimes, file=ifsFilename)

    signIF <- resIFs$signIF
    resGrads <- phaseGradientComplexMultiplication(xph=htsArray,
                                                   pixelSpacing=pixelSpacing,
                                                   signIF=signIF)
    resGradsWithTimes <- c(list(times=htDatacube$timesToSave, sampleRate=htDatacube$actualSampleRate), resGrads)
    gradientsFilename <- sprintf(gradientsFilenamePattern, 
                                  sessionLabel, lowCutoff, highCutoff, order, 
                                  zScore,
                                  htDatacube$actualSampleRate, 
                                  min(elecNumbers), max(elecNumbers), 
                                  fromTime, toTime)
    save(resGradsWithTimes, file=gradientsFilename)
}

processAll()

rm(processAll)
