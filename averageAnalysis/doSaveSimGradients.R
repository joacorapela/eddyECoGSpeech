
source("doLoadSources.R")

processAll <- function() {
    lowCutoff <- 12.0
    highCutoff <- 15.0
    order <- 2
    zScore <- FALSE
    sessionLabel <- "simulatedPlaneTWs"
    elecNumbers <- 1:256
    fromTime <- 0.0
    duration <- 1.0
    pixelSpacing <- 1
    nrow <- 16
    ncol <- 16
    desiredSampleRate <- 1000
    htFilenamePattern <- "results/%s/htFilteredFrom%.02fTo%.02fOrder%02dZScored%dWav%d%d.RData"
    gradientsFilenamePattern <- "results/%s/gradientsHTFilteredFrom%.02fTo%.02fOrder%02dZScored%dFPS%.02fElecFrom%03dTo%03dTimeFrom%.02fTo%.02f.RData"

    # lets search for a valid ht just to get the sample rate
    validHTFound <- FALSE
    i <- 1
    while(!validHTFound && i<=length(elecNumbers)) {
        elecNumber <- elecNumbers[i]
        res <- getGroupAndElecNumber(elecNumber=elecNumber)
        htFilename <- sprintf(htFilenamePattern, 
                               sessionLabel,
                               lowCutoff, highCutoff, order,
                               zScore,
                               res$groupNumber, res$elecNumber)
        if(file.exists(htFilename)) {
            validHTFound <- TRUE
        } else {
            i <- i+1
        }
    }
    if(!validHTFound) {
        stop(sprintf("Could not find valid ht file (%s)", htFilenamePattern))
    }
    resGroupAndElecNumber <- getGroupAndElecNumber(elecNumber=elecNumber)
    oneHT <- get(load(sprintf(htFilenamePattern, sessionLabel, lowCutoff, highCutoff, order, zScore, resGroupAndElecNumber$groupNumber, resGroupAndElecNumber$elecNumber)))
    # done searchging for a valid ht file

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
    resIF <- computeInstantaneousFrequencyForDatacube(xph=htsArray, Fs=oneHT$ecogSampleRate)
    signIF <- resIF$signIF
    resGrad <- phaseGradientComplexMultiplication(xph=htsArray,
                                                   pixelSpacing=pixelSpacing,
                                                   signIF=signIF)
    resGradWithTimes <- c(list(times=htDatacube$timesToSave), resGrad)
    gradientsFilename <- sprintf(gradientsFilenamePattern, 
                                  sessionLabel, lowCutoff, highCutoff, order, 
                                  zScore,
                                  htDatacube$actualSampleRate, 
                                  min(elecNumbers), max(elecNumbers), 
                                  fromTime, toTime)
    save(resGradWithTimes, file=gradientsFilename)
}

processAll()

rm(processAll)
