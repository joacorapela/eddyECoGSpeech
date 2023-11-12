
source("doLoadSources.R")
require(ggplot2)

processAll <- function() {
    cwesWithPositiveSpeed <- FALSE
    sessionName <- "EC2_B105"
    elecNumbers <- 135:142
    significance <- .01
    rThreshold <- .85
    nResamples <- 2000
    sdNoiseFirstTime <- 5
    nBins <- 10
    minCWEDuration <- 0.36
    cwesStartTime <- 30 # seconds
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    zScore <- FALSE
    saveFromTime <- 0
    saveToTime <- 700
    saveDT <- .1
    cwesPhase <- "initiation"
    xlim <- c(0, .5)
    colVLineT <- "red"
    if(cwesWithPositiveSpeed) {
        xlab <- "Mean CVS Start Time - CWE Positive Speed Start Time"
    } else {
        xlab <- "Mean CVS Start Time - CWE Negative Speed Start Time"
    }
    ylab <- "Count"
    transcriptionSampleRate <- 1e7
    transcriptionFilename <-
     "../../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final_completed.lab"
    contiguousWaveEventsFilenamePattern <- 
     "results/%s/contiguousWaveEventsWithoutPhaseUnwrappingDatacubeSignificance%.02frTheshold%.02fFilteredFrom%.02fTo%.02fOrder%02dZScored%dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.%s"
    if(cwesWithPositiveSpeed) {
        resultsFilenamePattern <- 
         "results/%s/pValueMeanLatenciesBtwCVSsAndPositiveSpeedCWEsInitiationsSignificance%.02frTheshold%.02fFilteredFrom%.02fTo%.02fOrder%02dZScored%dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.RData"
        figFilenamePattern <-
         "figures/%s/histMeanLatenciesBtwCVSsAndPositiveSpeedCWEsInitiationsSignificance%.02frTheshold%.02fFilteredFrom%.02fTo%.02fOrder%02dZScored%dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.eps"
    } else {
        resultsFilenamePattern <- 
         "results/%s/pValueMeanLatenciesBtwCVSsAndNegativeSpeedCWEsInitiationsSignificance%.02frTheshold%.02fFilteredFrom%.02fTo%.02fOrder%02dZScored%dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.RData"
        figFilenamePattern <-
         "figures/%s/histMeanLatenciesBtwCVSsAndNegativeSpeedCWEsInitiationsSignificance%.02frTheshold%.02fFilteredFrom%.02fTo%.02fOrder%02dZScored%dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.eps"
    }

    infoInit <- getInfoCVSsInitiations(transcriptionFilename=
                                         transcriptionFilename,
                                        transcriptionSampleRate=
                                         transcriptionSampleRate,
                                        ecogSampleRate=transcriptionSampleRate)
    infoTerm <- getInfoCVSsTerminations(transcriptionFilename=
                                          transcriptionFilename,
                                         transcriptionSampleRate=
                                          transcriptionSampleRate,
                                         ecogSampleRate=transcriptionSampleRate)
    contiguousWaveEventsFilename <- sprintf(contiguousWaveEventsFilenamePattern, sessionName, significance, rThreshold, lowCutoff, highCutoff, order, zScore, saveFromTime, saveToTime, saveDT, elecNumbers[1], elecNumbers[length(elecNumbers)], "RData")
    resultsFilename <- sprintf(resultsFilenamePattern, sessionName, significance, rThreshold, lowCutoff, highCutoff, order, zScore, saveFromTime, saveToTime, saveDT, elecNumbers[1], elecNumbers[length(elecNumbers)], "RData")
    figFilename <- sprintf(figFilenamePattern, sessionName, significance, rThreshold, lowCutoff, highCutoff, order, zScore, saveFromTime, saveToTime, saveDT, elecNumbers[1], elecNumbers[length(elecNumbers)], "RData")
    cwes <- get(load(file=contiguousWaveEventsFilename))
    cwes <- cwes[cwes[,2]>cwesStartTime,]
    cwes <- cwes[cwes[,2]-cwes[,1]+saveDT>minCWEDuration,] # only look at CWEs longer than minCWEDuration
    if(cwesWithPositiveSpeed) {
        cwes <- cwes[which(cwes[,3]>0),] # only look at CWEs with positive speeds
    } else {
        cwes <- cwes[which(cwes[,3]<0),] # only look at CWEs with negative speeds
    }

    cvsStarts <- infoInit$time
    cvsEnds <- infoTerm$time
    overlappingCWEIndices <- c()
    for(i in 1:nrow(cwes)) {
        cweStart <- cwes[i, 1]
        cweEnd <- cwes[i, 2]
        if(cweOverlapsCVSs(cweStart=cweStart, 
                            cweEnd=cweEnd,
                            cvsStarts=cvsStarts, 
                            cvsEnds=cvsEnds)) {
            overlappingCWEIndices <- c(overlappingCWEIndices, i)
        }
    }
    overlappingCWEs <- cwes[overlappingCWEIndices,]

# assign(x="caseBands", value=c(), env=.GlobalEnv)
    res <- resampleStatNullLatenciesBtwCVSsAndCWEsPhase(
            stat=mean, infoInit=infoInit, infoTerm=infoTerm, 
            cwes=overlappingCWEs, nResamples=nResamples, cwesPhase=cwesPhase, 
            sdNoiseFirstTime=sdNoiseFirstTime)
    pValue <- sum(res$t0>res$t)/length(res$t)
    show(sprintf("p-value=%f", pValue))
    result <- c(res, pValue=pValue)
    save(file=resultsFilename, result)

    df <- data.frame(t=res$t)
    p <- ggplot(data=df, aes(x=t))
    p <- p + geom_histogram(bins=nBins)
    # p <- p + xlim(xlim)
    p <- p + xlab(xlab)
    p <- p + ylab(ylab)
    p <- p + geom_vline(xintercept=res$t0, col=colVLineT)
    ggsave(plot=p, file=figFilename)
    print(p)

    browser()
}

processAll()

rm(processAll)
