
source("doLoadSources.R")

processAll <- function() {
    prevCVSEndLag <- -0.1
    followingCVSStartLag <- 0.1
    sessionName <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    saveFromTime <- 0
    saveToTime <- 700
    saveDT <- .1
    # elecNumbers <- c(70, 71, 72, 88, 104, 120)
    # elecNumbers <- c(54, 70, 71, 72, 88, 104, 120, 121, 
                     # 138, 139, 140, 141, 158, 174,
                     # 154, 155, 156, 157, 173,
                                    # 172)
    # elecNumbers <- c(95, 110, 109, 108, 107, 123)
    elecNumbers <- 135:142
    # elecNumbers <- 136:141
    rThreshold <- .85
    significance <- .01
    cwesStartTime <- 30 # seconds
    nResamples <- 10000
    sdNoiseFirstTime <- 5
    nBins <- 50
    # xlim <- c(0, .5)
    colVLineT <- "red"
    xlab <- "Proportion of CWEs"
    ylab <- "Count"
    transcriptionSampleRate <- 1e7
    transcriptionFilename <-
     "../../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final_completed.lab"
    contiguousWaveEventsFilenamePattern <- 
     "results/%s/contiguousWaveEventsWith1DPhaseUnwrappingSignificance%.02frTheshold%.02fFilteredFrom%.02fTo%.02fOrder%02dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.%s"
    resultsFilenamePattern <-
     "results/%s/pValuesPropOfEndTimesInSilenceOfCWEsWithNegSpeedFilteredFrom%.02fTo%.02fOrder%02dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.%s"
    figFilenamePattern <-
     "figures/%s/pValuesPropOfEndTimesInSilenceOfCWEsWithNegSpeedFilteredFrom%.02fTo%.02fOrder%02dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.eps"

    contiguousWaveEventsFilename <- sprintf(contiguousWaveEventsFilenamePattern, sessionName, significance, rThreshold, lowCutoff, highCutoff, order, saveFromTime, saveToTime, saveDT, elecNumbers[1], elecNumbers[length(elecNumbers)], "RData")
    cwes <- get(load(file=contiguousWaveEventsFilename))
    cwes <- cwes[cwes[,2]>cwesStartTime,]
    cwes <- cwes[which(cwes[,3]<0),] # only look at CWEs with negative speeds
    infoInit <- getInfoCVSsInitiations(transcriptionFilename=
                                         transcriptionFilename,
                                        transcriptionSampleRate=
                                         transcriptionSampleRate,
                                        ecogSampleRate=transcriptionSampleRate)
    infoTerm <- getInfoCVSsTerminations(transcriptionFilename=
                                          transcriptionFilename,
                                         transcriptionSampleRate=
                                          transcriptionSampleRate,
                                         ecogSampleRate=
                                          transcriptionSampleRate)
    cvsProductionTimingInfo <- data.frame(startTime=infoInit$time,
                                           endTime=infoTerm$time)
    res <- resamplePropCWEsInSilence(infoInit=infoInit, 
                                      infoTerm=infoTerm,
                                      cwes=cwes, 
                                      nResamples=nResamples,
                                      sdNoiseFirstTime=sdNoiseFirstTime,
                                      prevCVSEndLag=prevCVSEndLag,
                                      followingCVSStartLag=followingCVSStartLag)
    pValue <- sum(res$t0<res$t)/length(res$t)
    show(sprintf("p-value=%f", pValue))
    result <- c(res, pValue=pValue)
    resultsFilename <- sprintf(resultsFilenamePattern, sessionName, lowCutoff, highCutoff, order, saveFromTime, saveToTime, saveDT, elecNumbers[1], elecNumbers[length(elecNumbers)], "RData")
    save(file=resultsFilename, result)

    df <- data.frame(t=res$t)
    p <- ggplot(data=df, aes(x=t))
    p <- p + geom_histogram(bins=nBins)
    # p <- p + xlim(xlim)
    p <- p + xlab(xlab)
    p <- p + ylab(ylab)
    p <- p + geom_vline(xintercept=res$t0, col=colVLineT)
    print(p)

    figFilename <- sprintf(figFilenamePattern, sessionName, lowCutoff, highCutoff, order, saveFromTime, saveToTime, saveDT, elecNumbers[1], elecNumbers[length(elecNumbers)], "RData")
    ggsave(plot=p, file=figFilename)

    p <- ggplotly(p)
    print(p)

    browser()
}
    
processAll()

rm(processAll)

