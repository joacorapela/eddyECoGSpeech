
source("doLoadSources.R")

processAll <- function() {
    beforeCVSEventLag <- -0.1
    afterCVSEventLag <- 0.1
    sessionName <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    saveFromTime <- 0
    saveToTime <- 700
    saveDT <- .1
    elecNumbers <- 136:141
    significance <- .01
    cwesStartTime <- 30 # seconds
    nResamples <- 2000
    sdNoiseFirstTime <- 5
    nBins <- 50
    # xlim <- c(0, .5)
    colVLineT <- "red"
    xlab <- "Proportion of CWEs"
    ylab <- "Count"
    transcriptionSampleRate <- 1e7
    transcriptionFilename <-
     "../../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final_completed_randomized.lab"
     # "../../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final_completed.lab"
    contiguousWaveEventsFilenamePattern <- 
     "results/%s/contiguousWaveEventsWith1DPhaseUnwrappingFilteredFrom%.02fTo%.02fOrder%02dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.%s"
    resultsFilenamePattern <-
     "results/%s/pValuesPropCWEsWithPosSpeedAroundCVSStartOrEndTimesFilteredFrom%.02fTo%.02fOrder%02dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.%s"
    figFilenamePattern <-
     "figures/%s/pValuesPropCWEsWithPosSpeedAroundCVSStartOrEndTimesFilteredFrom%.02fTo%.02fOrder%02dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.eps"

    contiguousWaveEventsFilename <- sprintf(contiguousWaveEventsFilenamePattern, sessionName, lowCutoff, highCutoff, order, saveFromTime, saveToTime, saveDT, elecNumbers[1], elecNumbers[length(elecNumbers)], "RData")
    cwes <- get(load(file=contiguousWaveEventsFilename))
    cwes <- cwes[cwes[,2]>cwesStartTime,]
    cwes <- cwes[which(cwes[,3]>0),] # only look at CWEs with negative speeds
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
    res <- resampleNullPropEventsAlignedToCVSs(
            propFunc=computePropEventsAroundStartOrEndOfCVS,
            eventTimes=cwes[,2],
            cvsProductionTimingInfo=cvsProductionTimingInfo,
            sdNoiseFirstTime=sdNoiseFirstTime,
            infoInit=infoInit, infoTerm=infoTerm, 
            nResamples=nResamples,
            beforeCVSEventLag=beforeCVSEventLag,
            afterCVSEventLag=afterCVSEventLag)
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
    browser()
}
    
processAll()

rm(processAll)

