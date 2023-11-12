
source("doLoadSources.R")

processAll <- function() {
    sessionName <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    saveFromTime <- 0
    saveToTime <- 700
    saveDT <- .1
    elecNumbers <- 136:141
    significance <- .01
    minCWEDuration <- 0.005
    cwesStartTime <- 30 # seconds
    nResamples <- 2000
    sdNoiseFirstTime <- .5
    nBins <- 50
    # xlim <- c(0, .5)
    colVLineT <- "red"
    xlab <- "Proportion of CWEs"
    ylab <- "Count"
    transcriptionSampleRate <- 1e7
    transcriptionFilename <-
     "../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final_completed.lab"
    contiguousWaveEventsFilenamePattern <- 
     "results/%s/contiguousWaveEventsFromElec%03dToElec%03dSign%.02f.RData"
    resultsFilenamePattern <-
     "results/%s/pValuePropCWEsWithPositiveSpeedOverlappingCVSs.R"
    figFilenamePattern <-
     "figures/%s/pValuePropCWEsWithPositiveSpeedOverlappingCVSs.eps"

    resultsFilename <- sprintf(resultsFilenamePattern, sessionName)
    figFilename <- sprintf(figFilenamePattern, sessionName)
    contiguousWaveEventsFilename <- sprintf(contiguousWaveEventsFilenamePattern,
                                             sessionName,
                                             min(elecNumbers), 
                                             max(elecNumbers),
                                             significance)
    cwes <- get(load(file=contiguousWaveEventsFilename))
    cwes <- cwes[cwes[,2]>cwesStartTime,]
    cwes <- cwes[cwes[,2]-cwes[,1]>minCWEDuration,] # only look at CWEs longer than minCWEDuration
    cwes <- cwes[which(cwes[,3]>0),] # only look at CWEs with positive speeds
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
    res <- resampleNullPropCWEs(
            propFunc=computePropCWEsOverlappingCVSs,
            cwes=cwes,
            sdNoiseFirstTime=sdNoiseFirstTime,
            infoInit=infoInit, infoTerm=infoTerm, 
            nResamples=nResamples)
    pValue <- sum(res$t0<res$t)/length(res$t)
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
    print(p)

    ggsave(plot=p, file=figFilename)
    browser()
}
    
processAll()

rm(processAll)

