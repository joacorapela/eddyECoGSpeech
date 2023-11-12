
source("doLoadSources.R")

processAll <- function() {
    stat <- median
    sessionName <- "EC2_B105"
    elecNumbers <- 136:141
    nResamples <- 2000
    cwesPhase <- "termination"
    sdNoiseFirstTime <- 5
    minCWEDuration <- 0.36
    cwesStartTime <- 30 # seconds
    nBins <- 50
    xlim <- c(0, .5)
    colVLineT <- "red"
    xlab <- "Median"
    ylab <- "Count"
    transcriptionSampleRate <- 1e7
    transcriptionFilename <-
     "../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final_completed.lab"
    contiguousWaveEventsFilenamePattern <- 
     "results/%s/contiguousWaveEventsFromElec%03dToElec%03d.RData"
    resultsFilenamePattern <-
     "results/%s/pValueMedianLatenciesBtwCVSsAndPositiveSpeedCWEsTerminations.RData"
    figFilenamePattern <-
     "figures/%s/histMedianLatenciesBtwCVSsAndPositiveSpeedCWEsTerminations.eps"

    figFilename <- sprintf(figFilenamePattern, sessionName)


    resultsFilename <- sprintf(resultsFilenamePattern, sessionName)

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
    contiguousWaveEventsFilename <- sprintf(contiguousWaveEventsFilenamePattern,
                                             sessionName,
                                             elecNumbers[1], 
                                             elecNumbers[length(elecNumbers)])
    cwes <- get(load(file=contiguousWaveEventsFilename))
    cwes <- cwes[cwes[,2]>cwesStartTime,]
    cwes <- cwes[cwes[,2]-cwes[,1]+saveDT>minCWEDuration,] # only look at CWEs longer than minCWEDuration
    cwes <- cwes[which(cwes[,3]>0),] # only look at CWEs with positive speeds

    res <- resampleStatNullLatenciesBtwCVSsAndCWEsPhase(
            stat=stat, infoInit=infoInit, infoTerm=infoTerm, cwes=cwes, 
            nResamples=nResamples, cwesPhase=cwesPhase, 
            sdNoiseFirstTime=sdNoiseFirstTime)
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
