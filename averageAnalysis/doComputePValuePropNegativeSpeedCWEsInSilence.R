
source("doLoadSources.R")

processAll <- function() {
    sessionName <- "EC2_B105"
    elecNumbers <- 136:141
    nResamples <- 2000
    sdNoiseFirstTime <- .5
    minCWEDuration <- 0.005
    cwesStartTime <- 30 # seconds
    nBins <- 30
    xlim <- c(0, .5)
    colVLineT <- "red"
    xlab <- "Proportion of CWEs with Speed<0 in Silence"
    ylab <- "Count"
    transcriptionSampleRate <- 1e7
    transcriptionFilename <-
     "../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final_completed.lab"
    contiguousWaveEventsFilenamePattern <- 
     "results/%s/contiguousWaveEventsFromElec%03dToElec%03d.RData"
    resultsFilenamePattern <-
     "results/%s/pValueVarLatenciesBtwCVSsAndPositiveSpeedCWEsTerminations.RData"
    figFilenamePattern <- "figures/%s/histPropNegativeSpeedCWEsInSilence.eps"

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
                                             min(elecNumbers), 
                                             max(elecNumbers))
    cwes <- get(load(file=contiguousWaveEventsFilename))
    cwes <- cwes[cwes[,2]>cwesStartTime,]
    cwes <- cwes[cwes[,2]-cwes[,1]>minCWEDuration,] # only look at CWEs longer than minCWEDuration
    cwes <- cwes[which(cwes[,3]<0),] # only look at CWEs with negative speeds

    res <- resamplePropCWEsInSilence(
            infoInit=infoInit, infoTerm=infoTerm, cwes=cwes, 
            nResamples=nResamples,
            sdNoiseFirstTime=sdNoiseFirstTime)
    pValue <- sum(res$t0<res$t)/length(res$t)
    show(sprintf("p-value=%.f", pValue))

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
