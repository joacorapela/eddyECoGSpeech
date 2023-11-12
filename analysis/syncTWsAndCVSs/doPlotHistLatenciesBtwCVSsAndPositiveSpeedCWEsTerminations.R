
source("doLoadSources.R")

processAll <- function() {
    sessionName <- "EC2_B105"
    elecNumbers <- 136:141
    significance <- 0.01
    nBins <- 20
    minCWEDuration <- 0.36
    cwesStartTime <- 30 # seconds
    xlab <- "Latency CVS-\"CWE with Speed>0\" Termination (sec)"
    ylab <- "Number of Events"
    width <- 6
    height <- 6
    units <- "in"
    vlineAt0Col <- "gray"
    vlineAtMedianCol <- "red"
    vlineAtQuantilesCol <- "red"
    transcriptionSampleRate <- 1e7
    transcriptionFilename <-
     "../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final_completed.lab"
    contiguousWaveEventsFilenamePattern <- 
     "results/%s/contiguousWaveEventsFromElec%03dToElec%03dSign%.02f.RData"
    figFilenamePattern <- 
     "figures/%s/histLatenciesBtwCVSsAndPositiveSpeedCWETerminationsFromElec%03dToElec%03d.eps"

    figFilename <- sprintf(figFilenamePattern, sessionName, elecNumbers[1],
                                               elecNumbers[length(elecNumbers)])
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
    contiguousWaveEventsFilename <- sprintf(contiguousWaveEventsFilenamePattern,
                                             sessionName,
                                             elecNumbers[1], 
                                             elecNumbers[length(elecNumbers)],
                                             significance)
    cwes <- get(load(file=contiguousWaveEventsFilename))
    cwes <- cwes[cwes[,2]>cwesStartTime,]
    cwes <- cwes[cwes[,2]-cwes[,1]+saveDT>minCWEDuration,] # only look at CWEs longer than minCWEDuration
    cwes <- cwes[which(cwes[,3]>0),] # only look at CWEs with positive speeds

    cvsStarts <- infoInit$time
    cvsEnds <- infoTerm$time
    latencies <- array(data=NA, dim=length(nrow(cwes)))
    for(i in 1:nrow(cwes)) {
        cweStart <- cwes[i, 1]
        cweEnd <- cwes[i, 2]
        if(cweOverlapsCVSs(cweStart=cweStart, 
                            cweEnd=cweEnd,
                            cvsStarts=cvsStarts, 
                            cvsEnds=cvsEnds)) {
            index <- which.min(abs(cvsEnds-cweEnd))
            latency <- cvsEnds[index]-cweEnd
            latencies[i] <- latency
        }
    }
    quantiles <- quantile(x=latencies, probs=c(.05, .5, .95), na.rm=TRUE)
    df <- data.frame(latencies=latencies)
    p <- ggplot(df, aes(x=latencies))
    p <- p + geom_histogram(bins=nBins)
    p <- p + xlab(xlab)
    p <- p + ylab(ylab)
    p <- p + geom_vline(aes(xintercept=0), color=vlineAt0Col)
    p <- p + geom_vline(aes(xintercept=quantiles[2]), color=vlineAtMedianCol)
    p <- p + scale_color_discrete("")
    # p <- p + geom_vline(aes(xintercept=quantiles[1]), color=vlineAtQuantilesCol)
    # p <- p + geom_vline(aes(xintercept=quantiles[2]), color=vlineAtQuantilesCol)
    p <- p + xlim(quantiles[c(1,3)])
    ggsave(p, file=figFilename, width=width, height=height, units=units)
    print(p)

    browser()
}

processAll()

rm(processAll)
