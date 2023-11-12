
source("doLoadSources.R")

processAll <- function() {
    sessionName <- "EC2_B105"
    elecNumbers <- 136:141
    nBins <- 30
    minCWEDuration <- 0.005
    cwesStartTime <- 30 # seconds
    xlab <- "Latency CVS-\"TW with Speed>0\" (sec)"
    ylab <- "Number of Events"
    width <- 6
    height <- 6
    units <- "in"
    vlineAt0Col <- "gray"
    vlineAtMedianCol <- "red"
    vlineAtQuantilesCol <- "red"
    transcriptionSampleRate <- 1e7
    transcriptionFilename <-
     "../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final_completed_randomized.lab"
    contiguousWaveEventsFilenamePattern <- 
     "results/%s/contiguousWaveEventsFromElec%03dToElec%03d.RData"
    figFilenamePattern <- 
     "figures/%s/histLatenciesBtwCVSsAndPositiveAndNegativeSpeedCWEInitiationsRandomizedFromElec%03dToElec%03d.png"

    figFilename <- sprintf(figFilenamePattern, sessionName, min(elecNumbers),
                                               max(elecNumbers))
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
                                             min(elecNumbers), 
                                             max(elecNumbers))
    cwes <- get(load(file=contiguousWaveEventsFilename))
    cwes <- cwes[cwes[,2]>cwesStartTime,]
    cwes <- cwes[cwes[,2]-cwes[,1]>minCWEDuration,] # only look at CWEs longer than minCWEDuration
    cwesPos <- cwes[which(cwes[,3]>0),] # only look at CWEs with positive speeds

    initLatencies <- array(data=NA, dim=length(nrow(cwes)))
    termLatencies <- array(data=NA, dim=length(nrow(cwes)))
    for(i in 1:nrow(cwesPos)) {
        cweInitiationTime <- cwes[i, 1]
        index <- which.min(abs(infoInit$time-cweInitiationTime))
        initLatency <- infoInit$time[index]-cweInitiationTime
        initLatencies[i] <- initLatency

        cweTerminationTime <- cwes[i, 2]
        index <- which.min(abs(infoTerm$time-cweTerminationTime))
        termLatency <- infoTerm$time[index]-cweTerminationTime
        termLatencies[i] <- termLatency
    }
    x <- c(initLatencies, termLatencies)
    group <- factor(c(rep("init", length(initLatencies)), 
                       rep("term", length(termLatencies))),
                     levels=c("init", "term"))
    df <- data.frame(x=x, group=group)
    # quantiles <- quantile(x=c(initLatencies, termLatencies), probs=c(.05, .5, .95), na.rm=TRUE)
    p <- ggplot(data=df, mapping=aes(x=x, fill=group))
    p <- p + geom_histogram(alpha=.5, bins=nBins, position="identity")
    p <- p + scale_fill_manual(name="phase", values=c("blue", "red"),
                                             labels=c("initiation",
                                                      "termination"))
    p <- p + xlab(xlab)
    p <- p + ylab(ylab)
    p <- p + geom_vline(aes(xintercept=0), color=vlineAt0Col)
    # p <- p + geom_vline(aes(xintercept=quantiles[2]), color=vlineAtMedianCol)
    p <- p + scale_color_discrete("")
    # p <- p + geom_vline(aes(xintercept=quantiles[1]), color=vlineAtQuantilesCol)
    # p <- p + geom_vline(aes(xintercept=quantiles[2]), color=vlineAtQuantilesCol)
    # p <- p + xlim(quantiles[c(1,3)])
    ggsave(p, file=figFilename, width=width, height=height, units=units)
    print(p)

    browser()
}

processAll()

rm(processAll)
