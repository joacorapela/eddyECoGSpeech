
source("doLoadSources.R")

processAll <- function() {
    sessionName <- "EC2_B105"
    elecNumbers <- 136:141
    significance <- 0.01
    nBins <- 10
    minCWEDuration <- 0.005
    cwesStartTime <- 30 # seconds
    xlab <- "Latency 'term CWE speed>0' - 'term CVS' (sec)"
    ylab <- "Number of Events"
    width <- 6
    height <- 6
    units <- "in"
    vlineAt0Col <- "gray"
    vlineAtMedianCol <- "red"
    vlineAtQuantilesCol <- "blue"
    vlineAtQuantilesLinetype <- "dotted"
    transcriptionSampleRate <- 1e7
    transcriptionFilename <-
     "../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final_completed.lab"
    rTranscriptionFilename <-
     "../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final_completed_randomized.lab"
    contiguousWaveEventsFilenamePattern <- 
     "results/%s/contiguousWaveEventsFromElec%03dToElec%03dSign%.02f.RData"
    figFilenamePattern <- 
     "figures/%s/histLatenciesBtwCVSsAndPositiveSpeedCWETerminationsWithRandomizedFromElec%03dToElec%03d.png"

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
                                         ecogSampleRate=transcriptionSampleRate)
    rInfoInit <- 
     getInfoCVSsInitiations(transcriptionFilename=
                              rTranscriptionFilename,
                             transcriptionSampleRate=transcriptionSampleRate,
                             ecogSampleRate=transcriptionSampleRate)
    rInfoTerm <- 
     getInfoCVSsTerminations(transcriptionFilename=
                               rTranscriptionFilename,
                              transcriptionSampleRate=transcriptionSampleRate,
                              ecogSampleRate=transcriptionSampleRate)
    contiguousWaveEventsFilename <- sprintf(contiguousWaveEventsFilenamePattern,
                                             sessionName,
                                             min(elecNumbers), 
                                             max(elecNumbers),
                                             significance)
    cwes <- get(load(file=contiguousWaveEventsFilename))
    cwes <- cwes[cwes[,2]>cwesStartTime,]
    cwes <- cwes[cwes[,2]-cwes[,1]>minCWEDuration,] # only look at CWEs longer than minCWEDuration
    cwes <- cwes[which(cwes[,3]>0),] # only look at CWEs with positive speeds

    cvsStarts <- infoInit$time
    cvsEnds <- infoTerm$time
    rCVSStarts <- rInfoInit$time
    rCVSEnds <- rInfoTerm$time
    latencies <- array(data=NA, dim=length(nrow(cwes)))
    rLatencies <- array(data=NA, dim=length(nrow(cwes)))
    for(i in 1:nrow(cwes)) {
        cweStart <- cwes[i, 1]
        cweEnd <- cwes[i, 2]
        if(cweOverlapsCVSs(cweStart=cweStart, 
                            cweEnd=cweEnd,
                            cvsStarts=cvsStarts, 
                            cvsEnds=cvsEnds)) {
            index <- which.min(abs(cvsEnds-cweEnd))
            latency <- cweEnd-cvsEnds[index]
            latencies[i] <- latency
        }
        if(cweOverlapsCVSs(cweStart=cweStart, 
                            cweEnd=cweEnd,
                            cvsStarts=rCVSStarts, 
                            cvsEnds=rCVSEnds)) {
            index <- which.min(abs(rCVSEnds-cweEnd))
            rLatency <- cweEnd-rCVSEnds[index]
            rLatencies[i] <- rLatency
        }
    }
    quantiles <- quantile(x=latencies, probs=c(.05, .95), na.rm=TRUE)
    x <- c(latencies, rLatencies)
    group <- factor(c(rep("experimental", length(latencies)), 
                       rep("randomized", length(rLatencies))),
                     levels=c("experimental", "randomized"))
    df <- data.frame(x=x, group=group)
    p <- ggplot(data=df, mapping=aes(x=x, fill=group))
    p <- p + geom_histogram(alpha=.5, bins=nBins, position="identity")
    p <- p + scale_fill_manual(name="CVS init order", values=c("blue", "red"),
                                             labels=c("experimental",
                                                      "randomized"))
    p <- p + xlab(xlab)
    p <- p + ylab(ylab)
    p <- p + geom_vline(aes(xintercept=0), color=vlineAt0Col)
    # p <- p + geom_vline(aes(xintercept=quantiles[2]), color=vlineAtMedianCol)
    # p <- p + scale_color_discrete("")
    p <- p + geom_vline(aes(xintercept=quantiles[1]), color=vlineAtQuantilesCol, linetype=vlineAtQuantilesLinetype)
    p <- p + geom_vline(aes(xintercept=quantiles[2]), color=vlineAtQuantilesCol, linetype=vlineAtQuantilesLinetype)
    # p <- p + xlim(quantiles[c(1,3)])
    ggsave(p, file=figFilename, width=width, height=height, units=units)
    print(p)

    browser()
}

processAll()

rm(processAll)
