
source("doLoadSources.R")
require(ggplot2)

processAll <- function() {
    ventroDorsalSpeed <- FALSE
    sessionName <- "EC2_B105"
    elecNumbers <- 135:142
    significance <- 0.01
    rThreshold <- .85
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
    if(ventroDorsalSpeed) {
        xlab <- "Latency 'start ventro-dorsal CWE' - 'start CVS' (sec)"
    } else {
        xlab <- "Latency 'start dorso-ventral CWE' - 'start CVS' (sec)"
    }
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
     "../../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final_completed.lab"
    rTranscriptionFilename <-
     "../../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final_completed_randomized.lab"
    contiguousWaveEventsFilenamePattern <- 
     "results/%s/contiguousWaveEventsWithoutPhaseUnwrappingDatacubeSignificance%.02frTheshold%.02fFilteredFrom%.02fTo%.02fOrder%02dZScored%dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.%s"
    if(ventroDorsalSpeed) {
        figFilenamePattern <- 
         "figures/%s/histLatenciesBtwCVSsAndPositiveSpeedCWEInitiationsWithRandomizedSignificance%.02frTheshold%.02fFilteredFrom%.02fTo%.02fOrder%02dZScored%dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.png"
    } else {
        figFilenamePattern <- 
         "figures/%s/histLatenciesBtwCVSsAndNegativeSpeedCWEInitiationsWithRandomizedSignificance%.02frTheshold%.02fFilteredFrom%.02fTo%.02fOrder%02dZScored%dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.png"
    }

    figFilename <- sprintf(figFilenamePattern, sessionName, significance, rThreshold, lowCutoff, highCutoff, order, zScore, saveFromTime, saveToTime, saveDT, elecNumbers[1], elecNumbers[length(elecNumbers)])
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
    rInfoInit<- 
     getInfoCVSsInitiations(transcriptionFilename=
                              rTranscriptionFilename,
                             transcriptionSampleRate=transcriptionSampleRate,
                             ecogSampleRate=transcriptionSampleRate)
    rInfoTerm<- 
     getInfoCVSsTerminations(transcriptionFilename=
                              rTranscriptionFilename,
                             transcriptionSampleRate=transcriptionSampleRate,
                             ecogSampleRate=transcriptionSampleRate)
    contiguousWaveEventsFilename <- sprintf(contiguousWaveEventsFilenamePattern, sessionName, significance, rThreshold, lowCutoff, highCutoff, order, zScore, saveFromTime, saveToTime, saveDT, elecNumbers[1], elecNumbers[length(elecNumbers)], "RData")
    cwes <- get(load(file=contiguousWaveEventsFilename))
    cwes <- cwes[cwes[,2]>cwesStartTime,]
    cwes <- cwes[cwes[,2]-cwes[,1]+saveDT>minCWEDuration,] # only look at CWEs longer than minCWEDuration
    if(ventroDorsalSpeed) {
        cwes <- cwes[which(cwes[,3]>0),] # only look at CWEs with positive speeds
    } else {
        cwes <- cwes[which(cwes[,3]<0),] # only look at CWEs with positive speeds
    }

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
            index <- which.min(abs(cvsStarts-cweStart))
            latency <- cweStart-cvsStarts[index]
            latencies[i] <- latency
        }
        if(cweOverlapsCVSs(cweStart=cweStart, 
                            cweEnd=cweEnd,
                            cvsStarts=rCVSStarts, 
                            cvsEnds=rCVSEnds)) {
            index <- which.min(abs(rCVSStarts-cweStart))
            rLatency <- cweStart-rCVSStarts[index]
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
    p <- p + scale_fill_manual(name="CVS start order", values=c("blue", "red"),
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
