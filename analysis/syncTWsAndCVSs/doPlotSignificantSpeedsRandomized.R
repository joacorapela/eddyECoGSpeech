
source("doLoadSources.R")
require(ggplot2)
require(plotly)

processAll <- function() {
    sessionName <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    minCWEDuration <- .35 # seconds
    saveFromTime <- 0
    saveToTime <- 700
    saveDT <- .1
    plotFromTime <- 000
    plotToTime <- 700
    plotStep <- 30
    elecNumbers <- 135:142
    ylimQuantileProbs <- c(0.0, 1.0)
    significance <- .01
    rThreshold <- .85
    xlab <- "Time (sec)"
    ylab <- "Speed (m/sec)"
    width <- 30
    height <- 7.5
    units <- "in"
    colLines <- "black"
    colPoints <- "red"
    colVLines <- "black"
    colHLines <- "black"
    sizeAnnotate <- 4
    transcriptionSampleRate <- 1e7
    transcriptionFilename <-
     "../../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final_completed_randomized.lab"
#      "../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final.lab"
    waveEventsFilenamePattern <- "results/%s/waveEventsWith1DPhaseUnwrappingFilteredFrom%.02fTo%.02fOrder%02dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.%s"
    contiguousWaveEventsFilenamePattern <- 
     "results/%s/contiguousWaveEventsWith1DPhaseUnwrappingSignificance%.02frTheshold%.02fFilteredFrom%.02fTo%.02fOrder%02dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.%s"

    waveEventsFilename <- sprintf(waveEventsFilenamePattern, sessionName, lowCutoff, highCutoff, order, saveFromTime, saveToTime, saveDT, elecNumbers[1], elecNumbers[length(elecNumbers)], "RData")
    contiguousWaveEventsFilename <- sprintf(contiguousWaveEventsFilenamePattern, sessionName, significance, rThreshold, lowCutoff, highCutoff, order, saveFromTime, saveToTime, saveDT, elecNumbers[1], elecNumbers[length(elecNumbers)], "RData")
    waveEvents <- get(load(file=waveEventsFilename))
    cwes <- get(load(file=contiguousWaveEventsFilename))
    cwes <- cwes[cwes[,2]-cwes[,1]+saveDT>minCWEDuration,] # only look at CWEs longer than minCWEDuration
    significantIndices <- waveEvents$pValue<significance
    significantWaveEvents <- waveEvents[significantIndices,]

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
    eventsFactor <- factor(c(rep(x="init", times=length(infoInit$time)),
                              rep(x="term", times=length(infoTerm$time)),
                              rep(x="cweStart", times=nrow(cwes)),
                              rep(x="cweEnd", times=nrow(cwes))), 
                            levels=c("init", "term", "cweStart", 
                                     "cweEnd"))
    eventsDF <- data.frame(time=c(infoInit$time, 
                                                 infoTerm$time, cwes$startTime,
                                                 cwes$endTime), 
                            event=eventsFactor)
    ylim <- quantile(significantWaveEvents$speeds, probs=ylimQuantileProbs)

    p <- ggplot()
    p <- p + geom_line(data=significantWaveEvents, mapping=aes(x=times,
                                                                y=speeds),
                                                   colour=colLines)
    p <- p + geom_point(data=significantWaveEvents, mapping=aes(x=times,
                                                                 y=speeds),
                                                    colour=colPoints)
    p <- p + geom_vline(data=eventsDF, mapping=aes(xintercept=time,
                                                    linetype=event,
                                                    colour=event))
    p <- p + scale_linetype_manual(name="event", 
                                    values=c(init="solid", term="dotted",
                                                           cweStart="solid",
                                                           cweEnd="solid"),
                                    labels=c(init="CVS start",
                                              term="CVS end",
                                              cweStart="CWE start",
                                              cweEnd="CWE end"))
    p <- p + scale_colour_manual(name="event", 
                                    values=c(before="black", init="black",
                                                             term="black",
                                                             cweStart="blue",
                                                             cweEnd="red"),
                                    labels=c(before="CVS preparation",
                                              init="CVS initiation",
                                              term="CVS termintation",
                                              cweStart="Wave start",
                                              cweEnd="Wave end"))
    p <- p + xlab(xlab)
    p <- p + ylab(ylab)
    p <- p + ylim(ylim)
    p <- p + geom_hline(aes(yintercept=0), colour=colHLines)
    # p <- p + guides(col = guide_legend(reverse=TRUE))
    # p <- p + guides(line = guide_legend(keywidth = 10, keyheight = 10))
    # p <- p + theme(legend.key.size = unit(keySize, "cm")) 
    for(i in 1:length(infoInit$cvSyllables)) {
        p <- p + annotate("text", label=infoInit$cvSyllables[i], x=infoInit$times[i], y=ylim[2], size=sizeAnnotate)
    }
    p <- ggplotly(p)

    print(p)

    browser()
}

processAll()

rm(processAll)
