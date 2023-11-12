
source("doLoadSources.R")

processAll <- function() {
    sessionName <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    minCWEDuration <- .005 # seconds
    saveFromTime <- 0
    saveToTime <- 700
    saveDT <- .05
    plotFromTime <- 000
    plotToTime <- 700
    plotStep <- 30
    elecNumbers <- 136:141
    ylimQuantileProbs <- c(.01, .99)
    significance <- .01
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
     "../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final_completed.lab"
#      "../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final.lab"
    waveEventsFilenamePattern <- "results/%s/waveEventsFilteredFrom%.02fTo%.02fOrder%02dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.%s"
    contiguousWaveEventsFilenamePattern <- 
     "results/%s/contriguousWaveEventsFilteredFrom%.02fTo%.02fOrder%02dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.%s"
    figFilenamePattern <- 
     "figures/%s/significantSpeedsFilteredFrom%.02fTo%.02fOrder%02dDT%.02fFromElec%03dToElec%03dPlotFromTime%06.02fPlotToTime%06.02fSign%.02f.png"

    waveEventsFilename <- sprintf(waveEventsFilenamePattern, sessionName, lowCutoff, highCutoff, order, saveFromTime, saveToTime, saveDT, min(elecNumbers), max(elecNumbers), "RData")
    contiguousWaveEventsFilename <- sprintf(contiguousWaveEventsFilenamePattern, sessionName, lowCutoff, highCutoff, order, saveFromTime, saveToTime, saveDT, min(elecNumbers), max(elecNumbers), "RData")
    waveEvents <- get(load(file=waveEventsFilename))
    cwes <- get(load(file=contiguousWaveEventsFilename))
    cwes <- cwes[which(cwes$endTime-cwes$startTime>minCWEDuration),]
    significantIndices <- waveEvents$pValue<significance
    significantWaveEvents <- waveEvents[significantIndices,]

    infoInit <- getInfoCVSsInitiations(transcriptionFilename=
                                         transcriptionFilename,
                                        transcriptionSampleRate=
                                         transcriptionSampleRate,
                                        ecogSampleRate=transcriptionSampleRate)
    infoCVTrans <- getInfoCVSsCVTransitions(transcriptionFilename=
                                              transcriptionFilename,
                                             transcriptionSampleRate=
                                              transcriptionSampleRate,
                                             ecogSampleRate=
                                              transcriptionSampleRate)
    infoTerm <- getInfoCVSsTerminations(transcriptionFilename=
                                          transcriptionFilename,
                                         transcriptionSampleRate=
                                          transcriptionSampleRate,
                                         ecogSampleRate=
                                          transcriptionSampleRate)
    eventsFactor <- factor(c(rep(x="init", times=length(infoInit$time)),
                              rep(x="cvTrans", times=length(infoCVTrans$time)),
                              rep(x="term", times=length(infoTerm$time)),
                              rep(x="cweStart", times=nrow(cwes)),
                              rep(x="cweEnd", times=nrow(cwes))), 
                            levels=c("init", "cvTrans", "term", "cweStart", 
                                     "cweEnd"))
    eventsDF <- data.frame(time=c(infoInit$time, infoCVTrans$time,
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
                                    values=c(init="solid", cvTrans="dashed",
                                                           term="dotted",
                                                           cweStart="solid",
                                                           cweEnd="solid"),
                                    labels=c(init="CVS start",
                                              cvTrans="CV transition",
                                              term="CVS end",
                                              cweStart="CWE start",
                                              cweEnd="CWE end"))
    p <- p + scale_colour_manual(name="event", 
                                    values=c(before="black", init="black",
                                                             cvTrans="black",
                                                             term="black",
                                                             cweStart="blue",
                                                             cweEnd="red"),
                                    labels=c(before="CVS preparation",
                                              init="CVS initiation",
                                              cvTrans="CV transition",
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
    plotFromTimes <- seq(from=plotFromTime, to=plotToTime, by=plotStep)
    for(aPlotFromTime in plotFromTimes) {
        anXlim <- aPlotFromTime+c(0, plotStep)
        figFilename <- sprintf(figFilenamePattern, sessionName, lowCutoff, highCutoff, order, saveDT, min(elecNumbers), max(elecNumbers), anXlim[1], anXlim[2], significance)
        # p <- p + xlim(anXlim)
        p <- p + scale_x_continuous(limits=anXlim, breaks=seq(round(min(eventsDF$time)), round(max(eventsDF$time))))
        ggsave(filename=figFilename, width=width, height=height, units=units)
    }

    browser()
}

processAll()

rm(processAll)
