
source("doLoadSources.R")

processAll <- function() {
    sessionName <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    saveFromTime <- 0
    saveToTime <- 700
    saveDT <- .1
    plotFromTime <- 0
    plotToTime <- 700
    plotStep <- 30
    elecNumbers <- 136:141
    significance <- .01
    xlab <- "Time (sec)"
    ylab <- "Speed (m/sec)"
    width <- 30
    height <- 30
    units <- "in"
    colLines <- "black"
    colPoints <- "red"
    colVLines <- "black"
    colHLines <- "black"
    transcriptionSampleRate <- 1e7
    transcriptionFilename <-
     "../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final_completed.lab"
    waveEventsFilenamePattern <- "results/%s/waveEventsFilteredFrom%.02fTo%.02fOrder%02dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.%s"
    figFilenamePattern <- 
     "figures/EC2_B105/significantSpeedsFilteredFrom%.02fTo%.02fOrder%02dFromElec%03dToElec%03dPlotFromTime%06.02fPlotToTime%06.02f.png"

    waveEventsFilename <- sprintf(waveEventsFilenamePattern, sessionName, lowCutoff, highCutoff, order, saveFromTime, saveToTime, saveDT, min(elecNumbers), max(elecNumbers), "RData")
    waveEvents <- get(load(file=waveEventsFilename))
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
    infoTerm <- getInfoCVSsCVTerminations(transcriptionFilename=
                                            transcriptionFilename,
                                           transcriptionSampleRate=
                                            transcriptionSampleRate,
                                           ecogSampleRate=
                                            transcriptionSampleRate)

    eventsFactor <- factor(c(rep(x="init", times=length(infoInit$time)),
                              rep(x="cvTrans", times=length(infoCVTrans$time)),
                              rep(x="term", times=length(infoTerm$time))), 
                            levels=c("init", "cvTrans", "term"))
    eventsDF <- data.frame(time=c(infoInit$time, infoCVTrans$time, infoTerm$time), event=eventsFactor)
    p <- ggplot()
    p <- p + geom_line(data=significantWaveEvents, mapping=aes(x=time,
                                                                y=speed),
                                                   colour=colLines)
    p <- p + geom_point(data=significantWaveEvents, mapping=aes(x=time,
                                                                 y=speed),
                                                    colour=colPoints)
    p <- p + geom_vline(data=eventsDF, mapping=aes(xintercept=time,
                                                    linetype=event),
                                       colour=colVLines)
    p <- p + scale_linetype_manual(name="event", 
                                    values=c(init="solid", cvTrans="dashed", term="dotted"),
                                    labels=c(init="CVS initiation", cvTrans="CV transition", term="CVS termintation"))
    p <- p + xlab(xlab)
    p <- p + ylab(ylab)
    p <- p + ylim(quantile(significantWaveEvents$speed, probs=c(.05, .95)))
    p <- p + geom_hline(aes(yintercept=0), colour=colHLines)
    # p <- p + guides(col = guide_legend(reverse=TRUE))
    # p <- p + guides(line = guide_legend(keywidth = 10, keyheight = 10))
    # p <- p + theme(legend.key.size = unit(keySize, "cm")) 
    for(i in 1:length(infoInit$cvSyllables)) {
        p <- p + annotate("text", label=infoInit$cvSyllables[i],
                          x=infoInit$times[i], y=Im(hts[1,i]),
                    size=sizeAnnotate)    }
    plotFromTimes <- seq(from=plotFromTime, to=plotToTime, by=plotStep)
    for(aPlotFromTime in plotFromTimes) {
        anXlim <- aPlotFromTime+c(0, plotStep)
        figFilename <- sprintf(figFilenamePattern, lowCutoff, highCutoff, order, min(elecNumbers), max(elecNumbers), anXlim[1], anXlim[2])
        p <- p + xlim(anXlim)
        ggsave(filename=figFilename, width=width, height=height, units=units)
    }

    browser()
}

processAll()

rm(processAll)
