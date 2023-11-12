
source("doLoadSources.R")

processAll <- function() {
    ratioNumberOfOnesOverNumberOfZeros <- function(x) {
        indicesOnes <- which(x==1)
        indicesZeros <- which(x==0)
        ratio <- length(indicesOnes)/length(indicesZeros)
        return(ratio)
    }

    sessionLabel <- "EC2_B105"
    # lowCutoff <- 0.4
    # highCutoff <- 0.8
    # peaksLowpassCutoff <- 0.8
    # peaksLowpassTransitionWidth <- 0.2
    # order <- 2
    lowCutoff <- 1.0
    highCutoff <- 1.4
    peaksLowpassCutoff <- 1.4
    peaksLowpassTransitionWidth <- 1.0
    order <- 3
    # elecNumbers <- 1:256
    elecNumbers <- 136
    duration <- 697
    fromTime <- 0
    peaksLowpassRipple <- 0.1
    transcriptionSampleRate <- 1e7
    width <- 6
    height <- 6
    xlim <- c(0, 700)
    ylim <- c(0, 1)
    runningWinSizeSamples <- 21
    pointColor <- "black"
    plotLowpass <- FALSE
    xlab <- "Time (sec)"
    ylab <- "Running Number of CVSs / Number of Peaks"
    bandpassedFilenamePattern <- "results/%s/bandpassedFilteredFrom%.02fTo%.02fOrder%02dWav%d%d.RData"
    transcriptionFilenamePattern <- "../data/transcriptionFiles/%s/%s_transcription_final_completed.lab"
    figFilenamePattern <- "figures/%s/runningNCVSsOverNPeaksElec%03dFPS%.02fFromFreq%.02fToFreq%.02fFromTime%03dToTime%03d_medianWindowSize%d.png"

    toTime <- fromTime + duration
    transcriptionFilename <- sprintf(transcriptionFilenamePattern, sessionLabel, sessionLabel)
    for(elecNumber in elecNumbers) {
        show(sprintf("Processing electrode %d", elecNumber))
        res <- getGroupAndElecNumber(elecNumber=elecNumber)
        bandpassedFilename <- sprintf(bandpassedFilenamePattern, 
                                       sessionLabel,
                                       lowCutoff, highCutoff, order,
                                       res$groupNumber, res$elecNumber)
        if(file.exists(bandpassedFilename)) {
            loadRes <- get(load(bandpassedFilename))
            voltages <- scale(loadRes$filteredECoG)
            times <- ((1:length(voltages))-1)/loadRes$ecogSampleRate
            resInitiations <- getInfoCVSsInitiations(transcriptionFilename=
                                                       transcriptionFilename,
                                                      transcriptionSampleRate=
                                                       transcriptionSampleRate,
                                                      ecogSampleRate=
                                                       loadRes$ecogSampleRate)
            peaksInfo <- getClosestPeakInfoForCVSs(x=voltages, 
                                                    times=times,
                                                    cvsStartTimes=
                                                     resInitiations$times, 
                                                    lowpassCutoffHz=peaksLowpassCutoff,
                                                    lowpassTransitionWidthHz=
                                                     peaksLowpassTransitionWidth,
                                                    lowpassRipple=peaksLowpassRipple,
                                                    sampleRate=
                                                     loadRes$ecogSampleRate,
                                                    plotLowpass=plotLowpass)

            cvssAndPeakTimes <- c(peaksInfo$cvsStartTimes, peaksInfo$peakTimes)
            indicators <- 
             c(rep(1, times=length(peaksInfo$cvsStartTimes)),
                rep(0, times=length(peaksInfo$peakTimes)))
            sortRes <- sort(cvssAndPeakTimes, index.return=TRUE)
            sortedCVSsAndPeakTimes <- sortRes$x
            sortedIndicators <- indicators[sortRes$ix]
            
            res <- runningFunction(x=sortedIndicators, 
                                    fun=ratioNumberOfOnesOverNumberOfZeros, 
                                    winSize=runningWinSizeSamples)
            ratioNCVSsOverNPeaks <- res$runningValue
            ratioNCVSsOverNPeaksTimes <- 
             sortedCVSsAndPeakTimes[res$validIndices]

            d <- data.frame(time=ratioNCVSsOverNPeaksTimes, 
                             ratioNCVSsOverNPeaks=ratioNCVSsOverNPeaks)
            p <- ggplot(data=d, mapping=aes(x=time, y=ratioNCVSsOverNPeaks))
            p <- p + geom_point(color=pointColor)
            # p <- p + geom_hline(yintercept=0.5, color="grey")
            p <- p + xlim(xlim)
            # p <- p + ylim(ylim)
            p <- p + xlab(xlab)
            p <- p + ylab(ylab)
            # yTickMarks <- seq(from=0, to=1, by=.1)
            # labels <- sprintf("%.02f", yTickMarks)
            # p <- p + scale_y_continuous(breaks=yTickMarks, label=labels, limits=ylim)
            # p <- p + scale_y_continuous(breaks=yTickMarks, label=labels)
            # p <- p + theme(panel.grid.minor.y=element_blank())
            figFilename <- sprintf(figFilenamePattern, sessionLabel, elecNumber, loadRes$ecogSampleRate, lowCutoff, highCutoff, xlim[1], xlim[2], runningWinSizeSamples)
            ggsave(plot=p, filename=figFilename, width=width, height=height)
            print(p)
        } else {
            warning(sprintf("File %s does not exist", bandpassedFilename))
        }
    }
}

processAll()

rm(processAll)
