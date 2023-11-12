
source("doLoadSources.R")

processAll <- function() {
    sessionLabel <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    peaksLowpassCutoff <- 0.8
    peaksLowpassTransitionWidth <- 0.2
    order <- 2
    # lowCutoff <- 1.0
    # highCutoff <- 1.4
    # peaksLowpassCutoff <- 1.4
    # peaksLowpassTransitionWidth <- 1.0
    # order <- 3
    # elecNumbers <- 1:256
    elecNumbers <- 136
    peaksLowpassRipple <- 0.1
    width <- 6
    height <- 6
    xlim <- c(0, 700)
    runningWinSizeSamples <- 10
    lineColor <- "black"
    pointColor <- "black"
    hlineCol <- "red"
    plotLowpass <- FALSE
    xlab <- "Time (sec)"
    ylab <- "Inter-Peak Interval (sec)"
    bandpassedFilenamePattern <- "results/%s/bandpassedFilteredFrom%.02fTo%.02fOrder%02dWav%d%d.RData"
    figFilenamePattern <- "figures/%s/interPeakIntervalsElec%03dFPS%.02fFromFreq%.02fToFreq%.02fFromTime%03dToTime%03d_medianWindowSize%d.png"

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
            peaksInfo <- getPeakInfo(x=voltages, 
                                      times=times, 
                                      lowpassCutoffHz=peaksLowpassCutoff,
                                      lowpassTransitionWidthHz=peaksLowpassTransitionWidth,
                                      lowpassRipple=peaksLowpassRipple,
                                      sampleRate=loadRes$ecogSampleRate,
                                      plotLowpass=plotLowpass)
            interPeakIntervals <- 
             peaksInfo$peakTimes[2:length(peaksInfo$peakTimes)]-
             peaksInfo$peakTimes[1:(length(peaksInfo$peakTimes)-1)]
            interPeakIntervalsTimes <- peaksInfo$peakTimes[2:(length(peaksInfo$peakTimes)-1)]
            res <- runningFunction(x=interPeakIntervals, 
                                    fun=median, 
                                    winSize=runningWinSizeSamples)
            filteredInterPeakIntervals <- res$runningValue
            filteredInterPeakIntervalsTimes <- interPeakIntervalsTimes[res$validIndices]

            d <- data.frame(time=filteredInterPeakIntervalsTimes, 
                             interPeakIntervals=filteredInterPeakIntervals)
            p <- ggplot(data=d, mapping=aes(x=time, y=interPeakIntervals))
            p <- p + geom_line(color=lineColor)
            p <- p + geom_point(color=pointColor)
            p <- p + geom_hline(yintercept=median(interPeakIntervals), 
                                 color=hlineCol)
            p <- p + xlim(xlim)
            p <- p + xlab(xlab)
            p <- p + ylab(ylab)
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
