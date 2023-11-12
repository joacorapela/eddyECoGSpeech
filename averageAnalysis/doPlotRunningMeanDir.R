
source("doLoadSources.R")

processAll <- function() {
    sessionLabel <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    peaksLowpassCutoff <- 0.8
    peaksLowpassTransitionWidth <- 0.2
#     lowCutoff <- 1.0
#     highCutoff <- 1.4
#     order <- 3
#     peaksLowpassCutoff <- 1.4
#     peaksLowpassTransitionWidth <- 1.0
    elecNumbers <- 1:256
    duration <- 697
    fromTime <- 0
    peaksLowpassRipple <- 0.1
    transcriptionSampleRate <- 1e7
    xlim <- c(0, 700)
    ylim <- pi*c(-1, 1)
    runningWinSizeSecs <- 60
    lineColor <- "black"
    pointColor <- "black"
    plotLowpass <- FALSE
    xlab <- "Time (sec)"
    ylab <- "Mean Direction (radians)"
    yTickMarks <- pi*seq(from=-1, to=1, by=.25)
    labels <- c(expression(-pi), expression(-3*pi/4), expression(-pi/2), expression(-pi/4), 0, expression(pi/4), expression(pi/2), expression(3*pi/4), expression(pi))
    width <- 6
    height <- 6
    bandpassedFilenamePattern <- "results/%s/bandpassedFilteredFrom%.02fTo%.02fOrder%02dWav%d%d.RData"
    transcriptionFilenamePattern <- "../data/transcriptionFiles/%s/%s_transcription_final_completed.lab"
    figFilenamePattern <- "figures/%s/meanDir_elec%03dFPS%.02fFromFreq%.02fToFreq%.02fOrder%dRunningWinSizeSec%d.png"

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
                                                    lowpassCutoffHz=
                                                     peaksLowpassCutoff,
                                                    lowpassTransitionWidthHz=
                                                     peaksLowpassTransitionWidth,
                                                    lowpassRipple=
                                                     peaksLowpassRipple,
                                                    sampleRate=
                                                     loadRes$ecogSampleRate,
                                                    plotLowpass=plotLowpass)
            validIndices <- which(!is.na(peaksInfo$cvsPhases))
            validCVSPhases <- peaksInfo$cvsPhases[validIndices]
            validCVSStartTimes <- peaksInfo$cvsStartTimes[validIndices]

            meanInterSyllableSeparation <- mean(validCVSStartTimes[-1]-validCVSStartTimes[-length(validCVSStartTimes)])
            runningWinSizeSamples <- round(runningWinSizeSecs/
                                               meanInterSyllableSeparation)
            if(length(validCVSStartTimes)>runningWinSizeSamples) {
                res <- runningFunction(x=validCVSPhases, 
                                        fun=computeMeanDirection, 
                                        winSize=runningWinSizeSamples)
                runningMeanDir <- res$runningValues
                runningMeanDirTimes <- validCVSStartTimes[res$validIndices]

                d <- data.frame(time=runningMeanDirTimes, runningMeanDir=runningMeanDir)
                p <- ggplot(data=d, mapping=aes(x=time, y=runningMeanDir))
                p <- p + geom_line(color=lineColor)
                p <- p + geom_point(color=pointColor)
                p <- p + scale_y_continuous(breaks=yTickMarks,
                                             label=labels, limits=ylim)
                p <- p + xlim(xlim)
                p <- p + xlab(xlab)
                p <- p + ylab(ylab)
            } else {
                warning(sprintf("Not enough samples to plot in %s", bandpassedFilename))
                p <- getEmptyPlot()
            }
            figFilename <- sprintf(figFilenamePattern, sessionLabel, elecNumber, loadRes$ecogSampleRate, lowCutoff, highCutoff, order, runningWinSizeSecs)
            ggsave(plot=p, filename=figFilename, width=width, height=height)
            print(p)
        } else {
            warning(sprintf("File %s does not exist", bandpassedFilename))
        }
# browser()
    }
}

processAll()

rm(processAll)
