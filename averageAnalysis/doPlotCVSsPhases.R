
source("doLoadSources.R")

processAll <- function() {
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
    ylim <- pi*c(-1,1)
    syncPhaseLimits <- c(400, 700)
    runningWinSizeSamples <- 3
    lineColor <- "black"
    pointColor <- "black"
    plotLowpass <- FALSE
    xlab <- "Time (sec)"
    ylab <- "CVS Phase (radians)"
    bandpassedFilenamePattern <- "results/%s/bandpassedFilteredFrom%.02fTo%.02fOrder%02dWav%d%d.RData"
    transcriptionFilenamePattern <- "../data/transcriptionFiles/%s/%s_transcription_final_completed.lab"
    figFilenamePattern <- "figures/%s/cvsPhasesElec%03dFPS%.02fFromFreq%.02fToFreq%.02fFromTime%03dToTime%03d_medianWindowSize%d.png"

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
            validIndices <- which(!is.na(peaksInfo$cvsPhases))
            validCVSPhases <- peaksInfo$cvsPhases[validIndices]
            validCVSStartTimes <- peaksInfo$cvsStartTimes[validIndices]

            syncPhaseIndices <- which(syncPhaseLimits[1]<=validCVSStartTimes &
                                       validCVSStartTimes<=syncPhaseLimits[2])
            syncPhases <- validCVSPhases[syncPhaseIndices]
            itc <- computeITCFromPhases(phases=syncPhases)
            meanDirection <- computeMeanDirection(angles=syncPhases)
            pValue <- rayleighUniformityTest(phases=syncPhases)
            annotation <- sprintf("ITC=%.02f, p=%.04f", itc, pValue)
            # annotation <- "list(ITC==1.1, p==0.4,bar(theta)==0.3)"

            res <- runningFunction(x=validCVSPhases, 
                                    fun=computeMeanDirection, 
                                    winSize=runningWinSizeSamples)
            filteredPhases <- res$runningValue
            filteredTimes <- validCVSStartTimes[res$validIndices]

            d <- data.frame(time=filteredTimes, 
                             delays=filteredPhases)
            p <- ggplot(data=d, mapping=aes(x=time, y=delays))
            p <-  p + annotate("text", x=600, y=3.14, label=annotation, color="red", parse=FALSE)
            # p <-  p + annotate("text", x=500, y=3.14, label=annotation, parse=FALSE)
            p <- p + geom_line(color=lineColor)
            p <- p + geom_point(color=pointColor)
            p <- p + xlim(xlim)
            p <- p + xlab(xlab)
            p <- p + ylab(ylab)
            yTickMarks <- pi*seq(from=-1, to=1, by=.25)
            labels <- c(expression(-pi), expression(-3*pi/4), expression(-pi/2), expression(-pi/4), 0, expression(pi/4), expression(pi/2), expression(3*pi/4), expression(pi))
            p <- p + scale_y_continuous(breaks=yTickMarks,
                                         label=labels, limits=ylim)
            p <- p + geom_hline(yintercept=0, color="grey")
            p <- p + geom_hline(yintercept=meanDirection, color="red")
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
