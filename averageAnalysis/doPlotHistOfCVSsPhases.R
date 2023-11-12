
source("doLoadSources.R")

processAll <- function() {
    sessionLabel <- "EC2_B105"
    # lowCutoff <- 0.4
    # highCutoff <- 0.8
    # peaksLowpassCutoff <- 0.8
    # peaksLowpassTransitionWidth <- 0.2
    # order <- 2
    # fromTime <- 340
    # syncPhaseLimits <- c(340, 400)
    lowCutoff <- 1.0
    highCutoff <- 1.4
    peaksLowpassCutoff <- 1.4
    peaksLowpassTransitionWidth <- 1.0
    order <- 3
    fromTime <- 240
    syncPhaseLimits <- c(240, 300)
    elecNumbersToPlot <- seq(from=256, to=1, by=-1)
    duration <- 60
    peaksLowpassRipple <- 0.1
    transcriptionSampleRate <- 1e7
    maxRadius <- 12.5
    itcThr <- 0.4
    width <- 60
    height <- 60
    plotLowpass <- FALSE
    colMeanResultantVector <- "red"
    bandpassedFilenamePattern <- "results/%s/bandpassedFilteredFrom%.02fTo%.02fOrder%02dWav%d%d.RData"
    transcriptionFilenamePattern <- "../data/transcriptionFiles/%s/%s_transcription_final_completed.lab"
    figFilenamePattern <- "figures/%s/histCVSPhasesFPS%.02fFromFreq%.02fToFreq%.02fFromTime%03dToTime%03dITCThr%.02f.png"

    toTime <- fromTime + duration
    transcriptionFilename <- sprintf(transcriptionFilenamePattern, sessionLabel, sessionLabel)
    plots <- list()
    for(i in 1:length(elecNumbersToPlot)) {
        elecNumberToPlot  <- elecNumbersToPlot[i]
        show(sprintf("Processing electrode %d", elecNumberToPlot))
        res <- getGroupAndElecNumber(elecNumber=elecNumberToPlot)
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
            if(length(syncPhases)>0) {
                itc <- computeITCFromPhases(phases=syncPhases)
                meanDirection <- computeMeanDirection(angles=syncPhases)
                meanResultantLength <- computeMeanResultantLength(angles=
                                                                   syncPhases)
                pValue <- rayleighUniformityTest(phases=syncPhases)
                title <- sprintf("%d, ITC=%.02f, p=%.04f", elecNumberToPlot, itc, 
                                 pValue)
    
                df <- data.frame(phases=syncPhases)
                p <- ggplot(df, aes(x=phases))
                p <- p + geom_histogram(breaks=seq(from=-pi, to=pi, length.out=20))
                if(itc>=itcThr) {
            # p <- p + geom_segment(aes(x=meanDirection, 
            #                            y=0, 
            #                            xend=meanDirection,
            #                            yend=meanResultantLength*maxRadius), 
            #                        col=colMeanResultantVector, 
            #                        arrow=arrow())
                    p <- p + geom_vline(xintercept=meanDirection, 
                                         color=colMeanResultantVector)
                }
                p <- p + coord_polar(start=pi/2, direction=-1)
                p <- p + xlab("")
                p <- p + ylab("")
                p <- p + ylim(c(0, maxRadius))
                p <- p + ggtitle(title)
                plots[[i]] <- p
            } else {
                plots[[i]] <- getEmptyPlot()
            }
        } else {
            plots[[i]] <- getEmptyPlot()
        }
    }
    figFilename <- sprintf(figFilenamePattern, sessionLabel, loadRes$ecogSampleRate, lowCutoff, highCutoff, fromTime, toTime, itcThr)
    layoutMatrix <- matrix(data=1:length(plots), ncol=sqrt(length(plots)))
    p <- arrangeGrob(grobs=plots, cols=sqrt(length(plots)), layout_matrix=layoutMatrix)
    ggsave(filename=figFilename, plot=p, width=width, height=height, limitsize=FALSE)
    browser()
}

processAll()

rm(processAll)
