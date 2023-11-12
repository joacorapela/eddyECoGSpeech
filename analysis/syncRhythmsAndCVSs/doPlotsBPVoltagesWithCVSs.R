
source("doLoadSources.R")

processAll <- function() {
    sessionLabel <- "EC2_B105"
    # lowCutoff <- 1.0
    # highCutoff <- 1.4
    # peaksLowpassCutoff <- 1.4
    # peaksLowpassTransitionWidth <- 1.0
    # order <- 3
    lowCutoff <- 0.4
    highCutoff <- 0.8
    peaksLowpassCutoff <- 0.8
    peaksLowpassTransitionWidth <- 0.2
    order <- 2
    elecNumber <- 136
    fromTime <- 0
    duration <- 697
    plotsStartTime <- 0
    plotsStepTime <- 10
    peaksLowpassRipple <- 0.1
    transcriptionSampleRate <- 1e7
    width <- 6
    height <- 6
    plotLowpass <- FALSE
    bandpassedFilenamePattern <- "results/%s/bandpassedFilteredFrom%.02fTo%.02fOrder%02dWav%d%d.RData"
    transcriptionFilenamePattern <- "../data/transcriptionFiles/%s/%s_transcription_final_completed.lab"
    figFilenamePattern <- "figures/%s/bpVoltagesWithCVSsElec%03dFPS%.02fFromFreq%.02fToFreq%.02fOrder%02dFromTime%03dToTime%03d_plotFromTime%03dToTime%03d.png"

    toTime <- fromTime + duration
    fromPlotTimes <- seq(from=plotsStartTime, to=duration, by=plotsStepTime)
    res <- getGroupAndElecNumber(elecNumber=elecNumber)
    bandpassedFilename <- sprintf(bandpassedFilenamePattern, 
                                   sessionLabel,
                                   lowCutoff, highCutoff, order,
                                   res$groupNumber, res$elecNumber)
    transcriptionFilename <- sprintf(transcriptionFilenamePattern, sessionLabel, sessionLabel)

    if(file.exists(bandpassedFilename)) {
        loadRes <- get(load(bandpassedFilename))
        voltages <- scale(loadRes$filteredECoG)
        times <- ((1:length(voltages))-1)/loadRes$ecogSampleRate
        resInitiations <- getInfoCVSsInitiations(transcriptionFilename=
                                                   transcriptionFilename,
                                                  transcriptionSampleRate=
                                                   transcriptionSampleRate,
                                                  ecogSampleRate=loadRes$ecogSampleRate)
        peaksInfo <- getClosestPeakInfoForCVSs(x=voltages, 
                                                times=times,
                                                cvsStartTimes=
                                                 resInitiations$times, 
                                                lowpassCutoffHz=peaksLowpassCutoff,
                                                lowpassTransitionWidthHz=
                                                 peaksLowpassTransitionWidth,
                                                lowpassRipple=peaksLowpassRipple,
                                                sampleRate=loadRes$ecogSampleRate,
                                                plotLowpass=plotLowpass)
        peakTimesWOCVSs <- getPeakTimesWOCVSs(peaksInfo=peaksInfo, 
                                               cvsStartTimes=
                                                resInitiations$times)

        for(fromPlotTime in fromPlotTimes) {
            show(sprintf("Processing time %d (%d)", fromPlotTime, 
                         fromPlotTimes[length(fromPlotTimes)]))
            xlim <- c(fromPlotTime, fromPlotTime + plotsStepTime)
            p <- getPlotTimeSeriesWithPeaksAndCVSs(x=voltages, times=times, lowpassedX=peaksInfo$lowpassedX, matchedPeakTimes=peaksInfo$closestPeakTimes, unmatchedPeakTimes=peakTimesWOCVSs, cvsStartTimes=resInitiations$times, xlim=xlim, ylim=range(voltages))
            figFilename <- sprintf(figFilenamePattern, sessionLabel, elecNumber, loadRes$ecogSampleRate, lowCutoff, highCutoff, order, fromTime, toTime, xlim[1], xlim[2])
            ggsave(plot=p, filename=figFilename, width=width, height=height)
        }
    } else {
        warning(sprintf("File %s does not exist", bandpassedFilename))
        browser()
    }
}

processAll()

rm(processAll)
