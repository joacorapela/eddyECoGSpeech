
source("doLoadSources.R")

processAll <- function() {
    sessionLabel <- "EC2_B105"
    lowCutoff <- 1.0
    highCutoff <- 1.4
    order <- 3
    duration <- 697
    plotsStartTime <- 0
    plotsStepTime <- 10
    xMin <- 7
    xMax <- 14
    yMin <- 5
    yMax <- 13
    fromTime <- 0
    lowpassCutoffHz <- 1.4
    lowpassTransitionWidthHz <- 1.0
    lowpassRipple <- 0.1
    frameRate <- 381.47
    transcriptionSampleRate <- 1e7
    width <- 6
    height <- 6
    plotLowpass <- FALSE
    mcpInfoFilenamePattern <- "results/%s/meanCosPhaseInfo%.02ffpsFromFreq%.02fToFreq%.02fOrder%.02dFromTime%03dToTime%03d_squareXMin%02dXMax%02dYMin%02dYMax%02d.RData"
    transcriptionFilenamePattern <- "../data/transcriptionFiles/%s/%s_transcription_final_completed.lab"
    figFilenamePattern <- "figures/%s/meanCosPhase%.02ffpsFromFreq%.02fToFreq%.02fOrder%02dFromTime%03dToTime%03d_squareXMin%02dXMax%02dYMin%02dYMax%02d_plotFromTime%03dToTime%03d.png"

    toTime <- fromTime + duration
    fromPlotTimes <- seq(from=plotsStartTime, to=duration, by=plotsStepTime)
    mcpInfoFilename <- sprintf(mcpInfoFilenamePattern, sessionLabel, frameRate, lowCutoff, highCutoff, order, fromTime, toTime, xMin, xMax, yMin, yMax)
    transcriptionFilename <- sprintf(transcriptionFilenamePattern, sessionLabel, sessionLabel)

    mcpInfo <- get(load(mcpInfoFilename))

    resInitiations <- getInfoCVSsInitiations(transcriptionFilename=
                                           transcriptionFilename,
                                          transcriptionSampleRate=
                                           transcriptionSampleRate,
                                          ecogSampleRate=frameRate)
    mcpPeaksInfo <- getClosestMCPPeakInfoForCVSs(meansCosPhaseInfo=mcpInfo, 
                                                  cvsStartTimes=
                                                   resInitiations$times, 
                                                  lowpassCutoffHz=
                                                   lowpassCutoffHz,
                                                  lowpassTransitionWidthHz=
                                                   lowpassTransitionWidthHz,
                                                  lowpassRipple=lowpassRipple,
                                                  sampleRate=frameRate,
                                                  plotLowpass=plotLowpass)
    mcpPeakTimesWOCVSs <- getMCPPeakTimesWOCVSs(mcpPeaksInfo=mcpPeaksInfo, 
                                                 cvsStartTimes=
                                                  resInitiations$times)

    for(fromPlotTime in fromPlotTimes) {
        show(sprintf("Processing time %d (%d)", fromPlotTime, 
                     fromPlotTimes[length(fromPlotTimes)]))
        xlim <- c(fromPlotTime, fromPlotTime + plotsStepTime)
        p <- getPlotMeansCosPhaseInSquare(mcpInfo=mcpInfo, lowpassedMCP=mcpPeaksInfo$lowpassedMCP, matchedMCPPeakTimes=mcpPeaksInfo$closestMCPPeakTimes, unmatchedMCPPeakTimes=mcpPeakTimesWOCVSs, cvsStartTimes=resInitiations$times, xlim=xlim)
        figFilename <- sprintf(figFilenamePattern, sessionLabel, frameRate, lowCutoff, highCutoff, order, fromTime, toTime, xMin, xMax, yMin, yMax, xlim[1], xlim[2])
        ggsave(plot=p, filename=figFilename, width=width, height=height)
    }
}

processAll()

rm(processAll)
