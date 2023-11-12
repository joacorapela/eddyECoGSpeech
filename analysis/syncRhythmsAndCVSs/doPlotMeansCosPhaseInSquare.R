
source("doLoadSources.R")

processAll <- function() {
#     sessionLabel <- "EC2_B1"
#     lowCutoff <- 0.6
#     highCutoff <- 1.2
#     duration <- 647
    sessionLabel <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    duration <- 697
    xMin <- 7
    xMax <- 14
    yMin <- 5
    yMax <- 13
    fromTime <- 0
    lowpassCutoffHz <- 0.9
    lowpassTransitionWidthHz <- 0.2
    lowpassRipple <- 0.05
    frameRate <- 381.47
    transcriptionSampleRate <- 1e7
    width <- 6
    height <- 6
    # xlim <- c(390, 400)
    # xlim <- c(49, 59)
    plotFromTime <- 550
    plotDuration <- 10
    mcpInfoFilenamePattern <- "results/%s/meanCosPhaseInfo%.02ffpsFromFreq%.02fToFreq%.02fFromTime%03dToTime%03d_squareXMin%02dXMax%02dYMin%02dYMax%02d.RData"
    transcriptionFilenamePattern <- "../data/transcriptionFiles/%s/%s_transcription_final_completed.lab"
    figFilenamePattern <- "figures/%s/meanCosPhase%.02ffpsFromFreq%.02fToFreq%.02fFromTime%03dToTime%03d_squareXMin%02dXMax%02dYMin%02dYMax%02d_plotFromTime%03dToTime%03d.eps"

    toTime <- fromTime + duration
    xlim <- plotFromTime + c(0, plotDuration)
    mcpInfoFilename <- sprintf(mcpInfoFilenamePattern, sessionLabel, frameRate, lowCutoff, highCutoff, fromTime, toTime, xMin, xMax, yMin, yMax)
    transcriptionFilename <- sprintf(transcriptionFilenamePattern, sessionLabel, sessionLabel)

    mcpInfo <- get(load(mcpInfoFilename))

    resEpochsSamples <- getEpochsSamples(transcriptionFilename=
                                           transcriptionFilename,
                                          transcriptionSampleRate=
                                           transcriptionSampleRate,
                                          ecogSampleRate=frameRate)
    mcpPeaksInfo <- getClosestMCPPeakInfoForCVSs(meansCosPhaseInfo=mcpInfo, 
                                                  cvsStartTimes=
                                                   resEpochsSamples$times, 
                                                  lowpassCutoffHz=
                                                   lowpassCutoffHz,
                                                  lowpassTransitionWidthHz=
                                                   lowpassTransitionWidthHz,
                                                  lowpassRipple=lowpassRipple,
                                                  sampleRate=frameRate)
    mcpPeakTimesWOCVSs <- getMCPPeakTimesWOCVSs(mcpPeaksInfo=mcpPeaksInfo, 
                                                 cvsStartTimes=
                                                  resEpochsSamples$times)

    p <- getPlotMeansCosPhaseInSquare(mcpInfo=mcpInfo, lowpassedMCP=mcpPeaksInfo$lowpassedMCP, matchedMCPPeakTimes=mcpPeaksInfo$closestMCPPeakTimes, unmatchedMCPPeakTimes=mcpPeakTimesWOCVSs, cvsStartTimes=resEpochsSamples$times, xlim=xlim)
    figFilename <- sprintf(figFilenamePattern, sessionLabel, frameRate, lowCutoff, highCutoff, fromTime, toTime, xMin, xMax, yMin, yMax, xlim[1], xlim[2])
    ggsave(plot=p, filename=figFilename, width=width, height=height)
    print(p)
}

processAll()

rm(processAll)
