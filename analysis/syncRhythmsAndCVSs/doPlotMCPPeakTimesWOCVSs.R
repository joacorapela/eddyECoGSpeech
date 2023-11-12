
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
    height <- 2
    xlab <- "Time (sec)"
    mcpInfoFilenamePattern <- "results/%s/meanCosPhaseInfo%.02ffpsFromFreq%.02fToFreq%.02fFromTime%03dToTime%03d_squareXMin%02dXMax%02dYMin%02dYMax%02d.RData"
    transcriptionFilenamePattern <- "../data/transcriptionFiles/%s/%s_transcription_final_completed.lab"
    figFilenamePattern <- "figures/%s/meanCosPhasePeakTimesWOCVSs%.02ffpsFromFreq%.02fToFreq%.02fFromTime%03dToTime%03d_squareXMin%02dXMax%02dYMin%02dYMax%02d.eps"

    toTime <- fromTime + duration
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
    show("Peaks without CVSs")
    show(mcpPeakTimesWOCVSs)
    show(sprintf("#mcpPeaks=%d, #mcpPeaksWOCVS=%d, propMCPPeaksWOCVSs=%.02f%%",
                 length(mcpPeaksInfo$mcpPeakTimes), length(mcpPeakTimesWOCVSs),
                 length(mcpPeakTimesWOCVSs)/length(mcpPeaksInfo$mcpPeakTimes)*100))

    p <- ggplot()
    p <- p + geom_vline(aes(xintercept=mcpPeakTimesWOCVSs))
    p <- p + xlab(xlab)
    p <- p + ylim(c(0,1))
    p <- p + xlim(c(fromTime, toTime))
    p <- p + theme(axis.ticks.y=element_blank(), axis.text.y=element_blank(), panel.grid.major.y=element_blank(), panel.grid.minor.y=element_blank())
    figFilename <- sprintf(figFilenamePattern, sessionLabel, frameRate, lowCutoff, highCutoff, fromTime, toTime, xMin, xMax, yMin, yMax, xlim[1], xlim[2])
    ggsave(plot=p, filename=figFilename, width=width, height=height)
    browser()
}

processAll()

rm(processAll)
