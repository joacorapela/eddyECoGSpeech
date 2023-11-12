
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
    lowpassCutoffHz <- 2.0
    lowpassTransitionWidthHz <- 1.0
    lowpassRipple <- 0.1
    frameRate <- 381.47
    transcriptionSampleRate <- 1e7
    width <- 12
    height <- 6
    # xlim <- c(390, 400)
    # xlim <- c(49, 59)
    xlim <- c(678, 690)
    ylim <- c(-1,1)
    xlab <- "Time (sec)"
    ylab <- expression(bar(cos(phi)))
    legendNameMCP <- expression(bar(cos(phi)))
    legendNameHlines <- "event"
    labelsMCP <- c("original", "lowpassed ")
    linetypesMCP <- c("solid", "dashed")
    labelMCPPeak <- expression(bar(cos(phi))~"peak")
    labelExpectedCVSStart <- "expected CVS start"
    labelCVSStart <- "CVS start"
    colorMCPPeak <- "red"
    colorExpectedCVSStart <- "blue"
    colorCVSStart <- "green"
    linetypeMCPPeak <- "solid"
    linetypeExpectedCVSStart <- "solid"
    linetypeCVSStart <- "solid"
    nFinalClosestMCPPeaks <- 4
    nExpectedCVSStarts <- 7
    mcpInfoFilenamePattern <- "results/%s/meanCosPhaseInfo%.02ffpsFromFreq%.02fToFreq%.02fFromTime%03dToTime%03d_squareXMin%02dXMax%02dYMin%02dYMax%02d.RData"
    transcriptionFilenamePattern <- "../data/transcriptionFiles/%s/%s_transcription_final_completed.lab"
    figFilenamePattern <- "figures/%s/meanCosPhaseEndSession%.02ffpsFromFreq%.02fToFreq%.02fFromTime%03dToTime%03d_squareXMin%02dXMax%02dYMin%02dYMax%02d_plotFromTime%03dToTime%03d.eps"

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

    nClosestMCPPeaks <- length(mcpPeaksInfo$closestMCPPeakTimes)
    timeBTWFinalClosestMCPPeaks <- mcpPeaksInfo$closestMCPPeakTimes[(nClosestMCPPeaks-nFinalClosestMCPPeaks+2):nClosestMCPPeaks]-mcpPeaksInfo$closestMCPPeakTimes[(nClosestMCPPeaks-nFinalClosestMCPPeaks+1):(nClosestMCPPeaks-1)]
    meanTimeBTWFinalClosestMCPPeaks <- mean(timeBTWFinalClosestMCPPeaks)
    expectedCVSStarts <- mcpPeaksInfo$closestMCPPeakTimes[nClosestMCPPeaks]+(1:nExpectedCVSStarts)*meanTimeBTWFinalClosestMCPPeaks

    cvsStartTimes <- resEpochsSamples$times

    d <- data.frame(times=mcpInfo$times, mcpValues=mcpInfo$meansCosPhase, 
                                         lowpassedMCP=mcpPeaksInfo$lowpassedMCP)
    md <- melt(d, id="times")

    p <- ggplot()
    p <- p + geom_line(data=md, mapping=aes(x=times, y=value, linetype=variable))
    p <- p + scale_linetype_manual(name=legendNameMCP, labels=labelsMCP, values=linetypesMCP)
    p <- p + geom_vline(aes(xintercept=mcpPeaksInfo$mcpPeakTimes, color="mcpPeak"))
    p <- p + geom_vline(aes(xintercept=expectedCVSStarts, color="expectedCVSStart"))
    p <- p + geom_vline(aes(xintercept=cvsStartTimes, color="cvsStartTime"))
    p <- p + scale_color_manual(name=legendNameHlines,
                                    labels=c(mcpPeak=labelMCPPeak, expectedCVSStart=labelExpectedCVSStart, cvsStartTime=labelCVSStart),
                                    values = c(mcpPeak=colorMCPPeak, expectedCVSStart=colorExpectedCVSStart, cvsStartTime=colorCVSStart))
    p <- p + xlim(xlim)
    p <- p + ylim(ylim)
    p <- p + xlab(xlab)
    p <- p + ylab(ylab)

    figFilename <- sprintf(figFilenamePattern, sessionLabel, frameRate, lowCutoff, highCutoff, fromTime, toTime, xMin, xMax, yMin, yMax, xlim[1], xlim[2])
    ggsave(plot=p, filename=figFilename, width=width, height=height)
    print(p)
    browser()
}

processAll()

rm(processAll)
