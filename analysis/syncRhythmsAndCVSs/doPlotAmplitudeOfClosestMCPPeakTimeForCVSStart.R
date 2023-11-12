
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
    lowpassCutoffMCP <- 0.05
    frameRate <- 381.47
    transcriptionSampleRate <- 1e7
    width <- 6
    height <- 6
    # xlim <- c(390, 400)
    # xlim <- c(49, 59)
    # xlim <- c(29, 60)
    xlim <- c(0, 700)
#     xlim <- c(500, 600)
    medianWindowSize <- 7
    lineColor <- "black"
    pointColor <- "black"
    hlineColor <- "gray"
    mcpInfoFilenamePattern <- "results/%s/meanCosPhaseInfo%.02ffpsFromFreq%.02fToFreq%.02fFromTime%03dToTime%03d_squareXMin%02dXMax%02dYMin%02dYMax%02d.RData"
    transcriptionFilenamePattern <- "../data/transcriptionFiles/%s/%s_transcription_final_completed.lab"
    figFilenamePattern <- "figures/%s/amplitudeClosestMCPPeakForCVSStart%.02ffpsFromFreq%.02fToFreq%.02fFromTime%03dToTime%03d_squareXMin%02dXMax%02dYMin%02dYMax%02d_plotFromTime%03dToTime%03d_medianWindowSize%d.eps"

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
                                                  sampleRate=frameRate, 
                                                  filterOrderFIR=
                                                   as.integer(round(frameRate)),
                                                  lowpassCutoff=
                                                   lowpassCutoffMCP)

    xlab <- "Time (sec)"
    ylab <- expression(bar(cos(phi)) ~ "Peak Amplitude")
    fileteredMCPPeakValues <- filter(MedianFilter(medianWindowSize), mcpPeaksInfo$closestMCPPeakValues)
#     ylim <- max(abs(fileteredMCPPeakValues))*c(-1,1)
    d <- data.frame(time=mcpPeaksInfo$cvsStartTimes, 
                     delays=fileteredMCPPeakValues)
    p <- ggplot(data=d, mapping=aes(x=time, y=delays))
    p <- p + geom_line(color=lineColor)
    p <- p + geom_point(color=pointColor)
    p <- p + xlim(xlim)
#     p <- p + ylim(ylim)
    p <- p + xlab(xlab)
    p <- p + ylab(ylab)
    
    figFilename <- sprintf(figFilenamePattern, sessionLabel, frameRate, lowCutoff, highCutoff, fromTime, toTime, xMin, xMax, yMin, yMax, xlim[1], xlim[2], medianWindowSize)
    ggsave(plot=p, filename=figFilename, width=width, height=height)
    print(p)
    browser()
}

processAll()

rm(processAll)
