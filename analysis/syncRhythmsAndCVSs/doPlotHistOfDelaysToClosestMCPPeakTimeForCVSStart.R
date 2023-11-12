
source("doLoadSources.R")

processAll <- function() {
#     sessionLabel <- "EC2_B1"
#     lowCutoff <- 0.6
#     highCutoff <- 1.2
#     duration <- 647
    sessionLabel <- "EC2_B105"
    entrainmentFreq <- 1/1.62
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
    transientDuration <- 90
    upperQ <- .75
    lowerQ <- .25
    medianLabel <- "50% (median)"
    lqColor <- "blue"
    uqColor <- "green"
    medianColor <- "red"
    xlab <- expression("CVS Start Time - " ~ bar(cos(phi)) ~ "Peak Time (sec)")
    ylab <- "Count"
    width <- 6
    height <- 6
    mcpInfoFilenamePattern <- "results/%s/meanCosPhaseInfo%.02ffpsFromFreq%.02fToFreq%.02fFromTime%03dToTime%03d_squareXMin%02dXMax%02dYMin%02dYMax%02d.RData"
    transcriptionFilenamePattern <- "../data/transcriptionFiles/%s/%s_transcription_final_completed.lab"
    figFilenamePattern <- "figures/%s/histDelayToClosestMCPPeakForCVSStart%.02ffpsFromFreq%.02fToFreq%.02fFromTime%03dToTime%03d_squareXMin%02dXMax%02dYMin%02dYMax%02d_lowerQ%dUppwerQ%d.eps"
    quantilesFilenamePattern <- "results/%s/quantilesHistDelayToClosestMCPPeakForCVSStart%.02ffpsFromFreq%.02fToFreq%.02fFromTime%03dToTime%03d_squareXMin%02dXMax%02dYMin%02dYMax%02d_lowerQ%dUppwerQ%d.eps"

    lqLabel <- sprintf("%02d%%", lowerQ*100)
    uqLabel <- sprintf("%02d%%", upperQ*100)
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
    delaysWOTransient <- -mcpPeaksInfo$delaysMCPPeakAndCVS[mcpPeaksInfo$cvsStartTimes>transientDuration]

    quantiles <- quantile(x=delaysWOTransient, 
                           probs=c(lowerQ, .5, upperQ))

    d <- data.frame(x=delaysWOTransient)
    p <- ggplot(data=d, mapping=aes(x=x))
    p <- p + geom_histogram()
    p <- p + geom_vline(aes(xintercept=quantiles[1], color="lq"))
    p <- p + geom_vline(aes(xintercept=quantiles[2], color="median"))
    p <- p + geom_vline(aes(xintercept=quantiles[3], color="uq"))
    p <- p + scale_color_manual(name="Percentile", labels=c("lq"=lqLabel, "median"=medianLabel, "uq"=uqLabel), values = c("lq"=lqColor, "median"=medianColor, "uq"=uqColor))
    p <- p + xlab(xlab)
    p <- p + ylab(ylab)
    figFilename <- sprintf(figFilenamePattern, sessionLabel, frameRate, lowCutoff, highCutoff, fromTime, toTime, xMin, xMax, yMin, yMax, lowerQ*100, upperQ*100)
    quantilesFilename <- sprintf(quantilesFilenamePattern, sessionLabel, frameRate, lowCutoff, highCutoff, fromTime, toTime, xMin, xMax, yMin, yMax, lowerQ*100, upperQ*100)
    ggsave(plot=p, filename=figFilename, width=width, height=height)

    quantilesInRad <- quantiles*2*pi*entrainmentFreq
    sink(quantilesFilename)

    show(sprintf("Percentile %02d%%=%02f sec, 50%% (median)=%02f sec, %02d%%=%02f sec", lowerQ*100, quantiles[1], quantiles[2], upperQ*100, quantiles[3]))

    show(sprintf("Percentile %02d%%=%f>-pi/%d rad , 50%% (median)=%f<pi/%d rad, %02d%%=%.02f<pi/%d rad", lowerQ*100, quantilesInRad[1], floor(-pi/quantilesInRad[1]), quantilesInRad[2], floor(pi/quantilesInRad[2]), upperQ*100, quantilesInRad[3], floor(pi/quantilesInRad[3])))

    show(sprintf("%02d-%02d%%=%.02f sec", upperQ*100, lowerQ*100, quantiles[3]-quantiles[1]))
    show(sprintf("%02d-%02d%%=%.02f rad<pi/%d", upperQ*100, lowerQ*100, quantilesInRad[3]-quantilesInRad[1], floor(pi/(quantilesInRad[3]-quantilesInRad[1]))))
    sink()
    browser()
}

processAll()

rm(processAll)
