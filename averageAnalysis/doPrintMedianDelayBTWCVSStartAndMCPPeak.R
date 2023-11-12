
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
    transientDuration <- 90
    nResamples <- 2000
    conf <- .95
    mcpInfoFilenamePattern <- "results/%s/meanCosPhaseInfo%.02ffpsFromFreq%.02fToFreq%.02fFromTime%03dToTime%03d_squareXMin%02dXMax%02dYMin%02dYMax%02d.RData"
    transcriptionFilenamePattern <- "../data/transcriptionFiles/%s/%s_transcription_final_completed.lab"

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
    delaysWOTransient <- mcpPeaksInfo$delaysMCPPeakAndCVS[mcpPeaksInfo$cvsStartTimes>transientDuration]
    bootRes <- bootstrapOneParamFunction(x=delaysWOTransient,
                                          oneParamFunction=median,
                                          nResamples=nResamples)
    medianBootCI <- getBootstrapCIs(bootRes=bootRes, conf=conf)
    bootRes <- bootstrapOneParamFunction(x=delaysWOTransient,
                                          oneParamFunction=mad,
                                          nResamples=nResamples)
    madBootCI <- getBootstrapCIs(bootRes=bootRes, conf=conf)

    show(sprintf("median=%.02f, 95%%CI=(%.02f, %.02f)", 
                 medianBootCI[1], medianBootCI[2], medianBootCI[3]))
    show(sprintf("mad=%.02f, 95%%CI=(%.02f, %.02f)", 
                 madBootCI[1], madBootCI[2], madBootCI[3]))

    browser()
}

processAll()

rm(processAll)
