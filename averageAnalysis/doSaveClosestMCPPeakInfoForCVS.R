
source("doLoadSources.R")

processAll <- function() {
    sessionLabel <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    duration <- 697
    frameRate <- 381.47
    xMin <- 7
    xMax <- 14
    yMin <- 5
    yMax <- 13
    fromTime <- 0
    lowpassCutoffMCP <- 0.05
    meanCosPhaseInfoFilenamePattern <- "results/%s/meanCosPhaseInfo%.02ffpsFromFreq%.02fToFreq%.02fFromTime%03dToTime%03d_squareXMin%02dXMax%02dYMin%02dYMax%02d.RData"
    transcriptionSampleRate <- 1e7
    transcriptionFilenamePattern <- "../data/matlabData/%s/%s_transcription_final.lab"
    closestMCPPeakInfoForCVSsFilenamePattern <- "results/%s/closestMCPPeakInfoForCVSs%.02ffpsFromFreq%.02fToFreq%.02fFromTime%03dToTime%03d_squareXMin%02dXMax%02dYMin%02dYMax%02d.RData"
    
    toTime <- fromTime + duration
    meanCosPhaseInfoFilename <- sprintf(meanCosPhaseInfoFilenamePattern, sessionLabel, frameRate, lowCutoff, highCutoff, fromTime, toTime, xMin, xMax, yMin, yMax)
    meansCosPhaseInfo <- get(load(meanCosPhaseInfoFilename))
    transcriptionFilename <- sprintf(transcriptionFilenamePattern, 
                                      sessionLabel, sessionLabel)

    resEpochsSamples <- getEpochsSamples(transcriptionFilename=
                                           transcriptionFilename,
                                          transcriptionSampleRate=
                                           transcriptionSampleRate,
                                          ecogSampleRate=frameRate)

    closestMCPPeakInfoForCVSs <- 
     getClosestMCPPeakInfoForCVSs(meansCosPhaseInfo=meansCosPhaseInfo, 
                                   cvsStartTimes=resEpochsSamples$times,
                                   sampleRate=frameRate,
                                   filterOrderFIR=as.integer(round(frameRate)),
                                   lowpassCutoff=lowpassCutoffMCP)
    closestMCPPeakInfoForCVSsFilename <- sprintf(closestMCPPeakInfoForCVSsFilenamePattern, sessionLabel, frameRate, lowCutoff, highCutoff, fromTime, toTime, xMin, xMax, yMin, yMax)
    save(closestMCPPeakInfoForCVSs, file=closestMCPPeakInfoForCVSsFilename)
}

processAll()

rm(processAll)
