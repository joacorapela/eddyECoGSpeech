
source("doLoadSources.R")

processAll <- function() {
    freq <- 0.62
    waveLength <- 8 # in pixels
    fromTime <- 0
    toTime <- 1
    sampleRate <- 762.9395
    htFilenamePatternPattern <- "results/simulated/htTWFreq%.02fWaveLength%.02fFromTime%.02fToTime%.02fSR%.02fWav%%d%%d.RData"
    groupNumbers <- 1:4
    elecNumbers <- 1:64
    lowCutoff <- 0.4
    highCutoff <- 0.8
    htFromTime <- 0
    htToTime <- 1
    avgFromTime <- 0
    avgToTime <- 0.5

    htFilenamePattern <- sprintf(htFilenamePatternPattern, freq, waveLength,
                                                           fromTime, toTime,
                                                           sampleRate)
    arrayAvgPhases <- computeArrayAvgPhases(htFilenamePattern=
                                              htFilenamePattern, 
                                             avgFromTime=avgFromTime, 
                                             avgToTime=avgToTime,
                                             groupNumbers=groupNumbers, 
                                             elecNumbers=elecNumbers)
    browser()
}

processAll()
