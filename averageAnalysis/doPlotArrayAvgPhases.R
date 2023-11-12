
source("doLoadSources.R")

processAll <- function() {
    htFilenamePatternPattern <- "results/EC2_B105/htFilteredBPFrom%.02fBPTo%.02fTimeFrom%.02fTimeTo%.02fWav%%d%%d.RData"
    groupNumbers <- 1:4
    elecNumbers <- 1:64
    lowCutoff <- 0.4
    highCutoff <- 0.8
    htFromTime <- 390
    htToTime <- 400
    avgFromTime <- 391.0
    avgToTime <- 391.1

    htFilenamePattern <- sprintf(htFilenamePatternPattern, lowCutoff,
                                                           highCutoff,
                                                           htFromTime,
                                                           htToTime)
    arrayAvgPhases <- computeArrayAvgPhases(htFilenamePattern=
                                              htFilenamePattern, 
                                             avgFromTime=avgFromTime, 
                                             avgToTime=avgToTime,
                                             groupNumbers=groupNumbers, 
                                             elecNumbers=elecNumbers)
    image(arrayAvgPhases, xaxt="n", yaxt="n")
    browser()
}

processAll()
