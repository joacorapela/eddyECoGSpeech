
source("doLoadSources.R")

processAll <- function() {
    sessionName <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    # sessionName <- "EC2_B15"
    # lowCutoff <- 0.8
    # highCutoff <- 1.2
    order <- 2
    t0 <- 29
    referenceElecNumber <- 141
    targetElecNumber <- 136
    xlab <- "Time (sec)"
    ylab <- "Latency from Prev Peak Diff. (sec)"
    latenciesFromPrevPeakFilenamePattern <- "results/%s/latenciesFromPrevPeakFilteredFrom%.02fTo%.02fOrder%02dWav%d%d.RData"
    figFilenamePattern <- "figures/%s/latenciesFromPrevPeakDiffsVsTimeInSecsRefElec%03dTestElec%03dFilteredFrom%.02fTo%.02fOrder%02d.eps"

    res <- getGroupAndElecNumber(elecNumber=referenceElecNumber)
    groupNumber <- res$groupNumber
    elecNumber <- res$elecNumber
    latenciesFromPrevPeakFilename <- 
        sprintf(latenciesFromPrevPeakFilenamePattern, 
                 sessionName, lowCutoff, highCutoff, order, 
                 groupNumber, elecNumber)
    if(!file.exists(latenciesFromPrevPeakFilename)) {
        stop(sprintf("File %s does not exist", latenciesFromPrevPeakFilename))
    }
    res <- get(load(latenciesFromPrevPeakFilename))
    referencePeakTimes <- res$peakTimes
    
    res <- getGroupAndElecNumber(elecNumber=targetElecNumber)
    groupNumber <- res$groupNumber
    elecNumberInGroup <- res$elecNumber

    latenciesFromPrevPeakFilename <- 
     sprintf(latenciesFromPrevPeakFilenamePattern, 
              sessionName, lowCutoff, highCutoff, order, 
              groupNumber, elecNumberInGroup)
    latenciesFromPrevPeakDiffs <- c()
    if(file.exists(latenciesFromPrevPeakFilename)) {
        res <- get(load(latenciesFromPrevPeakFilename))
        targetPeakTimes <- res$peakTimes
        for(i in 1:length(targetPeakTimes)) {
            targetPeakTime <- targetPeakTimes[i]
            index <- which.min(abs(targetPeakTime-referencePeakTimes))
            latencyFromPrevPeakDiffInSecs <- 
             targetPeakTime-referencePeakTimes[index]
            latenciesFromPrevPeakDiffsInSecs <- 
             c(latenciesFromPrevPeakDiffsInSecs,
                latencyFromPrevPeakDiffInSecs)
        }
    } else {
        warning(sprintf("%s not found", latenciesFromPrevPeakFilename))
    }
    figFilename <- sprintf(figFilenamePattern, sessionName,
                                               referenceElecNumber,
                                               targetElecNumber,
                                               lowCutoff, highCutoff, order, 
                                               t0)

    df <- data.frame(targetPeakTimes=targetPeakTimes,
                      latenciesFromPrevPeakDiffs=latenciesFromPrevPeakDiffs)
    p <- ggplot(data=df, mapping=aes(x=targetPeakTimes,
                                      y=latenciesFromPrevPeakDiffs))
    p <- p + geom_point()
    p <- p + geom_line()
    p <- p + xlab(xlab)
    p <- p + ylab(ylab)
    ggsave(plot=p, file=figFilename)
    print(p)

    browser()
}

processAll()
