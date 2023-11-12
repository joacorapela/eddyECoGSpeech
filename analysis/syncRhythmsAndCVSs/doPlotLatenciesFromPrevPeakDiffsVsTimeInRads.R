
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
    referenceElecNumber <- 140
    targetElecNumber <- 139
    titlePattern <- "%d-%d"
    xlim <- c(380, 385)
    ylim <- 2*pi*c(-1,1)
    xlab <- "Time (sec)"
    ylab <- "Latency from Prev Peak Diff. (sec)"
    colVLine <- "lightgray"
    latenciesFromPrevPeakFilenamePattern <- "results/%s/latenciesFromPrevPeakFilteredFrom%.02fTo%.02fOrder%02dWav%d%d.RData"
    figFilenamePattern <- "figures/%s/latenciesFromPrevPeakDiffsVsTimeInRadsRefElec%03dTestElec%03dFilteredFrom%.02fTo%.02fOrder%02d.eps"

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
    referenceTimes <- res$times
    referenceLatenciesFromPrevPeakInRads <- res$latenciesFromPrevPeakInRads

    res <- getGroupAndElecNumber(elecNumber=targetElecNumber)
    groupNumber <- res$groupNumber
    elecNumberInGroup <- res$elecNumber

    latenciesFromPrevPeakFilename <- 
     sprintf(latenciesFromPrevPeakFilenamePattern, 
              sessionName, lowCutoff, highCutoff, order, 
              groupNumber, elecNumberInGroup)
    if(file.exists(latenciesFromPrevPeakFilename)) {
        res <- get(load(latenciesFromPrevPeakFilename))
        targetTimes <- res$times
        targetLatenciesFromPrevPeakInRads <- res$latenciesFromPrevPeakInRads
        latenciesFromPrevPeakDiffs <- targetLatenciesFromPrevPeakInRads-
                                       referenceLatenciesFromPrevPeakInRads
        toWrapIndices <- which(latenciesFromPrevPeakDiffs>pi)
        latenciesFromPrevPeakDiffs[toWrapIndices] <-
         latenciesFromPrevPeakDiffs[toWrapIndices]-pi
        toWrapIndices <- which(latenciesFromPrevPeakDiffs<(-pi))
        latenciesFromPrevPeakDiffs[toWrapIndices] <-
         latenciesFromPrevPeakDiffs[toWrapIndices]+pi
    } else {
        stop(sprintf("%s not found", latenciesFromPrevPeakFilename))
    }
    figFilename <- sprintf(figFilenamePattern, sessionName,
                                               referenceElecNumber,
                                               targetElecNumber,
                                               lowCutoff, highCutoff, order, 
                                               t0)

    df <- data.frame(times=referenceTimes,
                      latenciesFromPrevPeakDiffs=latenciesFromPrevPeakDiffs)
    p <- ggplot(data=df, mapping=aes(x=times, 
                                      y=latenciesFromPrevPeakDiffs))
    p <- p + geom_point()
    p <- p + geom_line()
    p <- p + xlim(xlim)
    p <- p + xlab(xlab)
    p <- p + ylab(ylab)
    p <- p + geom_hline(yintercept=0, color=colVLine)
    yTickMarks <- pi*seq(from=-2, to=2, by=.25)
    labels <- c(expression(-2*pi), expression(-7*pi/4), expression(-3*pi/2), expression(-5*pi/4), expression(-pi), expression(-3*pi/4), expression(-pi/2), expression(-pi/4), 0, expression(pi/4), expression(pi/2), expression(3*pi/4), expression(pi), expression(5*pi/4), expression(3*pi/2), expression(7*pi/4), expression(2*pi))
    p <- p + scale_y_continuous(breaks=yTickMarks, label=labels, limits=ylim)
    p <- p + ggtitle(sprintf(titlePattern, targetElecNumber, 
                                           referenceElecNumber))
    ggsave(plot=p, file=figFilename)
    print(p)

    browser()
}

processAll()
