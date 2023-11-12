
source("doLoadSources.R")

processAll <- function() {
    sessionName <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    zScore <- TRUE
    targetElecNumber <- 138
    refElecNumber <- 141
    titlePattern <- "%d-%d"
    xlim <- c(380, 400)
    ylim <- 2*pi*c(0,1)
    xlab <- "Time (sec)"
    ylab <- "Phase Difference (radians)"
    colVLine <- "lightgray"
    htFilenamePattern <- "results/%s/htFilteredFrom%.02fTo%.02fOrder%02dZScored%dWav%d%d.RData"
    figFilenamePattern <- "figures/%s/phaseDiffsVsTimeRefElec%03dTestElec%03dFilteredFrom%.02fTo%.02fOrder%02dTimeFrom%.02fTo%.02f.eps"

    res <- getGroupAndElecNumber(elecNumber=refElecNumber)
    groupNumber <- res$groupNumber
    elecNumber <- res$elecNumber
    htFilename <- sprintf(htFilenamePattern,
                           sessionName,
                           lowCutoff, highCutoff, order, zScore,
                           groupNumber, elecNumber)
    if(file.exists(htFilename)) {
        res <- get(load(htFilename))
        refElecTimes <- ((1:length(res$ht))-1)/res$ecogSampleRate
        refElecPhases <- Arg(res$ht)
    } else {
        stop(sprintf("%s not found", htFilename))
    }

    res <- getGroupAndElecNumber(elecNumber=targetElecNumber)
    groupNumber <- res$groupNumber
    elecNumber <- res$elecNumber
    htFilename <- sprintf(htFilenamePattern,
                           sessionName,
                           lowCutoff, highCutoff, order, zScore,
                           groupNumber, elecNumber)
    if(file.exists(htFilename)) {
        res <- get(load(htFilename))
        targetElecPhases <- Arg(res$ht)
    } else {
        stop(sprintf("%s not found", htFilename))
    }

    phaseDiffs <- targetElecPhases-refElecPhases
    indices <- which(phaseDiffs<0)
    phaseDiffs[indices] <- phaseDiffs[indices]+2*pi

    figFilename <- sprintf(figFilenamePattern, sessionName,
                                               refElecNumber,
                                               targetElecNumber,
                                               lowCutoff, highCutoff, order, 
                                               xlim[1], xlim[2])

    df <- data.frame(times=refElecTimes, phaseDiffs=phaseDiffs)
    p <- ggplot(data=df, mapping=aes(x=times, y=phaseDiffs))
    p <- p + geom_point()
    p <- p + geom_line()
    p <- p + xlim(xlim)
    p <- p + xlab(xlab)
    p <- p + ylab(ylab)
    p <- p + geom_hline(yintercept=0, color=colVLine)
    yTickMarks <- pi*seq(from=0, to=2, by=.25)
    labels <- c(0, expression(pi/4), expression(pi/2), expression(3*pi/4), expression(pi), expression(5*pi/4), expression(3*pi/2), expression(7*pi/4), expression(2*pi))
    p <- p + scale_y_continuous(breaks=yTickMarks, label=labels, limits=ylim)
    p <- p + ggtitle(sprintf(titlePattern, targetElecNumber, 
                                           refElecNumber))
    ggsave(plot=p, file=figFilename)
    print(p)

    browser()
}

processAll()
