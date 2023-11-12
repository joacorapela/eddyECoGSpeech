
source("doLoadSources.R")

processAll <- function() {
    # sessionName <- "EC2_B1"
    # lowCutoff <- 0.7
    # highCutoff <- 1.1
    # order <- 2
    # nBins <- 15
    sessionName <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    nBins <- 30
    # sessionName <- "EC2_B15"
    # lowCutoff <- 0.8
    # highCutoff <- 1.2
    # order <- 2
    # nBins <- 15
    # sessionName <- "EC2_B76"
    # lowCutoff <- 0.6
    # highCutoff <- 1.0
    # order <- 2
    # nBins <- 30
    # sessionName <- "EC2_B8"
    # lowCutoff <- 0.7
    # highCutoff <- 1.1
    # order <- 2
    # nBins <- 30
    # sessionName <- "EC2_B89"
    # lowCutoff <- 0.8
    # highCutoff <- 1.2
    # order <- 2
    # nBins <- 30
    # sessionName <- "EC2_B9"
    # lowCutoff <- 0.7
    # highCutoff <- 1.1
    # order <- 2
    # nBins <- 30
    ylimVsTime <- c(0, .15)
    xlimHist <- c(0, .15)

    elecNumbers <- c(141, 140, 139, 138, 137, 136)
    significance <- .01
    splineDF <- 5
    medianVelCol <- "red"
    annotationPattern <- "number of cycles=%d\nproportion detected TWs (p<%.02f)=%.02f\nmedian velocity=%.04f m/s"
    xAnnotation <- Inf
    yAnnotation <- Inf
    hjustAnnotation <- 1
    vjustAnnotation <- 1
    colorAnnotation <- "red"
    statsFilenamePattern <- "results/%s/velocityStatsFilteredFrom%.02fTo%.02fOrder%02dFromElec%03dTo%03d.RData"
    velVsTimeFilenamePattern <- "figures/%s/velocityVsTimeFilteredFrom%.02fTo%.02fOrder%02dFromElec%03dTo%03d.eps"
    velHistFilenamePattern <- "figures/%s/velocityHistFilteredFrom%.02fTo%.02fOrder%02dFromElec%03dTo%03d.eps"
    corPhasorLatencyAndDistanceFilenamePattern <- "figures/%s/corPLDFilteredFrom%.02fTo%.02fOrder%02dFromElec%03dTo%03d.eps"

    statsFilename <- sprintf(statsFilenamePattern, sessionName, 
                                                   lowCutoff, highCutoff, 
                                                   order,
                                                   min(elecNumbers), 
                                                   max(elecNumbers))
    velVsTimeFilename <- sprintf(velVsTimeFilenamePattern, sessionName, 
                                                           lowCutoff, 
                                                           highCutoff, 
                                                           order,
                                                           min(elecNumbers), 
                                                           max(elecNumbers))
    velHistFilename <- sprintf(velHistFilenamePattern, sessionName, 
                                                       lowCutoff, 
                                                       highCutoff, 
                                                       order,
                                                       min(elecNumbers), 
                                                       max(elecNumbers))
    corPhasorLatencyAndDistanceFilename <- sprintf(corPhasorLatencyAndDistanceFilenamePattern, sessionName, 
                                                       lowCutoff, 
                                                       highCutoff, 
                                                       order,
                                                       min(elecNumbers), 
                                                       max(elecNumbers))
    statsDF <- get(load(statsFilename))
    significantStatsDF <- subset(statsDF, p<significance)

    nCycles <- nrow(statsDF)
    medianVel <- median(statsDF$slope)
    pDetectedTWs <- nrow(significantStatsDF)/nCycles
    annotation <- sprintf(annotationPattern, nCycles, significance, 
                                             pDetectedTWs, medianVel)

    splineRes <- smooth.spline(x=significantStatsDF$time, 
                                y=significantStatsDF$slope,
                                df=splineDF)
    splineResDF <- data.frame(time=splineRes$x, slope=splineRes$y)
    p1 <- ggplot()
    p1 <- p1 + geom_point(data=significantStatsDF, 
                           mapping=aes(x=time, y=slope))
    p1 <- p1 + geom_line(data=splineResDF, 
                           mapping=aes(x=time, y=slope),
                           colour="red")
    p1 <- p1 + annotate("text", label=annotation, x=xAnnotation, y=yAnnotation,
                        hjust=hjustAnnotation, vjust=vjustAnnotation, 
                        color=colorAnnotation)
    p1 <- p1 + xlab("Time (sec)")
    p1 <- p1 + ylab("Velocity (m/sec)")
    p1 <- p1 + ylim(ylimVsTime)
    ggsave(p1, file=velVsTimeFilename)

    p2 <- ggplot(data=significantStatsDF, mapping=aes(x=slope))
    p2 <- p2 + geom_histogram(bins=nBins)
    p2 <- p2 + geom_vline(xintercept=medianVel, colour=medianVelCol)
    p2 <- p2 + annotate("text", label=annotation, x=xAnnotation, y=yAnnotation,
                        hjust=hjustAnnotation, vjust=vjustAnnotation, 
                        color=colorAnnotation)
    p2 <- p2 + xlab("Velocity (m/sec)")
    p2 <- p2 + ylab("Count")
    p2 <- p2 + xlim(xlimHist)
    ggsave(p2, file=velHistFilename)

    p3 <- ggplot()
    p3 <- p3 + geom_point(data=significantStatsDF, mapping=aes(x=time, y=r))
    p3 <- p3 + annotate("text", label=annotation, x=xAnnotation, y=yAnnotation,
                        hjust=hjustAnnotation, vjust=vjustAnnotation, 
                        color=colorAnnotation)
    p3 <- p3 + xlab("Time (sec)")
    p3 <- p3 + ylab("Correlation Coefficient between Phasor Latency and Distance")
    ggsave(p3, file=corPhasorLatencyAndDistanceFilename)

    browser()
}

processAll()
