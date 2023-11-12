
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
    t0 <- 245.0
    elecNumbers <- c(141, 140, 139, 138, 137, 136)
    distancesFromFirstElectorde <- c(0, 4, 8, 12, 16, 20)*1e-3
    nOscillations <- 1
    delayAfterPeakOfFirstElectrode <- 0.1
    colorRegressionLine <- "red"
    xAnnotation <- -Inf
    yAnnotation <- Inf
    hjustAnnotation <- 0
    vjustAnnotation <- 1
    colorAnnotation <- "red"
    sizeAnnotation <- 6
    xlab <- "Latency (sec)"
    ylab <- "Distance (m)"
    latenciesFromLastPeakFilenamePattern <- "results/%s/latenciesFromLastPeakFilteredFrom%.02fTo%.02fOrder%02dWav%d%d.RData"
    figFilenamePattern <- "figures/%s/phasorLatencyVsDistanceFilteredFrom%.02fTo%.02fOrder%02dT0%.02f.eps"

    # First get latencyTimes
    res <- getGroupAndElecNumber(elecNumber=elecNumbers[1])
    groupNumber <- res$groupNumber
    elecNumber <- res$elecNumber
    latenciesFromLastPeakFilename <- 
        sprintf(latenciesFromLastPeakFilenamePattern, 
                 sessionName, lowCutoff, highCutoff, order, 
                 groupNumber, elecNumber)
    if(!file.exists(latenciesFromLastPeakFilename)) {
        stop(sprintf("File %s does not exist", latenciesFromLastPeakFilename))
    }
    res <- get(load(latenciesFromLastPeakFilename))
    peakTimes <- res$peakTimes
    validPeakTimesIndices <- which(peakTimes>=t0)
    latencyTimesToSearch <- peakTimes[validPeakTimesIndices[1:nOscillations]]+delayAfterPeakOfFirstElectrode
    # done getting latencyTimes
    
    latenciesFromLastPeakToFit <- c()
    distancesFromFirstElectordeToFit <- c()
    for(i in 1:length(elecNumbers)) {
        elecNumber <- elecNumbers[i]
        show(sprintf("Processing electrode %d", elecNumber))
        res <- getGroupAndElecNumber(elecNumber=elecNumber)
        groupNumber <- res$groupNumber
        elecNumberInGroup <- res$elecNumber

        latenciesFromLastPeakFilename <- 
         sprintf(latenciesFromLastPeakFilenamePattern, 
                  sessionName, lowCutoff, highCutoff, order, 
                  groupNumber, elecNumberInGroup)
        if(file.exists(latenciesFromLastPeakFilename)) {
            res <- get(load(latenciesFromLastPeakFilename))
            electrodeLatencyTimes <- res$times
            electrodeLatenciesFormLastPeak <- res$latenciesFromLastPeak
            for(latencyTimeToSearch in latencyTimesToSearch) {
                index <- which.min(abs(latencyTimeToSearch-
                                        electrodeLatencyTimes))
                latenciesFromLastPeakToFit <- 
                 c(latenciesFromLastPeakToFit, 
                    electrodeLatenciesFormLastPeak[index])
                distancesFromFirstElectordeToFit <- 
                 c(distancesFromFirstElectordeToFit, 
                    distancesFromFirstElectorde[i])
            }
        } else {
            warning(sprintf("%s not found", latenciesFromLastPeakFilename))
        }
    }
    lmRes <- lm(distancesFromFirstElectordeToFit~latenciesFromLastPeakToFit)
    corTestRes <- cor.test(distancesFromFirstElectordeToFit,
                            latenciesFromLastPeakToFit)
    figFilename <- sprintf(figFilenamePattern, sessionName, 
                                               lowCutoff, highCutoff, order, 
                                               t0)

    df <- data.frame(distancesFromFirstElectordeToFit=
                       distancesFromFirstElectordeToFit,
                      latenciesFromLastPeakToFit=
                       latenciesFromLastPeakToFit)
    p <- ggplot(data=df, mapping=aes(y=distancesFromFirstElectordeToFit, 
                                      x=latenciesFromLastPeakToFit))
    p <- p + geom_point()
    p <- p + geom_abline(intercept=lmRes$coefficients[1], slope=lmRes$coefficients[2], colour=colorRegressionLine)
    p <- p + annotate("text", label=sprintf("slope=%.04f, r=%.02f, p=%.04f", lmRes$coefficients[2], corTestRes$estimate, corTestRes$p.value), x=xAnnotation, y=yAnnotation, hjust=hjustAnnotation, vjust=vjustAnnotation, colour=colorAnnotation, size=sizeAnnotation)
    p <- p + xlab(xlab)
    p <- p + ylab(ylab)
    ggsave(plot=p, file=figFilename)
    print(p)

    browser()
}

processAll()
