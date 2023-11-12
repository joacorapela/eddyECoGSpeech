
source("doLoadSources.R")

processAll <- function() {
    sessionName <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    zScore <- TRUE
    T <- 1.62
    # sessionName <- "EC2_B15"
    # lowCutoff <- 0.8
    # highCutoff <- 1.2
    # plotFromTime <- 30
    # plotToTime <- 700
    plotFromTime <- 75.5
    plotToTime <- 80
    plotDT <- .1
    # elecNumbers <- c(247, 231, 215, 199, 183, 167, 151, 135, 119, 103, 87, 71, 55, 39, 23, 7)
    # elecNumbers <- c(151, 135, 119, 103, 87)
    # elecNumbers <- c(103, 87, 71, 55)
    # elecNumbers <- c(166, 150, 134, 118, 102)
    # elecNumbers <- c(215, 199, 183, 167, 151, 135, 119)
    elecNumbers <- c(142, 141, 140, 139, 138, 137, 136, 135)
    # elecNumbers <- c(70, 71, 72, 88, 104, 120)
    # elecNumbers <- c(95, 110, 109, 108, 107, 123)
    # elecNumbers <- c(136, 137, 138, 139, 140, 141)
    # elecNumbers <- c(54, 70, 71, 72, 88, 104, 120, 121, 
                     # 138, 139, 140, 141, 158, 174,
                     # 154, 155, 156, 157, 173,
                                    # 172)
    distancesFromRefElec <- seq(from=0, to=length(elecNumbers)-1)*4*1e-3
    nResamples <- 1000
    colorRegressionLine <- "red"
    xAnnotation <- -Inf
    yAnnotation <- Inf
    hjustAnnotation <- 0
    vjustAnnotation <- 1
    colorAnnotation <- "red"
    sizeAnnotation <- 6
    xlab <- "Distance (m)"
    ylab <- "Phase Difference (radians)"
    ylim <- 2*pi*c(-1,1)
    htFilenamePattern <- 
     "results/%s/htFilteredFrom%.02fTo%.02fOrder%02dZScored%dWav%d%d.RData"
    # figFilenamePattern <- NA
    figFilenamePattern <- "figures/%s/phaseDifVsDistanceFilteredFromFreq%.02fTo%.02fOrder%02dFromElec%03dTo%03dPlotTime%.02f.png"

    res <- getGroupAndElecNumber(elecNumber=elecNumbers[1])
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

    plotFromSample <- which.min(abs(refElecTimes-plotFromTime))
    plotToSample <- which.min(abs(refElecTimes-plotToTime))
    plotBySamples <- as.integer(plotDT*res$ecogSampleRate)
    samplesToPlot <- seq(from=plotFromSample, to=plotToSample, by=plotBySamples)

    phaseDiffsSubset <- c()
    distancesFromRefElecSubset <- c()
    for(i in 1:length(elecNumbers)) {
        elecNumber <- elecNumbers[i]
        show(sprintf("Processing electrode %d", elecNumber))
        res <- getGroupAndElecNumber(elecNumber=elecNumber)
        groupNumber <- res$groupNumber
        elecNumberInGroup <- res$elecNumber

        htFilename <- sprintf(htFilenamePattern,
                               sessionName,
                               lowCutoff, highCutoff, order, zScore,
                               groupNumber, elecNumberInGroup)
        if(file.exists(htFilename)) {
            res <- get(load(htFilename))
            targetElecPhases <- Arg(res$ht)
            phaseDiffsSubset <- cbind(phaseDiffsSubset, 
                                       targetElecPhases-refElecPhases)
            distancesFromRefElecSubset <- c(distancesFromRefElecSubset, 
                                             distancesFromRefElec[i])
        } else {
            warning(sprintf("%s not found", htFilename))
        }
    }
    for(sampleToPlot in samplesToPlot) {
        timeToPlot <- refElecTimes[sampleToPlot]
        phaseDiffs <- phaseDiffsSubset[sampleToPlot,]
        p <- getPlotPhaseDiffsVsDistances(phaseDiffs=phaseDiffs, 
                                           distances=distancesFromRefElec,
                                           timeToPlot=timeToPlot,
                                           nResamples=nResamples,
                                           colorRegressionLine=
                                            colorRegressionLine, 
                                           xAnnotation=xAnnotation, 
                                           yAnnotation=yAnnotation,
                                           hjustAnnotation=hjustAnnotation,
                                           vjustAnnotation=vjustAnnotation,
                                           colorAnnotation=colorAnnotation,
                                           sizeAnnotation=sizeAnnotation,
                                           xlab=xlab, ylab=ylab, ylim=ylim)
        print(p)
        if(!is.na(figFilenamePattern)) {
            figFilename <- sprintf(figFilenamePattern, sessionName, 
                                                       lowCutoff, highCutoff, 
                                                       order,
                                                       min(elecNumbers),
                                                       max(elecNumbers),
                                                       timeToPlot)
            ggsave(plot=p, file=figFilename)
        }
        # browser()
    }
}

processAll()

rm(processAll)
