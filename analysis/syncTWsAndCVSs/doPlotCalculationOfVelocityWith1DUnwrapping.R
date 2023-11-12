
source("doLoadSources.R")

processAll <- function() {
    sessionName <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    zScore <- TRUE
    fromTime <- 0
    toTime <- 700
    unwrapThreshold <- pi
    # sessionName <- "EC2_B15"
    # lowCutoff <- 0.8
    # highCutoff <- 1.2
    saveFromTime <- 37.8
    saveToTime <- 700
    saveDT <- .05
    # elecNumbers <- c(247, 231, 215, 199, 183, 167, 151, 135, 119, 103, 87, 71, 55, 39, 23, 7)
    # elecNumbers <- c(151, 135, 119, 103, 87)
    # elecNumbers <- c(103, 87, 71, 55)
    # elecNumbers <- c(166, 150, 134, 118, 102)
    # elecNumbers <- c(215, 199, 183, 167, 151, 135, 119)
    # elecNumbers <- c(141, 140, 139, 138, 137, 136)
    # elecNumbers <- c(70, 71, 72, 88, 104, 120)
    # elecNumbers <- c(95, 110, 109, 108, 107, 123)
    # elecNumbers <- c(136, 137, 138, 139, 140, 141)
    elecNumbers <- c(54, 70, 71, 72, 88, 104, 120, 121, 
                     138, 139, 140, 141, 158, 174,
                     154, 155, 156, 157, 173,
                                    172)
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
    metaDataPhasesDatacubeFilenamePattern <- "results/%s/metaDataPhasesDatacubeFilteredFrom%.02fTo%.02fOrder%02dZScored%dFromTime%.02fTo%.02f.%s"
    phasesDatacubeFilenamePattern <- "results/%s/phasesDatacubeFilteredFrom%.02fTo%.02fOrder%02dZScored%dFromTime%.02fTo%.02f.%s"
    figFilenamePattern <- NA
     # "figures/%s/phaseDifVsDistanceFilteredFromFreq%.02fTo%.02fOrder%02dFromElec%03dTo%03dPlotTime%.02f.png"

    metaDataPhasesDatacubeFilename <- 
     sprintf(metaDataPhasesDatacubeFilenamePattern, sessionName, lowCutoff, 
                                                    highCutoff, order, zScore,
                                                    fromTime, toTime, "txt")
    con <- file(description=metaDataPhasesDatacubeFilename, open="r")
    width <- as.integer(readLines(con=con, n=1))
    height <- as.integer(readLines(con=con, n=1))
    nFrames <- as.integer(readLines(con=con, n=1))
    Fs <- as.integer(readLines(con=con, n=1))
    close(con)
    phasesDatacubeFilename <- sprintf(phasesDatacubeFilenamePattern, 
                                       sessionName,
                                       lowCutoff, highCutoff, order, zScore,
                                       fromTime, toTime, "bin")
    con <- file(description=phasesDatacubeFilename, open="rb")
    buffer <- readBin(con=con, what=single(), n=width*height*nFrames, size=4)
    close(con)
    phasesDatacube <- array(buffer, dim=c(height, width, nFrames))

    electrodesPhases <- 
     getElectrodesPhasesFromDatacube(elecNumbers=elecNumbers, 
                                      phasesDatacube=phasesDatacube)
    times <- (1:dim(phasesDatacube)[3])/Fs
    for(timeToPlot in seq(from=saveFromTime, to=saveToTime, by=saveDT)) {
        samplesToPlot <- which.min(abs(times-timeToPlot))
        unwrappedElectrodesPhases <- 
         unwrap1DPhases(phases=electrodesPhases[samplesToPlot,],
                         threshold=unwrapThreshold)
show(paste("Wrapped phases: ", electrodesPhases[samplesToPlot,]))
show(paste("Unwrapped phases: ", unwrappedElectrodesPhases))
        phaseDiffs <- unwrappedElectrodesPhases-unwrappedElectrodesPhases[1]
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
                                                       elecNumbers[1],
                                                       elecNumbers[length(elecNumbers)],
                                                       timeToPlot)
            ggsave(plot=p, file=figFilename)
        }
        browser()
    }
}

processAll()

rm(processAll)
