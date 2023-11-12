
source("doLoadSources.R")

processAll <- function() {
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    zScore <- FALSE
    sessionLabel <- "EC2_B105"
    titlePattern <- "%d:%02d"
    # elecNumbers <- 162:256
    # elecNumbers <- c(37:39, 50:54, 67:70, 85:86, 101:103)
    elecNumbers <- 1:256
    fromTime <- 0.0
    toTime <- 700.0
    saveFromTime <- 340
    saveDuration <- 60
    nrow <- 16
    ncol <- 16
    desiredFrameRate <- 25
    titlePattern <- "%d:%02d"
    zlim <- c(-5, 5)*pi
    scaleName <- "Phase\n(rad)"
    metaDataPhasesDatacubeFilenamePattern <- "results/%s/metaDataPhasesDatacubeFilteredFrom%.02fTo%.02fOrder%02dZScored%dFromTime%.02fTo%.02f.%s"
    phasesDatacubeFilenamePattern <- "results/%s/unwrapped3DPhasesDatacubeFilteredFrom%.02fTo%.02fOrder%02dZScored%dFromTime%.02fTo%.02f.%s"
    figDirnamePattern <- "videos/%s/3DUnwrappedPhasesElec%03d-%03dFPS%.02fFrom%03dTo%03dHtFilteredFrom%.02fTo%0.2fOrder%02dZScored%d"
    figFilenamePattern <- "3DUnwrappedPhaseIndex%06d.png"

    saveToTime <- saveFromTime + saveDuration
    metaDataPhasesDatacubeFilename <- 
     sprintf(metaDataPhasesDatacubeFilenamePattern, sessionLabel, lowCutoff, 
                                                    highCutoff, order, zScore,
                                                    fromTime, toTime, "txt")
    con <- file(description=metaDataPhasesDatacubeFilename, open="r")
    width <- as.integer(readLines(con=con, n=1))
    height <- as.integer(readLines(con=con, n=1))
    nFrames <- as.integer(readLines(con=con, n=1))
    Fs <- as.integer(readLines(con=con, n=1))
    close(con)

    phasesDatacubeFilename <- sprintf(phasesDatacubeFilenamePattern, 
                                       sessionLabel,
                                       lowCutoff, highCutoff, order, zScore,
                                       fromTime, toTime, "bin")
    con <- file(description=phasesDatacubeFilename, open="rb")
    buffer <- readBin(con=con, what=single(), n=width*height*nFrames, size=4)
    close(con)
    phasesDatacube <- array(buffer, dim=c(height, width, nFrames))
    times <- (1:dim(phasesDatacube)[3])/Fs

    downsampleFactor <- round(Fs/desiredFrameRate)
    actualFrameRate <- Fs/downsampleFactor
    indicesToSave <- seq(from=1, to=dim(phasesDatacube)[3], by=downsampleFactor)
    phasesDatacubeToSave <- phasesDatacube[,,indicesToSave]
    timesToSave <- times[indicesToSave]

    figDirname <- sprintf(figDirnamePattern, sessionLabel, min(elecNumbers), max(elecNumbers), actualFrameRate, saveFromTime, saveToTime, lowCutoff, highCutoff, order, zScore)

#     scaledPhasesDatacubeToSave <- phasesDatacubeToSave
#     scaledPhasesDatacubeToSave <- (scaledPhasesDatacubeToSave-
#                                     mean(scaledPhasesDatacubeToSave,
#                                           na.rm=TRUE))/
#                                     sd(scaledPhasesDatacubeToSave, na.rm=TRUE)
#     saveImagesFromDatacube(datacube=scaledPhasesDatacubeToSave,
    saveImagesFromDatacube(datacube=phasesDatacubeToSave,
                            timesToSave=timesToSave, zlim=zlim, 
                            scaleName=scaleName,
                            titlePattern=titlePattern,
                            plotTimeInMinSec=TRUE,
                            figDirname=figDirname, 
                            figFilenamePattern=figFilenamePattern)
}

processAll()

rm(processAll)
