
source("doLoadSources.R")

processAll <- function() {
    cvs <- "ree"
    sessionLabel <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    zScore <- FALSE
    sampleRate <- 24.61
    elecNumbers <- 1:256
    elecNumbersToPlot <- 1:256
    fromTimeHT <- 00
    durationHT <- 700
    fromTimeAfterCVSInitiation <- 0 # seconds
    maxToTimeAfterCVSInitiation <- 1 # seconds
    pauseTimeBTWCVSs <- 0.5 # seconds
    titlePattern <- "%d:%02d"
    arrowLength <- 0.35
    arrowAngle <- 20
    vectorCol <- "blue"
    nrow <- 16
    ncol <- 16
    anXlim <- c(1-5, ncol+5)
    anYlim <- c(1-5, nrow+5)
    # anXlim <- c(1, ncol)
    # anYlim <- c(1, nrow)
    transcriptionSampleRate <- 1e7
    transcriptionFilename <-
     "../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final_completed.lab"
    gradientsFilenamePattern <- "results/%s/gradientsHTFilteredFrom%.02fTo%.02fOrder%02dZScored%dFPS%.02fElecFrom%03dTo%03dTimeFrom%.02fTo%.02f.RData"
    figDirnamePattern <- "videos/%s/gradientsCVS%sElec%03d-%03dFR%.02fFromTime%03dTo%03dHtFilteredFrom%.02fTo%0.2fOrder%02dZScored%d"
    figFilenamePattern <- "gradientsIndex%06d.png"

    toTimeHT <- fromTimeHT + durationHT
    gradientsFilename <- sprintf(gradientsFilenamePattern, 
                                  sessionLabel, lowCutoff, highCutoff, order, 
                                  zScore,
                                  sampleRate, 
                                  min(elecNumbers), max(elecNumbers), 
                                  fromTimeHT, toTimeHT)
    resGradWithTimes <- get(load(gradientsFilename))

    saveCVSGradientImages(cvs=cvs,
                           sessionLabel=sessionLabel, 
                           dxs=resGradWithTimes$dx,
                           dys=resGradWithTimes$dy,
                           times=resGradWithTimes$times,
                           elecNumbers=elecNumbersToPlot,
                           fromTimeAfterCVSInitiation=
                            fromTimeAfterCVSInitiation, 
                           maxToTimeAfterCVSInitiation=maxToTimeAfterCVSInitiation,
                           lowCutoff=lowCutoff, highCutoff=highCutoff, 
                           order=order,
                           zScore=zScore,
                           sampleRate=resGradWithTimes$sampleRate,
                           pauseTimeBTWCVSs=pauseTimeBTWCVSs,
                           titlePattern=titlePattern, 
                           arrowLength=arrowLength, arrowAngle=arrowAngle,
                           vectorCol=vectorCol,
                           nrow=nrow, ncol=ncol,
                           anXlim=anXlim, anYlim=anYlim,
                           transcriptionSampleRate=transcriptionSampleRate,
                           transcriptionFilename=transcriptionFilename,
                           figDirnamePattern=figDirnamePattern,
                           figFilenamePattern=figFilenamePattern)

    browser()
}

processAll()

rm(processAll)
