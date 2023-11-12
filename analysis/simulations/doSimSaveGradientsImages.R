
source("doLoadSources.R")

processAll <- function() {
    sessionLabel <- "simulatedPlaneTWs"
    lowCutoff <- 12.0
    highCutoff <- 15.0
    order <- 2
    zScore <- FALSE
    sampleRate <- 1000.0
    elecNumbers <- 1:256
    # elecNumbersToPlot <- c(135, 151, 167, 183, 199, 215, 231, 247,
    #                  134, 150, 166, 182, 198, 214, 230, 246, 
    #                  119, 103,  87,  71,  55,  39,  23,   7, 
    #                  118, 102,  86,  70,  54,  38,  22,   6)
    elecNumbersToPlot <- 1:256
    fromTimeHT <- 0.0
    durationHT <- 1.0
    fromTimeToPlot <- 0.0
    durationToPlot <- 1.0
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
    height <- 6
    gradientsFilenamePattern <- "results/%s/gradientsHTFilteredFrom%.02fTo%.02fOrder%02dZScored%dFPS%.02fElecFrom%03dTo%03dTimeFrom%.02fTo%.02f.RData"
    figDirnamePattern <- "videos/%s/gradientsElec%03d-%03dFR%.02fFromTime%03dTo%03dHtFilteredFrom%.02fTo%0.2fOrder%02dZScored%d"
    figFilenamePattern <- "gradientsIndex%06d.png"

    toTimeHT <- fromTimeHT + durationHT
    gradientsFilename <- sprintf(gradientsFilenamePattern, 
                                  sessionLabel, lowCutoff, highCutoff, order, 
                                  zScore,
                                  sampleRate, 
                                  min(elecNumbers), max(elecNumbers), 
                                  fromTimeHT, toTimeHT)
    resGradWithTimes <- get(load(gradientsFilename))

    toTimeToPlot <- fromTimeToPlot + durationToPlot
    saveGradientImages(sessionLabel=sessionLabel, 
                        dxs=resGradWithTimes$dx, 
                        dys=resGradWithTimes$dy, 
                        times=resGradWithTimes$times, 
                        figDirnamePattern=figDirnamePattern,
                        figFilenamePattern=figFilenamePattern,
                        elecNumbers=elecNumbersToPlot,
                        fromTime=fromTimeToPlot, toTime=toTimeToPlot,
                        lowCutoff=lowCutoff, highCutoff=highCutoff, order=order,
                        zScore=zScore,
                        sampleRate=sampleRate,
                        titlePattern=titlePattern, 
                        arrowLength=arrowLength, arrowAngle=arrowAngle,
                        vectorCol=vectorCol,
                        nrow=nrow, ncol=ncol,
                        anXlim=anXlim, anYlim=anYlim)

    browser()
}

processAll()

rm(processAll)
