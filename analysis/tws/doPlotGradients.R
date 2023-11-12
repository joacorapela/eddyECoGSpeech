
source("doLoadSources.R")

processAll <- function() {
    averageFromTime <- 210.00 # seconds
    averageDuration <- 60.00 # seconds
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    sessionLabel <- "EC2_B105"
    elecNumbers <- 1:256
    fromTime <- 00
    duration <- 700
    sampleRate <- 24.61
    normalizationLength <- 0
    # normalizationLength <- 0.5
    vectorCol <- "blue"
    # electrodeLabelCol <- "dimgray"
    electrodeLabelCol <- "gray"
    width <- 6
    height <- 6
    ncol <- 16
    nrow <- 16
    gradientsFilenamePattern <- "results/%s/gradientsHTFilteredFrom%.02fTo%.02fOrder%02dFPS%.02fElecFrom%03dTo%03dTimeFrom%.02fTo%.02f.RData"
    figFilenamePattern <- "figures/%s/gradientsHTFilteredFrom%.02fTo%.02fOrder%02dFPS%.02fElecFrom%03dTo%03dTimeFrom%.02fTo%.02fAverageTimeFrom%.02fTo%.02fNormalizationLength%.02f.eps"

    toTime <- fromTime + duration
    gradientsFilename <- sprintf(gradientsFilenamePattern, 
                                  sessionLabel, lowCutoff, highCutoff, order, 
                                  sampleRate, 
                                  min(elecNumbers), max(elecNumbers), 
                                  fromTime, toTime)
    resGradWithTimes <- get(load(gradientsFilename))

    averageToTime <- averageFromTime + averageDuration
    averageFromFrame <- which.min(abs(averageFromTime-resGradWithTimes$times))
    averageToFrame <- which.min(abs(averageToTime-resGradWithTimes$times))
    averageDX <- apply(X=resGradWithTimes$dx, MARGIN=c(1,2), FUN=median)
    averageDY <- apply(X=resGradWithTimes$dy, MARGIN=c(1,2), FUN=median)
    if(normalizationLength>0) {
        normalization <- normalizationLength/sqrt(averageDX^2+averageDY^2)
        averageDX <- averageDX*normalization
        averageDY <- averageDY*normalization
    }
    resMeshgrid <- meshgrid(x=1:dim(averageDX)[2], y=1:dim(averageDX)[1])
    figFilename <- sprintf(figFilenamePattern, 
                                  sessionLabel, lowCutoff, highCutoff, order, 
                                  sampleRate, 
                                  min(elecNumbers), max(elecNumbers), 
                                  fromTime, toTime, 
                                  averageFromTime, averageToTime,
                                  normalizationLength)
    p <- getPlotVectorField(x=as.vector(resMeshgrid$Y),
                     y=as.vector(resMeshgrid$X),
                     dx=as.vector(averageDX),
                     dy=as.vector(averageDY),
                     col=vectorCol,
                     arrowLength=0.35,
                     arrowAngle=20)
    for(elecNumber in elecNumbers) {
        electrodeIndexInArray <-
         getElectrodeIndexInArrayGGPlot(elecNumber=elecNumber)
        p <- p + annotate("text", label=sprintf("%d", elecNumber), 
                          x=electrodeIndexInArray[2],
                          y=electrodeIndexInArray[1],
                          col=electrodeLabelCol)
    }
    p <- p + scale_x_continuous(breaks=seq(from=1:ncol))
    p <- p + scale_y_continuous(breaks=seq(from=1:nrow))
    p <- p + theme(panel.grid.minor=element_blank())
    p <- p + xlab("")
    p <- p + ylab("")

    ggsave(plot=p, filename=figFilename, width=width, height=height)
    print(p)

    browser()
}

processAll()

rm(processAll)
