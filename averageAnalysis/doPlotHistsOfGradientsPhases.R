
source("doLoadSources.R")

processAll <- function() {
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    zScore <- TRUE
    scale <- TRUE
    sessionLabel <- "EC2_B105"
    elecNumbers <- 1:256
    fromTime <- 00
    duration <- 700
    sampleRate <- 24.61
    elecNumbersToPlot <- seq(from=256, to=1, by=-1)
    # xlim <- c(340, 370)
    xlim <- c(370, 400)
    width <- 60
    height <- 60

    gradientsFilenamePattern <- "results/%s/gradientsHTFilteredFrom%.02fTo%.02fOrder%02dZScored%dFPS%.02fElecFrom%03dTo%03dTimeFrom%.02fTo%.02f.RData"
    figFilenamePattern <- "figures/%s/histGradientsPhasesHTFilteredFrom%.02fTo%.02fOrder%02dZScored%dFPS%.02fElecFrom%03dTo%03dTimeFrom%.02fTo%.02f.eps"

    toTime <- fromTime + duration
    gradientsFilename <- sprintf(gradientsFilenamePattern, 
                                  sessionLabel, lowCutoff, highCutoff, order, 
                                  zScore, sampleRate, 
                                  min(elecNumbers), max(elecNumbers), 
                                  fromTime, toTime)
    resGradWithTimes <- get(load(gradientsFilename))
    plots <- list()
    for(elecNumberToPlot in elecNumbersToPlot) {
        show(sprintf("Processing electrode %d", elecNumberToPlot))
        electrodeIndexInArray <- 
         getElectrodeIndexInArrayGGPlot(elecNumber=elecNumberToPlot)
        gradientPhases <- resGradWithTimes$pd[electrodeIndexInArray[1], 
                                                electrodeIndexInArray[2],] 
        df <- data.frame(times=resGradWithTimes$times, phases=gradientPhases)
        p <- ggplot(df, aes(x=phases))
        p <- p + geom_histogram(breaks=seq(from=-pi, to=pi, length.out=20))
        p <- p + coord_polar(start=pi/2, direction=-1)
        p <- p + xlab("")
        p <- p + ylab("")
        p <- p + ggtitle(sprintf("%d", elecNumberToPlot))
        plots <- c(plots, list(p))
    }
    figFilename <- sprintf(figFilenamePattern, 
                            sessionLabel, lowCutoff, highCutoff, order, 
                            zScore, sampleRate, 
                            min(elecNumbers), max(elecNumbers), 
                            fromTime, toTime)
    layoutMatrix <- matrix(data=1:length(plots), ncol=sqrt(length(plots)))
    p <- arrangeGrob(grobs=plots, cols=sqrt(length(plots)), layout_matrix=layoutMatrix)
    ggsave(filename=figFilename, plot=p, width=width, height=height, limitsize=FALSE)
    browser()
}

processAll()

rm(processAll)
