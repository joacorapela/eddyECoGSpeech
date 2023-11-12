
source("doLoadSources.R")

processAll <- function() {
    elecNumber <- 134
    sessionLabel <- "EC2_B105"
    lowCutoff <- 80
    highCutoff <- 110
    plotFromTime <- 400
    plotDuration <- 2

    plotToTime <- plotFromTime+plotDuration

    bandpassedFilenamePattern <- "results/%s/bandpassedFilteredFrom%.02fTo%.02fWav%d%d.RData"

    res <- getGroupAndElecNumber(elecNumber=elecNumber)
    bandpassedFilename <- sprintf(bandpassedFilenamePattern, 
                                   sessionLabel,
                                   lowCutoff, highCutoff,
                                   res$groupNumber, res$elecNumber)
    if(file.exists(bandpassedFilename)) {
        bandpassedLoadRes <- get(load(bandpassedFilename))
        times <- ((1:length(bandpassedLoadRes$filteredECoG))-1)/
                  bandpassedLoadRes$ecogSampleRate
        samplesToPlot <- which(plotFromTime<=times & times<=plotToTime)
        timesToPlot <- times[samplesToPlot]
        bandpassedToPlot <- bandpassedLoadRes$filteredECoG[samplesToPlot]
        htToPlot <- HilbertTransform(bandpassedToPlot)
        analyticSignalToPlot <- htToPlot

        amplitudeEnvolopsToPlot <- Mod(analyticSignalToPlot)
        instantaneousPhasesToPlot <- Arg(analyticSignalToPlot)
        df <- data.frame(times=timesToPlot,
                          bandpassed=bandpassedToPlot,
                          posAmplitudeEnvelop=amplitudeEnvolopsToPlot,
                          negAmplitudeEnvelop=-amplitudeEnvolopsToPlot)
#                           instantaneousPhase=instantaneousPhasesToPlot)
        mDF <- melt(df, id.vars="times")
        p <- ggplot(mDF, aes(x=times, y=value, color=variable))
        p <- p + geom_line()
        print(p)
    } else {
        stop(sprintf("File %s does not exist", htFilename))
    }
    browser()
}

processAll()

rm(processAll)

