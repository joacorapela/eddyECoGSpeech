
source("doLoadSources.R")

processAll <- function() {
    elecNumbers <- c(134:138)
    sessionLabel <- "EC2_B105"
    lowCutoff <- .4
    highCutoff <- .8
    plotFromTime <- 150
    plotDuration <- 4
    firstElectrode <- TRUE

    plotToTime <- plotFromTime+plotDuration

    bandpassedFilenamePattern <- "results/%s/bandpassedFilteredFrom%.02fTo%.02fWav%d%d.RData"

    for(elecNumber in elecNumbers) {
        res <- getGroupAndElecNumber(elecNumber=elecNumber)
        bandpassedFilename <- sprintf(bandpassedFilenamePattern, 
                                       sessionLabel,
                                       lowCutoff, highCutoff,
                                       res$groupNumber, res$elecNumber)
        if(file.exists(bandpassedFilename)) {
            bandpassedLoadRes <- get(load(bandpassedFilename))
            if(firstElectrode) {
                firstElectrode <- FALSE
                times <- ((1:length(bandpassedLoadRes$filteredECoG))-1)/
                          bandpassedLoadRes$ecogSampleRate
                samplesToPlot <- which(plotFromTime<=times & times<=plotToTime)
                timesToPlot <- times[samplesToPlot]
                df <- data.frame(times=timesToPlot)
            }
            bandpassedToPlot <- bandpassedLoadRes$filteredECoG[samplesToPlot]
            analyticSignalToPlot <- HilbertTransform(bandpassedToPlot)
            instantaneousPhasesToPlot <- Arg(analyticSignalToPlot)
            uwInstantaneousPhasesToPlot <- unwrap(instantaneousPhasesToPlot)
            df <- cbind(df, uwInstantaneousPhasesToPlot)
        } else {
            stop(sprintf("File %s does not exist", bandpassedFilename))
        }
    }
    colnames(df) <- c("times", sprintf("e%02d", elecNumbers))
    mDF <- melt(df, id.vars="times")
    p <- ggplot(mDF, aes(x=times, y=value, color=variable))
    p <- p + geom_line()
    print(p)
    browser()
}

processAll()

rm(processAll)

