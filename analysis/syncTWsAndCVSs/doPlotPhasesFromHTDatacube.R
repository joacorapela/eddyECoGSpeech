
source("doLoadSources.R")
library("plotly")

processAll <- function() {
    plotTime <- 250.0
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    # lowCutoff <- 1.0
    # highCutoff <- 1.4
    # order <- 3
    zScore <- TRUE
    sessionLabel <- "EC2_B105"
    fromTime <- 00
    duration <- 700
    xlab <- "Electrode Position X"
    ylab <- "Electrode Position Y"
    legendLabel <- "Phase\n(radians)"
    htDatacubeFilenamePattern <- "results/%s/htDatacubeFilteredFrom%.02fTo%.02fOrder%02dZScored%dFromTime%.02fTo%.02f.RData"

    toTime <- fromTime + duration
    htDatacubeFilename <- sprintf(htDatacubeFilenamePattern, 
                                   sessionLabel,
                                   lowCutoff, highCutoff, order, zScore,
                                   fromTime, toTime)
    htDatacube <- get(load(htDatacubeFilename))
#     plotSample <- which.min(abs(plotTime-htDatacube$timesToSave))
plotSample <- 1025
    plotArray <- Arg(htDatacube$htsArray[,,plotSample])
#     mPlotArray <- melt(plotArray)
#     p <- ggplot(mPlotArray, aes(x=X2, y=X1, fill=value)) + geom_tile()
#     p <- p + scale_fill_gradient(name=legendLabel)
#     p <- p + xlab(xlab)
#     p <- p + ylab(ylab)

    p <- plot_ly(z = ~plotArray) %>% add_surface()
    print(p)
    browser()
}

processAll()

rm(processAll)
