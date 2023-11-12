
source("doLoadSources.R")
require(plotly)

processAll <- function() {
    plotTime <- 400.2
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    # lowCutoff <- 1.0
    # highCutoff <- 1.4
    # order <- 3
    zScore <- FALSE
    sessionLabel <- "EC2_B105"
    fromTime <- 00
    duration <- 700
    xlab <- "Electrode Index X"
    ylab <- "Electrode Index Y"
    zlab <- "Phase (radians)"
    colorbarTitle <- "Phase\n(radians)"
    titlePattern <- "Time (sec) %.02f"
    metaDataPhasesDatacubeFilenamePattern <- "results/%s/metaDataPhasesDatacubeFilteredFrom%.02fTo%.02fOrder%02dZScored%dFromTime%.02fTo%.02f.%s"
    phasesDatacubeFilenamePattern <- "results/%s/unwrapped3DPhasesDatacubeFilteredFrom%.02fTo%.02fOrder%02dZScored%dFromTime%.02fTo%.02f.%s"

    toTime <- fromTime + duration
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

    plotSample <- plotTime*Fs
    plotArray <- phasesDatacube[,,plotSample]
#     mPlotArray <- melt(plotArray)
#     p <- ggplot(mPlotArray, aes(x=X2, y=X1, fill=value)) + geom_tile()
#     p <- p + scale_fill_gradient(name=legendLabel)
#     p <- p + xlab(xlab)
#     p <- p + ylab(ylab)

    p <- plot_ly(z = ~plotArray) %>% 
         add_surface() %>%
         layout(title=sprintf(titlePattern, plotTime),
                scene=list(xaxis=list(title=xlab), 
                           yaxis=list(title=ylab),
                           zaxis=list(title=zlab))) %>%
         colorbar(title=colorbarTitle)

    print(p)

    browser()
}

processAll()

rm(processAll)
