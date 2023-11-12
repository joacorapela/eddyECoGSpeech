
source("doLoadSources.R")

processAll <- function() {
    elecNumbersToPlot <- seq(from=256, to=1, by=-1)
    # elecNumbers <- 1:64
    # elecNumbers <- 1:256
    # elecNumbers <- c(3)
    sessionLabel <- "EC2_B105"
    epochFromTime <- -10.0
    epochToTime <- 10.0
    medianInterCVSIntervalHz <- 1/1.62
    medianInterCVSIntervalHzCol <- "red"
    transX <- "log10"
    transY <- "log10"
    anXlab <- "Frequency (Hz)"
    anYlab <- "Power"
    width <- 40
    height <- 40
    resultsFilenamePattern <- "results/%s/averagedPSDElec%dEpochFromTime%.02fToTime%.02f.RData"
    figFilenamePattern <- "figures/%s/averagedPSDEpochFromTime%.02fToTime%.02f_%dx%d.png"

    plots <- list()
    for(elecNumber in elecNumbersToPlot) {
        resultsFilename <- sprintf(resultsFilenamePattern, sessionLabel,
                                                           elecNumber,
                                                           epochFromTime,
                                                           epochToTime)
        if(file.exists(resultsFilename)) {
            spectrumRes <- get(load(resultsFilename))
            d <- data.frame(freq=spectrumRes$freq, 
                             averagedPSD=spectrumRes$averagedPSD,
                             ciUpper=spectrumRes$averagedPSDCIUpper,
                             ciLower=spectrumRes$averagedPSDCILower)
            p <- ggplot(data=d, mapping=aes(x=freq, y=averagedPSD))
            p <- p + geom_line()
            p <- p + scale_x_continuous(trans=transX)
            p <- p + scale_y_continuous(trans=transY)
            p <- p + geom_errorbar(aes(x=freq, ymin=ciLower, ymax=ciUpper))
            p <- p + geom_vline(xintercept=medianInterCVSIntervalHz,
                                 color=medianInterCVSIntervalHzCol)
            p <- p + xlab(label=anXlab)
            p <- p + ylab(label=anYlab)
        } else {
            p <- getEmptyPlot()
        }
        p <- p + ggtitle(sprintf("%d", elecNumber))
        plots <- c(plots, list(p))
    }
    figFilename <- sprintf(figFilenamePattern, sessionLabel, 
                                               epochFromTime, epochToTime,
                                               width, height)
    layoutMatrix <- matrix(data=1:length(plots), ncol=sqrt(length(plots)))
    p <- arrangeGrob(grobs=plots, ncol=sqrt(length(plots)), layout_matrix=layoutMatrix)
    ggsave(filename=figFilename, plot=p, width=width, height=height, limitsize=FALSE)

    browser()
}

processAll()

rm(processAll)
