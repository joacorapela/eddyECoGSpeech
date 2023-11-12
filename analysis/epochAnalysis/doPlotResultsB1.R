
source("doLoadSources.R")

processAll <- function() {
    groupNumbers <- 3
    elecNumbers <- 62
    # groupNumbers <- 1:4
    # elecNumbers <- 1:64
    resultsFilenamePattern <- "results/EC2_B1/resultsWav%d%d.RData"
    erpFigFilenamePattern <- "figures/EC2_B1/erp%d%d.eps"
    itcFigFilenamePattern <- "figures/EC2_B1/itc%d%d.eps"
    erspFigFilenamePattern <- "figures/EC2_B1/ersp%d%d.eps"
    xlim <- c(-.5, .6)
    ylim <- c(2, 300)
    # ylim <- NULL
    # zlim <- c(0, 1)
    zlimITC <- c(0, .4)
    zlimERSP <- NULL
    contour <- FALSE
    width <- 8
    height <- 8

    for(groupNumber in groupNumbers) {
        for(elecNumber in elecNumbers) {
            show(sprintf("Processing Wav%d%d", groupNumber, elecNumber))
            resultsFilename <- sprintf(resultsFilenamePattern, groupNumber,
                                                               elecNumber)
            erpFigFilename <- sprintf(erpFigFilenamePattern, groupNumber,
                                                             elecNumber)
            itcFigFilename <-sprintf(itcFigFilenamePattern, groupNumber,
                                                            elecNumber)
            erspFigFilename <-sprintf(erspFigFilenamePattern, groupNumber,
                                                              elecNumber)
            results <- get(load(resultsFilename))
            plotOneSetOfResults(results=results,
                                 erpFigFilename=erpFigFilename,
                                 itcFigFilename=itcFigFilename,
                                 erspFigFilename=erspFigFilename,
                                 xlim=xlim,
                                 ylim=ylim,
                                 zlimITC=zlimITC,
                                 zlimERSP=zlimERSP,
                                 contour=contour,
                                 width=width,
                                 height=height)

        }
    }
}

processAll()

rm(processAll)
