
source("doLoadSources.R")

processAll <- function() {
    groupNumbers <- 2
    elecNumbers <- 1
    # groupNumbers <- 1:4
    # elecNumbers <- 1:64
    resultsFilenamePattern <- "results/EC2_B105/resultsWav%d%d.RData"
    erpFigFilenamePattern <- "figures/EC2_B105/erp%d.eps"
    itcFigFilenamePattern <- "figures/EC2_B105/itc%d.eps"
    erspFigFilenamePattern <- "figures/EC2_B105/ersp%d.eps"
    xlim <- c(-.5, .6)
    ylim <- c(2, 300)
    # ylim <- NULL
    # zlim <- c(0, 1)
    zlimITC <- c(0, .4)
    zlimERSP <- NULL
    contour <- FALSE
    width <- 8
    height <- 8
    plotERP <- FALSE
    plotITC <- FALSE
    plotERSP <- TRUE

    for(groupNumber in groupNumbers) {
        for(elecNumber in elecNumbers) {
            absoluteElecNumber <- (groupNumber-1)*64+elecNumber
            show(sprintf("Processing electrode %d", absoluteElecNumber))
            resultsFilename <- sprintf(resultsFilenamePattern, groupNumber,
                                                               elecNumber)
            erpFigFilename <- sprintf(erpFigFilenamePattern,
                                       absoluteElecNumber)
            itcFigFilename <-sprintf(itcFigFilenamePattern, absoluteElecNumber)
            erspFigFilename <-sprintf(erspFigFilenamePattern,
                                       absoluteElecNumber)
            if(file.exists(resultsFilename)) {
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
                                     height=height,
                                     plotERP=plotERP,
                                     plotITC=plotITC,
                                     plotERSP=plotERSP)

            }
        }
    }
}

processAll()

rm(processAll)
