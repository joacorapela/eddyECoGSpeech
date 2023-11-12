
source("doLoadSources.R")

processAll <- function() {
    elecNumber <- 108
    neighborSize <- 3
    sessionLabel <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    zScore <- TRUE
    elecNumbers <- 1:256
    fromTime <- 300
    toTime <- 600
    nrow <- 16
    ncol <- 16
    htFilenamePattern <- "results/%s/htFilteredFrom%.02fTo%.02fOrder%02dZScored%dWav%d%d.RData"
    htDatacubeFilenamePattern <- "results/%s/htDatacubeFilteredFrom%.02fTo%.02fOrder%02dZScored%dFromTime%.02fTo%.02f.RData"

    htDatacubeFilename <- sprintf(htDatacubeFilenamePattern, 
                                   sessionLabel,
                                   lowCutoff, highCutoff, order, zScore,
                                   fromTime, toTime)
    htDatacubeLoadRes <- get(load(htDatacubeFilename))
    htsArray <- htDatacubeLoadRes$htsArray
    htsArray[is.na(htsArray)] <- 0.0
    electrodeIndexInArray <- 
     getElectrodeIndexInArrayGGPlot(elecNumber=elecNumber)
    phaseVariabilities <- 
     computePhaseVariabilityAtElectrode(phasesDatacube=Arg(htsArray), 
                                         electrodeRow=electrodeIndexInArray[1],
                                         electrodeCol=electrodeIndexInArray[2],
                                         neighborSize=neighborSize)
    phaseVariabilitiesFlipped <- phaseVariabilities
    colnames(phaseVariabilitiesFlipped) <- 
     colnames(phaseVariabilities)[ncol(phaseVariabilities):1]
    phaseVariabilitiesFlippedMelted <- 
     data.frame(melt(phaseVariabilitiesFlipped, varnames=c("y", "x")))
    p <- ggplot(data=phaseVariabilitiesFlippedMelted, 
                 mapping=aes(x=x, y=y, fill=value))
    p <- p + geom_tile()
    p <- p + scale_fill_gradient(low="yellow", high="red", limits=c(0,1))
    p <- p + ggtitle(sprintf("%d, min=%.02f, max%.02f", elecNumber, 
                             min(phaseVariabilities),
                             max(phaseVariabilities)))
    # p <- p + guides(fill=guide_legend(title="Variability"))
    print(p)

    browser()
}

processAll()
