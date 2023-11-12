
source("doLoadSources.R")

processAll <- function() {
    elecNumber <- 108
    neighborElecNumber <- 93
    neighborSize <- 1
    sessionLabel <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    elecNumbers <- 1:256
    fromTime <- 0
    toTime <- 700
    nrow <- 16
    ncol <- 16
    nlflvDatacubeFilenamePattern <- "results/%s/nlflvDatacubeFilteredFrom%.02fTo%.02fOrder%02dFromTime%.02fTo%.02f.RData"

    nlflvDatacubeFilename <- sprintf(nlflvDatacubeFilenamePattern, 
                                   sessionLabel,
                                   lowCutoff, highCutoff, order,
                                   fromTime, toTime)
    nlflvDatacubeLoadRes <- get(load(nlflvDatacubeFilename))
    nlflvsArray <- nlflvDatacubeLoadRes$nlflvsArray
    nlflvsArray[is.na(nlflvsArray)] <- 0.0
    electrodeIndexInArray <- 
     getElectrodeIndexInArrayGGPlot(elecNumber=elecNumber)
    phaseVariabilities <- 
     computePhaseVariabilityAtElectrode(phasesDatacube=nlflvsArray, 
                                         electrodeRow=electrodeIndexInArray[1],
                                         electrodeCol=electrodeIndexInArray[2],
                                         neighborSize=neighborSize)
    phaseVariabilitiesMelted <- 
     data.frame(melt(phaseVariabilities, varnames=c("y", "x")))
    p1 <- ggplot(data=phaseVariabilitiesMelted, 
                 mapping=aes(x=x, y=y, fill=value))
    p1 <- p1 + geom_tile()
    p1 <- p1 + scale_fill_gradient(low="yellow", high="red", limits=c(0,1))
    p1 <- p1 + ggtitle(sprintf("%d, min=%.02f, max%.02f", elecNumber, 
                             min(phaseVariabilities),
                             max(phaseVariabilities)))
    for(i in 1:nrow(phaseVariabilitiesMelted)) {
        p1 <- p1 + annotate("text", label=sprintf("%f", phaseVariabilitiesMelted[i,"value"]), x=phaseVariabilitiesMelted[i,"x"], y=phaseVariabilitiesMelted[i,"y"])
    }
    # p <- p + guides(fill=guide_legend(title="Variability"))
    # print(p)

    neighborElectrodeIndexInArray <- 
     getElectrodeIndexInArrayGGPlot(elecNumber=neighborElecNumber)
    neighborPhaseVariabilities <- 
     computePhaseVariabilityAtElectrode(phasesDatacube=nlflvsArray, 
                                         electrodeRow=neighborElectrodeIndexInArray[1],
                                         electrodeCol=neighborElectrodeIndexInArray[2],
                                         neighborSize=neighborSize)
    neighborPhaseVariabilitiesMelted <- 
     data.frame(melt(neighborPhaseVariabilities, varnames=c("y", "x")))
    p2 <- ggplot(data=neighborPhaseVariabilitiesMelted, 
                 mapping=aes(x=x, y=y, fill=value))
    p2 <- p2 + geom_tile()
    p2 <- p2 + scale_fill_gradient(low="yellow", high="red", limits=c(0,1))
    p2 <- p2 + ggtitle(sprintf("%d, min=%.02f, max%.02f", neighborElecNumber, 
                             min(neighborPhaseVariabilities),
                             max(neighborPhaseVariabilities)))
    for(i in 1:nrow(neighborPhaseVariabilitiesMelted)) {
        p2 <- p2 + annotate("text", label=sprintf("%f", neighborPhaseVariabilitiesMelted[i,"value"]), x=neighborPhaseVariabilitiesMelted[i,"x"], y=neighborPhaseVariabilitiesMelted[i,"y"])
    }
    # p2 <- p2 + guides(fill=guide_legend(title="Variability"))
    # print(p2)

    grid.arrange(p1, p2, ncol=1)

    browser()
}

processAll()
