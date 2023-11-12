
source("doLoadSources.R")

computeResidueMap <- function(phasesArray) {

    continuityPhaseMap <- matrix(0, ncol=ncol(phasesArray), 
                                 nrow=nrow(phasesArray))

    for(m in 1:(nrow(continuityPhaseMap)-1)) {
        for(n in 1:(ncol(continuityPhaseMap)-1)) {
            cp <- round((phasesArray[m+1,n  ]-phasesArray[m  ,n  ])/2*pi)+
                  round((phasesArray[m+1,n+1]-phasesArray[m+1,n  ])/2*pi)+
                  round((phasesArray[m  ,n+1]-phasesArray[m+1,n+1])/2*pi)+
                  round((phasesArray[m  ,n  ]-phasesArray[m  ,n+1])/2*pi)
            continuityPhaseMap[m, n] <- as.integer(cp)
        }
    }
    return(continuityPhaseMap)
}

processAll <- function() {
    require(plotly)

    plotTime <- 500.0
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
    surfaceOpacity <- 1.0
    legendLabel <- "Phase\nContinuity"
    htDatacubeFilenamePattern <- "results/%s/htDatacubeFilteredFrom%.02fTo%.02fOrder%02dZScored%dFromTime%.02fTo%.02f.RData"

    toTime <- fromTime + duration
    htDatacubeFilename <- sprintf(htDatacubeFilenamePattern, 
                                   sessionLabel,
                                   lowCutoff, highCutoff, order, zScore,
                                   fromTime, toTime)
    htDatacube <- get(load(htDatacubeFilename))
    plotSample <- which.min(abs(plotTime-htDatacube$timesToSave))
    phasesArray <- Arg(htDatacube$htsArray[,,plotSample])
    residueMap <- computeResidueMap(phasesArray=phasesArray)
    positiveResidueIndices <- which(residueMap==1)
    nRowPhasesArray <- nrow(phasesArray)
    nColPhasesArray <- ncol(phasesArray)
    positiveResidueIndices <- which(residueMap==1)
    colsPositiveResidueIndices <- 
     ((positiveResidueIndices-1)%/%nRowPhasesArray)+1
    rowsPositiveResidueIndices <-
     ((positiveResidueIndices-1)%%nRowPhasesArray)+1
    negativeResidueIndices <- which(residueMap==-1)
    colsNegativeResidueIndices <- 
     ((negativeResidueIndices-1)%/%nRowPhasesArray)+1
    rowsNegativeResidueIndices <-
     ((negativeResidueIndices-1)%%nRowPhasesArray)+1

    positiveResiduesDf <- data.frame(y=rowsPositiveResidueIndices-1,
                                      x=colsPositiveResidueIndices-1,
                                      z=phasesArray[rowsPositiveResidueIndices+
                                                     (colsPositiveResidueIndices-1)*nRowPhasesArray])
    negativeResiduesDf <- data.frame(y=rowsNegativeResidueIndices-1,
                                      x=colsNegativeResidueIndices-1,
                                      z=phasesArray[rowsNegativeResidueIndices+
                                                     (colsNegativeResidueIndices-1)*nRowPhasesArray])
    p <- plot_ly(z=~phasesArray, type="surface", opacity=surfaceOpacity) %>%
     add_trace(data=positiveResiduesDf, x=~x, y=~y, z=~z, 
                                        mode="markers", type="scatter3d", 
                                        marker=list(size=3, color="red",
                                                            symbol=104)) %>%
     add_trace(data=negativeResiduesDf, x=~x, y=~y, z=~z, 
                                        mode="markers", type="scatter3d", 
                                        marker=list(size=3, color="blue",
                                                            symbol=104))
    print(p)
#     mCPM <- melt(cpm)
#     p <- ggplot(mCPM, aes(x=X2, y=X1, fill=factor(value))) + geom_tile()
#     p <- p + scale_fill_discrete(name=legendLabel)
#     p <- p + xlab(xlab)
#     p <- p + ylab(ylab)

#     print(p)

browser()

}

processAll()

rm(processAll)
