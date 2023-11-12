
source("doLoadSources.R")

removeNAsFromPhasesArray <- function(phasesArray) {
    phasesArrayNoNAs <- phasesArray
    nRow <- dim(phasesArrayNoNAs)[1]
    nCol <- dim(phasesArrayNoNAs)[3]
    for(i in 1:dim(phasesArrayNoNAs)[3]) {
        phasesImage <- phasesArrayNoNAs[,,i]
        naIndices <- which(is.na(phasesImage))
        if(length(naIndices)>0) {
            if(length(naIndices)==1) {
                phasesImage[naIndices] <-
                 mean(getNeighborsOfPixel(vectorizedImage=phasesImage, 
                                           pixelIndex=naIndices, 
                                           nRow=nRow, nCol=nCol))
            } else {
                stop("More than one missing value found in phase image")
            }
        }
        phasesArrayNoNAs[,,i] <- phasesImage
    }
    return(phasesArrayNoNAs)
}

processAll <- function() {
    sessionLabel <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    zScore <- FALSE
    elecNumbers <- 1:256
    fromTime <- 0
    toTime <- 700
    nrow <- 16
    ncol <- 16
    htDatacubeFilenamePattern <- "results/%s/htDatacubeFilteredFrom%.02fTo%.02fOrder%02dZScored%dFromTime%.02fTo%.02f.%s"
    phasesDatacubeFilenamePattern <- "results/%s/phasesDatacubeFilteredFrom%.02fTo%.02fOrder%02dZScored%dFromTime%.02fTo%.02f.%s"
    metaDataPhasesDatacubeFilenamePattern <- "results/%s/metaDataPhasesDatacubeFilteredFrom%.02fTo%.02fOrder%02dZScored%dFromTime%.02fTo%.02f.%s"

    htDatacubeFilename <- sprintf(htDatacubeFilenamePattern, 
                                   sessionLabel,
                                   lowCutoff, highCutoff, order, zScore,
                                   fromTime, toTime, "RData")
    htDataCube <- get(load(htDatacubeFilename))
    htsArray <- htDataCube$htsArray
    # htsArray[which(is.na(htsArray))] <- 0
    phasesArray <- Arg(htDatacube$htsArray)
    phasesArray <- removeNAsFromPhasesArray(phasesArray=phasesArray)
    phasesArray <- as.single(phasesArray)
    phasesArray <- as.vector(phasesArray)
    phasesDatacubeFilename <- sprintf(phasesDatacubeFilenamePattern, 
                                       sessionLabel,
                                       lowCutoff, highCutoff, order, zScore,
                                       fromTime, toTime, "bin")
    con <- file(description=phasesDatacubeFilename, open="wb")
    writeBin(object=phasesArray, con=con, size=4)
    close(con)
for(i in 0:19) {
    show(phasesArray[length(phasesArray)-i])
}
    metaDataPhasesDatacubeFilename <- 
     sprintf(metaDataPhasesDatacubeFilenamePattern, sessionLabel, lowCutoff, 
                                                    highCutoff, order, zScore, 
                                                    fromTime, toTime, "txt")
    con <- file(metaDataPhasesDatacubeFilename, "w")
    writeLines(sprintf("%d", dim(htDatacube$htsArray)), con=con)
    writeLines(sprintf("%f", htDataCube$sampleRate), con=con)
    close(con)
}

processAll()
