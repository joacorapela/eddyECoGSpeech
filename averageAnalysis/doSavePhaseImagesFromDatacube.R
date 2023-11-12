
source("doLoadSources.R")

processAll <- function() {
    timesToSave <- c(200.0, 250.0, 300.0, 350.0)
    sessionLabel <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    zScore <- FALSE
    elecNumbers <- 1:256
    fromTime <- 0
    toTime <- 700
    nRow <- 16
    nCol <- 16
    htDatacubeFilenamePattern <- "results/%s/htDatacubeFilteredFrom%.02fTo%.02fOrder%02dZScored%dFromTime%.02fTo%.02f.%s"
    phasesImageFilenamePattern <- "results/%s/phasesImageFilteredFrom%.02fTo%.02fOrder%02dZScored%dTimeFrom%.02fTo%.02f.%s"
    metaDataPhasesImagesFilenamePattern <- "results/%s/metaDataPhasesImagesFilteredFrom%.02fTo%.02fOrder%02dZScored%d.%s"

    htDatacubeFilename <- sprintf(htDatacubeFilenamePattern, 
                                   sessionLabel,
                                   lowCutoff, highCutoff, order, zScore,
                                   fromTime, toTime, "RData")
    htDatacube <- get(load(htDatacubeFilename))
    htsArray <- htDatacube$htsArray
    # htsArray[which(is.na(htsArray))] <- 0
    phasesArray <- Arg(htDatacube$htsArray)
    imagesTimes <- htDatacube$timesToSave
    samplesToSave <-rep(x=NA, times=length(timesToSave))
    for(i in 1:length(timesToSave)) {
        samplesToSave[i] <- which.min(abs(timesToSave[i]-imagesTimes))
    }
    T <- mean(imagesTimes[2:length(imagesTimes)]-
               imagesTimes[1:(length(imagesTimes)-1)])
    for(i in 1:length(samplesToSave)) {
        phasesImage <- phasesArray[,,samplesToSave[i]]
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
        phasesImageAsSingle <- as.single(phasesImage)
        phasesImageAsSingleAsVector <- as.vector(phasesImageAsSingle)
        phasesImageFilename <- sprintf(phasesImageFilenamePattern, 
                                        sessionLabel,
                                        lowCutoff, highCutoff, order, zScore,
                                        imagesTimes[samplesToSave[i]], 
                                        imagesTimes[samplesToSave[i]]+T, "bin")
        con <- file(description=phasesImageFilename, open="wb")
        writeBin(object=phasesImageAsSingleAsVector, con=con, size=4)
        close(con)
    }
}

processAll()
