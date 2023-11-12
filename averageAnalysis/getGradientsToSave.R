getGradientsToSave <- function(dxs, dys, elecNumbers, samplesToSave) {
    xsToSaveOneImage <- array(NaN, dim=length(elecNumbers))
    ysToSaveOneImage <- array(NaN, dim=length(elecNumbers))
    dxsToSave <- array(NaN, dim=c(length(elecNumbers), length(samplesToSave)))
    dysToSave <- array(NaN, dim=c(length(elecNumbers), length(samplesToSave)))
    for(i in 1:length(elecNumbers)) {
        elecNumber <- elecNumbers[i]
        electrodeIndexInArray <-
         getElectrodeIndexInArrayGGPlot(elecNumber=elecNumber)
        xsToSaveOneImage[i] <- electrodeIndexInArray[2]
        ysToSaveOneImage[i] <- electrodeIndexInArray[1]
    }
    # this subset is tricky
    # a[i,j,k] = a[i+(j-1)*dim(a)[1]+(k-1)*dim(a)[1]*dim(a)[2]]
    frameIndices <- ysToSaveOneImage+
                     (xsToSaveOneImage-1)*dim(dxs)[1]
    for(i in 1:length(samplesToSave)) {
        sampleToSave <- samplesToSave[i]
        dxsToSave[,i] <- dxs[,,sampleToSave][frameIndices]
        dysToSave[,i] <- dys[,,sampleToSave][frameIndices]
    }
    return(list(xsToSaveOneImage=xsToSaveOneImage, 
                 ysToSaveOneImage=ysToSaveOneImage, 
                 dxsToSave=dxsToSave, 
                 dysToSave=dysToSave))
}
