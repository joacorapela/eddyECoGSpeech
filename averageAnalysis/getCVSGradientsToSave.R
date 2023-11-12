getCVSGradientsToSave <- function(dxs, dys, cvsSamplesByRepeat, cvsSamplesAll, 
                                       elecNumbers, sampleRate, 
                                       pauseTimeBTWCVSs, pauseValueBTWCVSs) {
    xsToSaveOneImage <- array(NaN, dim=length(elecNumbers))
    ysToSaveOneImage <- array(NaN, dim=length(elecNumbers))
    for(i in 1:length(elecNumbers)) {
        elecNumber <- elecNumbers[i]
        electrodeIndexInArray <-
         getElectrodeIndexInArrayGGPlot(elecNumber=elecNumber)
        xsToSaveOneImage[i] <- electrodeIndexInArray[2]
        ysToSaveOneImage[i] <- electrodeIndexInArray[1]
    }
    # this subset is tricky
    # a[i,j] = a[i+(j-1)*dim(a)[1]
    frameIndices <- ysToSaveOneImage+(xsToSaveOneImage-1)*dim(dxs)[1]
    pauseNFramesBTWCVSs <- pauseTimeBTWCVSs*sampleRate
    nRepeatsCVS <- length(cvsSamplesByRepeat)
    timesToSave <- c()
    for(i in 1:nRepeatsCVS) {
        timesToSave <- c(timesToSave, cvsSamplesByRepeat[[i]]/sampleRate, 
                                      rep(0, times=pauseNFramesBTWCVSs))
    }
    totalNCVSSamples <- length(cvsSamplesAll)
    totalNFrames <- totalNCVSSamples+nRepeatsCVS*pauseNFramesBTWCVSs
    dxsToSave <- array(NaN, dim=c(length(elecNumbers), totalNFrames))
    dysToSave <- array(NaN, dim=c(length(elecNumbers), totalNFrames))
    pauseFrame <- rep(0, times=length(elecNumbers))
    colIndex <- 1
    for(i in 1:nRepeatsCVS) {
browser()
        for(j in 1:length(cvsSamplesByRepeat[[i]])) {
            cvsSampleRepiTimej <- cvsSamplesByRepeat[[i]][j]
            dxsToSave[,colIndex] <- dxs[,,cvsSampleRepiTimej][frameIndices]
            dysToSave[,colIndex] <- dys[,,cvsSampleRepiTimej][frameIndices]
            colIndex <- colIndex + 1
        }
        for(j in 1:pauseNFramesBTWCVSs) {
            dxsToSave[,colIndex] <- pauseFrame
            dysToSave[,colIndex] <- pauseFrame
            colIndex <- colIndex + 1
        }
    }
    return(list(xsToSaveOneImage=xsToSaveOneImage, 
                 ysToSaveOneImage=ysToSaveOneImage, 
                 dxsToSave=dxsToSave, 
                 dysToSave=dysToSave,
                 timesToSave=timesToSave))
}
