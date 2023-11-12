getMeansCosPhaseInSquare <- function(sessionLabel, htFilenamePattern,
                                                   elecNumbers,
                                                   xMin, xMax, yMin, yMax,
                                                   lowCutoff, highCutoff, order,
                                                   fromTime, toTime,
                                                   desiredFrameRate,
                                                   nrow, ncol) {
    res <- getPhasesArray(sessionLabel=sessionLabel, 
                                   htFilenamePattern=htFilenamePattern, 
                                   elecNumbers=elecNumbers, 
                                   lowCutoff=lowCutoff, 
                                   highCutoff=highCutoff, 
                                   order=order,
                                   fromTime=fromTime, 
                                   toTime=toTime, 
                                   desiredFrameRate=desiredFrameRate,
                                   nrow=nrow, ncol=ncol)
    xIndicesMask <- xMin:xMax
    yIndicesMask <- yMin:yMax

    meansCosPhase <- array(NA, dim(res$phasesArray)[3])
    for(i in 1:length(meansCosPhase)) {
        meansCosPhase[i] <-mean(cos(res$phasesArray[yIndicesMask, xIndicesMask, i]))
    }
    return(list(meansCosPhase=meansCosPhase, times=res$timesToSave, 
                                             frameRate=res$actualFrameRate))
}

