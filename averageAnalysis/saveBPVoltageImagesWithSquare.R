saveBPVoltageImagesWithSquare <- function(sessionLabel,
                                          bandpassedFilenamePattern,
                                          figDirnamePattern,
                                          figFilenamePattern,
                                          elecNumbers,
                                          xMin, xMax, yMin, yMax,
                                          lowCutoff, highCutoff, order,
                                          fromTime, toTime,
                                          desiredFrameRate,
                                          titlePattern, 
                                          nrow, ncol) {
    res <- getBPVoltagesArray(sessionLabel=sessionLabel, 
                             bandpassedFilenamePattern=bandpassedFilenamePattern, 
                             elecNumbers=elecNumbers, 
                             lowCutoff=lowCutoff, highCutoff=highCutoff, order=order,
                             fromTime=fromTime, toTime=toTime, 
                             desiredFrameRate=desiredFrameRate,
                             nrow=nrow, ncol=ncol)
    voltagesArray <- res$voltagesArray
    zlim <- quantile(x=voltagesArray, probs=c(.001, .999), na.rm=TRUE)
    voltagesArray[voltagesArray>zlim[2]] <- zlim[2]
    voltagesArray[voltagesArray<zlim[1]] <- zlim[1]
    figDirname <- sprintf(figDirnamePattern, sessionLabel, min(elecNumbers), max(elecNumbers), res$actualFrameRate, fromTime, toTime, lowCutoff, highCutoff, order, xMin, xMax, yMin, yMax)
    saveImagesWithSquareFromArray(anArray=voltagesArray,
                         timesToSave=res$timesToSave,
                         xMin=xMin, xMax=xMax, yMin=yMin, yMax=yMax,
                         zlim=range(voltagesArray, na.rm=TRUE),
                         scaleName="voltage",
                         figDirname=figDirname, 
                         figFilenamePattern=figFilenamePattern,
                         titlePattern=titlePattern)
}

