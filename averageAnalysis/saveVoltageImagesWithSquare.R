saveVoltageImagesWithSquare <- function(sessionLabel, 
                                         voltagesFilenamePattern, 
                                         figDirnamePattern,
                                         figFilenamePattern,
                                         elecNumbers,
                                         xMin, xMax, yMin, yMax,
                                         fromTime, toTime,
                                         desiredFrameRate,
                                         titlePattern, 
                                         nrow, ncol) {
    res <- getVoltagesArray(sessionLabel=sessionLabel, 
                             voltagesFilenamePattern=voltagesFilenamePattern, 
                             elecNumbers=elecNumbers, 
                             fromTime=fromTime, toTime=toTime, 
                             desiredFrameRate=desiredFrameRate,
                             nrow=nrow, ncol=ncol)
    voltagesArray <- res$voltagesArray
    zlim <- quantile(x=voltagesArray, probs=c(.001, .999), na.rm=TRUE)
    voltagesArray[voltagesArray>zlim[2]] <- zlim[2]
    voltagesArray[voltagesArray<zlim[1]] <- zlim[1]
    figDirname <- sprintf(figDirnamePattern, sessionLabel, res$actualFrameRate, fromTime, toTime, xMin, xMax, yMin, yMax)
    saveImagesWithSquareFromArray(anArray=voltagesArray,
                                   timesToSave=res$timesToSave,
                                   xMin=xMin, xMax=xMax, yMin=yMin, yMax=yMax,
                                   zlim=range(voltagesArray, na.rm=TRUE),
                                   scaleName="voltage",
                                   figDirname=figDirname, 
                                   figFilenamePattern=figFilenamePattern,
                                   titlePattern=titlePattern)
}

