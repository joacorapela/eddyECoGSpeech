saveCosPhaseImagesWithSquare <- function(sessionLabel,
                                          htFilenamePattern,
                                          figDirnamePattern,
                                          figFilenamePattern,
                                          elecNumbers,
                                          xMin, xMax, yMin, yMax,
                                          lowCutoff, highCutoff, order,
                                          fromTime, toTime,
                                          desiredFrameRate,
                                          titlePattern, 
                                          nrow, ncol) {
    res <- getPhasesArray(sessionLabel=sessionLabel, 
                           htFilenamePattern=htFilenamePattern, 
                           elecNumbers=elecNumbers, 
                           lowCutoff=lowCutoff, highCutoff=highCutoff, 
                           order=order,
                           fromTime=fromTime, toTime=toTime, 
                           desiredFrameRate=desiredFrameRate,
                           nrow=nrow, ncol=ncol)
    figDirname <- sprintf(figDirnamePattern, sessionLabel, res$actualFrameRate, fromTime, toTime, lowCutoff, highCutoff, order, xMin, xMax, yMin, yMax)
    saveImagesWithSquareFromArray(anArray=cos(res$phasesArray),
                         timesToSave=res$timesToSave,
                         xMin=xMin, xMax=xMax, yMin=yMin, yMax=yMax,
                         zlim=c(-1, 1),
                         scaleName=expression(cos(phi)),
                         figDirname=figDirname, 
                         figFilenamePattern=figFilenamePattern,
                         titlePattern=titlePattern)
}

