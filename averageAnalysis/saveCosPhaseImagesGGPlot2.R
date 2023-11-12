saveCosPhaseImagesGGPlot2 <- function(sessionLabel,
                                          htFilenamePattern,
                                          figDirnamePattern,
                                          figFilenamePattern,
                                          elecNumbers,
                                          lowCutoff, highCutoff, order,
                                          zScore,
                                          fromTime, toTime,
                                          desiredFrameRate,
                                          titlePattern, 
                                          nrow, ncol) {
    res <- getPhasesArray(sessionLabel=sessionLabel, 
                           htFilenamePattern=htFilenamePattern, 
                           elecNumbers=elecNumbers, 
                           lowCutoff=lowCutoff, highCutoff=highCutoff, 
                           order=order,
                           zScore=zScore,
                           fromTime=fromTime, toTime=toTime, 
                           desiredFrameRate=desiredFrameRate,
                           nrow=nrow, ncol=ncol)
    figDirname <- sprintf(figDirnamePattern, sessionLabel, min(elecNumbers), max(elecNumbers), res$actualFrameRate, fromTime, toTime, lowCutoff, highCutoff, order, zScore)
    saveImagesGGPlot2FromArray(anArray=cos(res$phasesArray),
                         timesToSave=res$timesToSave,
                         zlim=c(-1, 1),
                         scaleName=expression(cos(phi)),
                         figDirname=figDirname, 
                         figFilenamePattern=figFilenamePattern,
                         titlePattern=titlePattern)
}

