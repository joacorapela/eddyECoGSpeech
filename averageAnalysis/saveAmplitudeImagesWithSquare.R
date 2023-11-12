saveAmplitudeImagesWithSquare <- function(sessionLabel,
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
    res <- getAmplitudesArray(sessionLabel=sessionLabel, 
                           htFilenamePattern=htFilenamePattern, 
                           elecNumbers=elecNumbers, 
                           lowCutoff=lowCutoff, highCutoff=highCutoff, order=order,
                           fromTime=fromTime, toTime=toTime, 
                           desiredFrameRate=desiredFrameRate,
                           nrow=nrow, ncol=ncol)
    amplitudesArrayInDB <- 20*log10(res$amplitudesArray)
    zlim <- quantile(x=amplitudesArrayInDB, probs=c(.001, .999), na.rm=TRUE)
    amplitudesArrayInDB[amplitudesArrayInDB>zlim[2]] <- zlim[2]
    amplitudesArrayInDB[amplitudesArrayInDB<zlim[1]] <- zlim[1]
    figDirname <- sprintf(figDirnamePattern, sessionLabel, res$actualFrameRate, fromTime, toTime, lowCutoff, highCutoff, order, xMin, xMax, yMin, yMax)
    saveImagesWithSquareFromArray(anArray=amplitudesArrayInDB,
                         timesToSave=res$timesToSave,
                         xMin=xMin, xMax=xMax, yMin=yMin, yMax=yMax,
                         zlim=zlim,
                         scaleName="amplitude (dB)",
                         figDirname=figDirname, 
                         figFilenamePattern=figFilenamePattern,
                         titlePattern=titlePattern)
}

