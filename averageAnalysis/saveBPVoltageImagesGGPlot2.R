saveBPVoltageImagesGGPlot2 <- function(sessionLabel,
                                          bandpassedFilenamePattern,
                                          figDirnamePattern,
                                          figFilenamePattern,
                                          elecNumbers,
                                          lowCutoff, highCutoff, order,
                                          fromTime, toTime,
                                          desiredFrameRate,
                                          blurSigma=0,
                                          titlePattern, 
                                          nrow, ncol) {
    res <- getBPVoltagesArray(sessionLabel=sessionLabel, 
                               bandpassedFilenamePattern=
                                bandpassedFilenamePattern, 
                               elecNumbers=elecNumbers, 
                               lowCutoff=lowCutoff, highCutoff=highCutoff, 
                               order=order,
                               fromTime=fromTime, toTime=toTime, 
                               desiredFrameRate=desiredFrameRate,
                               nrow=nrow, ncol=ncol)
    if(blurSigma>0) {
        voltagesArray <- blurImageArray(imageArray=res$voltagesArray, 
                                               sigma=blurSigma)
    } else {
        voltagesArray <- res$voltagesArray
    }
    zlim <- quantile(x=voltagesArray, probs=c(.001, .999), na.rm=TRUE)
    voltagesArray[voltagesArray>zlim[2]] <- zlim[2]
    voltagesArray[voltagesArray<zlim[1]] <- zlim[1]
    figDirname <- sprintf(figDirnamePattern, sessionLabel, min(elecNumbers), max(elecNumbers), res$actualFrameRate, fromTime, toTime, lowCutoff, highCutoff, order, blurSigma)
    saveImagesGGPlot2FromArray(anArray=voltagesArray,
                         timesToSave=res$timesToSave,
                         zlim=range(voltagesArray, na.rm=TRUE),
                         scaleName="voltage",
                         figDirname=figDirname, 
                         figFilenamePattern=figFilenamePattern,
                         titlePattern=titlePattern)
}

