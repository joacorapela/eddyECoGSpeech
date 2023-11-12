saveCVSBPVoltageImages <- function(cvs,
                                    sessionLabel,
                                    transcriptionSampleRate,
                                    transcriptionFilename,
                                    bandpassedFilenamePattern,
                                    figDirnamePattern,
                                    figFilenamePattern,
                                    elecNumbers,
                                    lowCutoff, highCutoff, order,
                                    fromTimeAfterCVSInitiation, 
                                    maxToTimeAfterCVSInitiation,
                                    pauseTimeBTWCVSs,
                                    pauseValueBTWCVSs,
                                    desiredFrameRate,
                                    blurSigma=0,
                                    titlePattern, 
                                    nrow, ncol) {
    res <- getCVSBPVoltagesArray(cvs=cvs,
                                  sessionLabel=sessionLabel, 
                                  transcriptionSampleRate=
                                   transcriptionSampleRate,
                                  transcriptionFilename=transcriptionFilename,
                                  bandpassedFilenamePattern=
                                   bandpassedFilenamePattern, 
                                  elecNumbers=elecNumbers, 
                                  lowCutoff=lowCutoff, highCutoff=highCutoff, 
                                  order=order,
                                  fromTimeAfterCVSInitiation=
                                   fromTimeAfterCVSInitiation, 
                                  maxToTimeAfterCVSInitiation=
                                   maxToTimeAfterCVSInitiation, 
                                  pauseTimeBTWCVSs=pauseTimeBTWCVSs,
                                  pauseValueBTWCVSs=pauseValueBTWCVSs,
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
    figDirname <- sprintf(figDirnamePattern, sessionLabel, cvs, min(elecNumbers), max(elecNumbers), res$actualFrameRate, fromTimeAfterCVSInitiation, maxToTimeAfterCVSInitiation, lowCutoff, highCutoff, order, blurSigma)
    saveImagesGGPlot2FromArray(anArray=voltagesArray,
                         timesToSave=res$timesToSave,
                         zlim=range(voltagesArray, na.rm=TRUE),
                         scaleName="voltage",
                         figDirname=figDirname, 
                         figFilenamePattern=figFilenamePattern,
                         titlePattern=titlePattern)
}

