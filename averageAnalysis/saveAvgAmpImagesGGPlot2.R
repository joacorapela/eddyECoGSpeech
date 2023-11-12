saveAvgAmpImagesGGPlot2 <- function(sessionLabel,
                                     fromTime,
                                     toTime,
                                     freqPhase,
                                     freqAmp,
                                     htFilenamePattern,
                                     pacFilenamePattern,
                                     figDirnamePattern,
                                     figFilenamePattern,
                                     elecNumbers,
                                     lowCutoff, highCutoff, order,
                                     zScore,
                                     desiredFrameRate,
                                     blurSigma=0,
                                     titlePattern, 
                                     nrow, ncol,
                                     scaleName=
                                     expression(atop("z-scored",
                                                     "high-"*gamma~"amp"))) {
    res <- getAvgAmpArray(sessionLabel=sessionLabel, 
                           freqPhase=freqPhase, 
                           freqAmp=freqAmp,
                           htFilenamePattern=htFilenamePattern, 
                           pacFilenamePattern=pacFilenamePattern,
                           elecNumbers=elecNumbers, 
                           lowCutoff=lowCutoff, highCutoff=highCutoff, 
                           order=order,
                           zScore=zScore,
                           fromTime=fromTime, toTime=toTime, 
                           desiredFrameRate=desiredFrameRate,
                           nrow=nrow, ncol=ncol)
    if(blurSigma>0) {
        avgAmpArray <- blurImageArray(imageArray=res$avgAmpArray, 
                                       sigma=blurSigma)
    } else {
        avgAmpArray <- res$avgAmpArray
    }
    figDirname <- sprintf(figDirnamePattern, sessionLabel, min(elecNumbers), max(elecNumbers), res$actualFrameRate, freqPhase, freqAmp, fromTime, toTime, lowCutoff, highCutoff, order, zScore, blurSigma)
    saveImagesGGPlot2FromArray(anArray=avgAmpArray,
                                   timesToSave=res$timesToSave,
                                   zlim=range(res$avgAmpArray, na.rm=TRUE),
                                   scaleName=scaleName,
                                   figDirname=figDirname, 
                                   figFilenamePattern=figFilenamePattern,
                                   titlePattern=titlePattern)
}

