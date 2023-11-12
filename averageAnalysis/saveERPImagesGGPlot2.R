saveERPImagesGGPlot2 <- function(sessionLabel,
                                  elecNumbers,
                                  epochFromTime, epochToTime,
                                  baselineFromTime, baselineToTime,
                                  transcriptionSampleRate,
                                  minSeparation,
                                  desiredFrameRate,
                                  titlePattern,
                                  plotTimeInMinSec,
                                  nrow, ncol,
                                  ecogFilenamePattern,
                                  transcriptionFilename,
                                  figDirnamePattern,
                                  figFilenamePattern) {
    res <- getERPsArray(sessionLabel=sessionLabel, 
                         elecNumbers=elecNumbers, 
                         epochFromTime=epochFromTime, 
                         epochToTime=epochToTime, 
                         baselineFromTime=baselineFromTime, 
                         baselineToTime=baselineToTime, 
                         transcriptionSampleRate=transcriptionSampleRate,
                         minSeparation=minSeparation,
                         desiredFrameRate=desiredFrameRate,
                         nrow=nrow, ncol=ncol,
                         ecogFilenamePattern=ecogFilenamePattern,
                         transcriptionFilename=transcriptionFilename,
                         figDirnamePattern=figDirnamePattern,
                         figFilenamePattern=figFilenamePattern)
    erpsArray <- res$erpsArray
    zlim <- quantile(x=erpsArray, probs=c(.001, .999), na.rm=TRUE)
    erpsArray[erpsArray>zlim[2]] <- zlim[2]
    erpsArray[erpsArray<zlim[1]] <- zlim[1]
    figDirname <- sprintf(figDirnamePattern, sessionLabel, min(elecNumbers), max(elecNumbers), res$actualFrameRate, epochFromTime, epochToTime, baselineFromTime, baselineToTime)
    saveImagesGGPlot2FromArray(anArray=erpsArray,
                                timesToSave=res$epochTimes,
                                zlim=range(erpsArray, na.rm=TRUE),
                                scaleName="ERP",
                                titlePattern=titlePattern,
                                plotTimeInMinSec=plotTimeInMinSec,
                                figDirname=figDirname, 
                                figFilenamePattern=figFilenamePattern)
}

