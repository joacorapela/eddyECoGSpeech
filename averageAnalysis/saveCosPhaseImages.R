saveCosPhaseImages <- function(sessionLabel,
                             htFilenamePattern,
                             figDirnamePattern,
                             figFilenamePattern,
                             elecNumbers,
                             lowCutoff, highCutoff,
                             fromTime, toTime,
                             desiredFrameRate,
                             titlePattern, 
                             nrow, ncol) {
    res <- getPhasesArray(sessionLabel=sessionLabel, 
                           htFilenamePattern=htFilenamePattern, 
                           elecNumbers=elecNumbers, 
                           lowCutoff=lowCutoff, highCutoff=highCutoff, 
                           fromTime=fromTime, toTime=toTime, 
                           desiredFrameRate=desiredFrameRate,
                           nrow=nrow, ncol=ncol)
    saveImagesFromArray(anArray=cos(res$phasesArray),
                         sessionLabe=sessionLabel,
                         actualFrameRate=res$actualFrameRate,
                         timesToSave=res$timesToSave,
                         lowCutoff=lowCutoff, highCutoff=highCutoff, 
                         fromTime=fromTime, toTime=toTime,
                         figDirnamePattern=figDirnamePattern, 
                         figFilenamePattern=figFilenamePattern,
                         scaleName=expression(cos(phi)),
                         titlePattern=titlePattern)
}

