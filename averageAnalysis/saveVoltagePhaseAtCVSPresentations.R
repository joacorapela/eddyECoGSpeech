saveVoltagePhaseAtCVSPresentationsImagesGGPlot2 <- function(sessionLabel,
                                          bandpassedFilenamePattern,
                                          figDirnamePattern,
                                          figFilenamePattern,
                                          elecNumbers,
                                          lowCutoff, highCutoff, order,
                                          fromTime, toTime,
                                          transcriptionFilename,
                                          transcriptionSampleRate,
                                          peaksLowpassCutoff,
                                          peaksLowpassTransitionWidth,
                                          peaksLowpassRipple,
                                          titlePattern, 
                                          plotLowpass,
                                          nrow, ncol) {
    res <- getVoltagePhasesAtCVSPresentationsArray(sessionLabel=sessionLabel, 
                           bandpassedFilenamePattern=bandpassedFilenamePattern, 
                           elecNumbers=elecNumbers, 
                           lowCutoff=lowCutoff, highCutoff=highCutoff, 
                           order=order,
                           fromTime=fromTime, toTime=toTime, 
                           transcriptionFilename=transcriptionFilename,
                           transcriptionSampleRate=transcriptionSampleRate,
                           peaksLowpassCutoff=peaksLowpassCutoff,
                           peaksLowpassTransitionWidth=peaksLowpassTransitionWidth,
                           peaksLowpassRipple=peaksLowpassRipple,
                           plotLowpass=plotLowpass,
                           nrow=nrow, ncol=ncol)
    figDirname <- sprintf(figDirnamePattern, sessionLabel, min(elecNumbers), max(elecNumbers), fromTime, toTime, lowCutoff, highCutoff, order)
    saveImagesGGPlot2FromArray(anArray=res$phasesArray,
                         timesToSave=res$timesToSave,
                         zlim=pi*c(-1, 1),
                         scaleName=expression(phi),
                         figDirname=figDirname, 
                         figFilenamePattern=figFilenamePattern,
                         titlePattern=titlePattern)
}

