saveCVSGradientImages <- function(cvs, sessionLabel, dxs, dys, times,
                                       elecNumbers,
                                       fromTimeAfterCVSInitiation, 
                                       maxToTimeAfterCVSInitiation,
                                       lowCutoff, highCutoff, order,
                                       zScore,
                                       sampleRate,
                                       pauseTimeBTWCVSs,
                                       titlePattern,
                                       arrowLength, arrowAngle, 
                                       vectorCol,
                                       nrow, ncol,
                                       anXlim=NA, anYlim=NA,
                                       transcriptionFilename,
                                       transcriptionSampleRate,
                                       figDirnamePattern,
                                       figFilenamePattern) {
    infoInit <- getInfoCVSsInitiations(transcriptionFilename=
                                         transcriptionFilename,
                                        transcriptionSampleRate=
                                         transcriptionSampleRate,
                                        ecogSampleRate=sampleRate)
    cvsSamplesRes <- getCVSSamples(cvs=cvs, 
                                    cvss=infoInit$cvSyllables,
                                    cvssInitiationSamples=
                                     infoInit$samples,
                                    fromTimeAfterCVSInitiation=
                                     fromTimeAfterCVSInitiation,
                                    maxToTimeAfterCVSInitiation=
                                     maxToTimeAfterCVSInitiation,
                                    sampleRate=sampleRate)
    resCVSGradients <- getCVSGradientsToSave(dxs=dxs, dys=dys, 
                                              cvsSamplesByRepeat=
                                               cvsSamplesRes$byRepeat, 
                                              cvsSamplesAll=
                                               cvsSamplesRes$all,
                                              elecNumbers=elecNumbers,
                                              sampleRate=sampleRate,
                                              pauseTimeBTWCVSs=pauseTimeBTWCVSs)
    figDirname <- sprintf(figDirnamePattern, sessionLabel, cvs, min(elecNumbers), max(elecNumbers), sampleRate, fromTimeAfterCVSInitiation, maxToTimeAfterCVSInitiation, lowCutoff, highCutoff, order, zScore)
    saveVectorFieldImages(xsOneImage=resCVSGradients$xsToSaveOneImage, 
                           ysOneImage=resCVSGradients$ysToSaveOneImage, 
                           dxs=resCVSGradients$dxsToSave, 
                           dys=resCVSGradients$dysToSave,
                           elecNumbers=elecNumbers,
                           times=resCVSGradients$timesToSave,
                           arrowLength=arrowLength, 
                           arrowAngle=arrowAngle,
                           vectorCol=vectorCol,
                           figDirname=figDirname, 
                           figFilenamePattern=figFilenamePattern,
                           titlePattern=titlePattern,
                           ncol=ncol, nrow=nrow,
                           anXlim=anXlim, anYlim=anYlim)
}

