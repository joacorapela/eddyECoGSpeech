saveGradientImages <- function(sessionLabel, dxs, dys, times,
                                             figDirnamePattern,
                                             figFilenamePattern,
                                             elecNumbers,
                                             fromTime, toTime,
                                             lowCutoff, highCutoff, order,
                                             zScore,
                                             sampleRate,
                                             titlePattern,
                                             arrowLength, arrowAngle, 
                                             vectorCol,
                                             nrow, ncol,
                                             anXlim=NA, anYlim=NA) {
    samplesToSave <- which(fromTime<=times & times<=toTime)
# figDirname <- sprintf(figDirnamePattern, sessionLabel, min(elecNumbers), max(elecNumbers), sampleRate, fromTime, toTime, lowCutoff, highCutoff, order)
# if(!file.exists(figDirname)) {
#     dir.create(figDirname)
# }
# resMeshgrid <- meshgrid(x=1:dim(resGradWithTimes$dx)[2], 
#                          y=1:dim(resGradWithTimes$dx)[1])
# for(i in 1:length(samplesToSave)) {
# figFilename <- sprintf(figFilenamePattern, i)
# figAbsFilename <- sprintf("%s/%s", figDirname, figFilename)
# scale <- 1
# p <- getPlotVectorField(x=as.vector(resMeshgrid$Y),
#                          y=as.vector(resMeshgrid$X),
#                          u=scale*as.vector(resGradWithTimes$dx[,,i]),
#                          v=scale*as.vector(resGradWithTimes$dy[,,i]),
#                          arrowLength=arrowLength)
# ggsave(plot=p, filename=figAbsFilename)
# }
# return()

    timesToSave <- times[samplesToSave]
    resGradients <- getGradientsToSave(dxs=dxs, 
                               dys=dys, 
                               elecNumbers=elecNumbers, 
                               samplesToSave=samplesToSave)
    figDirname <- sprintf(figDirnamePattern, sessionLabel, min(elecNumbers), max(elecNumbers), sampleRate, fromTime, toTime, lowCutoff, highCutoff, order, zScore)
    saveVectorFieldImages(xsOneImage=resGradients$xsToSaveOneImage, 
                           ysOneImage=resGradients$ysToSaveOneImage, 
                           dxs=resGradients$dxsToSave, 
                           dys=resGradients$dysToSave,
                           elecNumbers=elecNumbers,
                           times=timesToSave,
                           arrowLength=arrowLength, 
                           arrowAngle=arrowAngle,
                           vectorCol=vectorCol,
                           figDirname=figDirname, 
                           figFilenamePattern=figFilenamePattern,
                           titlePattern=titlePattern,
                           ncol=ncol, nrow=nrow,
                           anXlim=anXlim, anYlim=anYlim)
}

