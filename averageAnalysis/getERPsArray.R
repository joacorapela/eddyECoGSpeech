getERPsArray <- function(sessionLabel, elecNumbers, 
                                       epochFromTime, epochToTime, 
                                       baselineFromTime, baselineToTime, 
                                       transcriptionSampleRate,
                                       minSeparation,
                                       desiredFrameRate,
                                       nrow, ncol,
                                       ecogFilenamePattern,
                                       transcriptionFilename,
                                       figDirnamePattern,
                                       figFilenamePattern) {
    firstElectrode <- TRUE
    for(elecNumber in elecNumbers) {
        show(sprintf("Processing electrode %d", elecNumber))
        res <- getGroupAndElecNumber(elecNumber=elecNumber)
        ecogFilename <- sprintf(ecogFilenamePattern, 
                                 sessionLabel,
                                 res$groupNumber, res$elecNumber)
        electrodeIndexInArray <-
         getElectrodeIndexInArrayGGPlot(elecNumber=elecNumber)
        if(file.exists(ecogFilename)) {
            res <- getECoGData(ecogFilename=ecogFilename)
            ecogData <- res$ecogData
            ecogSampleRate <- res$ecogSampleRate
            decimateFactor <- round(ecogSampleRate/desiredFrameRate)
            downsampledECoGData <- decimate(x=ecogData, q=decimateFactor)
            actualFrameRate <- ecogSampleRate/decimateFactor
            res <- getNonOverlappingEpochs(ecogData=downsampledECoGData, 
                                            ecogSampleRate=actualFrameRate,
                                            transcriptionFilename=
                                             transcriptionFilename, 
                                            transcriptionSampleRate=
                                             transcriptionSampleRate,
                                            epochFromTime=epochFromTime,
                                            epochToTime=epochToTime,
                                            minSeparation=minSeparation)
            epochs <- res$epochs
            if(!is.null(epochs)) {
                if(firstElectrode) {
                    epochTimes <- seq(from=epochFromTime, to=epochToTime, 
                                                          by=1/actualFrameRate)
                    baselineIndices <- which(baselineFromTime<=epochTimes & 
                                              epochTimes<=baselineToTime)
                    baselines <- colMeans(epochs[baselineIndices,])
                    baselineRemovedEochs <- t(epochs)-baselines
                    erp <- colMeans(baselineRemovedEochs)
                    erpsArray <- array(NaN, dim=c(nrow, ncol, length(erp)))
                    firstElectrode <- FALSE
                } else {
                    baselines <- colMeans(epochs[baselineIndices,])
                    baselineRemovedEochs <- t(epochs)-baselines
                    erp <- colMeans(baselineRemovedEochs)
                }
                erpsArray[electrodeIndexInArray[1], 
                           electrodeIndexInArray[2],] <- erp
            } else {
                warning(sprintf("Null epochs for %s", ecogFilename))
            }
        } else {
            warning(sprintf("File %s does not exist", ecogFilename))
        }
    }
    return(list(erpsArray=erpsArray, epochTimes=epochTimes, 
                                     actualFrameRate=actualFrameRate))
}
