getPhasesArray <- function(sessionLabel, htFilenamePattern, elecNumbers, 
                                         lowCutoff, highCutoff, order,
                                         zScore,
                                         fromTime, toTime, 
                                         desiredFrameRate,
                                         nrow, ncol) {

    firstElectrode <- TRUE

    for(elecNumber in elecNumbers) {
        show(sprintf("Processing electrode %d", elecNumber))
        res <- getGroupAndElecNumber(elecNumber=elecNumber)
        htFilename <- sprintf(htFilenamePattern, 
                               sessionLabel,
                               lowCutoff, highCutoff, order,
                               zScore,
                               res$groupNumber, res$elecNumber)
        electrodeIndexInArray <-
         getElectrodeIndexInArrayGGPlot(elecNumber=elecNumber)
        if(file.exists(htFilename)) {
            htLoadRes <- get(load(htFilename))
            if(length(htLoadRes$ht)>0) {
                if(firstElectrode) {
                    downsampleFactor <-
                     round(htLoadRes$ecogSampleRate/desiredFrameRate)
                    actualFrameRate <- htLoadRes$ecogSampleRate/downsampleFactor
                    phases <- downsamplPhases(x=Arg(htLoadRes$ht), 
                                               n=downsampleFactor)
                    times <- ((1:length(phases))-1)/actualFrameRate
                    samplesToSave <- which(fromTime<=times & times<=toTime)
                    timesToSave <- times[samplesToSave]
                    phasesArray <- array(NaN, 
                                            dim=c(nrow, ncol,
                                                   length(samplesToSave)))
                    firstElectrode <- FALSE
                } else {
#                     phases <- decimate(x=Arg(htLoadRes$ht), q=downsampleFactor)
                    phases <- downsamplePhases(phases=Arg(htLoadRes$ht), 
                                                n=downsampleFactor)
                }
                phasesArray[electrodeIndexInArray[1], 
                             electrodeIndexInArray[2],] <-
                 phases[samplesToSave]
            }
        } else {
            warning(sprintf("File %s does not exist", htFilename))
        }
    }
    return(list(phasesArray=phasesArray, actualFrameRate=actualFrameRate, 
                                         timesToSave=timesToSave))
}
