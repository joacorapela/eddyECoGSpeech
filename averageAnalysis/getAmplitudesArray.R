getAmplitudesArray <- function(sessionLabel, htFilenamePattern, elecNumbers, 
                                         lowCutoff, highCutoff, order,
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
                               res$groupNumber, res$elecNumber)
        electrodeIndexInArray <-
         getElectrodeIndexInArrayGGPlot(elecNumber=elecNumber)
        if(file.exists(htFilename)) {
            htLoadRes <- get(load(htFilename))
            if(length(htLoadRes$ht)>0) {
                if(firstElectrode) {
                    downsampleFactor <-
                     round(htLoadRes$ecogSampleRate/desiredFrameRate)
                    # actualFrameRate <- htLoadRes$ecogSampleRate/downsampleFactor
                    # amplitudes <- decimate(x=Mod(htLoadRes$ht), 
                                            # q=downsampleFactor)
                    times <- ((1:length(amplitudes))-1)/actualFrameRate
                    samplesToSave <- which(fromTime<=times & times<=toTime)
                    timesToSave <- times[samplesToSave]
                    amplitudesArray <- array(NaN, 
                                            dim=c(nrow, ncol,
                                                   length(samplesToSave)))
                    firstElectrode <- FALSE
                } else {
#                     amplitudes <- decimate(x=Mod(htLoadRes$ht), q=downsampleFactor)
                    amplitudes <- decimate(x=Mod(htLoadRes$ht), 
                                            q=downsampleFactor)
                }
                amplitudesArray[electrodeIndexInArray[1], 
                             electrodeIndexInArray[2],] <-
                 amplitudes[samplesToSave]
            }
        } else {
            warning(sprintf("File %s does not exist", htFilename))
        }
    }
    return(list(amplitudesArray=amplitudesArray, actualFrameRate=actualFrameRate, 
                                         timesToSave=timesToSave))
}
