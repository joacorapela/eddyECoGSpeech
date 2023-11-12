getAvgAmpArray <- function(sessionLabel, freqPhase, freqAmp,
                                         htFilenamePattern, 
                                         pacFilenamePattern,
                                         elecNumbers, 
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
                               lowCutoff, highCutoff, order, zScore,
                               res$groupNumber, res$elecNumber)
        pacFilename <- sprintf(pacFilenamePattern, sessionLabel, 
                                                   res$groupNumber, 
                                                   res$elecNumber,
                                                   fromTime, toTime,
                                                   freqPhase, freqAmp)
        electrodeIndexInArray <-
         getElectrodeIndexInArrayGGPlot(elecNumber=elecNumber)
        if(file.exists(htFilename) && file.exists(pacFilename)) {
            htLoadRes <- get(load(htFilename))
            pacLoadRes <- get(load(pacFilename))
            phaseBinsBreaks <- pacLoadRes$phaseBinsBreaks
            amplitudesBinnedByPhase <- pacLoadRes$allAmplitudesBinnedByPhase
            if(length(htLoadRes$ht)>0 && length(phaseBinsBreaks)>0 
                                      && length(amplitudesBinnedByPhase)>0) {
                if(firstElectrode) {
                    downsampleFactor <-
                     round(htLoadRes$ecogSampleRate/desiredFrameRate)
                    actualFrameRate <- htLoadRes$ecogSampleRate/downsampleFactor
                    phases <- decimate(x=Arg(htLoadRes$ht), 
                                        q=downsampleFactor)
                    times <- ((1:length(phases))-1)/actualFrameRate
                    samplesToSave <- which(fromTime<=times & times<=toTime)
                    timesToSave <- times[samplesToSave]
                    avgAmpArray <- array(NaN, 
                                            dim=c(nrow, ncol,
                                                   length(samplesToSave)))
                    firstElectrode <- FALSE
                } else {
                    phases <- decimate(x=Arg(htLoadRes$ht), 
                                        q=downsampleFactor)
                }
                avgAmplitudes <- 
                 getPACAmplitudesForPhases(phases=phases[samplesToSave], 
                                                  phaseBinsBreaks=
                                                   phaseBinsBreaks,
                                                  amplitudesBinnedByPhase=
                                                   amplitudesBinnedByPhase)
                avgAmpArray[electrodeIndexInArray[1], 
                             electrodeIndexInArray[2],] <- scale(avgAmplitudes)
            }
        } else {
            warning(sprintf("File %s or %s do not exist", htFilename, pacFilename))
        }
    }
    return(list(avgAmpArray=avgAmpArray, actualFrameRate=actualFrameRate, 
                                         timesToSave=timesToSave))
}
