getBPVoltagesArray <- function(sessionLabel, bandpassedFilenamePattern, 
                                             elecNumbers, 
                                             lowCutoff, highCutoff, order,
                                             fromTime, toTime, 
                                             desiredFrameRate,
                                             nrow, ncol) {

    firstElectrode <- TRUE

    for(elecNumber in elecNumbers) {
        show(sprintf("Processing electrode %d", elecNumber))
        res <- getGroupAndElecNumber(elecNumber=elecNumber)
        bandpassedFilename <- sprintf(bandpassedFilenamePattern, 
                                       sessionLabel,
                                       lowCutoff, highCutoff, order,
                                       res$groupNumber, res$elecNumber)
        electrodeIndexInArray <-
         getElectrodeIndexInArrayGGPlot(elecNumber=elecNumber)
        if(file.exists(bandpassedFilename)) {
            loadRes <- get(load(bandpassedFilename))
            if(length(loadRes$filteredECoG)>0) {
                if(firstElectrode) {
                    downsampleFactor <-
                     round(loadRes$ecogSampleRate/desiredFrameRate)
                    actualFrameRate <- loadRes$ecogSampleRate/downsampleFactor
                    voltages <- decimate(x=loadRes$filteredECoG, 
                                          q=downsampleFactor)
                    times <- ((1:length(voltages))-1)/actualFrameRate
                    samplesToSave <- which(fromTime<=times & times<=toTime)
                    timesToSave <- times[samplesToSave]
                    voltagesArray <- array(NaN, 
                                            dim=c(nrow, ncol,
                                                   length(samplesToSave)))
                    firstElectrode <- FALSE
                } else {
#                     voltages <- decimate(x=Mod(loadRes$filteredECoG), q=downsampleFactor)
                    voltages <- decimate(x=loadRes$filteredECoG,
                                          q=downsampleFactor)
                }
                voltagesArray[electrodeIndexInArray[1], 
                             electrodeIndexInArray[2],] <-
                 scale(voltages[samplesToSave])
            }
        } else {
            warning(sprintf("File %s does not exist", bandpassedFilename))
        }
    }
    return(list(voltagesArray=voltagesArray, actualFrameRate=actualFrameRate, 
                                             timesToSave=timesToSave))
}
