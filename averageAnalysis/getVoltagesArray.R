getVoltagesArray <- function(sessionLabel, voltagesFilenamePattern, 
                                           elecNumbers, 
                                           fromTime, toTime, 
                                           desiredFrameRate,
                                           nrow, ncol) {

    firstElectrode <- TRUE

    for(elecNumber in elecNumbers) {
        show(sprintf("Processing electrode %d", elecNumber))
        res <- getGroupAndElecNumber(elecNumber=elecNumber)
        voltagesFilename <- sprintf(voltagesFilenamePattern, 
                                       sessionLabel,
                                       res$groupNumber, res$elecNumber)
        electrodeIndexInArray <-
         getElectrodeIndexInArrayGGPlot(elecNumber=elecNumber)
        if(file.exists(voltagesFilename)) {
            readBuffer <- readVectorDoubleWithLengthHeader(filename=
                                                            voltagesFilename)
            voltages <- readBuffer[2:length(readBuffer)]
            if(length(voltages)>0) {
                if(firstElectrode) {
                    voltagesSampleRate <- readBuffer[1]
                    downsampleFactor <-
                     round(voltagesSampleRate/desiredFrameRate)
                    actualFrameRate <- voltagesSampleRate/downsampleFactor
                    voltages <- decimate(x=voltages, q=downsampleFactor)
                    times <- ((1:length(voltages))-1)/actualFrameRate
                    samplesToSave <- which(fromTime<=times & times<=toTime)
                    timesToSave <- times[samplesToSave]
                    voltagesArray <- array(0, dim=c(nrow, ncol,
                                                     length(samplesToSave)))
                    firstElectrode <- FALSE
                } else {
                    voltages <- decimate(x=voltages, q=downsampleFactor)
                }
                scaledVoltages <- scale(x=voltages[samplesToSave])
                voltagesArray[electrodeIndexInArray[1], 
                               electrodeIndexInArray[2],] <- scaledVoltages
            }
        } else {
            warning(sprintf("File %s does not exist", voltagesFilename))
        }
    }
    return(list(voltagesArray=voltagesArray, actualFrameRate=actualFrameRate, 
                                             timesToSave=timesToSave))
}
