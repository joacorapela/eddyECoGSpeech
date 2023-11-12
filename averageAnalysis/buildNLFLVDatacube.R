buildNLFLVDatacube <- function(elecNumbers, fromTime, toTime, 
                                            nlflvFilenamePattern, sessionLabel,
                                            lowCutoff, highCutoff, 
                                            order, desiredSampleRate, 
                                            nrow, ncol) {
    firstElectrode <- TRUE

    for(elecNumber in elecNumbers) {
        show(sprintf("Processing electrode %d", elecNumber))
        res <- getGroupAndElecNumber(elecNumber=elecNumber)
        nlflvFilename <- sprintf(nlflvFilenamePattern, 
                               sessionLabel,
                               lowCutoff, highCutoff, order, 
                               res$groupNumber, res$elecNumber)
        if(file.exists(nlflvFilename)) {
            nlflvLoadRes <- get(load(nlflvFilename))
            if(length(nlflvLoadRes$nlflv)>0) {
                decimateFactor <- 
                 round(nlflvLoadRes$ecogSampleRate/desiredSampleRate)
                decimatedNLFLV <- decimate(x=nlflvLoadRes$nlflv, 
                                            q=decimateFactor)
                if(firstElectrode) {
                    actualSampleRate <- nlflvLoadRes$ecogSampleRate/
                                         decimateFactor
                    times <- ((1:length(decimatedNLFLV))-1)/actualSampleRate
                    samplesToSave <- which(fromTime<=times & times<=toTime)
                    timesToSave <- times[samplesToSave]
                    nlflvsArray <- array(0, dim=c(nrow, ncol, 
                                                     length(samplesToSave)))
                    firstElectrode <- FALSE
                }
                electrodeIndexInArray <- 
                 getElectrodeIndexInArrayGGPlot(elecNumber=elecNumber)
                nlflvsArray[electrodeIndexInArray[1], 
                             electrodeIndexInArray[2],] <-
                 decimatedNLFLV[samplesToSave]
            }
        } else {
            warning(sprintf("File %s does not exist", nlflvFilename))
        }
    }
    return(list(nlflvsArray=nlflvsArray, timesToSave=timesToSave, 
                                         actualSampleRate=actualSampleRate))
}
