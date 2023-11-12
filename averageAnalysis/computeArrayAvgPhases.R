
computeArrayAvgPhases <- function(htFilenamePattern, 
                                   avgFromTime, avgToTime,
                                   groupNumbers, elecNumbers) {

    getElectrodeIndexInArray <- function(groupNumber, elecNumber) {
        absoluteElecNumber <- getAbsoluteElectrodeNumber(groupNumber=groupNumber, 
                                                          elecNumber=elecNumber)
        rowIndex <- 16-(absoluteElecNumber-1)%/%16
        colIndex <- (absoluteElecNumber-1)%%16+1
        indexInArray <- c(rowIndex, colIndex)
        return(indexInArray)
    }

    arraySideLength <- 16
    arrayAvgPhases <- matrix(NA, nrow=arraySideLength, ncol=arraySideLength)
    firstElectrode <- TRUE
    for(groupNumber in groupNumbers) {
        for(elecNumber in elecNumbers) {
            htFilename <- sprintf(htFilenamePattern, groupNumber, elecNumber)
            electrodeIndexInArray <-
             getElectrodeIndexInArray(groupNumber=groupNumber,
                                       elecNumber=elecNumber)
            if(file.exists(htFilename)) {
                loadRes <- get(load(htFilename))
                ht <- loadRes$ht
                if(firstElectrode) {
                    times <- loadRes$fromTime+(1:length(ht))/loadRes$ecogSampleRate
                    samplesToAvg <- which(avgFromTime<=times & times<=avgToTime)
                    firstElectrode <- FALSE
                }
                avgPhase <- computeAvgPhase(phases=Arg(ht[samplesToAvg]))
                arrayAvgPhases[electrodeIndexInArray[1],
                                electrodeIndexInArray[2]] <- avgPhase
            } else {
                arrayAvgPhases[electrodeIndexInArray] <- NaN
            }
        }
    }
    return(arrayAvgPhases)
}

computeAvgPhase <- function(phases) {
    unwrappedPhases <- unwrap(a=phases)
    avgPhase <- mean(unwrappedPhases)
    return(avgPhase)
}

