
computeArrayPhases <- function(htFilenamePattern, time, 
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
    arrayPhases <- matrix(NA, nrow=arraySideLength, ncol=arraySideLength)
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
                    sample <- which.min(abs(times-time))
                    firstElectrode <- FALSE
                }
                phase <- Arg(ht[sample])
                arrayPhases[electrodeIndexInArray[1], 
                             electrodeIndexInArray[2]] <- phase
            } else {
                arrayPhases[electrodeIndexInArray] <- NaN
            }
        }
    }
    # unwrappedPhases <- unwrapPhasesByColsThenRows(wrappedPhases=arrayPhases)
    unwrappedPhases <- cos(arrayPhases)
browser()
    return(unwrappedPhases)
}

unwrapPhasesByRows <- function(wrappedPhases) {
    unwrappedPhases <- matrix(NA, nrow=nrow(wrappedPhases), 
                                  ncol=ncol(wrappedPhases))
    for(i in 1:nrow(wrappedPhases)) {
        noNaNIndices <- which(!is.na(wrappedPhases[i,]))
        unwrappedPhases[i,noNaNIndices] <- unwrap(wrappedPhases[i,noNaNIndices])
    }
    return(unwrappedPhases)
}

unwrapPhasesByColsThenRows <- function(wrappedPhases) {
    unwrappedPhases <- matrix(NA, nrow=nrow(wrappedPhases), 
                                  ncol=ncol(wrappedPhases))
    for(i in 1:ncol(wrappedPhases)) {
        noNaNIndices <- which(!is.na(wrappedPhases[,i]))
        unwrappedPhases[noNaNIndices,i] <- unwrap(wrappedPhases[noNaNIndices,i])
    }
    for(i in 1:nrow(wrappedPhases)) {
        noNaNIndices <- which(!is.na(unwrappedPhases[i,]))
        unwrappedPhases[i,noNaNIndices] <- 
         unwrap(unwrappedPhases[i,noNaNIndices])
    }
    return(unwrappedPhases)
}

unwrapPhasesKay <- function(wrappedPhases) {
    meanWrappedPhases <- mean(as.vector(wrappedPhases), na.rm=TRUE)
    wrappedPhasesMinusMeanWrapped <- atan(tan(wrappedPhases-meanWrappedPhases))
    unwrappedPhases <- wrappedPhasesMinusMeanWrapped+meanWrappedPhases
    return(unwrappedPhases)
}

