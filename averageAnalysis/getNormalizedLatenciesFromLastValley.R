getNormalizedLatenciesFromLastValley <- function(x) {
    valleyIndices <- getPeakIndices(x=-x)
    nlflv <- rep(NA, times=length(x))
    for(i in 1:(length(valleyIndices)-1)) {
        lastValleyIndex <- valleyIndices[i]
        nextValleyIndex <- valleyIndices[i+1]
        currentCycleIndices <- lastValleyIndex:nextValleyIndex
        nlflv[currentCycleIndices] <- 2*pi*(currentCycleIndices-lastValleyIndex)/(nextValleyIndex-lastValleyIndex)-pi
    }
    if(valleyIndices[1]>1) {
        firstValleyIndex <- valleyIndices[1]
        secondValleyIndex <- valleyIndices[2]
        currentCycleIndices <- 1:(firstValleyIndex-1)
        nlflv[currentCycleIndices] <- pi-2*pi*(firstValleyIndex-currentCycleIndices)/(secondValleyIndex-firstValleyIndex)
    }
    if(valleyIndices[length(valleyIndices)]<length(x)) {
        lastValleyIndex <- valleyIndices[length(valleyIndices)]
        secondToLastValleyIndex <- valleyIndices[length(valleyIndices)-1]
        currentCycleIndices <- (lastValleyIndex+1):length(x)
        nlflv[currentCycleIndices] <- 2*pi*(currentCycleIndices-lastValleyIndex)/(lastValleyIndex-secondToLastValleyIndex)-pi
    }
    return(nlflv)
}
