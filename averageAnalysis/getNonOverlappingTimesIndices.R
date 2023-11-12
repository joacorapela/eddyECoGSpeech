
getNonOverlappingTimesIndices <- function(times, minSeparation) {
    lTimes <- length(times)

    nonOverlappingTimesIndices <- c()
    if((times[2]-times[1])>minSeparation) {
        nonOverlappingTimesIndices <- c(nonOverlappingTimesIndices, 1)
    }
    i <- 2
    while(i<lTimes-1) {
        if((times[i]-times[i-1])>minSeparation &&
            (times[i+1]-times[i])>minSeparation) {
            nonOverlappingTimesIndices <- c(nonOverlappingTimesIndices, i)
        }
        i <- i+1
    }
    if((times[lTimes]-times[lTimes-1])>minSeparation) {
        nonOverlappingTimesIndices <- c(nonOverlappingTimesIndices, lTimes)
    }
    return(nonOverlappingTimesIndices)
}

