
runningFunction <- function(x, fun, winSize) {
    # validIndices <- (winSize%/%2+1):(length(x)-winSize%/%2)
    validIndices <- 1:(length(x)-winSize+1)+as.integer(round(winSize/2))
    runningValues <- apply(X=embed(x=x, dimension=winSize), MARGIN=1, FUN=fun)
    return(list(runningValues=runningValues, validIndices=validIndices))
}
