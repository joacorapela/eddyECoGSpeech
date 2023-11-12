getPeakIndices <- function(x) {
    n <- length(x)
    indices <- which(x[(1:(n-2))]<x[(2:(n-1))] & x[(2:(n-1))]>x[(3:n)])
    return(indices)
}
