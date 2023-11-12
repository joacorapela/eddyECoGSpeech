getPhaseBinsCenters <- function(phaseBinsBreaks) {
    phaseBinsBreaks <- c(-pi, phaseBinsBreaks, pi)
    phaseBinsCenters <- c()
    for(i in 1:(length(phaseBinsBreaks)-1)) {
        phaseBinsCenters <- c(phaseBinsCenters, 
                              mean(phaseBinsBreaks[c(i,i+1)]))
    }
    return(phaseBinsCenters)
}

