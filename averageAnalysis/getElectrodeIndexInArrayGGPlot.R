
getElectrodeIndexInArrayGGPlot <- function(elecNumber, ncol=16, nrow=16) {
    colIndex <- ncol-(elecNumber-1)%/%ncol
    rowIndex <- (elecNumber-1)%%nrow+1
    indexInArray <- c(rowIndex, colIndex)
    return(indexInArray)
}
