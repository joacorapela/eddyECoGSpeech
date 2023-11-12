
getNeighborsOfPixel <- function(vectorizedImage, pixelIndex, nRow, nCol) {
    # vectorizedImage stored by columns starting at bottom left
    anImage <- matrix(vectorizedImage, nrow=nRow)
    pixelRow <- (pixelIndex-1)%%nRow+1
    pixelCol <- (pixelIndex-1)%/%nRow+1
    if(pixelRow==1 && pixelCol>1 && pixelCol<nCol) {
        neighborsOfPixel <- 
         c(anImage[2,pixelCol-1], anImage[2,pixelCol], anImage[2,pixelCol+1],
           anImage[1,pixelCol-1],                      anImage[2,pixelCol+1])
        return(neighborsOfPixel)
    }
    stop("Case not implements in getNeighborsOfPixel")
}

