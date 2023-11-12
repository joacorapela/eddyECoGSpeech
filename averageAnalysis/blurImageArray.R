blurImageArray <- function(imageArray, sigma) {
    blurredImageArray <- array(data=NA, dim=dim(imageArray))
    for(i in 1:dim(imageArray)[3]) {
        blurredImage <- as.matrix(blur(as.im(imageArray[,,i]), sigma=sigma))
        blurredImageArray[,,i] <- blurredImage
    }
    return(blurredImageArray)
}
