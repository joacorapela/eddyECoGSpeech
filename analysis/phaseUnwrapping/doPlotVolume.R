
processAll <- function() {
    width <- 256
    height <- 256
    nFrames <- 100
    frameToPlot <- 50
    volumeFilename <- "steepSurfaceObject_unwrapped.dat"

    con <- file(description=volumeFilename, open="rb")
    buffer <- readBin(con=con, what=single(), n=width*height*nFrames, size=4)
    close(con)

for(i in 1:20) {
    show(buffer[i])
}

    volume <- array(buffer, dim=c(height, width, nFrames))

    image(volume[,,frameToPlot], col=gray((1:256)/256))

    browser()
}

processAll()

rm(processAll)

