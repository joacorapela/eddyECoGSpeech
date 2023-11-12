
processAll <- function() {
    width <- 256
    height <- 256
    nFrames <- 100
    minRadius <- 1
    radiusStep <- 1
    noiseSD <- .75
    saveFilename <- "steepSurfaceObject_wrapped.dat"

    radius <- seq(from=minRadius, by=radiusStep, length.out=nFrames)
    z <- array(0, dim=c(width, height, nFrames))
    centerX <- width/2
    centerY <- height/2
    x1s <- seq(from=-3.5, to=3.5, length=width)
    y1s <- seq(from=-3.5, to=3.5, length=height)
    x2s <- seq(from=0, to=255, length=width)
    y2s <- seq(from=0, to=255, length=height)
    for(t in 1:nFrames) {
        show(sprintf("Processing %d (%d)", t, nFrames))
        for(i in 1:width) {
            x1 <- x1s[i]
            x2 <- x2s[i]
            for(j in 1:height) {
                y1 <- y1s[j]
                y2 <- y2s[j]
                z[i,j,t] <- 6.12*((1-x1^2)*exp(-x1^2-(y1+1)^2))-
                            20.6*((x1/5-x1^3-y1^5)*exp(-x1^2-y1^2))-
                            0.68*(exp(-(x1+1)^2-y1^2))+
                            0.1*(x2+y2)+
                            0.01*t
            }
        }
    }
    noisyZ <- z
    prevNoise <- matrix(rep(0, times=width*height), ncol=width, nrow=height)
    for(t in 1:nFrames) {
        currentNoise <- prevNoise + matrix(rnorm(length(prevNoise), sd=.165),
                                            nrow=width, ncol=height)
        prevNoise <- currentNoise
        noisyZ[,,t] <- noisyZ[,,t] + currentNoise
    }
    wrappedNoisyZ <- atan2(sin(noisyZ), cos(noisyZ))
    wrappedNoisyZ <- as.single(wrappedNoisyZ)
    vectorWrappedNoisyZ <- as.vector(wrappedNoisyZ)

    con <- file(description=saveFilename, open="wb")
    writeBin(object=vectorWrappedNoisyZ, con=con, size=4)
    close(con)

for(i in 1:20) {
    show(vectorWrappedNoisyZ[i])
}

    browser()
}

processAll()

rm(processAll)

