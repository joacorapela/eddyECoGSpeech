
processAll <- function() {
    freq <- 0.62
    waveLength <- 8 # in pixels
    fromTime <- 0
    toTime <- 1
    sampleRate <- 762.9395
    x0 <- 8
    y0 <- 8
    nrow <- 16
    ncol <- 16
    twFilenamePattern <- "results/twFreq%.02fWaveLength%.02fFromTime%.02fToTime%.02fSR%.02f.RData"

    twFilename <- sprintf(twFilenamePattern, freq, waveLength, fromTime, toTime,
                                             sampleRate)
    omega <- 2*pi*freq
    k <- 2*pi/waveLength

    travelingWave <- function(x, y, t, omega, k) {
        r <- sqrt(x^2+y^2)
        value <- sin(k*r-omega*t)
        return(value)
    }

    times <- seq(from=fromTime, to=toTime, by=1/sampleRate)
    tw <- array(NA, dim=c(nrow, ncol, length(times)))
    for(row in 1:nrow) {
        for(col in 1:ncol) {
            for(i in 1:length(times)) {
                tw[row, col, i] <- travelingWave(x=col-x0, y=row-y0,
                                                           t=times[i],
                                                           omega=omega,
                                                           k=k)
            }
        }
    }
    save(tw, file=twFilename)
    browser()
}

processAll()

rm(processAll)
