
source("doLoadSources.R")

processAll <- function() {
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    sessionLabel <- "EC2_B105"
    elecNumbers <- 1:256
    fromTime <- 00
    duration <- 700
    sampleRate <- 24.61
    elecNumberToInspect <- 211
    # xlim <- c(340, 370)
    xlim <- c(370, 400)

    gradientsFilenamePattern <- "results/%s/gradientsHTFilteredFrom%.02fTo%.02fOrder%02dFPS%.02fElecFrom%03dTo%03dTimeFrom%.02fTo%.02f.RData"

    toTime <- fromTime + duration
    gradientsFilename <- sprintf(gradientsFilenamePattern, 
                                  sessionLabel, lowCutoff, highCutoff, order, 
                                  sampleRate, 
                                  min(elecNumbers), max(elecNumbers), 
                                  fromTime, toTime)
    resGradWithTimes <- get(load(gradientsFilename))
    electrodeIndexInArray <- 
     getElectrodeIndexInArrayGGPlot(elecNumber=elecNumberToInspect)
    electrodeDXs <- resGradWithTimes$dx[electrodeIndexInArray[1], electrodeIndexInArray[2],]
    electrodeDYs <- resGradWithTimes$dy[electrodeIndexInArray[1], electrodeIndexInArray[2],]
    electrodePhases <- resGradWithTimes$pd[electrodeIndexInArray[1], electrodeIndexInArray[2],]
atan2(y=electrodeDYs, x=electrodeDXs)
    electrodeMod <- resGradWithTimes$pm[electrodeIndexInArray[1], electrodeIndexInArray[2],]

    df <- data.frame(times=resGradWithTimes$times, phases=electrodePhases, mod=electrodeMod)
    p1 <- ggplot(df, aes(x=times, y=phases)) + geom_line() + xlim(xlim) + xlab("Time (sec)") + ylab("Phase (radians)") + ggtitle(sprintf("Electrode %d", elecNumberToInspect))
    p2 <- ggplot(df, aes(x=times, y=mod)) + geom_line() + xlim(xlim) + xlab("Time (sec)") + ylab("Amplitude")
    p3 <- ggplot(df, aes(x=phases)) + 
          geom_histogram(breaks=seq(from=-pi, to=pi, length.out=20)) + 
          coord_polar(start=pi/2, direction=-1) + 
          xlab("Phase (radians)") + ylab("Count")
    multiplot(p1, p2, p3, cols=1)

    browser()
}

processAll()

rm(processAll)
