
source("doLoadSources.R")

processAll <- function() {
    sampleRate <- 250
    length <- 4
    times <- seq(from=0, to=length, by=1/sampleRate)
    fModulation <- 1
    modulations <- cos(2*pi*fModulation*times)
    fUnmodulatedSignal <- 10
    unmodulatedSignal <- cos(2*pi*fUnmodulatedSignal*times)
    modulatedSignal <- modulations*unmodulatedSignal

    analyticSignal <- HilbertTransform(modulatedSignal)
    amplitudeEnvolopsToPlot <- Mod(analyticSignal)
    instantaneousPhasesToPlot <- Arg(analyticSignal)
    df <- data.frame(times=times,
                      signal=modulatedSignal,
                      posAmplitudeEnvelop=amplitudeEnvolopsToPlot,
                      negAmplitudeEnvelop=-amplitudeEnvolopsToPlot)
#                       instantaneousPhase=instantaneousPhasesToPlot)
    mDF <- melt(df, id.vars="times")
    p <- ggplot(mDF, aes(x=times, y=value, color=variable))
    p <- p + geom_line()
    print(p)
    browser()
}

processAll()

rm(processAll)

