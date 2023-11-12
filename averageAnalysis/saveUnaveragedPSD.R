saveUnaveragedPSD <- function(ecogFilename, desiredFrameRate, spans, detrend,
                                            plot=FALSE, xlim=c(0,1), log="dB",
                                            resultsFilename) {
    readBuffer <- readVectorDoubleWithLengthHeader(filename=
                                                            ecogFilename)
    ecogData <- readBuffer[2:length(readBuffer)]
    ecogData <- decimate(x=ecogData, q=decimateFactor)
    ecogSampleRate <- as.integer(round(readBuffer[1]/decimateFactor))
    aTS <- ts(data=ecogData, frequency=ecogSampleRate)
    spectrumRes <- spectrum(x=aTS, spans=c(span), detrend=detrend, 
                                   plot=plot, xlim=xlim, log=log)
    toSaveList <- list(freq=spectruRes$freq, spec=spectrumRes$spec)
    save(toSaveList, file=resultsFilename)
}
