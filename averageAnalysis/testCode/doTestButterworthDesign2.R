source("doLoadSources.R")

processAll <- function() {
    highCutoff <- 0.8
    transitionWidth <- 1.0
    ripplesPassband <- 1.0
    ripplesStopband <- 15.0
    sessionLabel <- "EC2_B105"
    decimateFactor <- 8
    elecNumber <- 136
    xlim <- c(0, 2)
    ecogFilenamePattern <- "../data/rData/%s/RawHTK/Wav%d%d.bin"

    res <- getGroupAndElecNumber(elecNumber=elecNumber)
    ecogFilename <- sprintf(ecogFilenamePattern, sessionLabel, 
                                                 res$groupNumber, 
                                                 res$elecNumber)
    if(file.exists(ecogFilename)) {
        readBuffer <- readVectorDoubleWithLengthHeader(filename=
                                                        ecogFilename)
        ecogSampleRate <- as.integer(round(readBuffer[1]/decimateFactor))
        Wp <- (highCutoff)/(ecogSampleRate/2)
        Ws <- c(highCutoff+transitionWidth)/(ecogSampleRate/2)
        btord <- buttord(Wp=Wp, Ws=Ws, Rp=ripplesPassband, Rs=ripplesStopband)
        bt <- butter(btord)
        hf <- freqz(bt, Fs=ecogSampleRate)
        validFreqIndices <- which(xlim[1]<hf$f & hf$f<xlim[2])
        amplitudesDB <- 20*log10(abs(hf$h))
        ylim <- range(amplitudesDB[validFreqIndices])
        plot(hf$f, amplitudesDB, type = "b", xlim=xlim, ylim=ylim,
                   xlab = "Frequency (Hz)", 
                   ylab = "Attenuation (dB)")
        grid()
    } else {
        stop(sprintf("File %s not found", ecogFilename))
    }
    browser()
}

processAll()

rm(processAll)
