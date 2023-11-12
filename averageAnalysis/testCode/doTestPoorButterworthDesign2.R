source("doLoadSources.R")

processAll <- function() {
    lowCutoff <- 1.0
    highCutoff <- 1.4
    butterOrder <- 3
    sessionLabel <- "EC2_B105"
    decimateFactor <- 8
    elecNumber <- 136
    xlim <- c(0.0001, 2)
    nFreqz <- 20*1024
    ecogFilenamePattern <- "../../data/rData/%s/RawHTK/Wav%d%d.bin"

    res <- getGroupAndElecNumber(elecNumber=elecNumber)
    ecogFilename <- sprintf(ecogFilenamePattern, sessionLabel, 
                                                 res$groupNumber, 
                                                 res$elecNumber)
    if(file.exists(ecogFilename)) {
        readBuffer <- readVectorDoubleWithLengthHeader(filename=
                                                        ecogFilename)
        ecogSampleRate <- readBuffer[1]/decimateFactor

        bt <- butter(n=butterOrder, W=2*c(lowCutoff, highCutoff)/ecogSampleRate,
                                    type="pass")
        hf <- freqz(bt, Fs=ecogSampleRate, n=nFreqz)
        validFreqIndices <- which(xlim[1]<=hf$f & hf$f<=xlim[2])
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
