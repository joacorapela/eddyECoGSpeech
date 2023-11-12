
source("doLoadSources.R")

processAll <- function() {
    w0 <- 3/2*pi
    freqsForFWHM <- c(1/1.62, 100.0)
    decimateFactor <- 4
    ecogFilename <- "rData/EC2_B105/RawHTK/Wav21.bin"

    readBuffer <- readVectorDoubleWithLengthHeader(filename=ecogFilename)
    ecogSampleRate <- readBuffer[1]/decimateFactor
    info <- getMorletWaveletInfo(w0=w0, srate=ecogSampleRate,
                                        freqsForFWHM=freqsForFWHM)
    show(info)
    browser()
}

processAll()

rm(processAll)
