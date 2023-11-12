
source("doLoadSources.R")

processAll <- function() {
#     getFreqsForFWHM <- function(noctave, nvoice) {
#         freqsForFWHM <- c()
#         for(j in 0:(noctave)) {
#             twoToJ <- 2^j
#             for(i in 1:nvoice) {
#                 freqForFWHM <- twoToJ*(1+(i-1)/nvoice)
#                 freqsForFWHM <- c(freqsForFWHM, freqForFWHM)
#             }
#         }
#         return(freqsForFWHM)
#     }

    fmin <- .62
    fmax <- 256
    fstep <- 1
    nvoice <- 10
    w0 <- 3/2*pi
    entrainmentFreq <- 1/1.62
    noctaveForFWHM <- 7
    nvoiceForFWHM <- 10
    decimateFactor <- 8
    ecogFilename <- "rData/EC2_B105/RawHTK/Wav21.bin"

    readBuffer <- readVectorDoubleWithLengthHeader(filename=ecogFilename)
    ecogSampleRate <- readBuffer[1]/decimateFactor
    # freqsForFWHM <- getFreqsForFWHM(noctave=noctaveForFWHM, nvoice=nvoiceForFWHM)
    freqsForFWHM <- seq(from=fmin, to=fmax, by=fstep)
    plotMorletWaveletInfo(minFreq=fmin, nvoice=nvoice, w0=w0,
                                        srate=ecogSampleRate,
                                        freqOfWaveletToDisplay=entrainmentFreq,
                                        freqsForFWHM=freqsForFWHM)
    browser()
}

processAll()

rm(processAll)
