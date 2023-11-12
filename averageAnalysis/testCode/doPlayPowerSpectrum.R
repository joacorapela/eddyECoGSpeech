
cwd <- getwd()
setwd('../')
source("doLoadSources.R")
setwd(cwd)

processAll <- function() {
    sessionLabel <- "EC2_B105"

    elecNumbers <- 1:256
    decimateFactor <- 8
    elecNumbers <- 133:141
    span <- 40
    # detrend <- TRUE
    detrend <- FALSE
    # xlim <- c(0.0, 1.7)
    xlim <- c(0.5, 50.0)
    productionFreq <- 1/1.62
    nHarmonics <- 1
    ecogFilenamePattern <- "../../data/rData/%s/RawHTK/Wav%d%d.bin"

    nElec <- length(elecNumbers)
    oldPar <- par(mfrow=c(sqrt(nElec)*c(1,1)))
    for(elecNumber in elecNumbers) {
        res <- getGroupAndElecNumber(elecNumber=elecNumber)
        ecogFilename <- sprintf(ecogFilenamePattern, sessionLabel,
                                                     res$groupNumber, 
                                                     res$elecNumber)
        if(file.exists(ecogFilename)) {
            readBuffer <- readVectorDoubleWithLengthHeader(filename=
                                                            ecogFilename)
            ecogData <- readBuffer[2:length(readBuffer)]
            ecogData <- decimate(x=ecogData, q=decimateFactor)
            ecogSampleRate <- as.integer(round(readBuffer[1]/decimateFactor))
            aTS <- ts(data=ecogData, frequency=ecogSampleRate)
            spectrumRes <- spectrum(x=aTS, spans=c(span), detrend=detrend,
    xlim=xlim, log="dB", main=sprintf("%d", elecNumber))
            abline(v=productionFreq*seq(from=1, to=nHarmonics), col="red")
        }
    }
    par(oldPar)
    browser()
}

processAll()

rm(processAll)
