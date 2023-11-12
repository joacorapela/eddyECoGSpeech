
source("doLoadSources.R")

processAll <- function() {
    # groupNumbers <- 3
    # elecNumbers <- 26
    groupNumbers <- 4
    elecNumbers <- 1:32
    clipFromTime <- -0.5
    clipToTime <- 0.6
    epochFromTime <- clipFromTime-10.0
    epochToTime <- clipToTime+10.0
    fmin <- .1
    nvoice <- 10
    w0 <- 3/2*pi
    decimateFactor <- 8
    ecogFilenamePattern <- "rData/EC2_B89/RawHTK/Wav%d%d.bin"
    mwtFilenamePattern <- "results/EC2_B89/mwtSigWav%d%d.RData"

    for(groupNumber in groupNumbers) {
        for(elecNumber in elecNumbers) {
            show(sprintf("Processing Wav%d%d", groupNumber, elecNumber))
            ecogFilename <- sprintf(ecogFilenamePattern, groupNumber, 
                                                         elecNumber)
            mwtFilename <- sprintf(mwtFilenamePattern, groupNumber, elecNumber)
            if(file.exists(ecogFilename) && file.exists(mwtFilename)) {
                readBuffer <- readVectorDoubleWithLengthHeader(filename=
                                                                ecogFilename)
                ecogSampleRate <- readBuffer[1]/decimateFactor
                epochTimes <- seq(from=epochFromTime, to=epochToTime, 
                                                      by=1/ecogSampleRate)
                clippedFrames <- which(clipFromTime<=epochTimes & 
                                                     epochTimes<=clipToTime)
                clippedTimes <- epochTimes[clippedFrames]
                noctave <- ceiling(log2(w0*ecogSampleRate/
                                         (2*pi*fmin*(2-1/nvoice))))
                freqs <- getFreqs(noctave=noctave, nvoice=nvoice, w0=w0, 
                                       srate=ecogSampleRate)
                mwt <- get(load(mwtFilename))
                results <- list(mwt=mwt, times=clippedTimes, freqs=freqs)
                save(results, file=mwtFilename)
            }
        }
    }
}

processAll()

rm(processAll)
