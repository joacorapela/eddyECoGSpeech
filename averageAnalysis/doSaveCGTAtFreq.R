
source("doLoadSources.R")

processAll <- function() {
    lowCutoff <- 0.4
    highCutoff <- 0.8

    # groupNumbers <- 3
    # elecNumbers <- 26
    groupNumbers <- 1:4
    elecNumbers <- 1:64
    decimateFactor <- 4
    freq <- 0.62 # Hz
    nCycles <- 3
    ecogFilenamePattern <- "../data/rData/EC2_B105/RawHTK/Wav%d%d.bin"
    resultsFilenamePattern <- "results/EC2_B105/cgtATFreq%.02fWav%d%d.RData"

    for(groupNumber in groupNumbers) {
        for(elecNumber in elecNumbers) {
            absoluteElecNumber <- (groupNumber-1)*64+elecNumber
            show(sprintf("Processing electrode %d", absoluteElecNumber))
            ecogFilename <- sprintf(ecogFilenamePattern, groupNumber, 
                                                         elecNumber)
            if(file.exists(ecogFilename)) {
                readBuffer <- readVectorDoubleWithLengthHeader(filename=
                                                                ecogFilename)
                ecogData <- readBuffer[2:length(readBuffer)]
                ecogData <- decimate(x=ecogData, q=decimateFactor)
                ecogSampleRate <- readBuffer[1]/decimateFactor
                scale <- nCycles*ecogSampleRate/(2*pi*freq)                       
                cgtRes <- cgt(input=ecogData, nvoice=1,
                                              freqstep=2*freq/ecogSampleRate, 
                                              scale=scale, plot=FALSE)
                results <- list(cgtRes=cgtRes,
                                 ecogSampleRate=ecogSampleRate)
                resultsFilename <- sprintf(resultsFilenamePattern, 
                                            freq, groupNumber, elecNumber)
                save(results, file=resultsFilename)
            }
        }
    }
}

processAll()

rm(processAll)
