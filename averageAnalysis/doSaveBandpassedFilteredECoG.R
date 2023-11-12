
source("doLoadSources.R")

processAll <- function() {
    # sessionLabel <- "EC2_B1"
    # lowCutoff <- 0.7
    # highCutoff <- 1.1
    # butterOrder <- 2
    sessionLabel <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    butterOrder <- 2
    # sessionLabel <- "EC2_B76"
    # lowCutoff <- 0.6
    # highCutoff <- 1.0
    # butterOrder <- 2
    # sessionLabel <- "EC2_B8"
    # lowCutoff <- 0.7
    # highCutoff <- 1.1
    # butterOrder <- 2
    # sessionLabel <- "EC2_B89"
    # lowCutoff <- 0.8
    # highCutoff <- 1.2
    # butterOrder <- 2
    # sessionLabel <- "EC2_B9"
    # lowCutoff <- 0.7
    # highCutoff <- 1.1
    # butterOrder <- 2
    elecNumbers <- 136:141
    decimateFactor <- 8
    ecogFilenamePattern <- "../data/rData/%s/RawHTK/Wav%d%d.bin"
    resultsFilenamePattern <- "results/%s/bandpassedFilteredFrom%.02fTo%.02fOrder%02dWav%d%d.RData"

    for(elecNumber in elecNumbers) {
        show(sprintf("Processing electrode %d", elecNumber))
        res <- getGroupAndElecNumber(elecNumber=elecNumber)
        ecogFilename <- sprintf(ecogFilenamePattern, sessionLabel,
                                                     res$groupNumber, 
                                                     res$elecNumber)
        if(file.exists(ecogFilename)) {
            readBuffer <- readVectorDoubleWithLengthHeader(filename=
                                                            ecogFilename)
            ecogData <- readBuffer[2:length(readBuffer)]
            ecogData <- decimate(x=ecogData, q=decimateFactor)
            ecogSampleRate <- readBuffer[1]/decimateFactor
            bf <- butter(n=butterOrder, 
                          W=2*c(lowCutoff, highCutoff)/ecogSampleRate, 
                          type="pass")
            filteredECoGData <- filtfilt(filt=bf, x=ecogData)
            results <- list(filteredECoGData=filteredECoGData,
                             ecogSampleRate=ecogSampleRate)
            resultsFilename <- sprintf(resultsFilenamePattern, sessionLabel,
                                                               lowCutoff,
                                                               highCutoff,
                                                               butterOrder,
                                                               res$groupNumber,
                                                               res$elecNumber)
            save(results, file=resultsFilename)
        } else {
            warning(sprintf("File %s does not exist", ecogFilename))
        }
    }
}

processAll()

rm(processAll)
