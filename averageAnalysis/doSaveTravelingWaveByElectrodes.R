
processAll <- function() {
    groupNumbers <- 1:4
    elecNumbers <- 1:64
    freq <- 0.62
    waveLength <- 8 # in pixels
    fromTime <- 0
    toTime <- 1
    sampleRate <- 762.9395
    x0 <- 8
    y0 <- 8
    nrow <- 16
    ncol <- 16
    twFilenamePattern <- "results/simulated/twFreq%.02fWaveLength%.02fFromTime%.02fToTime%.02fSR%.02f.RData"
    elecFilenamePattern <- "results/simulated/twFreq%.02fWaveLength%.02fFromTime%.02fToTime%.02fSR%.02fWav%d%d.RData"

    twFilename <- sprintf(twFilenamePattern, freq, waveLength, fromTime, toTime,
                                             sampleRate)
    tw <- get(load(twFilename))
    for(groupNumber in groupNumbers) {
        for(elecNumber in elecNumbers) {
            res <- getElectrodeIndexInArray(groupNumber=groupNumber,
                                             elecNumber=elecNumber)
            rowIndex <- res[1]
            colIndex <- res[2]
            data <- tw[rowIndex, colIndex,]
            elecFilename <- sprintf(elecFilenamePattern, freq, waveLength, 
                                                         fromTime, toTime,
                                                         sampleRate,
                                                         groupNumber,
                                                         elecNumber)
            listToSave <- list(filteredECoGData=data,
                                ecogSampleRate=sampleRate)
            save(listToSave, file=elecFilename)
        }
    }
}

processAll()

rm(processAll)
