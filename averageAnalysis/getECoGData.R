getECoGData <- function(ecogFilename) {
    readBuffer <- readVectorDoubleWithLengthHeader(filename=ecogFilename)
    ecogSampleRate <- readBuffer[1]
    ecogData <- readBuffer[2:length(readBuffer)]
    return(list(ecogData=ecogData, ecogSampleRate=ecogSampleRate))
}
