
getECoGSampleRate <- function(ecogFilename) {
    readBuffer <- readVectorDoubleWithLengthHeader(filename=ecogFilename)
    ecogSampleRate <- readBuffer[1]
    return(ecogSampleRate)
}
