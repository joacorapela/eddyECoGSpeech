
source("doLoadSources.R")

processAll <- function() {
    # groupNumbers <- 3
    # elecNumbers <- 26
    groupNumbers <- 1
    elecNumbers <- 1:32
    transcriptionSampleRate <- 1e7
    clipFromTime <- -0.5
    clipToTime <- 0.6
    epochFromTime <- clipFromTime-10.0
    epochToTime <- clipToTime+10.0
    fmin <- .1
    nvoice <- 10
    w0 <- 3/2*pi
    minSeparation <- 0
    decimateFactor <- 4
    ecogFilenamePattern <- "../data/rData/EC2_B1/RawHTK/Wav%d%d.bin"
    resultsFilenamePattern <- "results/EC2_B1/mwtWav%d%d.RData"
    transcriptionFilename <- "../data/matlabData/EC2_B1/EC2_B1_transcription_final.lab"

    for(groupNumber in groupNumbers) {
        for(elecNumber in elecNumbers) {
            show(sprintf("Processing Wav%d%d", groupNumber, elecNumber))
            ecogFilename <- sprintf(ecogFilenamePattern, groupNumber, 
                                                         elecNumber)
            if(file.exists(ecogFilename)) {
                resultsFilename <- sprintf(resultsFilenamePattern, groupNumber, 
                                                                   elecNumber)
                saveClippedMorletWTs(
                 ecogFilename=ecogFilename,
                 resultsFilename=resultsFilename,
                 transcriptionFilename=transcriptionFilename,
                 transcriptionSampleRate=transcriptionSampleRate,
                 epochFromTime=epochFromTime,
                 epochToTime=epochToTime,
                 clipFromTime=clipFromTime,
                 clipToTime=clipToTime,
                 fmin=fmin,
                 nvoice=nvoice,
                 w0=w0,
                 minSeparation=minSeparation,
                 decimateFactor=decimateFactor)
            }
        }
    }
}

processAll()

rm(processAll)
