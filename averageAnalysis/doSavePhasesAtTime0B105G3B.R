
source("doLoadSources.R")

processAll <- function() {
    # groupNumbers <- 3
    # elecNumbers <- 26
    groupNumbers <- 3
    elecNumbers <- 33:64
    transcriptionSampleRate <- 1e7
    xlim <- c(-.5, .6)
    epochFromTime <- xlim[1]-10.0
    epochToTime <- xlim[2]+10.0
    fmin <- .1
    nvoice <- 10
    w0 <- 4*pi
    minSeparation <- 0
    decimateFactor <- 4
    baselineLimits <- c(593, 634)/1000
    time <- 0
    ecogFilenamePattern <- "rData/EC2_B105/RawHTK/Wav%d%d.bin"
    resultsFilenamePattern <- "results/EC2_B105/phasesAtTime%.2fWav%d%d.RData"
    transcriptionFilename <- "matlabData/EC2_B105/EC2_B105_transcription_final.lab"

    for(groupNumber in groupNumbers) {
        for(elecNumber in elecNumbers) {
            show(sprintf("Processing Wav%d%d", groupNumber, elecNumber))
            ecogFilename <- sprintf(ecogFilenamePattern, groupNumber, 
                                                         elecNumber)
            if(file.exists(ecogFilename)) {
                resultsFilename <- sprintf(resultsFilenamePattern, time, 
                                                                   groupNumber, 
                                                                   elecNumber)
                savePhasesAtTime(ecogFilename=ecogFilename,
                                  resultsFilename=resultsFilename,
                                  transcriptionFilename=transcriptionFilename,
                                  transcriptionSampleRate=
                                   transcriptionSampleRate,
                                  epochFromTime=epochFromTime,
                                  epochToTime=epochToTime,
                                  fmin=fmin,
                                  nvoice=nvoice,
                                  w0=w0,
                                  minSeparation=minSeparation,
                                  decimateFactor=decimateFactor,
                                  time=time)
            }
        }
    }
}

processAll()

rm(processAll)
