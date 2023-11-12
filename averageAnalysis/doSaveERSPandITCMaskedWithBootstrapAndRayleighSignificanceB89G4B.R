
source("doLoadSources.R")

processAll <- function() {
    # groupNumbers <- 3
    # elecNumbers <- 26
    groupNumbers <- 4
    elecNumbers <- 33:64
    transcriptionSampleRate <- 1e7
    xlim <- c(-.5, .6)
    epochFromTime <- xlim[1]-10.0
    epochToTime <- xlim[2]+10.0
    maskFromTime <- -0.5
    maskToTime <- 0.6
    fmin <- .1
    nvoice <- 10
    w0 <- 4*pi
    minSeparation <- 0
    decimateFactor <- 4
    baselineLimits <- c(593, 634)/1000
    nResamples <- 200
    conf <- .95
    significance <- .5
    ecogFilenamePattern <- "rData/EC2_B89/RawHTK/Wav%d%d.bin"
    resultsFilenamePattern <- "results/EC2_B89/erspITCSigWav%d%d.RData"
    transcriptionFilename <- "matlabData/EC2_B89/EC2_B89_transcription_final.lab"

    for(groupNumber in groupNumbers) {
        for(elecNumber in elecNumbers) {
            show(sprintf("Processing Wav%d%d", groupNumber, elecNumber))
            ecogFilename <- sprintf(ecogFilenamePattern, groupNumber, 
                                                         elecNumber)
            if(file.exists(ecogFilename)) {
                resultsFilename <- sprintf(resultsFilenamePattern, groupNumber, 
                                                                   elecNumber)
                saveERSPandITCMaskedWithBootstrapAndRayleighSignificance(
                 ecogFilename=ecogFilename,
                 resultsFilename=resultsFilename,
                 transcriptionFilename=transcriptionFilename,
                 transcriptionSampleRate=transcriptionSampleRate,
                 epochFromTime=epochFromTime,
                 epochToTime=epochToTime,
                 maskFromTime=maskFromTime,
                 maskToTime=maskToTime,
                 fmin=fmin,
                 nvoice=nvoice,
                 w0=w0,
                 minSeparation=minSeparation,
                 decimateFactor=decimateFactor,
                 baselineLimits=baselineLimits,
                 nResamples=nResamples,
                 conf=conf,
                 significance=significance)
            }
        }
    }
}

processAll()

rm(processAll)
