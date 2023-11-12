
source("doLoadSources.R")
source("/home/rapela/dev/research/programs/src/R/utils/writeVectorDouble.R")

processAll <- function() {
    transcriptionFilename <- "../data/matlabData/EC2_B105/EC2_B105_transcription_final.lab"
    ecogFilename <- "../data/rData/EC2_B105/RawHTK/Wav326.bin"
    epochsFilename <- "~/dev/research/claudia/data/epochsWav326.bin"

    transcriptionSampleRate <- 1e7
    xlim <- c(-.5, .6)
    epochFromTime=xlim[1]
    epochToTime=xlim[2]
    minSeparation <- 0
    decimateFactor <- 4

    res <- getNonOverlappingEpochs(ecogFilename=ecogFilename, 
                                    transcriptionFilename=
                                     transcriptionFilename, 
                                    transcriptionSampleRate=
                                     transcriptionSampleRate,
                                    epochFromTime=epochFromTime,
                                    epochToTime=epochToTime,
                                    decimateFactor=decimateFactor,
                                    minSeparation=minSeparation)
    epochs <- res$epochs
    ecogSampleRate <- res$srate
    buffer <- c(length(epochs)+3, nrow(epochs), ncol(epochs), ecogSampleRate, 
                                  as.vector(epochs))
    writeVectorDouble(vector=buffer, file=epochsFilename)
browser()
}

processAll()

rm(processAll)
