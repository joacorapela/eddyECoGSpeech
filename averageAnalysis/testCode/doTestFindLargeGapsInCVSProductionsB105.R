
source("doLoadSources.R")

processAll <- function() {
    groupNumbers <- 3
    elecNumbers <- 26
    transcriptionSampleRate <- 1e7
    epochFromTime <- -0.5
    epochToTime <- 0.6
    decimateFactor <- 8
    minSeparation <- 0
    gapThreshold <- 5
    ecogFilenamePattern <- "rData/EC2_B105/RawHTK/Wav%d%d.bin"
    transcriptionFilename <- "matlabData/EC2_B105/EC2_B105_transcription_final.lab"

    for(groupNumber in groupNumbers) {
        for(elecNumber in elecNumbers) {
            show(sprintf("Processing Wav%d%d", groupNumber, elecNumber))
            ecogFilename <- sprintf(ecogFilenamePattern, groupNumber, 
                                                         elecNumber)
            if(file.exists(ecogFilename)) {
                largeGaps <- findLargeGapsInCVSProductions(
                              gapThreshold=gapThreshold,
                              ecogFilename=ecogFilename,
                              transcriptionFilename=transcriptionFilename,
                              transcriptionSampleRate=transcriptionSampleRate,
                              epochFromTime=epochFromTime,
                              epochToTime=epochToTime,
                              decimateFactor=decimateFactor,
                              minSeparation=minSeparation)
                browser()
            }
        }
    }
}

processAll()

rm(processAll)
