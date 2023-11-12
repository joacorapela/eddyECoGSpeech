
source("doLoadSources.R")

processAll <- function() {
    # groupNumbers <- 2
    # elecNumbers <- 55
    groupNumbers <- 4
    elecNumbers <- 1:64
    transcriptionSampleRate <- 1e7
    epochFromTime <- -2.0
    epochToTime <- 2.4
    noctave <- 10
    nvoice <- 10
    w0 <- 2*pi
    minSeparation <- 1.10
    decimateFactor <- 2
    baselineLimits <- c(593, 634)/1000
    significance <- .05
    ecogFilenamePattern <- "rData/EC2_B105/RawHTK/Wav%d%d.bin"
    erpFigFilenamePattern <- "figures/EC2_B105/erp%d%d.eps"
    resultsFilenamePattern <- "results/EC2_B105/resultsWav%d%d.RData"
    transcriptionFilename <- "matlabData/EC2_B105/EC2_B105_transcription_final.lab"

    for(groupNumber in groupNumbers) {
        for(elecNumber in elecNumbers) {
            show(sprintf("Processing Wav%d%d", groupNumber, elecNumber))
            ecogFilename <- sprintf(ecogFilenamePattern, groupNumber, 
                                                         elecNumber)
            if(file.exists(ecogFilename)) {
                resultsFilename <- sprintf(resultsFilenamePattern, groupNumber, 
                                                                   elecNumber)
                saveOneSetOfResults(ecogFilename=ecogFilename,
                                     resultsFilename=resultsFilename,
                                     transcriptionFilename=transcriptionFilename,
                                     transcriptionSampleRate=
                                      transcriptionSampleRate,
                                     epochFromTime=epochFromTime,
                                     epochToTime=epochToTime,
                                     noctave=noctave,
                                     nvoice=nvoice,
                                     w0=w0,
                                     minSeparation=minSeparation,
                                     decimateFactor=decimateFactor,
                                     contour=contour,
                                     baselineLimits=baselineLimits,
                                     significance=significance,
                                     ecogFilenamePattern=ecogFilenamePattern, 
                                     resultsFilenamePattern=
                                      resultsFilenamePattern)
            }
        }
    }
}

processAll()

rm(processAll)
