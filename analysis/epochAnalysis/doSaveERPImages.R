
source("doLoadSources.R")

processAll <- function() {
    sessionLabel <- "EC2_B105"
    elecNumbers <- 1:256
    # elecNumbers <- 136:140
    epochFromTime <- -0.5
    epochToTime <- 0.6
    baselineFromTime <- -0.5
    baselineToTime <- -0.3
    epochToTime <- 0.6
    minSeparation <- 0
    titlePattern <- "%.02f"
    plotTimeInMinSec <- FALSE
    nrow <- 16
    ncol <- 16
    # desiredFrameRate <- 25
    desiredFrameRate <- 200
    transcriptionSampleRate <- 1e7
    transcriptionFilename <- "../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final_completed.lab"
    ecogFilenamePattern <- "../data/rData/%s/RawHTK/Wav%d%d.bin"
    figFilenamePattern <- "figures/%s/erpWithCI%d.png"
    figDirnamePattern <- "videos/%s/erpElec%03d-%03dFR%.02fEpochFrom%.02fTo%.02fBaselineFrom%.02fTo%.02f"
    figFilenamePattern <- "erpsIndex%06d.png"

    saveERPImagesGGPlot2(sessionLabel=sessionLabel,
                          elecNumbers=elecNumbers,
                          epochFromTime=epochFromTime, 
                          epochToTime=epochToTime,
                          baselineFromTime=baselineFromTime,
                          baselineToTime=baselineToTime,
                          transcriptionSampleRate=transcriptionSampleRate,
                          minSeparation=minSeparation,
                          desiredFrameRate=desiredFrameRate,
                          titlePattern=titlePattern, 
                          plotTimeInMinSec=plotTimeInMinSec,
                          nrow=nrow, ncol=ncol,
                          ecogFilenamePattern=ecogFilenamePattern, 
                          transcriptionFilename=transcriptionFilename,
                          figDirnamePattern=figDirnamePattern, 
                          figFilenamePattern=figFilenamePattern)
}

processAll()

rm(processAll)
