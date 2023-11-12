
source("doLoadSources.R")

processAll <- function() {
    # freqPhase <- 1/1.62
    # freqAmp <- 100.0
    # lowCutoff <- 0.4
    # highCutoff <- 0.8
    # order <- 2
    freqPhase <- 2/1.62
    freqAmp <- 100.0
    lowCutoff <- 1.0
    highCutoff <- 1.4
    order <- 3
    zScore <- FALSE
    sessionLabel <- "EC2_B105"
    titlePattern <- "%d:%02d"
    # elecNumbers <- 1:256
    # elecNumbers <- 136:140
    # elecNumbers <- c(135, 151, 167, 183, 199, 215, 231, 247,
    #                  134, 150, 166, 182, 198, 214, 230, 246, 
    #                  119, 103,  87,  71,  55,  39,  23,   7, 
    #                  118, 102,  86,  70,  54,  38,  22,   6)
    # elecNumbers <- c(54, 70, 71, 72, 88, 104, 120, 121, 
    #                  138, 139, 140, 141, 158, 174,
    #                  154, 155, 156, 157, 173,
    #                                 172)
    elecNumbers <- 87:92
    # fromTime <- 340
    # fromTime <- 30
    fromTime <- 240
    duration <- 60
    # blurSigma <- .75
    blurSigma <- 0.0
    nrow <- 16
    ncol <- 16
    desiredFrameRate <- 25
    htFilenamePattern <- "results/%s/htFilteredFrom%.02fTo%.02fOrder%02dZScored%dWav%d%d.RData"
    pacFilenamePattern <- 
     "results/%s/amplitudesBinnedByPhaseWav%d%dTimeFrom%.02fTimeTo%.02f_freqPhase%.2f_freqAmp%.2f.RData"
    figDirnamePattern <- "videos/%s/avgAmpElec%03d-%03dFR%.02fFreqPhase%.02fFreqAmp%.02fFrom%03dTo%03dHtFilteredFrom%.02fTo%0.2fOrder%02dZScored%dBlurSigma%.02f"
    figFilenamePattern <- "avgAmpIndex%06d.png"

    toTime <- fromTime + duration
    saveAvgAmpImagesGGPlot2(sessionLabel=sessionLabel,
                             fromTime=fromTime,
                             toTime=toTime,
                             freqPhase=freqPhase,
                             freqAmp=freqAmp,
                             htFilenamePattern=htFilenamePattern, 
                             pacFilenamePattern=pacFilenamePattern, 
                             figDirnamePattern=figDirnamePattern, 
                             figFilenamePattern=figFilenamePattern,
                             elecNumbers=elecNumbers,
                             lowCutoff=lowCutoff, highCutoff=highCutoff,
                             order=order,
                             zScore=zScore,
                             desiredFrameRate=desiredFrameRate,
                             blurSigma=blurSigma,
                             titlePattern=titlePattern, 
                             nrow=nrow, ncol=ncol)
}

processAll()

rm(processAll)
