
source("doLoadSources.R")

processAll <- function() {
    freqPhase <- 1/1.62
    freqAmp <- 100.0
#     lowCutoff <- 0.6
#     highCutoff <- 1.2
#     sessionLabel <- "EC2_B1"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    sessionLabel <- "EC2_B105"
    titlePattern <- "%d:%02d"
    elecNumbers <- 1:256
    xMin <- 7
    xMax <- 14
    yMin <- 5
    yMax <- 13
#     fromTime <- 130
    fromTime <- 210
    duration <- 60
    nrow <- 16
    ncol <- 16
    desiredFrameRate <- 25
    htFilenamePattern <- "results/%s/htFilteredFrom%.02fTo%.02fWav%d%d.RData"
    pacFilenamePattern <- 
     "results/EC2_B105/amplitudesBinnedByPhaseWav%d%d_freqPhase%.2f_freqAmp%.2f.RData"
    figDirnamePatternPattern <- "videos/%%s/avgAmpFreqPhase%.02fFreqAmp%.02f%%.02ffpsFrom%%03dTo%%03d_squareXMin%%02dXMax%%02dYMin%%02dYMax%%02d"
    figFilenamePattern <- "avgAmpHtFilteredFrom%.02fTo%0.2fIndex%06d.png"

    figDirnamePattern <- sprintf(figDirnamePatternPattern, freqPhase, freqAmp)
    toTime <- fromTime + duration
    saveAvgAmpImagesWithSquare(sessionLabel=sessionLabel,
                                freqPhase=freqPhase,
                                freqAmp=freqAmp,
                                htFilenamePattern=htFilenamePattern, 
                                pacFilenamePattern=pacFilenamePattern, 
                                figDirnamePattern=figDirnamePattern, 
                                figFilenamePattern=figFilenamePattern,
                                elecNumbers=elecNumbers,
                                xMin=xMin, xMax=xMax, yMin=yMin, yMax=yMax,
                                lowCutoff=lowCutoff, highCutoff=highCutoff,
                                fromTime=fromTime, toTime=toTime,
                                desiredFrameRate=desiredFrameRate,
                                titlePattern=titlePattern, 
                                nrow=nrow, ncol=ncol)
}

processAll()

rm(processAll)
