
source("doLoadSources.R")

processAll <- function() {
    bandpassedFilenamePattern <- "results/EC2_B105/bandpassedFilteredFrom%.02fTo%.02fWav%d%d.RData"
    figDirnamePattern <- "movies/EC2_B105/voltages%.02ffpsFrom%03dTo%03d"
    figFilenamePattern <- "bandpassedFilteredFrom%.02fTo%.02fIndex%06d.png"
    titlePattern <- "%d:%02d"
    elecNumbers <- 1:256
    lowCutoff <- 0.4
    highCutoff <- 0.8
    fromTime <- 0
    toTime <- 90
    nrow <- 16
    ncol <- 16
    desiredFrameRate <- 45
    width <- 6
    height <- 6

    saveVoltageImages(bandpassedFilenamePattern=bandpassedFilenamePattern,
                       figDirnamePattern=figDirnamePattern,
                       figFilenamePattern=figFilenamePattern, 
                       elecNumbers=elecNumbers,
                       lowCutoff=lowCutoff, highCutoff=highCutoff,
                       fromTime=fromTime, toTime=toTime, 
                       desiredFrameRate=desiredFrameRate,
                       titlePattern=titlePattern, 
                       nrow=nrow, ncol=ncol, 
                       width=width, height=height)
}

processAll()

rm(processAll)

