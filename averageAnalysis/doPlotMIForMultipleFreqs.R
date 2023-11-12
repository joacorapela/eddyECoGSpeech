
source("doLoadSources.R")

processAll <- function() {
    groupNumbers <- 3
    elecNumbers <- 4
    # groupNumbers <- 1
    # elecNumbers <- 1:64
    zlim <- c(0, 0.003)
    width <- 8
    height <- 8

    resultsFilenamePattern <- "results/EC2_B105/misForMultipleFreqsWav%d%d.RData"
    figFilenamePattern <- "figures/EC2_B105/misForMultipleFreqsE%d.eps"

    for(groupNumber in groupNumbers) {
        for(elecNumber in elecNumbers) {
            show(sprintf("Processing Wav%d%d", groupNumber, elecNumber))
            resultsFilename <- sprintf(resultsFilenamePattern, groupNumber, 
                                                               elecNumber)
            if(file.exists(resultsFilename)) {
                res <- get(load(resultsFilename))
                absoluteElecNumber <- 
                 getAbsoluteElectrodeNumber(groupNumber=groupNumber,
                                             elecNumber=elecNumber)
                figFilename <- sprintf(figFilenamePattern, absoluteElecNumber)
                trellis.device("postscript", color=TRUE, 
                               width=width, height=height,
                               onefile=FALSE, horizontal=FALSE,
                               file=figFilename)
                plotMIForMultipleFreqs(mis=res$mis, 
                                        freqsForAmplitude=res$freqsForAmplitude,
                                        freqsForPhase=res$freqsForPhase,
                                        zlim=zlim)
                dev.off() 
            }
        }
    }
}

processAll()

rm(processAll)
