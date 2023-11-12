

source("doLoadSources.R")

processAll <- function() {
    # groupNumbers <- 3
    # elecNumbers <- 26
    groupNumbers <- 1:4
    elecNumbers <- 1:64
    time <- 0
    entrainmentFreq <- 1/0.97
    nBins <- 6
    phasesAtTimeFilenamePattern <- 
     "results/EC2_B89/phasesAtTime%.2fWav%d%d.RData"
    histFilenamePattern <- 
     "figures/EC2_B89/hist%dTime%.2fFreq%.2f.eps"
    width <- 6
    height <- 6

    for(groupNumber in groupNumbers) {
        for(elecNumber in elecNumbers) {
            show(sprintf("Processing Wav%d%d", groupNumber, elecNumber))
            phasesAtTimeFilename <- 
             sprintf(phasesAtTimeFilenamePattern, time, groupNumber, elecNumber)
            if(file.exists(phasesAtTimeFilename)) {
                res <- get(load(phasesAtTimeFilename))
                freqs <- res$freqs
                phasesAtTime <- res$phasesAtTime
                entrainmentFreqIndex <- which.min(abs(freqs-entrainmentFreq))
                phasesAtTimeAndEntrainmentFreq <-
                 phasesAtTime[entrainmentFreqIndex,]

                absoluteElecNumber <- 
                 getAbsoluteElectrodeNumber(groupNumber=groupNumber,
                                             elecNumber=elecNumber)
                histFilename <- sprintf(histFilenamePattern, 
                                         absoluteElecNumber, 
                                         time, entrainmentFreq)
                xlab <- sprintf("Phase at Entrainment Freq. %.02f",
                                entrainmentFreq)
                trellis.device("postscript", color=TRUE, 
                               width=width, height=height,
                               onefile=FALSE, horizontal=FALSE,
                               file=histFilename)
                plotPhasesAtEntrainmentFreq(phasesAtEntrainmentFreq=
                                              phasesAtTimeAndEntrainmentFreq,
                                              nBins=nBins,
                                              xlab=xlab)
                dev.off()
            }
        }
    }
}

processAll()

rm(processAll)
