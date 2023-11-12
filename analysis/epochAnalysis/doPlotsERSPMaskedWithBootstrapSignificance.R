

source("doLoadSources.R")

processAll <- function() {
    groupNumbers <- 3
    elecNumbers <- 26
    # groupNumbers <- 1:4
    # elecNumbers <- 1:64
    erspWithBootstrapSigFilenamePattern <- 
     "results/EC2_B105/erspWithBootSigWav%d%d.RData"
    erspWithBootstrapSigFigFilenamePattern <-
     "figures/EC2_B105/erspWS%d.eps"
    xlim <- c(-.5, .6)
    ylim <- c(.5, 300)
    # ylim <- NULL
    # zlim <- c(0, 1)
    # zlim <- c(0, .6)
    zlim <- NULL
    contour <- FALSE
    width <- 8
    height <- 8

    maxERSPValue <- zlim[2]
    for(groupNumber in groupNumbers) {
        for(elecNumber in elecNumbers) {
            show(sprintf("Processing Wav%d%d", groupNumber, elecNumber))
            erspWithBootstrapSigFilename <- 
             sprintf(erspWithBootstrapSigFilenamePattern, groupNumber, elecNumber)
            if(file.exists(erspWithBootstrapSigFilename)) {
                erspWithBootstrapSig <- get(load(erspWithBootstrapSigFilename))
                erspWithBootstrapSig$maskedERSP[erspWithBootstrapSig$maskedERSP>
                                              maxERSPValue] <- maxERSPValue
                absoluteElecNumber <- 
                 getAbsoluteElectrodeNumber(groupNumber=groupNumber,
                                             elecNumber=elecNumber)
                erspWithBootstrapSigFigFilename <- 
                 sprintf(erspWithBootstrapSigFigFilenamePattern, 
                          absoluteElecNumber)
browser()
                trellis.device("postscript", color=TRUE, 
                               width=width, height=height,
                               onefile=FALSE, horizontal=FALSE,
                               file=erspWithBootstrapSigFigFilename)
                plotTimeFreqRes(timeFreqRes=erspWithBootstrapSig$maskedERSP, 
                                 times=erspWithBootstrapSig$times, 
                                 freqs=erspWithBootstrapSig$freqs, 
                                 xlim=xlim, ylim=ylim, zlim=zlim, 
                                 contour=contour, 
                                 ylab="Frequency (Hz)", xlab="Time (sec)")
                dev.off()
            }
        }
    }
}

processAll()

rm(processAll)
