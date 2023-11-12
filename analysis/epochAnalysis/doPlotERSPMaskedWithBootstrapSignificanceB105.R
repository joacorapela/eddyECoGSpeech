

source("doLoadSources.R")

processAll <- function() {
    # groupNumbers <- 1
    # elecNumbers <- 3
    groupNumbers <- 1:4
    elecNumbers <- 1:64
    cvProductionFreq <- 1/1.62
    erspWithSigFilenamePattern <- 
     "results/EC2_B105/erspSigWav%d%d.RData"
    erspWithBootstrapSigFigFilenamePattern <-
     "figures/EC2_B105/erspSWav%d.eps"
    xlim <- c(-.5, .6)
    ylim <- NULL
    zlim <- NULL
    contour <- FALSE
    logScale <- TRUE
    width <- 8
    height <- 8

    if(!is.null(zlim)) {
        maxERSPValue <- zlim[2]
    } else {
        maxERSPValue <- Inf
    }
    for(groupNumber in groupNumbers) {
        for(elecNumber in elecNumbers) {
            show(sprintf("Processing Wav%d%d", groupNumber, elecNumber))
            erspWithSigFilename <- 
             sprintf(erspWithSigFilenamePattern, groupNumber, elecNumber)
            if(file.exists(erspWithSigFilename)) {
                erspWithSig <- get(load(erspWithSigFilename))
                erspWithSig$maskedERSP[erspWithSig$maskedERSP>
                                              maxERSPValue] <- maxERSPValue
                absoluteElecNumber <- 
                 getAbsoluteElectrodeNumber(groupNumber=groupNumber,
                                             elecNumber=elecNumber)
                erspWithBootstrapSigFigFilename <- 
                 sprintf(erspWithBootstrapSigFigFilenamePattern, 
                          absoluteElecNumber)
                trellis.device("postscript", color=TRUE, 
                               width=width, height=height,
                               onefile=FALSE, horizontal=FALSE,
                               file=erspWithBootstrapSigFigFilename)
                plotTimeFreqRes(timeFreqRes=erspWithSig$maskedERSP, 
                                 times=erspWithSig$times, 
                                 freqs=erspWithSig$freqs, 
                                 xlim=xlim, ylim=ylim, zlim=zlim, 
                                 contour=contour, logScale=logScale,
                                 ylab="Frequency (Hz)", xlab="Time (sec)")
                dev.off()
            }
        }
    }
}

processAll()

rm(processAll)
