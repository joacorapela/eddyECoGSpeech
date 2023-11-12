

source("doLoadSources.R")

processAll <- function() {
    elecNumbers <- 129:144
    cvProductionFreq <- 1/1.62
    itcWithSignFilenamePattern <- 
     "results/EC2_B105/itcSigWav%d%d.RData"
    itcWithRayleighSigFigFilenamePattern <-
     "figures/EC2_B105/itcSBroadSpectrumWav%d.eps"
    xlim <- c(-.5, .6)
    ylim <- c(.1, 10)
    # ylim <- NULL
    zlim <- c(0, .6)
    # zlim <- NULL
    contour <- FALSE
    logScale <- FALSE
    width <- 8
    height <- 8

    if(!is.null(zlim)) {
        maxITCValue <- zlim[2]
    } else {
        maxITCValue <- Inf
    }
    for(elecNumber in elecNumbers) {
        show(sprintf("Processing electrode %d", elecNumber))
        res <- getGroupAndElecNumber(absoluteElecNumber=elecNumber)
        itcWithSigFilename <- 
         sprintf(itcWithSignFilenamePattern, res$groupNumber, res$elecNumber)
        if(file.exists(itcWithSigFilename)) {
            itcWithSig <- get(load(itcWithSigFilename))
            itcWithSig$maskedITC[itcWithSig$maskedITC>maxITCValue] <- 
             maxITCValue
            itcWithRayleighSigFigFilename <- 
             sprintf(itcWithRayleighSigFigFilenamePattern, elecNumber)
            trellis.device("postscript", color=TRUE, 
                           width=width, height=height,
                           onefile=FALSE, horizontal=FALSE,
                           file=itcWithRayleighSigFigFilename)
            plotTimeFreqRes(timeFreqRes=itcWithSig$maskedITC, 
                             times=itcWithSig$times, 
                             freqs=itcWithSig$freqs, 
                             hlines=(1:20)*cvProductionFreq,
                             xlim=xlim, ylim=ylim, zlim=zlim, 
                             contour=contour, logScale=logScale,
                             ylab="Frequency (Hz)", xlab="Time (sec)")
            dev.off()
        }
    }
}

processAll()

rm(processAll)
