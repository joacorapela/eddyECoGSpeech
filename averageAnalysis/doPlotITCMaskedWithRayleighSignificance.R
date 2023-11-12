

source("doLoadSources.R")

processAll <- function() {
    sessionLabel <- "EC2_B1"
    cvProductionFreq <- 1/1.12
    elecNumbers <- 129:144
    itcWithSignFilenamePattern <- 
     "results/%s/itcSigWav%d%d.RData"
    itcWithRayleighSigFigFilenamePattern <-
     "figures/%s/itcSWav%d.eps"
    xlim <- c(-.5, .6)
    ylim <- c(.1, 3)
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
        res <- getGroupAndElecNumber(elecNumber=elecNumber)
        show(sprintf("Processing electrode %s", elecNumber))
        itcWithSigFilename <- 
         sprintf(itcWithSignFilenamePattern, sessionLabel, 
                                             res$groupNumber, res$elecNumber)
        if(file.exists(itcWithSigFilename)) {
            itcWithSig <- get(load(itcWithSigFilename))
            itcWithSig$maskedITC[itcWithSig$maskedITC>maxITCValue] <- 
             maxITCValue
            itcWithRayleighSigFigFilename <- 
             sprintf(itcWithRayleighSigFigFilenamePattern, 
                      sessionLabel,  elecNumber)
            trellis.device("postscript", color=TRUE, 
                           width=width, height=height,
                           onefile=FALSE, horizontal=FALSE,
                           file=itcWithRayleighSigFigFilename)
            plotTimeFreqRes(timeFreqRes=itcWithSig$maskedITC, 
                             times=itcWithSig$times, 
                             freqs=itcWithSig$freqs, 
                             hline=cvProductionFreq,
                             xlim=xlim, ylim=ylim, zlim=zlim, 
                             contour=contour, logScale=logScale,
                             ylab="Frequency (Hz)", xlab="Time (sec)")
            dev.off()
        } else {
            show(sprintf("File %s does not exist", itcWithSigFilename))
        }
    }
}

processAll()

rm(processAll)
