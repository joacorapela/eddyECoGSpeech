

source("doLoadSources.R")

processAll <- function() {
    getPhaseBinCenters <- function(phaseBinsBreaks) {
        phaseBinsBreaks <- c(-pi, phaseBinsBreaks, pi)
        phaseBinCenters <- c()
        for(i in 1:(length(phaseBinsBreaks)-1)) {
            phaseBinCenters <- c(phaseBinCenters, 
                                  mean(phaseBinsBreaks[c(i,i+1)]))
        }
        return(phaseBinCenters)
    }

    sessionLabel <- "EC2_B105"
    # elecNumbers <- c(1:160, 162:256)
    elecNumbers <- c(136)
    freqForAmplitudes <- 100.0
    # fromTime <- 340.0
    # toTime <- 400.0
    fromTime <- 30.0
    toTime <- 90.0

    freqForPhases <- 1/1.62
    amplitudesScaleFactor <- 10^6
    nResamples <- 200
    amplitudesBinnedByPhaseFilenamePattern <- 
     "results/%s/amplitudesBinnedByPhaseWav%d%dTimeFrom%.02fTimeTo%.02f_freqPhase%.2f_freqAmp%.2f.RData"
    plotFilenamePattern <- 
     "figures/%s/%sElec%dFromTime%.02fToTime%.02f_freqPhase%.2f_freqAmp%.2f.eps"
    width <- 6
    height <- 6
    ylab <- expression(paste("Mean Amplitude (", mu, "V)"))
    xlab <- "Phase"

    breaks <- c(-pi, 0, pi, 2*pi, 3*pi)
    labels <- c(expression(-pi), 0, expression(pi), expression(2*pi), 
                                 expression(3*pi))
    for(elecNumber in elecNumbers) {
        show(sprintf("Processing electrode %d", elecNumber))
        res <- getGroupAndElecNumber(elecNumber=elecNumber)
        groupNumber <- res$groupNumber
        elecNumberInGroup <- res$elecNumber
        amplitudesBinnedByPhaseFilename <- 
         sprintf(amplitudesBinnedByPhaseFilenamePattern, 
                  sessionLabel,
                  groupNumber, elecNumberInGroup, 
                  fromTime, toTime,
                  freqForPhases, freqForAmplitudes)
        if(file.exists(amplitudesBinnedByPhaseFilename)) {
            res <- get(load(amplitudesBinnedByPhaseFilename))
            phaseBinCenters <- getPhaseBinCenters(phaseBinsBreaks=
                                                   res$phaseBinsBreaks)
            amplitudesBinnedByPhase <- res$allAmplitudesBinnedByPhase
            sAmplitudesBinnedByPhase <- res$allSAmplitudesBinnedByPhase
            mi <- computeModulationIndex(amplitudesBinnedByPhase=
                                          amplitudesBinnedByPhase)
            smi <- computeModulationIndex(amplitudesBinnedByPhase=
                                           sAmplitudesBinnedByPhase)
            annotation <- sprintf("MI=%.8f", mi)
            sannotation <- sprintf("MI=%.8f", smi)

            plotFilename <- sprintf(plotFilenamePattern, 
                                     sessionLabel,
                                     "pac",
                                     elecNumber, 
                                     fromTime, toTime,
                                     freqForPhases, freqForAmplitudes)
            trellis.device("postscript", color=TRUE, 
                           width=width, height=height,
                           onefile=FALSE, horizontal=FALSE,
                           file=plotFilename)
            plotMediansAnd95CIsOfListOfVectors(x=phaseBinCenters,
                                                listOfVectors=
                                                 amplitudesBinnedByPhase,
                                                amplitudesScaleFactor=
                                                 amplitudesScaleFactor,
                                                breaks=breaks,
                                                labels=labels,
                                                annotation=annotation,
                                                nResamples=nResamples,
                                                ylab=ylab,
                                                xlab=xlab)
            dev.off()
            plotFilename <- sprintf(plotFilenamePattern, 
                                     sessionLabel,
                                     "spac",
                                     elecNumber, 
                                     fromTime, toTime,
                                     freqForPhases, freqForAmplitudes)
            trellis.device("postscript", color=TRUE, 
                           width=width, height=height,
                           onefile=FALSE, horizontal=FALSE,
                           file=plotFilename)
            plotMediansAnd95CIsOfListOfVectors(x=phaseBinCenters,
                                                listOfVectors=
                                                 sAmplitudesBinnedByPhase,
                                                amplitudesScaleFactor=
                                                 amplitudesScaleFactor,
                                                annotation=sannotation,
                                                breaks=breaks,
                                                labels=labels,
                                                nResamples=nResamples,
                                                ylab=ylab,
                                                xlab=xlab)
            dev.off()
        }
    }
}

processAll()

rm(processAll)
