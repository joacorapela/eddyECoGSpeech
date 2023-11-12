

source("doLoadSources.R")

processAll <- function() {
    # groupNumbers <- 3
    # elecNumbers <- 26
    # freqForAmplitudes <- 100.0

    groupNumbers <- 1:4
    elecNumbers <- 1:64
    freqForAmplitudes <- 100.0

    freqForPhases <- 1/1.62
    amplitudesScaleFactor <- 10^6
    nResamples <- 200
    amplitudesBinnedByPhaseFilenamePattern <- 
     "results/EC2_B105/amplitudesBinnedByPhaseWav%d%d_freqPhase%.2f_freqAmp%.2f.RData"
    plotFilenamePattern <- 
     "figures/EC2_B105/%s%d_freqPhase%.2f_freqAmp%.2f.eps"
    width <- 6
    height <- 6
    ylab <- expression(paste("Mean Amplitude (", mu, "V)"))
    xlab <- "Phase (radians)"

    breaks <- c(-pi, 0, pi, 2*pi, 3*pi)
    labels <- c(expression(-pi), 0, expression(pi), expression(2*pi), 
                                 expression(3*pi))
    for(groupNumber in groupNumbers) {
        for(elecNumber in elecNumbers) {
            show(sprintf("Processing Wav%d%d", groupNumber, elecNumber))
            amplitudesBinnedByPhaseFilename <- 
             sprintf(amplitudesBinnedByPhaseFilenamePattern, 
                      groupNumber, elecNumber, 
                      freqForPhases, freqForAmplitudes)
            if(file.exists(amplitudesBinnedByPhaseFilename)) {
                res <- get(load(amplitudesBinnedByPhaseFilename))
                phaseBinsCenters <- getPhaseBinsCenters(phaseBinsBreaks=
                                                       res$phaseBinsBreaks)
                amplitudesBinnedByPhase <- res$allAmplitudesBinnedByPhase
                sAmplitudesBinnedByPhase <- res$allSAmplitudesBinnedByPhase
                mi <- computeModulationIndex(amplitudesBinnedByPhase=
                                              amplitudesBinnedByPhase)
                smi <- computeModulationIndex(amplitudesBinnedByPhase=
                                               sAmplitudesBinnedByPhase)
                annotation <- sprintf("MI=%.8f", mi)
                sannotation <- sprintf("MI=%.8f", smi)
                absoluteElecNumber <- 
                 getAbsoluteElectrodeNumber(groupNumber=groupNumber,
                                             elecNumber=elecNumber)

                plotFilename <- sprintf(plotFilenamePattern, 
                                         "pac",
                                         absoluteElecNumber, 
                                         freqForPhases, freqForAmplitudes)
                trellis.device("postscript", color=TRUE, 
                               width=width, height=height,
                               onefile=FALSE, horizontal=FALSE,
                               file=plotFilename)
                plotPhaseAmplitudeCoupling(phaseBinsCenters=phaseBinsCenters,
                                            amplitudesBinnedByPhase=
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
                                         "spac",
                                         absoluteElecNumber, 
                                         freqForPhases, freqForAmplitudes)
                trellis.device("postscript", color=TRUE, 
                               width=width, height=height,
                               onefile=FALSE, horizontal=FALSE,
                               file=plotFilename)
                plotPhaseAmplitudeCoupling(phaseBinsCenters=phaseBinsCenters,
                                            amplitudesBinnedByPhase=
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
}

processAll()

rm(processAll)
