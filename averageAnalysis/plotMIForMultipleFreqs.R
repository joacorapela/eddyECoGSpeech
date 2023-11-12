
plotMIForMultipleFreqs <- function(mis, freqsForPhase, freqsForAmplitude,
                                        xlim=c(0.1, 2), zlim=NA,
                                        highFreqRange=c(50, 200),
                                        lowFreqRange=c(7, 50),
                                        lowGradientColor="red",
                                        highGradientColor="yellow",
                                        xlab="Frequency for Phase",
                                        ylab="Frequency for Amplitude",
                                        legendLabel="MI",
                                        hjustAnnotation=0,
                                        sizeAnnotation=4,
                                        colourAnnotation="black") {
    # Begin annotation highFreq
    highFreqIndices <- which(highFreqRange[1]<=freqsForAmplitude &
                              freqsForAmplitude<=highFreqRange[2])
    misHighFreq <- mis
    misHighFreq[-highFreqIndices,] <- 0
    whichMaxHighFreq <- which.max(misHighFreq)
    maxFreqForAmplitudeHighFreq <-
     freqsForAmplitude[whichMaxHighFreq%%nrow(misHighFreq)]
    maxFreqForPhaseHighFreq <-
     freqsForPhase[whichMaxHighFreq%/%nrow(misHighFreq)+1]

    annotationHighFreq <- sprintf('* %.04f (%.02f, %.02f)', 
                                  misHighFreq[whichMaxHighFreq],
                                  maxFreqForPhaseHighFreq,
                                  maxFreqForAmplitudeHighFreq)
    #

    # Begin annotation lowFreq
    lowFreqIndices <- which(lowFreqRange[1]<=freqsForAmplitude &
                              freqsForAmplitude<=lowFreqRange[2])
    misLowFreq <- mis
    misLowFreq[-lowFreqIndices,] <- 0
    whichMaxLowFreq <- which.max(misLowFreq)
    maxFreqForAmplitudeLowFreq <-
     freqsForAmplitude[whichMaxLowFreq%%nrow(misLowFreq)]
    maxFreqForPhaseLowFreq <-
     freqsForPhase[whichMaxLowFreq%/%nrow(misLowFreq)+1]

    annotationLowFreq <- sprintf('* %.04f (%.02f, %.02f)', 
                                  misLowFreq[whichMaxLowFreq],
                                  maxFreqForPhaseLowFreq,
                                  maxFreqForAmplitudeLowFreq)
    #

    rownames(mis) <- freqsForAmplitude
    colnames(mis) <- freqsForPhase
    mMIs <- melt(mis)
    # colnames(mMIs) <- c(xlab, ylab, legendLabel)
    # ggplot(mMIs, aes(x=xlab, y=ylab, fill=legendLabel)) + 
    p <- ggplot(mMIs, aes(x=X2, y=X1, fill=value)) +
          geom_tile() + xlim(xlim)
    if(!is.na(zlim[1])) {
        p <- p + scale_fill_gradient(legendLabel, low=lowGradientColor, 
                                                  high=highGradientColor,
                                                  limits=zlim)
    } else {
        p <- p + scale_fill_gradient(legendLabel, low=lowGradientColor, 
                                                  high=highGradientColor)
    }
    p <- p + annotate("text", 
                      x=maxFreqForPhaseHighFreq, y=maxFreqForAmplitudeHighFreq,
                      label=annotationHighFreq, hjust=hjustAnnotation, 
                      size=sizeAnnotation, colour=colourAnnotation) +
             annotate("text", 
                      x=maxFreqForPhaseLowFreq, y=maxFreqForAmplitudeLowFreq,
                      label=annotationLowFreq, hjust=hjustAnnotation, 
                      size=sizeAnnotation, colour=colourAnnotation) +
             xlab(xlab) + ylab(ylab)
    print(p)
}
