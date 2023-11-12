
source("doLoadSources.R")

processAll <- function() {
    getMeanAnd95CIAmplitudeByPhase <- function(amplitudesBinnedByPhase) {
        cis <- matrix(NA, nrow=length(amplitudesBinnedByPhase), ncol=3)
        for(i in 1:length(amplitudesBinnedByPhase)) {
            aMean <- mean(amplitudesBinnedByPhase[[i]])
            aSE <- sd(amplitudesBinnedByPhase[[i]])/
                    sqrt(length(amplitudesBinnedByPhase[[i]]))
            cis[i,] <- c(aMean, aMean+1.96*aSE*c(-1,1))
        }
        return(cis)
    }

    freqPhase <- 1/1.62
    freqAmp <- 100.0
    # elecNumbers <- 6:12
    # elecNumbers <- 7:16
    elecNumbers <- 7:10
    groupNumbers <- rep(3, times=length(elecNumbers))
    amplitudesScaleFactor <- 10^6
    breaks <- c(-pi, 0, pi, 2*pi, 3*pi)
    labels <- c(expression(-pi), 0, expression(pi), expression(2*pi), 
                                 expression(3*pi))
    pacFilenamePattern <- 
     "results/EC2_B105/amplitudesBinnedByPhaseWav%d%d_freqPhase%.2f_freqAmp%.2f.RData"
    figFilenamePattern <- "figures/EC2_B105/pacEFrom%dETo%d.eps"
    # ylab <- expression(paste("Mean Amplitude (", mu, "V) at ", freqAmp, "Hz)"))
    ylab <- bquote(paste("Mean Amplitude at ", .(freqAmp), " Hz (", mu, "V)"))
    xlab <- "Phase (radians)"
    keySize <- 1.0
    width = 6
    height = 6
    units = "in"

    absoluteElecNumbers <- (groupNumbers-1)*64+elecNumbers
    figFilename <- sprintf(figFilenamePattern, min(absoluteElecNumbers),
                                               max(absoluteElecNumbers))
    yDataMatrix <- NULL
    for(i in 1:length(elecNumbers)) {
        groupNumber = groupNumbers[i]
        elecNumber = elecNumbers[i]
        pacFilename <- sprintf(pacFilenamePattern, groupNumber, elecNumber,
                                                   freqPhase, freqAmp)
        show(sprintf("Processing electrode %d", absoluteElecNumbers[i]))
        res <- get(load(pacFilename))
        phaseBinsBreaks <- res$phaseBinsBreaks
        phaseBinsCenters <- getPhaseBinsCenters(phaseBinsBreaks=phaseBinsBreaks)
        amplitudesBinnedByPhase <- res$allAmplitudesBinnedByPhase
        if(is.null(yDataMatrix)) {
            yDataMatrix <- matrix(NA, nrow=length(elecNumbers),
                                     ncol=2*length(phaseBinsCenters))
            colnames(yDataMatrix) <- sprintf("%.02f", c(phaseBinsCenters,
                                                        2*pi+phaseBinsCenters))
            rownames(yDataMatrix) <- sprintf("%d", absoluteElecNumbers)

            ymaxDataMatrix <- matrix(NA, nrow=length(elecNumbers),
                                     ncol=2*length(phaseBinsCenters))
            colnames(ymaxDataMatrix) <- sprintf("%.02f", c(phaseBinsCenters,
                                                        2*pi+phaseBinsCenters))
            rownames(ymaxDataMatrix) <- sprintf("%d", absoluteElecNumbers)

            yminDataMatrix <- matrix(NA, nrow=length(elecNumbers),
                                     ncol=2*length(phaseBinsCenters))
            colnames(yminDataMatrix) <- sprintf("%.02f", c(phaseBinsCenters,
                                                        2*pi+phaseBinsCenters))
            rownames(yminDataMatrix) <- sprintf("%d", absoluteElecNumbers)

        }
        cis <-  getMeanAnd95CIAmplitudeByPhase(amplitudesBinnedByPhase=
                                                amplitudesBinnedByPhase)
        meanAmplitudeByPhase <- cis[,1]
        meanCILowerByPhase <- cis[,2]
        meanCIUpperByPhase <- cis[,3]
        yDataMatrix[i,] <- amplitudesScaleFactor*
                           c(meanAmplitudeByPhase, meanAmplitudeByPhase)
        ymaxDataMatrix[i,] <- amplitudesScaleFactor*
                                   c(meanCILowerByPhase, meanCILowerByPhase)
        yminDataMatrix[i,] <- amplitudesScaleFactor*
                                   c(meanCIUpperByPhase, meanCIUpperByPhase)
    }
    meltedYDataMatrix <- melt(yDataMatrix, varnames=c("Electrode", "Phase"))
    meltedYMaxDataMatrix <- melt(ymaxDataMatrix, 
                                  varnames=c("Electrode", "Phase"))
    meltedYMinDataMatrix <- melt(yminDataMatrix, 
                                  varnames=c("Electrode", "Phase"))
    colnames(meltedYDataMatrix) <- c(colnames(meltedYDataMatrix)[1:2], 
                                     "Amplitude")
    colnames(meltedYMaxDataMatrix) <- c(colnames(meltedYMaxDataMatrix)[1:2], 
                                         "ymax")
    colnames(meltedYMinDataMatrix) <- c(colnames(meltedYMaxDataMatrix)[1:2], 
                                         "ymin")
    meltedDataMatrix <- merge(meltedYDataMatrix, merge(meltedYMaxDataMatrix,
                                                        meltedYMinDataMatrix))
    p <- ggplot(meltedDataMatrix, aes(x=Phase, y=Amplitude,
                                              colour=factor(Electrode), 
                                              group=factor(Electrode)))
    p <- p + geom_line(size=2)
    p <- p + geom_point(size=2)
    p <- p + geom_errorbar(aes(x=Phase, ymin=ymin, ymax=ymax))
    p <- p + scale_colour_discrete(name="Electrode")
    p <- p + xlab(xlab)
    p <- p + ylab(ylab)
    p <- p + guides(col = guide_legend(reverse=TRUE))
    if(!is.null(breaks) && !is.null(labels)) {
        p <- p + scale_x_continuous(breaks=breaks, labels=labels)
    }
    # p <- p + guides(line = guide_legend(keywidth = 10, keyheight = 10))
    # p <- p + theme(legend.key.size = unit(keySize, "cm")) 
    ggsave(filename=figFilename, width=width, height=height, units=units)
    # print(p)

    browser()
}

processAll()

rm(processAll)
