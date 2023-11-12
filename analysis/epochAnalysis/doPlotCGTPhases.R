
source("doLoadSources.R")

processAll <- function() {
    freq <- 0.62
    # elecNumbers <- 6:12
    # groupNumbers <- rep(3, times=length(groupNumbers))
    # elecNumbers <- 7:16
    elecNumbers <- 7:13
    groupNumbers <- rep(3, times=length(elecNumbers))
    transcriptionSampleRate <- 1e7
    transcriptionFilename <-
     "../data/matlabData/EC2_B105/EC2_B105_transcription_final.lab"
    cgtAtFreqFilenamePattern <- "results/EC2_B105/cgtATFreq%.02fWav%d%d.RData"
    figFilenamePattern <- 
     "figures/EC2_B105/cgtPhasesAtFreq%.02fEFrom%dETo%dTFrom%.02fTTo%.02f.eps"
    xlim <- c(391.00, 392.0) # time in sec
    xlab <- "Time (sec)"
    normalizedYlab <- "Normalized Cos(phase)"
    unNormalizedYlab <- "Cos(phase)"
    keySize <- 2.0
    normalize <- FALSE
    # normalize <- TRUE
    height = 7
    units = "in"

    absoluteElecNumbers <- (groupNumbers-1)*64+elecNumbers
    figFilename <- sprintf(figFilenamePattern, freq,
                                               min(absoluteElecNumbers),
                                               max(absoluteElecNumbers),
                                               xlim[1], xlim[2])
    dataMatrix <- NULL
    for(i in 1:length(elecNumbers)) {
        groupNumber = groupNumbers[i]
        elecNumber = elecNumbers[i]
        cgtAtFreqFilename <- sprintf(cgtAtFreqFilenamePattern, 
                                      freq, groupNumber, elecNumber)
        show(sprintf("Processing electrode %d", absoluteElecNumbers[i]))
        loadRes <- get(load(cgtAtFreqFilename))
        phases <- Arg(loadRes$cgtRes)
        ecogSampleRate <- loadRes$ecogSampleRate
        if(is.null(dataMatrix)) {
            times <- (1:length(phases))/ecogSampleRate
            samplesToShow <- which(xlim[1]<=times & times <=xlim[2])
            timesToShow <- times[samplesToShow]
            dataMatrix <- matrix(NA, nrow=length(elecNumbers),
                                     ncol=length(timesToShow))
            colnames(dataMatrix) <- sprintf("%.02f", timesToShow)
            rownames(dataMatrix) <- sprintf("%d", absoluteElecNumbers)
            res <- getEpochsSamples(transcriptionFilename=transcriptionFilename,
                                     transcriptionSampleRate=
                                      transcriptionSampleRate,
                                     ecogSampleRate=ecogSampleRate)
            epochSamples <- res$samples
            epochTimes <- epochSamples/ecogSampleRate
            epochSamplesToShow <- which(xlim[1]<=epochTimes & 
                                                 epochTimes<=xlim[2])
            epochTimesToShow <- epochTimes[epochSamplesToShow]
        }
        dataMatrix[i,] <- cos(phases[samplesToShow])
    }
    if(normalize) {
        rowMaxs <- apply(dataMatrix, 1, max)
        nDataMatrix <- dataMatrix*(1/rowMaxs)
    } else {
        nDataMatrix <- dataMatrix
    }
    meltedDataMatrix <- melt(nDataMatrix, varnames=c("Electrode", "Time"))
    colnames(meltedDataMatrix) <- c(colnames(meltedDataMatrix)[1:2],
                                     "cosPhase")
    p <- ggplot(meltedDataMatrix, aes(x=Time, y=cosPhase,
                                              colour=factor(Electrode), 
                                              group=factor(Electrode)))
    p <- p + geom_line(size=1)
    if(length(epochTimesToShow)>0) {
        p <- p + geom_vline(xintercept=epochTimesToShow)
    }
    p <- p + scale_colour_discrete(name="Electrode")
    p <- p + xlab(xlab)
    if(normalize) {
        p <- p + ylab(normalizedYlab)
    } else {
        p <- p + ylab(unNormalizedYlab)
    }
    p <- p + guides(col = guide_legend(reverse=TRUE))
    # p <- p + guides(line = guide_legend(keywidth = 10, keyheight = 10))
    # p <- p + theme(legend.key.size = unit(keySize, "cm")) 
    ggsave(filename=figFilename, height=height, units=units)
    print(p)

    browser()
}

processAll()

rm(processAll)
