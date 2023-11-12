
source("doLoadSources.R")

processAll <- function() {
    freqToPlot <- 0.62
    # elecNumbers <- 6:12
    # groupNumbers <- rep(3, times=length(groupNumbers))
    # elecNumbers <- 7:16
    elecNumbers <- 7:13
    groupNumbers <- rep(3, times=length(elecNumbers))
    transcriptionSampleRate <- 1e7
    transcriptionFilename <-
     "../data/matlabData/EC2_B105/EC2_B105_transcription_final.lab"
    mwtFilenamePattern <- "results/EC2_B105/mwtWav%d%d.RData"
    figFilenamePattern <- 
     "figures/EC2_B105/mwtPhasesAtFreq%.02fEFrom%dETo%d.eps"
    xlab <- "Time (sec)"
    ylab <- "Cos(phase)"
    keySize <- 2.0
    height <- 7
    units <- "in"
    epochIndex <- 184
    epochApproximateTime <- 400 # in secs
    ecogFilename <- "../data/rData/EC2_B105/RawHTK/Wav12.bin"

    readBuffer <- readVectorDoubleWithLengthHeader(filename=ecogFilename)
    ecogSampleRate <- readBuffer[1]
    absoluteElecNumbers <- (groupNumbers-1)*64+elecNumbers
    figFilename <- sprintf(figFilenamePattern, freqToPlot,
                                               min(absoluteElecNumbers),
                                               max(absoluteElecNumbers))
    dataMatrix <- NULL
    for(i in 1:length(elecNumbers)) {
        groupNumber = groupNumbers[i]
        elecNumber = elecNumbers[i]
        mwtFilename <- sprintf(mwtFilenamePattern, groupNumber, elecNumber)
        show(sprintf("Processing electrode %d", absoluteElecNumbers[i]))
        loadRes <- get(load(mwtFilename))
        if(is.null(dataMatrix)) {
            res <- getEpochsSamples(transcriptionFilename=transcriptionFilename,
                                     transcriptionSampleRate=
                                      transcriptionSampleRate,
                                     ecogSampleRate=ecogSampleRate)
            epochsTimes <- res$samples/ecogSampleRate
            if(is.nan(epochIndex)) {
                epochIndex <- which.min(abs(epochsTimes-epochApproximateTime))
                epochTime <- epochsTimes[epochIndex]
            } else {
                epochTime <- epochsTimes[epochIndex]
            }
            epochTimes <- loadRes$times
            dataMatrix <- matrix(NA, nrow=length(elecNumbers),
                                     ncol=length(epochTimes))
            colnames(dataMatrix) <- sprintf("%.02f", epochTimes)
            rownames(dataMatrix) <- sprintf("%d", absoluteElecNumbers)
            freqIndex <- which.min(abs(loadRes$freqs-freqToPlot))
        }
        phases <- Arg(loadRes$mwts[, freqIndex, epochIndex])
        dataMatrix[i,] <- cos(phases)
    }
    meltedDataMatrix <- melt(dataMatrix, varnames=c("Electrode", "Time"))
    colnames(meltedDataMatrix) <- c(colnames(meltedDataMatrix)[1:2],
                                     "cosPhase")
    p <- ggplot(meltedDataMatrix, aes(x=Time, y=cosPhase,
                                              colour=factor(Electrode), 
                                              group=factor(Electrode)))
    p <- p + geom_line(size=1)
#     if(length(epochTimesToShow)>0) {
#         p <- p + geom_vline(xintercept=epochTimesToShow)
#     }
    p <- p + scale_colour_discrete(name="Electrode")
    p <- p + xlab(xlab)
    p <- p + ylab(ylab)
    p <- p + guides(col = guide_legend(reverse=TRUE))
    p <- p + ggtitle(sprintf("Epoch %d at %.02f sec.", epochIndex, epochTime))
    # p <- p + guides(line = guide_legend(keywidth = 10, keyheight = 10))
    # p <- p + theme(legend.key.size = unit(keySize, "cm")) 
    ggsave(filename=figFilename, height=height, units=units)
    print(p)

    browser()
}

processAll()

rm(processAll)
