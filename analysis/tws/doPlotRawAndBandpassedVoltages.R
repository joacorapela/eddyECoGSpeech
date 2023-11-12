
source("doLoadSources.R")

processAll <- function() {
    # sessionLabel <- "EC2_B1"
    # lowCutoff <- 0.7
    # highCutoff <- 1.1
    # butterOrder <- 2
    sessionLabel <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    butterOrder <- 2
    # sessionLabel <- "EC2_B76"
    # lowCutoff <- 0.6
    # highCutoff <- 1.0
    # butterOrder <- 2
    # sessionLabel <- "EC2_B8"
    # lowCutoff <- 0.7
    # highCutoff <- 1.1
    # butterOrder <- 2
    # sessionLabel <- "EC2_B89"
    # lowCutoff <- 0.8
    # highCutoff <- 1.2
    # butterOrder <- 2
    # sessionLabel <- "EC2_B9"
    # lowCutoff <- 0.7
    # highCutoff <- 1.1
    # butterOrder <- 2
    elecNumber <- 136
    decimateFactor <- 8
    xlim <- c(280, 290)
    ecogFilenamePattern <- "../data/rData/%s/RawHTK/Wav%d%d.bin"
    figFilenamePattern <- "figures/%s/voltagesVsFilteredElectrode%dFromTime%.02fTo%.02fLow%.02fHigh%.02fOrder%d.eps"

    figFilename <- sprintf(figFilenamePattern, sessionLabel, elecNumber, xlim[1], xlim[2], lowCutoff, highCutoff, butterOrder)
    show(sprintf("Processing electrode %d", elecNumber))
    res <- getGroupAndElecNumber(elecNumber=elecNumber)
    ecogFilename <- sprintf(ecogFilenamePattern, sessionLabel,
                                                 res$groupNumber, 
                                                 res$elecNumber)
    if(file.exists(ecogFilename)) {
        readBuffer <- readVectorDoubleWithLengthHeader(filename=
                                                        ecogFilename)
        ecogData <- readBuffer[2:length(readBuffer)]
        ecogData <- decimate(x=ecogData, q=decimateFactor)
        ecogSampleRate <- readBuffer[1]/decimateFactor
        bf <- butter(n=butterOrder, 
                      W=2*c(lowCutoff, highCutoff)/ecogSampleRate, 
                      type="pass")
        filteredECoGData <- filtfilt(filt=bf, x=ecogData)
        times <- (1:length(ecogData))/ecogSampleRate

        df <- data.frame(time=times, raw=ecogData-mean(ecogData), 
                                     filtered=filteredECoGData)
        dfm <- melt(df, id.vars="time")
        p <- ggplot(data=dfm, mapping=aes(x=time, y=value, 
                                                  group=variable, 
                                                  color=variable))
        p <- p + geom_line()
        p <- p + xlim(xlim)
        p <- p + guides(colour=guide_legend(title="Signal"))
        # p <- p + scale_color_manual(name="Signal", label=c(raw="Raw", filtered="Filtered"))
        p <- p + ylab("Voltage (V)")
        p <- p + xlab("Time (sec)")
        ggsave(plot=p, file=figFilename)
        print(p)
        
        browser()
    } else {
        warning(sprintf("File %s does not exist", ecogFilename))
    }
}

processAll()

rm(processAll)
