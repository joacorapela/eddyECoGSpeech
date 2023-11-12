plotPhasesAtEntrainmentFreq <- function(phasesAtEntrainmentFreq, nBins,
                                         xlab="Phases at Entrainment Frequency",
                                         ylab="Density", colVLine="red") {
    df <- data.frame(phasesAtEntrainmentFreq=phasesAtEntrainmentFreq)
    p <- ggplot(df, aes(phasesAtEntrainmentFreq, ..density..))
    p <- p + geom_histogram(breaks=seq(-pi, pi, by=pi/3))
    p <- p + xlab(xlab)
    p <- p + ylab(ylab)
    p <- p + geom_vline(xintercept=0, color=colVLine)
    print(p)
}
