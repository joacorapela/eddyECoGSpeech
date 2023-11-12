
getPlotTimeSeriesWithPeaksAndCVSs <- function(x, times, 
                                          lowpassedX, 
                                          matchedPeakTimes, 
                                          unmatchedPeakTimes, 
                                          cvsStartTimes, 
                                          xlim, 
                                          ylim=c(-1,1), 
                                          xlab="Time (sec)", 
                                          ylab="z-scored Voltage",
                                          legendName="z-scored\nvoltage",
                                          legendNameHlines="event", 
                                          labels=c("original", "lowpassed"), 
                                          linetypes=c("solid", "dashed"), 
                                          labelMatchedPeak="matched peak", 
                                          labelUnmatchedPeak="unmatched peak", 
                                          labelCVSStart="CVS start", 
                                          colorMatchedPeak="black", 
                                          colorUnmatchedPeak="green", 
                                          colorCVSStart="red", 
                                          linetypeMatchedCPPeak="solid", 
                                          linetypeUnmatchedCPPeak="solid", 
                                          linetypeCVSStart="solid") {
    d <- data.frame(t=times, x=x, lowpassedX=lowpassedX)
    md <- melt(d, id="t")

    p <- ggplot()
    p <- p + geom_line(data=md, mapping=aes(x=t, y=value, linetype=variable))
    p <- p + scale_linetype_manual(name=legendName, labels=labels, values=linetypes)
    p <- p + geom_vline(aes(xintercept=matchedPeakTimes, color="matchedPeak"))
    p <- p + geom_vline(aes(xintercept=unmatchedPeakTimes, color="unmatchedPeak"))
    p <- p + geom_vline(aes(xintercept=cvsStartTimes, color="cvsStartTime"))
    p <- p + scale_color_manual(name=legendNameHlines, labels=c(matchedPeak=labelMatchedPeak, unmatchedPeak=labelUnmatchedPeak, cvsStartTime=labelCVSStart), values = c(matchedPeak=colorMatchedPeak, unmatchedPeak=colorUnmatchedPeak, cvsStartTime=colorCVSStart))
    p <- p + xlim(xlim)
    p <- p + ylim(ylim)
    p <- p + xlab(xlab)
    p <- p + ylab(ylab)

    return(p)
}
