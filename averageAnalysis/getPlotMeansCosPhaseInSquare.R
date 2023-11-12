
getPlotMeansCosPhaseInSquare <- function(mcpInfo, 
                                          lowpassedMCP, 
                                          matchedMCPPeakTimes, 
                                          unmatchedMCPPeakTimes, 
                                          cvsStartTimes, 
                                          xlim, 
                                          ylim=c(-1,1), 
                                          xlab="Time (sec)", 
                                          ylab=expression(bar(cos(phi))), 
                                          legendNameMCP=expression(bar(cos(phi))), 
                                          legendNameHlines="event", 
                                          labelsMCP=c("original", "lowpassed"), 
                                          linetypesMCP=c("solid", "dashed"), 
                                          labelMatchedMCPPeak="matched peak", 
                                          labelUnmatchedMCPPeak="unmatched peak", 

                                          labelCVSStart="CVS start", 
                                          colorMatchedMCPPeak="black", 
                                          colorUnmatchedMCPPeak="green", 
                                          colorCVSStart="red", 
                                          linetypeMatchedCPPeak="solid", 
                                          linetypeUnmatchedCPPeak="solid", 
                                          linetypeCVSStart="solid") {
    d <- data.frame(times=mcpInfo$times, mcpValues=mcpInfo$meansCosPhase, 
                                         lowpassedMCP=lowpassedMCP)
    md <- melt(d, id="times")

    p <- ggplot()
    p <- p + geom_line(data=md, mapping=aes(x=times, y=value, linetype=variable))
    p <- p + scale_linetype_manual(name=legendNameMCP, labels=labelsMCP, values=linetypesMCP)
    p <- p + geom_vline(aes(xintercept=matchedMCPPeakTimes, color="matchedMCPPeak"))
    p <- p + geom_vline(aes(xintercept=unmatchedMCPPeakTimes, color="unmatchedMCPPeak"))
    p <- p + geom_vline(aes(xintercept=cvsStartTimes, color="cvsStartTime"))
    p <- p + scale_color_manual(name=legendNameHlines, labels=c(matchedMCPPeak=labelMatchedMCPPeak, unmatchedMCPPeak=labelUnmatchedMCPPeak, cvsStartTime=labelCVSStart), values = c(matchedMCPPeak=colorMatchedMCPPeak, unmatchedMCPPeak=colorUnmatchedMCPPeak, cvsStartTime=colorCVSStart))
    p <- p + xlim(xlim)
    p <- p + ylim(ylim)
    p <- p + xlab(xlab)
    p <- p + ylab(ylab)

    return(p)
}
