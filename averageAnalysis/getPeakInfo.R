getPeakInfo <- function(x, times, lowpassCutoffHz, lowpassTransitionWidthHz, 
                           lowpassRipple, sampleRate, plotLowpass=FALSE) {
    lowpassedX <- lowpassKaiser(x=x, 
                                   cutoffHz=lowpassCutoffHz, 
                                   transitionWidthHz=lowpassTransitionWidthHz, 
                                   rippleDB=lowpassRipple,
                                   sampleRate=sampleRate,
                                   doPlot=plotLowpass)
    i <- getPeakIndices(x=lowpassedX)
    if(length(i)>0) {
        peakTimes <- times[i+1]
        peakValues <- x[i+1]
    } else {
        stop("No peaks found")
    }
    answer <- list(peakTimes=peakTimes, peakValues=peakValues, 
                                        lowpassedX=lowpassedX)
    return(answer)
}

