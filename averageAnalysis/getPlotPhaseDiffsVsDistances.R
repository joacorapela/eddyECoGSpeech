getPlotPhaseDiffsVsDistances <- function(phaseDiffs, distances, timeToPlot, 
                                                     nResamples,
                                                     colorRegressionLine, 
                                                     xAnnotation, 
                                                     yAnnotation,
                                                     hjustAnnotation,
                                                     vjustAnnotation,
                                                     colorAnnotation,
                                                     sizeAnnotation,
                                                     xlab, ylab,
                                                     ylim) {
    permRes <- permuteSkippedPearsonCorCoef(x=distances, y=phaseDiffs, nResamples=nResamples)
    r <- permRes$t0
    pValue <- sum(abs(permRes$t)>abs(permRes$t0))/length(permRes$t)
    # pbcorRes <- pbcor(x=distances, y=phaseDiffs)
    # r <- pbcorRes$cor
    # pValue <- pbcorRes$p.value

    outIndices <- getOutliersIndices(x=distances, y=phaseDiffs)$bivariateOutliersIndices
    outFactor <- rep(x="noOut", times=length(distances))
    outFactor[outIndices] <- rep(x="xyOut", times=length(outIndices))
    outFactor <- factor(outFactor)
    if(length(outIndices)>0) {
        lmRes <- lm(phaseDiffs[-outIndices]~distances[-outIndices])
    } else {
        lmRes <- lm(phaseDiffs~distances)
    }
    # lmRes <- lm(phaseDiffs~distances)

    df <- data.frame(distances=distances, phaseDiffs=phaseDiffs, outFactor=outFactor)
    # df <- data.frame(distances=distances, phaseDiffs=phaseDiffs)
    p <- ggplot(data=df, mapping=aes(x=distances, y=phaseDiffs))
    # p <- p + geom_point()
    p <- p + geom_point(mapping=aes(colour=outFactor))
    p <- p + scale_colour_manual(values=c(noOut="black", xyOut="green2"), guide=FALSE)
    p <- p + geom_abline(intercept=lmRes$coefficients[1], slope=lmRes$coefficients[2], colour=colorRegressionLine)
    p <- p + annotate("text", label=sprintf("speed=%.04f m/s, r=%.02f, p=%.04f", 2*pi/(lmRes$coefficients[2]*T), r, pValue), x=xAnnotation, y=yAnnotation, hjust=hjustAnnotation, vjust=vjustAnnotation, colour=colorAnnotation, size=sizeAnnotation)
    p <- p + xlab(xlab)
    p <- p + ylab(ylab)
    yTickMarks <- pi*seq(from=-2, to=2, by=.5)
    # labels <- c(expression(-2*pi), expression(-7*pi/4), expression(-3*pi/2), expression(-5*pi/4), expression(-pi), expression(-3*pi/4), expression(-pi/2), expression(-pi/4), 0, expression(pi/4), expression(pi/2), expression(3*pi/4), expression(pi), expression(5*pi/4), expression(3*pi/2), expression(7*pi/4), expression(2*pi))
    labels <- c(expression(-2*pi), expression(-3*pi/2), expression(-pi), expression(-pi/2), 0, expression(pi/2), expression(pi), expression(3*pi/2), expression(2*pi))
    # labels <- c(expression(-pi), expression(-pi/2), 0, expression(pi/2), expression(pi))
    p <- p + scale_y_continuous(breaks=yTickMarks, label=labels, limits=ylim)
    p <- p + ggtitle(sprintf("%.02f secs.", timeToPlot))
    return(p)
}
