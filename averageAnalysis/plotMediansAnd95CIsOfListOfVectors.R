plotMediansAnd95CIsOfListOfVectors <- function(x, listOfVectors,
                                                  amplitudesScaleFactor,
                                                  annotation,
                                                  breaks=NULL,
                                                  labels=NULL,
                                                  nResamples=2000,
                                                  ciType="perc",
                                                  ciTypeListName="percent",
                                                  ciConf=.95,
                                                  ylab,
                                                  xlab,
                                                  xAnnotation=Inf,
                                                  yAnnotation=Inf,
                                                  # xAnnotation=0,
                                                  # yAnnotation=1.0,
                                                  hjustAnnotation=1,
                                                  vjustAnnotation=1,
                                                  sizeAnnotation=5,
                                                  colourAnnotation="red") {
    cis <- matrix(NA, nrow=length(listOfVectors), ncol=3)
    for(i in 1:length(listOfVectors)) {
        # show(sprintf("Processing phases bin %d (out of %d)", i,
        #              length(listOfVectors)))
        listOfVectors[[i]] <- amplitudesScaleFactor*listOfVectors[[i]]
        # bootRes <- bootstrapMedian(x=listOfVectors[[i]], nResamples=nResamples)
        # bootCIRes <- boot.ci(bootRes, type=ciType, conf=ciConf, index=1)
        # cis[i,] <- c(bootCIRes$t0, bootCIRes[[ciTypeListName]][4:5])
        aMean <- mean(listOfVectors[[i]])
        aSE <- sd(listOfVectors[[i]])/sqrt(length(listOfVectors[[i]]))
        cis[i,] <- c(aMean, aMean+1.96*aSE*c(-1,1))
    }
    d <- data.frame(x=x,
                     y=rep(cis[,1], times=2),
                     ymin=rep(cis[,2], times=2), 
                     ymax=rep(cis[,3], times=2))
    p <- ggplot(data=d)
    p <- p + geom_line(aes(x=x, y=y))
    p <- p + geom_point(aes(x=x, y=y))
    p <- p + geom_errorbar(aes(x=x, ymin=ymin, ymax=ymax))
    p <- p + annotate("text", x=xAnnotation, y=yAnnotation, label=annotation,
                      hjust=hjustAnnotation, vjust=vjustAnnotation, 
                      size=sizeAnnotation, colour=colourAnnotation)
    p <- p + xlab(xlab) + ylab(ylab)
    if(!is.null(breaks) && !is.null(labels)) {
        p <- p + scale_x_continuous(breaks=breaks, labels=labels)
    }
    print(p)
}

