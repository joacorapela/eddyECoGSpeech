plotSignificantPhaseCoherencesBtwClosestNeighborsAtDelay <- 
 function(pcsWithSign, delay, significance=0.5, elecNumbers=1:256, 
                       ncol=16, nrow=16, colorSegment="blue",
                       sizeAnnotation=4, colorAnnotation="gray",
                       titlePattern="%.04f") {
    signPCsWithSign <- pcsWithSign[pcsWithSign[,"pValue"]<significance,]
    d <- as.data.frame(signPCsWithSign)
    p <- ggplot(data=d, mapping=aes(x=j1, y=i1, xend=j2, yend=i2))
    p <- p + geom_segment(color=colorSegment)
    p <- p + ggtitle(sprintf(titlePattern, delay))
    for(elecNumber in elecNumbers) {
        electrodeIndexInArray <-
         getElectrodeIndexInArrayGGPlot(elecNumber=elecNumber)
        p <- p + annotate("text", x=electrodeIndexInArray[2], 
                          y=electrodeIndexInArray[1], 
                          label=sprintf("%d", elecNumber),
                          color=colorAnnotation)
    }
    p <- p + scale_x_continuous(breaks=seq(from=1:ncol))
    p <- p + scale_y_continuous(breaks=seq(from=1:nrow))
    p <- p + theme(panel.grid.minor=element_blank())
    p <- p + xlab("")
    p <- p + ylab("")
    print(p)
}
