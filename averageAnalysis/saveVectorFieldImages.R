saveVectorFieldImages <- function(xsOneImage, ysOneImage, dxs, dys, 
                                              elecNumbers, times, 
                                              arrowLength, arrowAngle, 
                                              vectorCol, figDirname, 
                                              figFilenamePattern, 
                                              titlePattern,
                                              ncol, nrow,
                                              anXlim=NA,
                                              anYlim=NA,
                                              electrodeLabelCol="gray") {
    if(!file.exists(figDirname)) {
        dir.create(figDirname)
    }
    for(i in 1:length(times)) {
        if(i %% 100==0) {
            show(sprintf("Saved time %.02f (%.02f)", times[i], max(times)))
        }
        figFilename <- sprintf(figFilenamePattern, i)
        figAbsFilename <- sprintf("%s/%s", figDirname, figFilename)
        # timeInSecs <- times[i]-times[1]
        timeInSecs <- times[i]
        minutes <- timeInSecs%/%60
        seconds <- as.integer(timeInSecs%%60)
        title <- sprintf(titlePattern, minutes, seconds)

        p <- getPlotVectorField(x=xsOneImage, y=ysOneImage, 
                                 dx=dxs[,i], dy=dys[,i],
                                 col=vectorCol, arrowLength=arrowLength, 
                                 arrowAngle=arrowAngle,
                                 ncol=ncol,
                                 nrow=nrow,
                                 anXlim=anXlim,
                                 anYlim=anYlim)

        p <- p + ggtitle(title)
#         for(elecNumber in elecNumbers) {
#             electrodeIndexInArray <-
#              getElectrodeIndexInArrayGGPlot(elecNumber=elecNumber)
#             p <- p + annotate("text", label=sprintf("%d", elecNumber), 
#                                x=electrodeIndexInArray[2],
#                                y=electrodeIndexInArray[1],
#                                col=electrodeLabelCol)
#         }
        p <- p + xlab("")
        p <- p + ylab("")
        ggsave(plot=p, filename=figAbsFilename)
#         print(p)
    }
}

