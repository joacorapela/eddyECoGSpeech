source("doLoadSources.R")

processAll <- function() {
    elecNumbers <- 1:256
    nrow <- 16
    ncol <- 16

    phasesArray <- array(NaN, dim=c(nrow, ncol))
    for(elecNumber in elecNumbers) {
        show(sprintf("Processing electrode %d", elecNumber))
        electrodeIndexInArray <-
         getElectrodeIndexInArrayGGPlot(elecNumber=elecNumber)
        phasesArray[electrodeIndexInArray[1], electrodeIndexInArray[2]] <- 
         elecNumber
    }
#     matrixToDisplay <- rotateMatrixBy90DegreesClockwise(phasesArray)
#     X11()
#     image(matrixToDisplay, xaxt="n", yaxt="n",
#                            col=heat.colors(n=length(matrixToDisplay)))
    y <- 1:dim(phasesArray)[1]
    x <- 1:dim(phasesArray)[2]
    df <- expand.grid(y=y, x=x)
    df$z <- as.vector(phasesArray)
    p <- ggplot()
    p <- p + geom_tile(data=df, aes(x=x, y=y, fill=z))
    p <- p + scale_fill_continuous(limits=c(1, 256))

    dfRect <- data.frame(xmin=c(10-.5), xmax=c(15+.5), ymin=c(5-.5), ymax=c(10+.5))
    # p <- ggplot() + geom_rect(data=dfRect, mapping=aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=NULL, alpha=0.0), color="black")
    p <- p + geom_rect(data=dfRect, mapping=aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="black", fill=NA)
    X11()
    print(p)
    browser()
}

processAll()

rm(processAll)

