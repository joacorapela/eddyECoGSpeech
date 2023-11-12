
source("doLoadSources.R")

processAll <- function() {
    htFilenamePatternPattern <- "results/EC2_B105/htFilteredBPFrom%.02fBPTo%.02fTimeFrom%.02fTimeTo%.02fWav%%d%%d.RData"
    groupNumbers <- 1:4
    elecNumbers <- 1:64
    lowCutoff <- 0.4
    highCutoff <- 0.8
    htFromTime <- 390
    htToTime <- 400
    plotTime <- 391.3

    htFilenamePattern <- sprintf(htFilenamePatternPattern, lowCutoff,
                                                           highCutoff,
                                                           htFromTime,
                                                           htToTime)
    arrayPhases <- computeArrayPhases(htFilenamePattern=htFilenamePattern, 
                                       time=plotTime, 
                                       groupNumbers=groupNumbers, 
                                       elecNumbers=elecNumbers)
    # df <- expand.grid(x=1:dim(arrayPhases)[1], y=1:dim(arrayPhases)[2])
    # df$z <- as.vector(arrayPhases)
    jet.colors <- colorRampPalette(c("#00007F", "blue", 
                                     "#007FFF", "cyan",
                                     "#7FFF7F", "yellow", 
                                     "#FF7F00", "red", "#7F0000"))
    p <- ggplot(melt(arrayPhases), aes(x=X1, y=X2, fill=value)) 
    p <- p + geom_tile()
    p <- p + scale_fill_gradientn(colours = jet.colors(7))
    print(p)
    # image(arrayPhases, xaxt="n", yaxt="n")
    browser()
}

processAll()
