
source("doLoadSources.R")

processAll <- function() {
    sessionName <- "EC2_B105"
    resultsFilenamePattern <-
     "results/%s/propSignTWsWithPositiveSpeedBetweenBeforeAndAfterOverNSignTWsWithPositiveSpeed.RData"
    figFilenamePattern <-
     "figures/%s/propSignTWsWithPositiveSpeedBetweenBeforeAndAfterOverNSignTWsWithPositiveSpeed.eps"

    resultsFilename <- sprintf(resultsFilenamePattern, sessionName)
    figFilename <- sprintf(figFilenamePattern, sessionName)
    resLoad <- get(load(resultsFilename))
    propsAtLag <- resLoad$propsAtLag
    totalsAtLag <- resLoad$totalsAtLag
    beforeLags <- resLoad$beforeLags
    afterLags <- resLoad$afterLags
    maxIndex <- which.max(propsAtLags)
    colMax <- ((maxIndex-1)%/%nrow(propsAtLags))+1
    rowMax <- ((maxIndex-1)%%nrow(propsAtLags))+1
    y <- beforeLags
    x <- afterLags
    data <- expand.grid(Y=y, X=x)
    data$Z <- as.vector(propsAtLags)
    # Levelplot with ggplot2
    library(ggplot2)
    p <- ggplot(data=data, mapping=aes(x=X, y=Y, z=Z)) 
    p <- p + geom_tile(mapping=aes(fill = Z))
    p <- p + theme_bw()
    p <- p + annotate("text", x=afterLags[colMax], y=beforeLags[rowMax], label=sprintf("%.02f(%d)", propsAtLags[maxIndex], totalsAtLags[maxIndex]), color="red", size=4)
    p <-  p + xlab("Lag after end of CVS")
    p <-  p + ylab("Lag before start of CVS")
    ggsave(plot=p, filename=figFilename)

    browser()
}

processAll()

