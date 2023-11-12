

source("doLoadSources.R")

processAll <- function() {
    fromElecNumber <- 24
    toElecNumber <- 40
    groupOfCVSs <- c("mee")
    # groupOfCVSs <- c("poo")
    significance <- .01
    correctForMultipleComparisons <- FALSE
    # significance <- .05
    # correctForMultipleComparisons <- TRUE
    delayFrom <- -.5
    delayTo <- 1.0
    delayBy <- .01
    sessionLabel <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    fromTime <- 0
    toTime <- 700
    pcsWithSignFilenamePattern <- "results/%s/pcsWithSignFilteredFrom%.02fTo%.02fOrder%02dFromTime%.02fTo%.02fDelayFrom%.02fTo%.02fBy%.03fCVSs%s.RData"
    figDirnamePattern <- "videos/%s/signPCsFilteredFrom%.02fTo%.02fOrder%02dFromTime%.02fTo%.02fDelayFrom%.02fTo%.02fBy%.03fSign%fCVSs%s"
    figFilenamePattern <- "signPCsIndex%06d.png"

    pcsWithSignFilename <- sprintf(pcsWithSignFilenamePattern, 
                                    sessionLabel,
                                    lowCutoff, highCutoff, order,
                                    fromTime, toTime, 
                                    delayFrom, delayTo, delayBy,
                                    paste(groupOfCVSs, collapse="_"))
    pcsWithSign <- get(load(pcsWithSignFilename))

    nTestPerDelay <- sum(pcsWithSign[,"delayInSecs"]==0)
    if(correctForMultipleComparisons) {
        correctedSignificance <- significance/nTestPerDelay
    } else {
        correctedSignificance <- significance
    }

    fromElectrodeIndexInArray <-
     getElectrodeIndexInArrayGGPlot(elecNumber=fromElecNumber)
    toElectrodeIndexInArray <-
     getElectrodeIndexInArrayGGPlot(elecNumber=toElecNumber)
    selectedRowIndices <- which(pcsWithSign[,"i1"]==fromElectrodeIndexInArray[1] &
                         pcsWithSign[,"j1"]==fromElectrodeIndexInArray[2] &
                         pcsWithSign[,"i2"]==toElectrodeIndexInArray[1] & 
                         pcsWithSign[,"j2"]==toElectrodeIndexInArray[2] &
                         pcsWithSign[,"pValue"]<correctedSignificance)
    allRowIndices <- which(pcsWithSign[,"i1"]==fromElectrodeIndexInArray[1] &
                         pcsWithSign[,"j1"]==fromElectrodeIndexInArray[2] &
                         pcsWithSign[,"i2"]==toElectrodeIndexInArray[1] & 
                         pcsWithSign[,"j2"]==toElectrodeIndexInArray[2])
    delays <- pcsWithSign[selectedRowIndices, "delayInSecs"]
    coherences <- pcsWithSign[selectedRowIndices, "pc"]
    d <- data.frame(delay=delays, coherence=coherences)
    p <- ggplot(data=d, mapping=aes(x=delay, y=coherence))
    p <- p + geom_point()
    p <- p + geom_line()
    print(p)
    browser()
}

processAll()

rm(processAll)
