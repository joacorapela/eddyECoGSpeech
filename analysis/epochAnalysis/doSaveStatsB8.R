
source("doLoadSources.R")

processAll <- function() {
    # groupNumbers <- 1
    # elecNumbers <- 2
    groupNumbers <- 1:4
    elecNumbers <- 1:64
    winMinTime <- -.5
    winMaxTime <- 0
    winMinFreq <- 10
    winMaxFreq <- 300
    # largeITCThr <- .325
    largeITCThr <- .33
    resultsFilenamePattern <- "results/EC2_B8/resultsWav%d%d.RData"
    statsFilenamePattern <- "results/EC2_B8/allStats.%s"

    stats <- c()
    for(groupNumber in groupNumbers) {
        for(elecNumber in elecNumbers) {
            show(sprintf("Processing Wav%d%d", groupNumber, elecNumber))
            resultsFilename <- sprintf(resultsFilenamePattern, groupNumber,
                                                               elecNumber)
            results <- get(load(resultsFilename))
            statsItem <- getStatsItem(results=results, 
                                       winMinTime=winMinTime, 
                                       winMaxTime=winMaxTime, 
                                       winMinFreq=winMinFreq,
                                       winMaxFreq=winMaxFreq, 
                                       largeITCThr=largeITCThr)
            statsItem$groupNumber <- groupNumber
            statsItem$elecNumber <- elecNumber
            stats <- rbind(stats, statsItem)
        }
    }
    stats$decimalElecNumber <- (stats$groupNumber-1)*64+stats$elecNumber
    save(stats, file=sprintf(statsFilenamePattern, "RData"))
    write.table(stats, file=sprintf(statsFilenamePattern, "csv"), quote=FALSE, row.names=FALSE)

}

processAll()

rm(processAll)
