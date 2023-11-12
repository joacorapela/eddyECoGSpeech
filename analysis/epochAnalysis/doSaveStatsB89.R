
source("doLoadSources.R")

processAll <- function() {
    # groupNumbers <- 4
    # elecNumbers <- 10
    groupNumbers <- 1:4
    elecNumbers <- 1:64
    winMinTime <- -.5
    winMaxTime <- .6
    winFreq <- 1/0.97
    resultsFilenamePattern <- "results/EC2_B89/erspITCSigWav%d%d.RData"
    statsFilenamePattern <- "results/EC2_B89/allStats.%s"

    stats <- c()
    for(groupNumber in groupNumbers) {
        for(elecNumber in elecNumbers) {
            show(sprintf("Processing Wav%d%d", groupNumber, elecNumber))
            resultsFilename <- sprintf(resultsFilenamePattern, groupNumber,
                                                               elecNumber)
            if(file.exists(resultsFilename)) {
                results <- get(load(resultsFilename))
                statsItem <- getStatsItem(results=results, 
                                           winMinTime=winMinTime, 
                                           winMaxTime=winMaxTime, 
                                           winFreq=winFreq)
                statsItem$groupNumber <- groupNumber
                statsItem$elecNumber <- elecNumber
                stats <- rbind(stats, statsItem)
            }
        }
    }
    stats$decimalElecNumber <- (stats$groupNumber-1)*64+stats$elecNumber
    save(stats, file=sprintf(statsFilenamePattern, "RData"))
    write.table(stats, file=sprintf(statsFilenamePattern, "csv"), quote=FALSE, row.names=FALSE)

}

processAll()

rm(processAll)
