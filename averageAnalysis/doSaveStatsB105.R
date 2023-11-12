
source("doLoadSources.R")

processAll <- function() {
    # groupNumbers <- 3
    # elecNumbers <- 35
    groupNumbers <- 1:4
    elecNumbers <- 1:64

    freqPhase <- 1/1.62
    freqAmplitude <- 100.0
    itcWinMinTime <- -.5
    itcWinMaxTime <- .6
    itcWinFreq <- 1/1.62
    itcInfoFilenamePattern <- "results/EC2_B105/itcSigWav%d%d.RData"
    pacInfoFilenamePattern <- 
     "results/EC2_B105/amplitudesBinnedByPhaseWav%d%d_freqPhase%.2f_freqAmp%.2f.RData"
    statsFilenamePattern <- 
     "results/EC2_B105/allStats_freqPhase%.2f_freqAmp%.2f.%s"

    stats <- c()
    for(groupNumber in groupNumbers) {
        for(elecNumber in elecNumbers) {
            show(sprintf("Processing Wav%d%d", groupNumber, elecNumber))
            itcInfoFilename <- sprintf(itcInfoFilenamePattern, groupNumber,
                                                               elecNumber)
            pacInfoFilename <- sprintf(pacInfoFilenamePattern, groupNumber,
                                                               elecNumber,
                                                               freqPhase,
                                                               freqAmplitude)
            if(file.exists(itcInfoFilename) && file.exists(pacInfoFilename)) {
                itcInfo <- get(load(itcInfoFilename))
                pacInfo <- get(load(pacInfoFilename))
                statsItem <- getStatsItem(itcInfo=itcInfo, 
                                           pacInfo=pacInfo,
                                           itcWinMinTime=itcWinMinTime, 
                                           itcWinMaxTime=itcWinMaxTime, 
                                           itcWinFreq=itcWinFreq)
                statsItem$groupNumber <- groupNumber
                statsItem$elecNumber <- elecNumber
                stats <- rbind(stats, statsItem)
            }
        }
    }
    stats$decimalElecNumber <- (stats$groupNumber-1)*64+stats$elecNumber
    save(stats, file=sprintf(statsFilenamePattern, freqPhase, freqAmplitude, "RData"))
    write.table(stats, file=sprintf(statsFilenamePattern, freqPhase, freqAmplitude, "csv"), quote=FALSE, row.names=FALSE)

}

processAll()

rm(processAll)
