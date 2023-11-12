
source("doLoadSources.R")

processAll <- function() {
    # groupOfCVSs <- c("mee")
    groupOfCVSs <- c("naa")
    # groupOfCVSs <- c("poo")
    significance <- .01
    correctForMultipleComparisons <- FALSE
    # significance <- .05
    # correctForMultipleComparisons <- TRUE
    delayFrom <- -.2
    delayTo <- .5
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

    figDirname <- sprintf(figDirnamePattern, 
                                    sessionLabel,
                                    lowCutoff, highCutoff, order,
                                    fromTime, toTime, 
                                    delayFrom, delayTo, delayBy,
                                    correctedSignificance,
                                    paste(groupOfCVSs, collapse="_"))
    if(!file.exists(figDirname)) {
        dir.create(figDirname)
    }
    delays <- seq(from=delayFrom, to=delayTo, by=delayBy)
    uniqueDelays <- unique(pcsWithSign[,"delayInSecs"])
    for(i in 1:length(delays)) {
        delay <- delays[i]
        closestDelay <- uniqueDelays[which.min(abs(delay-uniqueDelays))]
        pcsWithSignAtDelay <- pcsWithSign[pcsWithSign[,"delayInSecs"]==
                                           closestDelay,]
        plotSignificantPhaseCoherencesBtwClosestNeighborsAtDelay(
         pcsWithSign=pcsWithSignAtDelay, 
         delay=delay,
         significance=correctedSignificance)
        figFilename <- sprintf(figFilenamePattern, i)
        fullFigFilename <- sprintf("%s/%s", figDirname, figFilename)
        ggsave(file=fullFigFilename)
    }
}

processAll()

rm(processAll)
