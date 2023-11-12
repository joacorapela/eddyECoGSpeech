
source("doLoadSources.R")

processAll <- function() {
    # sessionName <- "EC2_B15"
    # lowCutoff <- 0.8
    # highCutoff <- 1.2
    # order <- 2
    # sessionName <- "EC2_B1"
    # lowCutoff <- 0.7
    # highCutoff <- 1.1
    # order <- 2
    # sessionName <- "EC2_B76"
    # lowCutoff <- 0.6
    # highCutoff <- 1.0
    # order <- 2
    sessionName <- "EC2_B8"
    lowCutoff <- 0.7
    highCutoff <- 1.1
    order <- 2
    # sessionName <- "EC2_B89"
    # lowCutoff <- 0.8
    # highCutoff <- 1.2
    # order <- 2
    # sessionName <- "EC2_B9"
    # lowCutoff <- 0.7
    # highCutoff <- 1.1
    # order <- 2
    elecNumbers <- c(141, 140, 139, 138, 137, 136)
    distancesFromFirstElectorde <- c(0, 4, 8, 12, 16, 20)*1e-3
    delayAfterPeakOfFirstElectrode <- 0.1
    latenciesFromLastPeakFilenamePattern <- "results/%s/latenciesFromLastPeakFilteredFrom%.02fTo%.02fOrder%02dWav%d%d.RData"
    statsFilenamePattern <- "results/%s/velocityStatsFilteredFrom%.02fTo%.02fOrder%02dFromElec%03dTo%03d.RData"

    # First get latencyTimes
    res <- getGroupAndElecNumber(elecNumber=elecNumbers[1])
    groupNumber <- res$groupNumber
    elecNumber <- res$elecNumber
    latenciesFromLastPeakFilename <- 
        sprintf(latenciesFromLastPeakFilenamePattern, 
                 sessionName, lowCutoff, highCutoff, order, 
                 groupNumber, elecNumber)
    if(!file.exists(latenciesFromLastPeakFilename)) {
        stop(sprintf("File %s does not exist", latenciesFromLastPeakFilename))
    }
    res <- get(load(latenciesFromLastPeakFilename))
    latencyTimesToSearch <- res$peakTimes+delayAfterPeakOfFirstElectrode
    # done getting latencyTimes

    slopes <- c()
    rs <- c()
    ps <- c()
    for(latencyTimeToSearch in latencyTimesToSearch) {
        show(sprintf("Processing latency %.04f", latencyTimeToSearch))
        latenciesFromLastPeakToFit <- c()
        distancesFromFirstElectordeToFit <- c()
        for(i in 1:length(elecNumbers)) {
            elecNumber <- elecNumbers[i]
            res <- getGroupAndElecNumber(elecNumber=elecNumber)
            groupNumber <- res$groupNumber
            elecNumberInGroup <- res$elecNumber
    
            latenciesFromLastPeakFilename <- 
             sprintf(latenciesFromLastPeakFilenamePattern, 
                      sessionName, lowCutoff, highCutoff, order, 
                      groupNumber, elecNumberInGroup)
            if(file.exists(latenciesFromLastPeakFilename)) {
                res <- get(load(latenciesFromLastPeakFilename))
                electrodeLatencyTimes <- res$times
                electrodeLatenciesFormLastPeak <- res$latenciesFromLastPeak
                index <- which.min(abs(latencyTimeToSearch -
                                        electrodeLatencyTimes))
                latenciesFromLastPeakToFit <- 
                 c(latenciesFromLastPeakToFit, 
                    electrodeLatenciesFormLastPeak[index])
                distancesFromFirstElectordeToFit <- 
                 c(distancesFromFirstElectordeToFit, 
                    distancesFromFirstElectorde[i])
            } else {
                warning(sprintf("%s not found", latenciesFromLastPeakFilename))
            }
        }
        lmRes <- lm(distancesFromFirstElectordeToFit~latenciesFromLastPeakToFit)
        slopes <- c(slopes, lmRes$coefficients[2])
        corTestRes <- cor.test(distancesFromFirstElectordeToFit,
                                latenciesFromLastPeakToFit)
        rs <- c(rs, corTestRes$estimate)
        ps <- c(ps, corTestRes$p.value)
    }
    answer <- data.frame(time=latencyTimesToSearch, slope=slopes, r=rs, p=ps)
    statsFilename <- sprintf(statsFilenamePattern, sessionName, lowCutoff, highCutoff, order, min(elecNumbers), max(elecNumbers))
    save(answer, file=statsFilename)

    browser()
}

processAll()
