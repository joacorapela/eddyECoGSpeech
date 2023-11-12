
source("doLoadSources.R")

processAll <- function() {
    # sessionName <- "EC2_B1"
    # lowCutoff <- 0.7
    # highCutoff <- 1.1
    # order <- 2
    sessionName <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    # sessionName <- "EC2_B15"
    # lowCutoff <- 0.8
    # highCutoff <- 1.2
    # order <- 2
    # sessionName <- "EC2_B8"
    # lowCutoff <- 0.7
    # highCutoff <- 1.1
    order <- 2
    # sessionName <- "EC2_B89"
    # lowCutoff <- 0.8
    # highCutoff <- 1.2
    # order <- 2
    # sessionName <- "EC2_B9"
    # lowCutoff <- 0.7
    # highCutoff <- 1.1
    # order <- 2
    # sessionName <- "EC2_B76"
    # lowCutoff <- 0.6
    # highCutoff <- 1.0
    # order <- 2
    elecNumbers <- 136:141
    bandpassedFilenamePattern <- "results/%s/bandpassedFilteredFrom%.02fTo%.02fOrder%02dWav%d%d.RData"
    latenciesFromPrevPeakFilenamePattern <- "results/%s/latenciesFromPrevPeakFilteredFrom%.02fTo%.02fOrder%02dWav%d%d.RData"

    for(elecNumber in elecNumbers) {
        show(sprintf("Processing electrode %d", elecNumber))
        res <- getGroupAndElecNumber(elecNumber=elecNumber)
        groupNumber <- res$groupNumber
        elecNumber <- res$elecNumber
        bandpassedFilename <- sprintf(bandpassedFilenamePattern, 
                                       sessionName,
                                       lowCutoff, highCutoff, order,
                                       groupNumber, elecNumber)
        if(file.exists(bandpassedFilename)) {
            res <- get(load(bandpassedFilename))
            times <- ((1:length(res$filteredECoG))-1)/res$ecogSampleRate
            latenciesFromPrevPeakRes <- 
             getLatenciesFromPrevPeak(x=res$filteredECoG, times=times)
            latenciesFromPrevPeakFilename <- 
             sprintf(latenciesFromPrevPeakFilenamePattern, sessionName, lowCutoff, highCutoff, order, groupNumber, elecNumber)
            toSaveList <- list(latenciesFromPrevPeakInSecs=
                                 latenciesFromPrevPeakRes$latenciesInSecs,
                                latenciesFromPrevPeakInRads=
                                 latenciesFromPrevPeakRes$latenciesInRads,
                                peakTimes=latenciesFromPrevPeakRes$peakTimes,
                                times=times)
            save(toSaveList, file=latenciesFromPrevPeakFilename)
        } else {
            warning(sprintf("File %s does not exist", bandpassedFilename))
        }
    }
}

processAll()
