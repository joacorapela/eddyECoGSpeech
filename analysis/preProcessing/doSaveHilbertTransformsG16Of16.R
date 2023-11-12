
source("doLoadSources.R")

processAll <- function() {
    groupNumber <- 16
    numberOfGroups <- 16
    numberOfElectrodes <- 256
    sessionName <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    zScore <- TRUE
    bandpassedFilenamePattern <- "results/%s/bandpassedFilteredFrom%.02fTo%.02fOrder%02dWav%d%d.RData"
    htFilenamePattern <- "results/%s/htFilteredFrom%.02fTo%.02fOrder%02dZScored%dWav%d%d.RData"

    groupSize <- numberOfElectrodes/numberOfGroups
    elecNumbers <- (groupNumber-1)*groupSize+(1:groupSize)
    for(elecNumber in elecNumbers) {
        show(sprintf("Processing electrode %d", elecNumber))
        res <- getGroupAndElecNumber(elecNumber=elecNumber)
        bandpassedFilename <- sprintf(bandpassedFilenamePattern, 
                                       sessionName,
                                       lowCutoff, highCutoff, order, 
                                       res$groupNumber, res$elecNumber)
        if(file.exists(bandpassedFilename)) {
            htFilename <- sprintf(htFilenamePattern, 
                                   sessionName,
                                   lowCutoff, highCutoff, order, 
                                   zScore,
                                   res$groupNumber, res$elecNumber)
            res <- get(load(bandpassedFilename))
            if(zScore) {
                ht <- HilbertTransform(sig=scale(res$filteredECoG))
            } else {
                ht <- HilbertTransform(sig=res$filteredECoG)
            }
            toSaveList <- list(ht=ht, ecogSampleRate=res$ecogSampleRate)
            save(toSaveList, file=htFilename)
        } else {
            warning(sprintf("File %s does not exist", bandpassedFilename))
        }
    }
}

processAll()
