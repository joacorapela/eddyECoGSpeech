
source("doLoadSources.R")

processAll <- function() {
    groupNumber <- 8
    numberOfGroups <- 8
    numberOfElectrodes <- 256
    sessionName <- "EC2_B105"
    lowCutoff <- 1.0
    highCutoff <- 1.4
    order <- 3
    bandpassedFilenamePattern <- "results/%s/bandpassedFilteredFrom%.02fTo%.02fOrder%02dWav%d%d.RData"
    htFilenamePattern <- "results/%s/htFilteredFrom%.02fTo%.02fOrder%02dWav%d%d.RData"

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
                                   res$groupNumber, res$elecNumber)
            res <- get(load(bandpassedFilename))
            library(hht)
            ht <- HilbertTransform(res$filteredECoG)
            toSaveList <- list(ht=ht, ecogSampleRate=res$ecogSampleRate)
            save(toSaveList, file=htFilename)
        }
    }
}

processAll()
