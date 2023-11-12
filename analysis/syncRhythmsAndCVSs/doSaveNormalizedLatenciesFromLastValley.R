
source("doLoadSources.R")

processAll <- function() {
    sessionName <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    elecNumbers <- 1:256
    # elecNumbers <- 136
    zScore <- TRUE
    bandpassedFilenamePattern <- "results/%s/bandpassedFilteredFrom%.02fTo%.02fOrder%02dWav%d%d.RData"
    nlflvFilenamePattern <- "results/%s/nlflvFilteredFrom%.02fTo%.02fOrder%02dWav%d%d.RData"

    for(elecNumber in elecNumbers) {
        show(sprintf("Processing electrode %d", elecNumber))
        res <- getGroupAndElecNumber(elecNumber=elecNumber)
        bandpassedFilename <- sprintf(bandpassedFilenamePattern, 
                                       sessionName,
                                       lowCutoff, highCutoff, order,
                                       res$groupNumber, res$elecNumber)
        if(file.exists(bandpassedFilename)) {
            nlflvFilename <- sprintf(nlflvFilenamePattern, 
                                      sessionName,
                                      lowCutoff, highCutoff, order,
                                      res$groupNumber, res$elecNumber)
            res <- get(load(bandpassedFilename))
            nlflv <- getNormalizedLatenciesFromLastValley(x=res$filteredECoG)
            toSaveList <- list(nlflv=nlflv, ecogSampleRate=res$ecogSampleRate)
            save(toSaveList, file=nlflvFilename)
        } else {
            warning(sprintf("File %s does not exist", bandpassedFilename))
        }
    }
}

processAll()
