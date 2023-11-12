
source("doLoadSources.R")

processAll <- function() {
    sessionName <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    zScore <- 0
    saveFromTime <- 0
    saveToTime <- 700
    saveDT <- .1
    elecNumbers <- 135:142
    significance <- .01
    rThreshold <- .85
    waveEventsFilenamePattern <- 
     "results/%s/waveEventsWithoutPhaseUnwrappingFilteredFrom%.02fTo%.02fOrder%02dZScored%dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.RData"
    resultsFilenamePattern <- "results/%s/nSignWEsWithNegSpeedSig%.02frThreshold%.02fWithoutPhaseUnwrappingBetweenFilteredFrom%.02fTo%.02fOrder%02dZScored%dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.txt"

    waveEventsFilename <- sprintf(waveEventsFilenamePattern, sessionName, lowCutoff, highCutoff, order, zScore, saveFromTime, saveToTime, saveDT, elecNumbers[1], elecNumbers[length(elecNumbers)])
    resultsFilename <- sprintf(resultsFilenamePattern, sessionName, significance, rThreshold, lowCutoff, highCutoff, order, zScore, saveFromTime, saveToTime, saveDT, elecNumbers[1], elecNumbers[length(elecNumbers)])
    waveEvents <- get(load(file=waveEventsFilename))
    selectedWaveEvents <- waveEvents[waveEvents$pValues<significance & abs(waveEvents$r)>rThreshold & waveEvents$speed<0,]
    con <- file(description=resultsFilename, open="w")
    aText <- sprintf("Number of significant wave events (p<%.02f, abs(r)>%.02f)=%.02f", significance, rThreshold, nrow(selectedWaveEvents))
    writeLines(text=aText, con=con)
    close(con=con)
    show(aText)

    browser()
}
    
processAll()

rm(processAll)

