
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
    beforeLag <- 0.0
    afterLag <- 0.0
    nResamples <- 2000
    sdNoiseFirstTime <- 5
    nBins <- 50
    # xlim <- c(0, .5)
    colVLineT <- "red"
    xlab <- "Proportion of WEs"
    ylab <- "Count"
    transcriptionSampleRate <- 1e7
    transcriptionFilename <-
     "../../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final_completed.lab"
    waveEventsFilenamePattern <- 
     "results/%s/waveEventsWithoutPhaseUnwrappingFilteredFrom%.02fTo%.02fOrder%02dZScored%dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.RData"
    resultsFilenamePattern <- "results/%s/pValuePropSignWEsWithNegSpeedWithoutPhaseUnwrappingBetweenBefore%.02fAndAfter%.02fOverNSignWEsWithNegSpeedFilteredFrom%.02fTo%.02fOrder%02dZScored%dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.RData"
    figFilenamePattern <- "figures/%s/pValuePropSignWEsWithNegSpeedWithoutPhaseUnwrappingBetweenBefore%.02fAndAfter%.02fOverNSignWEsWithNegSpeedFilteredFrom%.02fTo%.02fOrder%02dZScored%dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.eps"

    waveEventsFilename <- sprintf(waveEventsFilenamePattern, sessionName, lowCutoff, highCutoff, order, zScore, saveFromTime, saveToTime, saveDT, elecNumbers[1], elecNumbers[length(elecNumbers)])
    resultsFilename <- sprintf(resultsFilenamePattern, sessionName, beforeLag, afterLag, lowCutoff, highCutoff, order, zScore, saveFromTime, saveToTime, saveDT, elecNumbers[1], elecNumbers[length(elecNumbers)])
    figFilename <- sprintf(figFilenamePattern, sessionName, beforeLag, afterLag, lowCutoff, highCutoff, order, zScore, saveFromTime, saveToTime, saveDT, elecNumbers[1], elecNumbers[length(elecNumbers)])
    waveEvents <- get(load(file=waveEventsFilename))
    infoInit <- getInfoCVSsInitiations(transcriptionFilename=
                                         transcriptionFilename,
                                        transcriptionSampleRate=
                                         transcriptionSampleRate,
                                        ecogSampleRate=transcriptionSampleRate)
    infoTerm <- getInfoCVSsTerminations(transcriptionFilename=
                                          transcriptionFilename,
                                         transcriptionSampleRate=
                                          transcriptionSampleRate,
                                         ecogSampleRate=
                                          transcriptionSampleRate)
    selectedWaveEvents <- waveEvents[waveEvents$pValues<significance & abs(waveEvents$r)>rThreshold & waveEvents$speed>0,]
    res <- resampleNullPropWEs(
            propFunc=computePropWEsBtwAfterAndBefore,
            waveEvents=selectedWaveEvents,
            sdNoiseFirstTime=sdNoiseFirstTime,
            beforeLag=beforeLag, afterLag=afterLag,
            infoInit=infoInit, infoTerm=infoTerm, nResamples=nResamples)

    pValue <- sum(res$t0<res$t)/length(res$t)
    show(sprintf("p-value=%f", pValue))
    result <- c(res, pValue=pValue)
    save(file=resultsFilename, result)

    df <- data.frame(t=res$t)
    p <- ggplot(data=df, aes(x=t))
    p <- p + geom_histogram(bins=nBins)
    # p <- p + xlim(xlim)
    p <- p + xlab(xlab)
    p <- p + ylab(ylab)
    p <- p + geom_vline(xintercept=res$t0, col=colVLineT)
    print(p)

    ggsave(plot=p, file=figFilename)
    browser()
}
    
processAll()

rm(processAll)

