
dirname <- "~/dev/research/eddyECoGSpeech/averageAnalysis"
source(sprintf("%s/getInfoCVSsInitiations.R", dirname))
source(sprintf("%s/getInfoCVSsTerminations.R", dirname))
rm(dirname)

processAll <- function() {
    require(plotly)

    sessionName <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    zScore <- FALSE
    # elecNumbers <- c(95, 110, 109, 108, 107, 123)
    elecNumbers <- c(136, 137, 138, 139, 140, 141)
    # elecNumbers <- c(54, 70, 71, 72, 88, 104, 120, 121, 
                     # 138, 139, 140, 141, 158, 174,
                     # 154, 155, 156, 157, 173,
                                    # 172)
    # elecNumbers <- c(70, 71, 72, 88, 104, 120)
    saveFromTime <- 0
    saveToTime <- 700
    saveDTFromHTs <- .10
    saveDTWithoutPhaseUnwrapping <- .10
    significance <- .01
    jitterSD <- .005
    xlab <- "Time (sec)"
    ylab <- "Speed (m/sec)"
    vLinesCol <- "gray"
    vLinesOpacity <- 0.3
    linetypeInit <- "solid"
    linetypeTerm <- "dash"
    transcriptionSampleRate <- 1e7
    transcriptionFilename <-
     "../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final_completed.lab"
    metaDataPhasesDatacubeFilenamePattern <- "results/%s/metaDataPhasesDatacubeFilteredFrom%.02fTo%.02fOrder%02dZScored%dFromTime%.02fTo%.02f.%s"
    waveEventsFromHTsFilenamePattern <- "results/%s/waveEventsFilteredFrom%.02fTo%.02fOrder%02dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.%s"
    waveEventWithoutPhaseUnwrappingtsFilenamePattern <- "results/%s/waveEventsWithoutPhaseUnwrappingFilteredFrom%.02fTo%.02fOrder%02dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.%s"

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
    waveEventsFromHTsFilename <- sprintf(waveEventsFromHTsFilenamePattern, sessionName, lowCutoff, highCutoff, order, saveFromTime, saveToTime, saveDTFromHTs, elecNumbers[1], elecNumbers[length(elecNumbers)], "RData")
    waveEventsWithoutPhaseUnwrappingFilename <-
sprintf(waveEventWithoutPhaseUnwrappingtsFilenamePattern, sessionName, lowCutoff, highCutoff, order, saveFromTime, saveToTime, saveDTWithoutPhaseUnwrapping, elecNumbers[1], elecNumbers[length(elecNumbers)], "RData")

    waveEventsFromHTs <- get(load(waveEventsFromHTsFilename))
    waveEventsFromHTs$speed[waveEventsFromHTs$pValue>significance] <- NA
    waveEventsWithoutPhaseUnwrapping <- get(load(waveEventsWithoutPhaseUnwrappingFilename))
    waveEventsWithoutPhaseUnwrapping$speeds[waveEventsWithoutPhaseUnwrapping$pValues>significance] <- NA

    waveEventsToPlotFromHTs <- data.frame(times=waveEventsFromHTs$time, speeds=waveEventsFromHTs$speed+rnorm(n=nrow(waveEventsFromHTs), sd=jitterSD))
    waveEventsToPlotWithoutPhaseUnwrapping <- data.frame(times=waveEventsWithoutPhaseUnwrapping$times, speeds=waveEventsWithoutPhaseUnwrapping$speeds+rnorm(n=nrow(waveEventsWithoutPhaseUnwrapping), sd=jitterSD))

    allSpeeds <- c(waveEventsToPlotFromHTs$speeds,waveEventsToPlotWithoutPhaseUnwrapping$speeds)
    maxAllSpeeds <- max(allSpeeds, na.rm=TRUE)
    minAllSpeeds <- min(allSpeeds, na.rm=TRUE)
    shapesInit <- list()
    for(i in 1:length(infoInit$time)) {
        shapesInit <- c(shapesInit, list(list(fillcolor=vLinesCol,
                                               line=list(color=vLinesCol,
                                                          dash=linetypeInit),
                                               opacity=vLinesOpacity,
                                               type="line",
                                               x0=infoInit$time[i],
                                               x1=infoInit$time[i],
                                               xref="x",
                                               y0=minAllSpeeds,
                                               y1=maxAllSpeeds,
                                               yref="y")))
    }
    for(i in 1:length(infoTerm$time)) {
        shapesInit <- c(shapesInit, list(list(fillcolor=vLinesCol,
                                               line=list(color=vLinesCol,
                                                          dash=linetypeTerm),
                                               opacity=vLinesOpacity,
                                               type="line",
                                               x0=infoTerm$time[i],
                                               x1=infoTerm$time[i],
                                               xref="x",
                                               y0=minAllSpeeds,
                                               y1=maxAllSpeeds,
                                               yref="y")))
    }
    nSigWaveEventsFromHTs <- sum(!is.na(waveEventsToPlotFromHTs$speeds))
    nSigWaveEventsWithoutUnwrapping <- sum(!is.na(waveEventsToPlotWithoutPhaseUnwrapping$speeds))
    shapesList <- c(shapesInit)
    p <- plot_ly() %>%
         add_trace(data=waveEventsToPlotFromHTs, x=~times, y=~speeds, name=sprintf("From HTs\n(%d)", nSigWaveEventsFromHTs), type="scatter", mode="lines+markers") %>%
         add_trace(data=waveEventsToPlotWithoutPhaseUnwrapping, x=~times,y=~speeds, name=sprintf("No unwrapping\n(%d)", nSigWaveEventsWithoutUnwrapping), type="scatter", mode="lines+markers") %>%
         layout(xaxis=list(title=xlab), yaxis=list(title=ylab),
                                        shapes=shapesList)
    print(p)

    browser()
}

processAll()
