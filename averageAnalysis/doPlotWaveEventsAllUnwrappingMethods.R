
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
    saveDT <- .1
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
    waveEvenWithoutPhaseUnwrappingtsFilenamePattern <- "results/%s/waveEventsWithoutPhaseUnwrappingFilteredFrom%.02fTo%.02fOrder%02dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.%s"
    waveEvenWith1DPhaseUnwrappingtsFilenamePattern <- "results/%s/waveEventsWith1DPhaseUnwrappingFilteredFrom%.02fTo%.02fOrder%02dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.%s"
    waveEvenWith2DPhaseUnwrappingtsFilenamePattern <- "results/%s/waveEventsWith2DPhaseUnwrappingFilteredFrom%.02fTo%.02fOrder%02dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.%s"
    waveEvenWith3DPhaseUnwrappingtsFilenamePattern <- "results/%s/waveEventsWith3DPhaseUnwrappingFilteredFrom%.02fTo%.02fOrder%02dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.%s"

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
    waveEventsWithoutPhaseUnwrappingFilename <- sprintf(waveEvenWithoutPhaseUnwrappingtsFilenamePattern, sessionName, lowCutoff, highCutoff, order, saveFromTime, saveToTime, saveDT, min(elecNumbers), max(elecNumbers), "RData")
    waveEventsWith1DPhaseUnwrappingFilename <- sprintf(waveEvenWith1DPhaseUnwrappingtsFilenamePattern, sessionName, lowCutoff, highCutoff, order, saveFromTime, saveToTime, saveDT, min(elecNumbers), max(elecNumbers), "RData")
    waveEventsWith2DPhaseUnwrappingFilename <- sprintf(waveEvenWith2DPhaseUnwrappingtsFilenamePattern, sessionName, lowCutoff, highCutoff, order, saveFromTime, saveToTime, saveDT, min(elecNumbers), max(elecNumbers), "RData")
    waveEventsWith3DPhaseUnwrappingFilename <- sprintf(waveEvenWith3DPhaseUnwrappingtsFilenamePattern, sessionName, lowCutoff, highCutoff, order, saveFromTime, saveToTime, saveDT, min(elecNumbers), max(elecNumbers), "RData")

    waveEventsWithoutPhaseUnwrapping <- get(load(waveEventsWithoutPhaseUnwrappingFilename))
    waveEventsWithoutPhaseUnwrapping$speeds[waveEventsWithoutPhaseUnwrapping$pValues>significance] <- NA
    waveEventsWith1DPhaseUnwrapping <- get(load(waveEventsWith1DPhaseUnwrappingFilename))
    waveEventsWith1DPhaseUnwrapping$speeds[waveEventsWith1DPhaseUnwrapping$pValues>significance] <- NA
    waveEventsWith2DPhaseUnwrapping <- get(load(waveEventsWith2DPhaseUnwrappingFilename))
    waveEventsWith2DPhaseUnwrapping$speeds[waveEventsWith2DPhaseUnwrapping$pValues>significance] <- NA
    waveEventsWith3DPhaseUnwrapping <- get(load(waveEventsWith3DPhaseUnwrappingFilename))
    waveEventsWith3DPhaseUnwrapping$speeds[waveEventsWith3DPhaseUnwrapping$pValues>significance] <- NA

    waveEventsToPlot <- data.frame(times=waveEventsWith2DPhaseUnwrapping$times,
                                    speedsNoUnwrapping=waveEventsWithoutPhaseUnwrapping$speed+rnorm(n=nrow(waveEventsWithoutPhaseUnwrapping), sd=jitterSD),
                                    speeds1DUnwrapping=waveEventsWith1DPhaseUnwrapping$speed+rnorm(n=nrow(waveEventsWith1DPhaseUnwrapping), sd=jitterSD),
                                    speeds2DUnwrapping=waveEventsWith2DPhaseUnwrapping$speed+rnorm(n=nrow(waveEventsWith2DPhaseUnwrapping), sd=jitterSD),
                                    speeds3DUnwrapping=waveEventsWith3DPhaseUnwrapping$speed+rnorm(n=nrow(waveEventsWith3DPhaseUnwrapping), sd=jitterSD))

    allSpeeds <- c(waveEventsToPlot$speedsNoUnwrapping,
                        waveEventsToPlot$speeds1DUnwrapping,
                        waveEventsToPlot$speeds2DUnwrapping,
                        waveEventsToPlot$speeds3DUnwrapping)
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
    nSigWaveEventsNoUnwrapping <- sum(!is.na(waveEventsToPlot$speedsNoUnwrapping))
    nSigWaveEvents1DUnwrapping <- sum(!is.na(waveEventsToPlot$speeds1DUnwrapping))
    nSigWaveEvents2DUnwrapping <- sum(!is.na(waveEventsToPlot$speeds2DUnwrapping))
    nSigWaveEvents3DUnwrapping <- sum(!is.na(waveEventsToPlot$speeds3DUnwrapping))
    shapesList <- c(shapesInit)
    p <- plot_ly(data=waveEventsToPlot, x=~times) %>%
         add_trace(y=~speedsNoUnwrapping, name=sprintf("No unwrapping\n(%d)", nSigWaveEventsNoUnwrapping), type="scatter", mode="lines+markers") %>%
         add_trace(y=~speeds1DUnwrapping, name=sprintf("1D unwrapping\n(%d)", nSigWaveEvents1DUnwrapping), type="scatter", mode="lines+markers") %>%
         add_trace(y=~speeds2DUnwrapping, name=sprintf("2D unwrapping\n(%d)", nSigWaveEvents2DUnwrapping), type="scatter", mode="lines+markers") %>%
         add_trace(y=~speeds3DUnwrapping, name=sprintf("3D unwrapping\n(%d)", nSigWaveEvents3DUnwrapping), type="scatter", mode="lines+markers") %>%
         layout(xaxis=list(title=xlab), yaxis=list(title=ylab),
                                        shapes=shapesList)
    print(p)

    browser()
}

processAll()
