
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
    elecNumbers <- 135:142
    # elecNumbers <- c(54, 70, 71, 72, 88, 104, 120, 121, 
                     # 138, 139, 140, 141, 158, 174,
                     # 154, 155, 156, 157, 173,
                                    # 172)
    # elecNumbers <- c(70, 71, 72, 88, 104, 120)
    saveFromTime <- 0
    saveToTime <- 700
    saveDT <- .1
    significance <- .01
    rThreshold <- .85
    minCWEDuration <- .35 # seconds
    xlab <- "Time (sec)"
    ylab <- "Speed (m/sec)"
    vlineColCVS <- "gray"
    vlineColCWEStart <- "blue"
    vlineColCWEEnd <- "red"
    vlineOpacity <- 0.3
    linetypeCVSSTart <- "solid"
    linetypeCVSEnd <- "dash"
    linetypeCWE <- "solid"
    transcriptionSampleRate <- 1e7
    transcriptionFilename <-
     "../../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final_completed.lab"
    metaDataPhasesDatacubeFilenamePattern <- "results/%s/metaDataPhasesDatacubeFilteredFrom%.02fTo%.02fOrder%02dZScored%dFromTime%.02fTo%.02f.%s"
    waveEvenWithoutPhaseUnwrappingtsFilenamePattern <- "results/%s/waveEventsWithoutPhaseUnwrappingFilteredFrom%.02fTo%.02fOrder%02dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.%s"
    contiguousWaveEventsFilenamePattern <- 
     "results/%s/contiguousWaveEventsWithoutPhaseUnwrappingSignificance%.02frTheshold%.02fFilteredFrom%.02fTo%.02fOrder%02dZScored%dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.%s"
    figureFilenamePattern <- 
     "figures/%s/contiguousWaveEventsWithoutPhaseUnwrappingSignificance%.02frTheshold%.02fFilteredFrom%.02fTo%.02fOrder%02dZScored%dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.%s"
    htmlFilenamePattern <- 
    htmlFilenamePattern <- sprintf("%s/%s", getwd(), "figures/%s/contiguousWaveEventsWithoutPhaseUnwrappingSignificance%.02frTheshold%.02fFilteredFrom%.02fTo%.02fOrder%02dZScored%dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.html")

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
    waveEventsWithoutPhaseUnwrappingFilename <- sprintf(waveEvenWithoutPhaseUnwrappingtsFilenamePattern, sessionName, lowCutoff, highCutoff, order, saveFromTime, saveToTime, saveDT, elecNumbers[1], elecNumbers[length(elecNumbers)], "RData")

    waveEventsWithoutPhaseUnwrapping <- get(load(waveEventsWithoutPhaseUnwrappingFilename))
    waveEventsWithoutPhaseUnwrapping$speeds[waveEventsWithoutPhaseUnwrapping$pValues>significance] <- NA
    contiguousWaveEventsFilename <- sprintf(contiguousWaveEventsFilenamePattern, sessionName, significance, rThreshold, lowCutoff, highCutoff, order, zScore, saveFromTime, saveToTime, saveDT, elecNumbers[1], elecNumbers[length(elecNumbers)], "RData")
    figureFilename <- sprintf(figureFilenamePattern, sessionName, significance, rThreshold, lowCutoff, highCutoff, order, zScore, saveFromTime, saveToTime, saveDT, elecNumbers[1], elecNumbers[length(elecNumbers)], "png")
    htmlFilename <- sprintf(htmlFilenamePattern, sessionName, significance, rThreshold, lowCutoff, highCutoff, order, zScore, saveFromTime, saveToTime, saveDT, elecNumbers[1], elecNumbers[length(elecNumbers)])

    waveEventsToPlot <- data.frame(times=waveEventsWithoutPhaseUnwrapping$times,
                                    speedsNoUnwrapping=waveEventsWithoutPhaseUnwrapping$speed)
    cwes <- get(load(file=contiguousWaveEventsFilename))
    cwes <- cwes[cwes[,2]-cwes[,1]+saveDT>minCWEDuration,] # only look at CWEs longer than minCWEDuration

    allSpeeds <- c(waveEventsToPlot$speedsNoUnwrapping)
    maxAllSpeeds <- max(allSpeeds, na.rm=TRUE)
    minAllSpeeds <- min(allSpeeds, na.rm=TRUE)
    shapesInit <- list()
    for(i in 1:length(infoInit$time)) {
        shapesInit <- c(shapesInit, list(list(fillcolor=vlineColCVS,
                                               line=list(color=vlineColCVS,
                                                          dash=linetypeCVSSTart),
                                               opacity=vlineOpacity,
                                               type="line",
                                               x0=infoInit$time[i],
                                               x1=infoInit$time[i],
                                               xref="x",
                                               y0=minAllSpeeds,
                                               y1=maxAllSpeeds,
                                               yref="y")))
    }
    for(i in 1:length(infoTerm$time)) {
        shapesInit <- c(shapesInit, list(list(fillcolor=vlineColCVS,
                                               line=list(color=vlineColCVS,
                                                          dash=linetypeCVSEnd),
                                               opacity=vlineOpacity,
                                               type="line",
                                               x0=infoTerm$time[i],
                                               x1=infoTerm$time[i],
                                               xref="x",
                                               y0=minAllSpeeds,
                                               y1=maxAllSpeeds,
                                               yref="y")))
    }
    for(i in 1:nrow(cwes)) {
        shapesInit <- c(shapesInit, list(list(fillcolor=vlineColCWEStart,
                                               line=list(color=vlineColCWEStart,
                                                          dash=linetypeCWE),
                                               opacity=vlineOpacity,
                                               type="line",
                                               x0=cwes[i,1],
                                               x1=cwes[i,1],
                                               xref="x",
                                               y0=minAllSpeeds,
                                               y1=maxAllSpeeds,
                                               yref="y",
                                               name="CWE start")),
                                    list(list(fillcolor=vlineColCWEEnd,
                                               line=list(color=vlineColCWEEnd,
                                                          dash=linetypeCWE),
                                               opacity=vlineOpacity,
                                               type="line",
                                               x0=cwes[i,2],
                                               x1=cwes[i,2],
                                               xref="x",
                                               y0=minAllSpeeds,
                                               y1=maxAllSpeeds,
                                               yref="y",
                                               name="CWE end"))
                                    )
    }
    nSigWaveEventsNoUnwrapping <- sum(!is.na(waveEventsToPlot$speedsNoUnwrapping))
    shapesList <- c(shapesInit)
    p <- plot_ly(data=waveEventsToPlot, x=~times) %>%
         add_trace(y=~speedsNoUnwrapping, type="scatter", mode="lines+markers") %>%
         layout(xaxis=list(title=xlab), yaxis=list(title=ylab),
                                        shapes=shapesList) %>%
         add_annotations(x=infoInit$times, y=maxAllSpeeds,
                                           text=infoInit$cvSyllables,
                                           xref="x", yref="y", showarrow=FALSE)
    rD <- RSelenium::rsDriver(browser = "firefox")
    export(p=p, file=figureFilename, selenium=rD)
    htmlwidgets::saveWidget(widget=p, file=htmlFilename)
    print(p)

    browser()
}

processAll()
