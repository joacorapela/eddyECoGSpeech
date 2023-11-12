
source("doLoadSources.R")

processAll <- function() {
    sessionName <- "EC2_B105"
    elecNumbers <- 136:141
    nBins <- 20
    minCWEDuration <- 0.005
    cwesStartTime <- 30 # seconds
    xlab <- "Latency CVS-\"TW with Speed>0\" Initiation (sec)"
    ylab <- "Number of Events"
    width <- 6
    height <- 6
    units <- "in"
    vlineAt0Col <- "gray"
    vlineAtMedianCol <- "red"
    vlineAtQuantilesCol <- "red"
    xlab <- "Traveling Wave Phase"
    ylab <- "Count"
    legendName <- "Behavior"
    transcriptionSampleRate <- 1e7
    transcriptionFilename <-
     "../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final_completed.lab"
#      "../data/transcriptionFiles/ECt2_B105/EC2_B105_transcription_final.lab"
    contiguousWaveEventsFilenamePattern <- 
     "results/%s/contiguousWaveEventsFromElec%03dToElec%03d.RData"
    figFilenamePattern <- 
     "figures/%s/histLatenciesBtwCVSsAndPositiveSpeedCWEInitiationsFromElec%03dToElec%03d.eps"

    figFilename <- sprintf(figFilenamePattern, sessionName, min(elecNumbers),
                                               max(elecNumbers))
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
    contiguousWaveEventsFilename <- sprintf(contiguousWaveEventsFilenamePattern,
                                             sessionName,
                                             min(elecNumbers), 
                                             max(elecNumbers))
    cwes <- get(load(file=contiguousWaveEventsFilename))
    cwes <- cwes[cwes[,2]>cwesStartTime,]
    cwes <- cwes[cwes[,2]-cwes[,1]>minCWEDuration,] # only look at CWEs longer than minCWEDuration
    cwes <- cwes[which(cwes[,3]<0),] # only look at CWEs with negative speeds

    nCWEsInitSilence <- 0
    nCWEsInitCVS <- 0
    nCWEsTermSilence <- 0
    nCWEsTermCVS <- 0
    nCWEsInitAndTermSilence <- 0
    nCWEsInitAndTermCVS <- 0
    for(i in 1:nrow(cwes)) {
show(sprintf("CWE=%d, nCWEsInitSilence=%d", i, nCWEsInitSilence))
show(sprintf("CWE=%d, nCWEsTermSilence=%d", i, nCWEsTermSilence))
show(sprintf("CWE=%d, nCWEsInitCVS=%d", i, nCWEsInitCVS))
show(sprintf("CWE=%d, nCWEsTermCVS=%d", i, nCWEsTermCVS))
        # check if the current cwe starts in silence or in a CVS
        cweInitiationTime <- cwes[i, 1]
        priorTermIndices <- which(infoTerm$time<cweInitiationTime)
        priorInfoTerm <- infoTerm[priorTermIndices,]
        indexCVSTermPrecedingCWEInitTime <- nrow(priorInfoTerm) # index of the CVS termination preceding cweInitiationTime
        if(indexCVSTermPrecedingCWEInitTime==nrow(infoInit) || 
            infoInit$time[indexCVSTermPrecedingCWEInitTime+1]>cweInitiationTime) { # if next CVS initation occurs after cweInitationTime
            initSilence <- TRUE
            nCWEsInitSilence <- nCWEsInitSilence+1
        } else {
            initSilence <- FALSE
            nCWEsInitCVS <- nCWEsInitCVS+1
        }
        # check if the current cwe terminates in silence or in a CVS
        cweTerminationTime <- cwes[i, 2]
        priorTermIndices <- which(infoTerm$time<cweTerminationTime)
        priorInfoTerm <- infoTerm[priorTermIndices,]
        indexCVSTermPrecedingCWETermTime <- nrow(priorInfoTerm) # index of the CVS termination preceding cweTermiationTime
        if(indexCVSTermPrecedingCWETermTime==nrow(infoInit) || infoInit$time[indexCVSTermPrecedingCWETermTime+1]>cweTerminationTime) { # if next CVS initation occurs after cweTerminationTime
            termSilence <- TRUE
            nCWEsTermSilence <- nCWEsTermSilence+1
        } else {
            termSilence <- FALSE
            nCWEsTermCVS <- nCWEsTermCVS+1
        }
        if(initSilence && termSilence) {
            nCWEsInitAndTermSilence <- nCWEsInitAndTermSilence + 1
        } else {
            if(!initSilence && !termSilence) {
                nCWEsInitAndTermCVS <- nCWEsInitAndTermCVS + 1
            }
        }
    }
    cwesInit <- c(rep("silent", times=nCWEsInitSilence), rep("talking", times=nCWEsInitCVS))
    cwesTerm <- c(rep("silent", times=nCWEsTermSilence), rep("talking", times=nCWEsTermCVS))
    cwesInitAndTerm <- c(rep("silent", times=nCWEsInitAndTermSilence), rep("talking", times=nCWEsInitAndTermCVS))
    df <- data.frame(twPhase=c(rep("init", length(cwesInit)),
                                rep("term", length(cwesTerm)),
                                rep("init&term", length(cwesInitAndTerm))), 
                     behavior=c(cwesInit, cwesTerm, cwesInitAndTerm))
    df$twPhase <- factor(df$twPhase, levels=c("init", "term", "init&term"))
    p <- ggplot(data=df, mapping=aes(x=twPhase))
    p <- p + geom_bar(mapping=aes(fill=behavior))
    p <- p + scale_fill_discrete(name=legendName)
    p <- p + xlab(xlab)
    p <- p + ylab(ylab)
    print(p)

    browser()
}

processAll()

rm(processAll)
