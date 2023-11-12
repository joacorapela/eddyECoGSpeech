
source("doLoadSources.R")

processAll <- function() {
    sessionName <- "EC2_B105"
    elecNumbers <- 136:141
    minCWEDuration <- 0.005
    cwesStartTime <- 30 # seconds
    width <- 6
    height <- 6
    units <- "in"
    xlab <- "CWE Phase"
    ylab <- "Count"
    legendName <- "Behavior"
    transcriptionSampleRate <- 1e7
    transcriptionFilename <-
     "../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final_completed.lab"
    randomizedTranscriptionFilename <-
     "../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final_completed_randomized.lab"
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
    randomizedInfoInit <- getInfoCVSsInitiations(transcriptionFilename=
                                         randomizedTranscriptionFilename,
                                        transcriptionSampleRate=
                                         transcriptionSampleRate,
                                        ecogSampleRate=transcriptionSampleRate)
    randomizedInfoTerm <- getInfoCVSsTerminations(transcriptionFilename=
                                          randomizedTranscriptionFilename,
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
    nCWEsRandomizedInitSilence <- 0
    nCWEsRandomizedInitCVS <- 0
    nCWEsRandomizedTermSilence <- 0
    nCWEsRandomizedTermCVS <- 0
    nCWEsRandomizedInitAndTermSilence <- 0
    nCWEsRandomizedInitAndTermCVS <- 0
    for(i in 1:nrow(cwes)) {
show(sprintf("CWE=%d, nCWEsInitSilence=%d", i, nCWEsInitSilence))
show(sprintf("CWE=%d, nCWEsTermSilence=%d", i, nCWEsTermSilence))
show(sprintf("CWE=%d, nCWEsInitCVS=%d", i, nCWEsInitCVS))
show(sprintf("CWE=%d, nCWEsTermCVS=%d", i, nCWEsTermCVS))
show(sprintf("CWE=%d, nCWEsRandomizedInitSilence=%d", i, nCWEsRandomizedInitSilence))
show(sprintf("CWE=%d, nCWEsRandomizedTermSilence=%d", i, nCWEsRandomizedTermSilence))
show(sprintf("CWE=%d, nCWEsRandomizedInitCVS=%d", i, nCWEsRandomizedInitCVS))
show(sprintf("CWE=%d, nCWEsRandomizedTermCVS=%d", i, nCWEsRandomizedTermCVS))
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

        randomizedPriorTermIndices <- which(randomizedInfoTerm$time<cweInitiationTime)
        randomizedPriorInfoTerm <- randomizedInfoTerm[randomizedPriorTermIndices,]
        randomizedIndexCVSTermPrecedingCWEInitTime <- nrow(randomizedPriorInfoTerm) # index of the CVS termination preceding cweInitiationTime
        if(randomizedIndexCVSTermPrecedingCWEInitTime==nrow(randomizedInfoInit) || 
            randomizedInfoInit$time[randomizedIndexCVSTermPrecedingCWEInitTime+1]>cweInitiationTime) { # if next CVS initation occurs after cweInitationTime
            randomizedInitSilence <- TRUE
            nCWEsRandomizedInitSilence <- nCWEsRandomizedInitSilence+1
        } else {
            randomizedInitSilence <- FALSE
            nCWEsRandomizedInitCVS <- nCWEsRandomizedInitCVS+1
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

        randomizedPriorTermIndices <- which(randomizedInfoTerm$time<cweTerminationTime)
        randomizedPriorInfoTerm <- randomizedInfoTerm[randomizedPriorTermIndices,]
        randomizedIndexCVSTermPrecedingCWETermTime <- nrow(randomizedPriorInfoTerm) # index of the CVS termination preceding cweTermiationTime
        if(randomizedIndexCVSTermPrecedingCWETermTime==nrow(randomizedInfoInit) || 
            randomizedInfoInit$time[randomizedIndexCVSTermPrecedingCWETermTime+1]>cweTerminationTime) { # if next CVS initation occurs after cweTerminationTime
            randomizedTermSilence <- TRUE
            nCWEsRandomizedTermSilence <- nCWEsRandomizedTermSilence+1
        } else {
            randomizedTermSilence <- FALSE
            nCWEsRandomizedTermCVS <- nCWEsRandomizedTermCVS+1
        }
        if(randomizedInitSilence && randomizedTermSilence) {
            nCWEsRandomizedInitAndTermSilence <- nCWEsRandomizedInitAndTermSilence + 1
        } else {
            if(!randomizedInitSilence && !randomizedTermSilence) {
                nCWEsRandomizedInitAndTermCVS <- nCWEsRandomizedInitAndTermCVS + 1
            }
        }
    }
    cwesInit <- c(rep("silent", times=nCWEsInitSilence), rep("talking", times=nCWEsInitCVS))
    cwesTerm <- c(rep("silent", times=nCWEsTermSilence), rep("talking", times=nCWEsTermCVS))
    cwesInitAndTerm <- c(rep("silent", times=nCWEsInitAndTermSilence), rep("talking", times=nCWEsInitAndTermCVS))
    cwesRandomizedInit <- c(rep("silent", times=nCWEsRandomizedInitSilence), rep("talking", times=nCWEsRandomizedInitCVS))
    cwesRandomizedTerm <- c(rep("silent", times=nCWEsRandomizedTermSilence), rep("talking", times=nCWEsRandomizedTermCVS))
    cwesRandomizedInitAndTerm <- c(rep("silent", times=nCWEsRandomizedInitAndTermSilence), rep("talking", times=nCWEsRandomizedInitAndTermCVS))
    df <- data.frame(twPhase=c(rep("init", length(cwesInit)),
                                rep("term", length(cwesTerm)),
                                rep("init&term", length(cwesInitAndTerm)),
                                rep("rInit", length(cwesRandomizedInit)),
                                rep("rTerm", length(cwesRandomizedTerm)),
                                rep("rInit&term", length(cwesRandomizedInitAndTerm))), 
                     behavior=c(cwesInit, cwesTerm, cwesInitAndTerm,
                                          cwesRandomizedInit,
                                          cwesRandomizedTerm,
                                          cwesRandomizedInitAndTerm))
    df$twPhase <- factor(df$twPhase, levels=c("init", "term", "init&term",
                                              "rInit", "rTerm", "rInit&term"))
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
