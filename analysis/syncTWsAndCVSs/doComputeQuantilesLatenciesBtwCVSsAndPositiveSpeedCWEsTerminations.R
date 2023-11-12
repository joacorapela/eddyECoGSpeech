
source("doLoadSources.R")

processAll <- function() {
    require(moments)

    sessionName <- "EC2_B105"
    elecNumbers <- 136:141
    significance <- .01
    minCWEDuration <- 0.36
    cwesStartTime <- 30 # seconds
    transcriptionSampleRate <- 1e7
    transcriptionFilename <-
     "../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final_completed.lab"
    contiguousWaveEventsFilenamePattern <- 
     "results/%s/contiguousWaveEventsFromElec%03dToElec%03dSign%.02f.RData"
    resultsFilenamePattern <-
     "results/%s/quantilesLatenciesBtwCVSsAndPositiveSpeedCWEsTerminationsSign%.02f.RData"
    resultsFilename <- sprintf(resultsFilenamePattern, sessionName, significance)

    infoInit <- getInfoCVSsInitiations(transcriptionFilename=
                                         transcriptionFilename,
                                        transcriptionSampleRate=
                                         transcriptionSampleRate,
                                        ecogSampleRate=transcriptionSampleRate)
    infoTerm <- getInfoCVSsTerminations(transcriptionFilename=
                                          transcriptionFilename,
                                         transcriptionSampleRate=
                                          transcriptionSampleRate,
                                         ecogSampleRate=transcriptionSampleRate)
    contiguousWaveEventsFilename <- sprintf(contiguousWaveEventsFilenamePattern,
                                             sessionName,
                                             elecNumbers[1], 
                                             elecNumbers[length(elecNumbers)],
                                             significance)
    cwes <- get(load(file=contiguousWaveEventsFilename))
    cwes <- cwes[cwes[,2]>cwesStartTime,]
    cwes <- cwes[cwes[,2]-cwes[,1]+saveDT>minCWEDuration,] # only look at CWEs longer than minCWEDuration
    cwes <- cwes[which(cwes[,3]>0),] # only look at CWEs with positive speeds

    cvsStarts <- infoInit$time
    cvsEnds <- infoTerm$time
    latencies <- array(data=NA, dim=length(nrow(cwes)))
    for(i in 1:nrow(cwes)) {
        cweStart <- cwes[i, 1]
        cweEnd <- cwes[i, 2]
        if(cweOverlapsCVSs(cweStart=cweStart, 
                            cweEnd=cweEnd,
                            cvsStarts=cvsStarts, 
                            cvsEnds=cvsEnds)) {
            index <- which.min(abs(cvsEnds-cweEnd))
            latency <- cvsEnds[index]-cweEnd
            latencies[i] <- latency
        }
    }
    quantiles <- quantile(x=latencies, probs=c(.05, .95), na.rm=TRUE)

    show(sprintf("(0.05, 0.95) quantiles=(%.02f, %.02f)", 
                 quantiles[1], quantiles[2]))
    save(file=resultsFilename, quantiles)
}

processAll()

rm(processAll)
