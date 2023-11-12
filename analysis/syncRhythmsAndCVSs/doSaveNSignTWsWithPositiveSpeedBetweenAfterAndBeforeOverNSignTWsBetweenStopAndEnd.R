
source("doLoadSources.R")

processAll <- function() {
    maxISI <- 2.7
    sessionName <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    saveFromTime <- 0
    saveToTime <- 700
    saveDT <- .1
    elecNumbers <- 136:141
    significance <- .01
    afterLags <- seq(from=0, to=1, by=.1)
    beforeLags <- seq(from=0, to=1, by=.1)
    transcriptionSampleRate <- 1e7
    transcriptionFilename <-
     "../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final.lab"
    waveEventsFilenamePattern <- 
     "results/%s/waveEventsFilteredFrom%.02fTo%.02fOrder%02dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.%s"
    resultsFilenamePattern <-
     "results/%s/propsOfSignTWsWithNegativeSpeedBetweenAfterAndBefore.RData"

    waveEventsFilename <- sprintf(waveEventsFilenamePattern, sessionName, lowCutoff, highCutoff, order, saveFromTime, saveToTime, saveDT, min(elecNumbers), max(elecNumbers), "RData")
    waveEvents <- get(load(file=waveEventsFilename))
    infoInit <- getInfoCVSsInitiations(transcriptionFilename=
                                         transcriptionFilename,
                                        transcriptionSampleRate=
                                         transcriptionSampleRate,
                                        ecogSampleRate=transcriptionSampleRate)
    infoCVTrans <- getInfoCVSsCVTransitions(transcriptionFilename=
                                              transcriptionFilename,
                                             transcriptionSampleRate=
                                              transcriptionSampleRate,
                                             ecogSampleRate=
                                              transcriptionSampleRate)
    infoTerm <- getInfoCVSsTerminations(transcriptionFilename=
                                          transcriptionFilename,
                                         transcriptionSampleRate=
                                          transcriptionSampleRate,
                                         ecogSampleRate=
                                          transcriptionSampleRate)
    cvsProductionTimingInfo <- data.frame(startTime=infoInit$time,
                                           transitionTime=infoCVTrans$time,
                                           endTime=infoTerm$time)
    propsAtLags <- matrix(NA, ncol=length(afterLags), nrow=length(beforeLags))
    totalsAtLags <- matrix(NA, ncol=length(afterLags), nrow=length(beforeLags))
    for(i in 1:length(afterLags)) {
        for(j in 1:length(beforeLags)) {
            res <- computePropSignTWsWithNegativeSpeedBetweenAfterAndBefore(
                    cvsProductionTimingInfo=cvsProductionTimingInfo,
                    waveEvents=waveEvents,
                    afterLag=afterLags[i],
                    beforeLag=beforeLags[j],
                    maxISI=maxISI,
                    significance=significance)
            propsAtLags[i, j] <- res$proportion
            totalsAtLags[i, j] <- res$total
        }
    }
    maxIndex <- which.max(propsAtLags)
    colMax <- ((maxIndex-1)%/%nrow(propsAtLags))+1
    rowMax <- ((maxIndex-1)%%nrow(propsAtLags))+1
    x <- afterLags
    y <- beforeLags
    data <- expand.grid(Y=y, X=x)
    data$Z <- as.vector(propsAtLags)
    # Levelplot with ggplot2
    library(ggplot2)
    p <- ggplot(data=data, mapping=aes(x=X, y=Y, z=Z)) 
    p <- p + geom_tile(mapping=aes(fill = Z))
    p <- p + theme_bw()
    p <- p + annotate("text", x=afterLags[colMax], y=beforeLags[rowMax], label=sprintf("%.02f(%d)", propsAtLags[maxIndex], totalsAtLags[maxIndex]), color="red", size=4)
    p <-  p + xlab("Lag after end of previous CVS")
    p <-  p + ylab("Lag before start of next CVS")
    print(p)

    browser()
}

computePropSignTWsWithNegativeSpeedBetweenAfterAndBefore <- 
 function(cvsProductionTimingInfo, waveEvents, afterLag, beforeLag, 
                                   maxISI, significance) {
    indicesBtwAfterAndBefore <- c()
    for(i in 1:(nrow(cvsProductionTimingInfo)-1)) {
        endTime <- cvsProductionTimingInfo[i, "endTime"]
        startTime <- cvsProductionTimingInfo[i+1, "startTime"]
        if((cvsProductionTimingInfo[i+1, "startTime"]-
             cvsProductionTimingInfo[i, "startTime"])<maxISI) {
            indicesBtwAfterAndBefore <- c(indicesBtwAfterAndBefore,
                                         which(waveEvents$pValue<significance &
                                                endTime+afterLag<=waveEvents$time &
                                                waveEvents$time<startTime-beforeLag))
        }
    }
    indicesWithNegativeSpeedBtwBeforeAndAfter <-
     which(waveEvents[indicesBtwAfterAndBefore,]$pValue<significance & 
            waveEvents[indicesBtwAfterAndBefore,]$speed<0)
    propSignTWsWithNegativeSpeedBtwAfterAndBefore <-
     length(indicesWithNegativeSpeedBtwBeforeAndAfter)/
      length(indicesBtwAfterAndBefore)
    nTWsWithNegativSpeedBtwAfterAndBefore <- length(indicesBtwAfterAndBefore)
    return(list(proportion=propSignTWsWithNegativeSpeedBtwAfterAndBefore,
                 total=nTWsWithNegativSpeedBtwAfterAndBefore))
}
    
processAll()

rm(processAll)

