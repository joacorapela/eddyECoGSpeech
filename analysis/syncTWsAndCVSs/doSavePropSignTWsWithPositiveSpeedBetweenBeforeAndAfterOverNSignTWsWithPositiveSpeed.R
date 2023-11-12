
source("doLoadSources.R")

processAll <- function() {
    sessionName <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    saveFromTime <- 0
    saveToTime <- 700
    saveDT <- .1
    elecNumbers <- 136:141
    significance <- .01
    # beforeLags <- c(0.3)
    # afterLags <- c(0)
    beforeLags <- seq(from=-.3, to=.5, by=.05)
    afterLags <- seq(from=-.5, to=0, by=.05)
    transcriptionSampleRate <- 1e7
    transcriptionFilename <-
     "../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final_completed.lab"
#      "../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final.lab"
    waveEventsFilenamePattern <- 
     "results/%s/waveEventsFilteredFrom%.02fTo%.02fOrder%02dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.%s"
    resultsFilenamePattern <-
     "results/%s/propSignTWsWithPositiveSpeedBetweenBeforeAndAfterOverNSignTWsWithPositiveSpeed.RData"

    resultsFilename <- sprintf(resultsFilenamePattern, sessionName)
    waveEventsFilename <- sprintf(waveEventsFilenamePattern, sessionName, lowCutoff, highCutoff, order, saveFromTime, saveToTime, saveDT, elecNumbers[1], elecNumbers[length(elecNumbers)], "RData")
    waveEvents <- get(load(file=waveEventsFilename))
browser()
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
    cvsProductionTimingInfo <- data.frame(startTime=infoInit$time,
                                           endTime=infoTerm$time)
    propsAtLags <- matrix(NA, ncol=length(afterLags), nrow=length(beforeLags))
    totalsAtLags <- matrix(NA, ncol=length(afterLags), nrow=length(beforeLags))
    for(i in 1:length(beforeLags)) {
        show(sprintf("Processing %d (%d)", i, length(beforeLags)))
        beforeLag <- beforeLags[i]
        for(j in 1:length(afterLags)) {
            afterLag <- afterLags[i]
            res <- computePropSignTWsWithPosSpeedBtwBeforeAndAfterOverNSignTWsWithPosSpeed(
                    cvsProductionTimingInfo=cvsProductionTimingInfo,
                    waveEvents=waveEvents,
                    beforeLag=beforeLags[i],
                    afterLag=afterLags[j],
                    significance=significance)
            propsAtLags[i, j] <- res$proportion
            totalsAtLags[i, j] <- res$total
        }
    }
    results <- list(propsAtLags=propsAtLags, totalsAtLags=totalsAtLags)
    save(file=resultsFilename, propsAtLags=propsAtLags, totalsAtLags=totalsAtLags, beforeLags=beforeLags, afterLags=afterLags)
    maxIndex <- which.max(propsAtLags)
    colMax <- ((maxIndex-1)%/%nrow(propsAtLags))+1
    rowMax <- ((maxIndex-1)%%nrow(propsAtLags))+1
    y <- beforeLags
    x <- afterLags
    data <- expand.grid(Y=y, X=x)
    data$Z <- as.vector(propsAtLags)
    # Levelplot with ggplot2
    library(ggplot2)
    p <- ggplot(data=data, mapping=aes(x=X, y=Y, z=Z)) 
    p <- p + geom_tile(mapping=aes(fill = Z))
    p <- p + theme_bw()
    p <- p + annotate("text", x=afterLags[colMax], y=beforeLags[rowMax], label=sprintf("%.02f(%d)", propsAtLags[maxIndex], totalsAtLags[maxIndex]), color="red", size=4)
    p <-  p + xlab("Lag after end of CVS")
    p <-  p + ylab("Lag before start of CVS")
    print(p)

    browser()
}

processAll()

rm(processAll)

