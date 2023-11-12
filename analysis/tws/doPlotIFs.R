
source("doLoadSources.R")

processAll <- function() {
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    # lowCutoff <- 1.0
    # highCutoff <- 1.4
    # order <- 3
    transcriptionSampleRate <- 1e7
    zScore <- TRUE
    sessionLabel <- "EC2_B105"
    elecNumbers <- 1:256
    fromTime <- 00
    duration <- 700
    actualSampleRate <- 24.61
    # elecNumbersToPlot <- 6+(0:15)*16
    # elecNumbersToPlot <- c(215, 199, 193, 167, 151)
    # elecNumbersToPlot <- c(214, 198, 192, 166, 150)
    # elecNumbersToPlot <- c(22, 38, 54, 70, 86)
    # elecNumbersToPlot <- c(200, 184, 152, 136, 120)
    elecNumbersToPlot <- 133:135
    xlim <- c(317.0, 322.0)
    ylim <- c(-2, 2)
    cvsTimesColor <- "black"
    cvsTimesLinetype <- "dotted"
    transcriptionFilenamePattern <- "../data/transcriptionFiles/%s/%s_transcription_final_completed.lab"
    ifsFilenamePattern <- "results/%s/ifsHTFilteredFrom%.02fTo%.02fOrder%02dZScored%dFPS%.02fElecFrom%03dTo%03dTimeFrom%.02fTo%.02f.RData"

    transcriptionFilename <- sprintf(transcriptionFilenamePattern, sessionLabel, sessionLabel)
    resInitiations <- getInfoCVSsInitiations(transcriptionFilename=
                                               transcriptionFilename,
                                              transcriptionSampleRate=
                                               transcriptionSampleRate,
                                              ecogSampleRate=actualSampleRate)
    toTime <- fromTime + duration
    ifsFilename <- sprintf(ifsFilenamePattern, 
                            sessionLabel, lowCutoff, highCutoff, order, zScore,
                            actualSampleRate, 
                            min(elecNumbers), max(elecNumbers), 
                            fromTime, toTime)
    ifsRes <- get(load(ifsFilename))

    data <- data.frame(ifsRes$times)
    for(elecNumberToPlot in elecNumbersToPlot) {
        electrodeIndexInArray <- 
         getElectrodeIndexInArrayGGPlot(elecNumber=elecNumberToPlot)
        data <- cbind(data, ifsRes$ft[electrodeIndexInArray[1], 
                                       electrodeIndexInArray[2],])
    }
    colnames(data) <- c("time", sprintf("%03d", elecNumbersToPlot))

    dataM <- melt(data, id.vars="time")
    p <- ggplot()
    p <- p + geom_point(data=dataM, mapping=aes(x=time, y=value, 
                                                        group=variable, 
                                                        color=variable))
    p <- p + geom_vline(aes(xintercept=resInitiations$times), 
                         color=cvsTimesColor,
                         linetype=cvsTimesLinetype)
    p <- p + xlab("Time (sec)")
    p <- p + ylab("Instantaneous Frequency")
    p <- p + xlim(xlim)
    p <- p + ylim(ylim)
    p <- p + geom_hline(yintercept=0, colour="gray")
    p <- p + guides(colour=guide_legend(title="Electrode"))
    p <- p + theme_bw()
    print(p)
}

processAll()

rm(processAll)
