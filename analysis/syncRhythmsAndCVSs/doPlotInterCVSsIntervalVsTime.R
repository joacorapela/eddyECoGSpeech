
source("doLoadSources.R")

processAll <- function() {
    sessionLabel <- "EC2_B1"
    transcriptionSampleRate <- 1e7
    runningWinSizeSamples <- 5
    xlab <- "Time (sec)"
    ylab <- "Inter-CVS Interval (sec)"
    hlineCol <- "red"
    width <- 6
    height <- 6
    annotationPattern <- "median inter-CVS interval=%.02f sec and frequency=%.02f Hz"
    xAnnotation <- -Inf
    yAnnotation <- Inf
    hjustAnnotation <- 0
    vjustAnnotation <- 1
    colorAnnotation <- "red"
    # transcriptionFilenamePattern <- "../data/transcriptionFiles/%s/%s_transcription_final_completed.lab"
    transcriptionFilenamePattern <- "../data/matlabData/%s/%s_transcription_final.lab"
    figFilenamePattern <- "figures/%s/interCVSIntervalsVsTimes.png"

    transcriptionFilename <- sprintf(transcriptionFilenamePattern, 
                                      sessionLabel, sessionLabel)
    figFilename <- sprintf(figFilenamePattern, sessionLabel)
    resInitiations <- getInfoCVSsInitiations(transcriptionFilename=
                                               transcriptionFilename,
                                              transcriptionSampleRate=
                                               transcriptionSampleRate,
                                              ecogSampleRate=1.0)
    interCVSIntervals <- 
     resInitiations$times[2:length(resInitiations$times)]-
     resInitiations$times[1:(length(resInitiations$times)-1)]
     transcriptionSampleRate
    interCVSIntervalsTimes <- 
     resInitiations$times[2:length(resInitiations$times)]
    res <- runningFunction(x=interCVSIntervals, fun=median, 
                                                winSize=runningWinSizeSamples)
    filteredInterCVSIntervals <- res$runningValue
    filteredInterCVSIntervalsTimes <- interCVSIntervalsTimes[res$validIndices]
    medianInterCVSIntervalSec <- median(interCVSIntervals)
    medianInterCVSIntervalHz <- 1/medianInterCVSIntervalSec
    annotation <- sprintf(annotationPattern, medianInterCVSIntervalSec, 
                                             medianInterCVSIntervalHz)

    df <- data.frame(times=filteredInterCVSIntervalsTimes,
                      interCVSIntervals=filteredInterCVSIntervals)
    p <- ggplot(data=df, mapping=aes(x=times, y=interCVSIntervals))
    p <- p + geom_point()
    p <- p + geom_line()
    p <- p + geom_hline(yintercept=medianInterCVSIntervalSec, color=hlineCol)
    p <- p + annotate("text", label=annotation, x=xAnnotation, y=yAnnotation, 
                      hjust=hjustAnnotation, vjust=vjustAnnotation, 
                      color=colorAnnotation)
    p <- p + xlab(xlab)
    p <- p + ylab(ylab)
  
    ggsave(plot=p, filename=figFilename, width=width, height=height)
    print(p)
    browser()
}

processAll()

rm(processAll)
