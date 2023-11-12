
source("doLoadSources.R")

processAll <- function() {
    sessionName <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    zScore <- TRUE
    elecNumber <- 136
    xlim <- c(693.0, 697.0)
    sizeAnnotate <- 5
    bandpassedFilenamePattern <- "results/%s/bandpassedFilteredFrom%.02fTo%.02fOrder%02dWav%d%d.RData"
    htFilenamePattern <- "results/%s/htFilteredFrom%.02fTo%.02fOrder%02dZScored%dWav%d%d.RData"
    nlflvFilenamePattern <- "results/%s/nlflvFilteredFrom%.02fTo%.02fOrder%02dWav%d%d.RData"

    groupAndElecNumber <- getGroupAndElecNumber(elecNumber=elecNumber)
    bandpassedFilename <- sprintf(bandpassedFilenamePattern, 
                                   sessionName,
                                   lowCutoff, highCutoff, order,
                                   groupAndElecNumber$groupNumber, 
                                   groupAndElecNumber$elecNumber)
    if(file.exists(bandpassedFilename)) {
        res <- get(load(bandpassedFilename))
        bpTimes <- ((1:length(res$filteredECoG))-1)/res$ecogSampleRate
        samplesToKeep <- which(xlim[1]<=bpTimes & bpTimes<=xlim[2])
        bpSubset <- res$filteredECoG[samplesToKeep]
        bpTimesSubset <- bpTimes[samplesToKeep]
    } else {
        warning(sprintf("%s not found", bandpassedFilename))
    }
    htFilename <- sprintf(htFilenamePattern,
                           sessionName,
                           lowCutoff, highCutoff, order, zScore,
                           groupAndElecNumber$groupNumber, 
                           groupAndElecNumber$elecNumber)
    if(file.exists(htFilename)) {
        res <- get(load(htFilename))
        htTimes <- ((1:length(res$ht))-1)/res$ecogSampleRate
        samplesToKeep <- which(xlim[1]<=htTimes & htTimes<=xlim[2])
        htSubset <- res$ht[samplesToKeep]
        htTimesSubset <- htTimes[samplesToKeep]
    } else {
        warning(sprintf("%s not found", htFilename))
    }

    nlflvFilename <- sprintf(nlflvFilenamePattern, 
                              sessionName, 
                              lowCutoff, highCutoff, order,
                              groupAndElecNumber$groupNumber, 
                              groupAndElecNumber$elecNumber)
    if(file.exists(nlflvFilename)) {
        res <- get(load(nlflvFilename))
        nlflvTimes <- ((1:length(res$nlflv))-1)/res$ecogSampleRate
        samplesToKeep <- which(xlim[1]<=nlflvTimes & nlflvTimes<=xlim[2])
        nlflvSubset <- res$nlflv[samplesToKeep]
        nlflvTimesSubset <- nlflvTimes[samplesToKeep]
    } else {
        warning(sprintf("%s not found", nlflvFilename))
    }

    bpDF <- data.frame(time=bpTimesSubset, bp=bpSubset)
    p0 <- ggplot(data=bpDF, mapping=aes(x=time, y=bp))
    p0 <- p0 + geom_line()
    p0 <- p0 + xlab("Time (sec)")
    p0 <- p0 + ylab("Voltages (V)")
    # print(p0)

    htDF <- data.frame(ht=htSubset)
    p1 <- ggplot(data=htDF, mapping=aes(x=Re(ht), y=Im(ht)))
    p1 <- p1 + geom_point()
    p1 <- p1 + xlab("Real")
    p1 <- p1 + ylab("Imaginary")
    # print(p1)

    dfPhases <- data.frame(time=c(htTimesSubset, nlflvTimesSubset),
                            phase=c(Arg(htSubset), nlflvSubset),
                            type=factor(c(rep("ht", times=length(htSubset)),
                                           rep("nlflv", 
                                               times=length(nlflvSubset)))))
    p2 <- ggplot(data=dfPhases, mapping=aes(x=time, y=phase, 
                                                    group=type, colour=type))
    p2 <- p2 + geom_line()
    p2 <- p2 + xlab("Time (sec)")
    p2 <- p2 + ylab("Phase (radians)")
    p2 <- p2 + guides(colour=guide_legend(title="Type"))
    # print(p2)

    grid.arrange(p0, p1, p2, ncol=2)
    # grid.newpage()
    # grid.draw(rbind(ggplotGrob(p2), ggplotGrob(p2), size="last"))

    browser()
}

processAll()
