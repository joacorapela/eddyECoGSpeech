
source("doLoadSources.R")

processAll <- function() {
    sessionName <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    zScore <- TRUE
    elecNumbers <- c(133, 134, 135)
    xlim <- c(320.0, 330.0)
    sizeAnnotate <- 5
    bandpassedFilenamePattern <- "results/%s/bandpassedFilteredFrom%.02fTo%.02fOrder%02dWav%d%d.RData"
    htFilenamePattern <- "results/%s/htFilteredFrom%.02fTo%.02fOrder%02dZScored%dWav%d%d.RData"
    latenciesFromLastPeakFilenamePattern <- "results/%s/latenciesFromLastPeakFilteredFrom%.02fTo%.02fOrder%02dWav%d%d.RData"

    bandpassedSignals <- NA
    hts <- NA
    latenciesFromLastPeak <- NA
    for(elecNumber in elecNumbers) {
        show(sprintf("Processing electrode %d", elecNumber))
        res <- getGroupAndElecNumber(elecNumber=elecNumber)
        groupNumber <- res$groupNumber
        elecNumber <- res$elecNumber

        bandpassedFilename <- sprintf(bandpassedFilenamePattern, 
                                       sessionName,
                                       lowCutoff, highCutoff, order,
                                       groupNumber, elecNumber)
        if(file.exists(bandpassedFilename)) {
            res <- get(load(bandpassedFilename))
            times <- ((1:length(res$filteredECoG))-1)/res$ecogSampleRate
            samplesToKeep <- which(xlim[1]<=times & times<=xlim[2])
            if(is.na(bandpassedSignals[1])) {
                bandpassedSignals <- times[samplesToKeep]
            }
            bandpassedSignals <- cbind(bandpassedSignals, 
                                        res$filteredECoG[samplesToKeep])
        } else {
            warning(sprintf("%s not found", bandpassedFilename))
        }
        htFilename <- sprintf(htFilenamePattern,
                               sessionName,
                               lowCutoff, highCutoff, order, zScore,
                               groupNumber, elecNumber)
        if(file.exists(htFilename)) {
            res <- get(load(htFilename))
            times <- ((1:length(res$ht))-1)/res$ecogSampleRate
            samplesToKeep <- which(xlim[1]<=times & times<=xlim[2])
            if(is.na(hts[1])) {
                hts <- c()
            }
            hts <- cbind(hts, res$ht[samplesToKeep])
        } else {
            warning(sprintf("%s not found", htFilename))
        }
        latenciesFromLastPeakFilename <- 
         sprintf(latenciesFromLastPeakFilenamePattern, 
                  sessionName, lowCutoff, highCutoff, order, 
                  groupNumber, elecNumber)
        if(file.exists(latenciesFromLastPeakFilename)) {
            res <- get(load(latenciesFromLastPeakFilename))
            times <- res$times
            samplesToKeep <- which(xlim[1]<=times & times<=xlim[2])
            if(is.na(latenciesFromLastPeak[1])) {
                latenciesFromLastPeak <- times[samplesToKeep]
            }
            latenciesFromLastPeak <- 
             cbind(latenciesFromLastPeak, 
                    res$latenciesFromLastPeak[samplesToKeep])
        } else {
            warning(sprintf("%s not found", latenciesFromLastPeakFilename))
        }
    }
    colMaxs <- apply(X=bandpassedSignals[,2:ncol(bandpassedSignals)], 
                      MARGIN=2, FUN=max)
    scaledBandpassedSignals <- bandpassedSignals%*%diag(c(1, 1/colMaxs))
    colnames(scaledBandpassedSignals) <- c("time", sprintf("%d", elecNumbers))
    colnames(hts) <- sprintf("%d", elecNumbers)
    colnames(latenciesFromLastPeak) <- c("time", sprintf("%d", elecNumbers))

    scaledBandpassedSignalsMelted <- melt(as.data.frame(scaledBandpassedSignals), 
                                     id.vars="time")
    p1 <- ggplot(data=scaledBandpassedSignalsMelted, 
                  mapping=aes(x=time, y=value, group=variable, 
                                      colour=variable))
    p1 <- p1 + geom_line()
    p1 <- p1 + xlab("Time (sec)")
    p1 <- p1 + ylab("Voltage")
    p1 <- p1 + guides(colour=guide_legend(title="Electrode"))
    p1 <- p1 + theme_bw()
    # print(p1)

    htsMelted <- melt(as.data.frame(hts))
    p2 <- ggplot(data=htsMelted, mapping=aes(x=Re(value), y=Im(value), group=variable, colour=variable))
    p2 <- p2 + geom_point()
    p2 <- p2 + xlab("Real")
    p2 <- p2 + ylab("Imaginary")
    p2 <- p2 + guides(colour=guide_legend(title="Electrode"))
    # p2 <- p2 + geom_hline(yintercept=0, colour="gray")
    # p2 <- p2 + geom_vline(xintercept=0, colour="gray")
    p2 <- p2 + theme_bw()
    for(i in 1:ncol(hts)) {
        p2 <- p2 + annotate("text", label="X", x=Re(hts[1,i]), y=Im(hts[1,i]), size=sizeAnnotate)
        p2 <- p2 + annotate("text", label="O", x=Re(hts[nrow(hts),i]), y=Im(hts[nrow(hts),i]), size=sizeAnnotate)
    }
    # print(p2)

    latenciesFromLastPeakMelted <- melt(as.data.frame(latenciesFromLastPeak), id.vars="time")
    p3 <- ggplot(data=latenciesFromLastPeakMelted, mapping=aes(x=time, y=value, group=variable, colour=variable))
    p3 <- p3 + geom_line()
    p3 <- p3 + xlab("Time (sec)")
    p3 <- p3 + ylab("Latency (sec)")
    p3 <- p3 + guides(colour=guide_legend(title="Electrode"))
    p3 <- p3 + theme_bw()
    # print(p3)

    grid.arrange(p1, p2, p3, ncol=2)

    browser()
}

processAll()
