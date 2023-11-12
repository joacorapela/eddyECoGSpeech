
source("doLoadSources.R")

processAll <- function() {
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    # elecNumbers <- 6:12
    # elecNumbers <- 7:16
    elecNumbers <- 136:140
    # elecNumbers <- seq(from=184, to=232, by=16)
    # elecNumbers <- seq(from=183, to=247, by=16)
    # elecNumbers <- seq(from=181, to=245, by=16)
    # elecNumbers <- seq(from=192, to=256, by=16)
    transcriptionSampleRate <- 1e7
    transcriptionFilename <-
     "../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final_completed.lab"
    bandpassedECoGFilenamePattern <- 
     "results/EC2_B105/bandpassedFilteredFrom%.02fTo%.02fOrder%02dWav%d%d.RData"
    figFilenamePattern <- 
     "figures/EC2_B105/travellingWaveBPFrom%.02fBPTo%.02fOrder%02dEFrom%dETo%dTFrom%.02fTTo%.02fNormalized%d.eps"
    # xlim <- 390.0 + c(0, 10.0) # time in sec
    xlim <- 381.0 + c(0, 5.0) # time in sec
    xlab <- "Time (sec)"
    normalizedYlab <- "Z-Scored Voltage (a.u.)"
    unNormalizedYlab <- expression(paste("Voltage (", mu, "V)"))
    keySize <- 1.0
    # normalize <- FALSE
    normalize <- TRUE
    width = 12
    height = 4
    units = "in"

    figFilename <- sprintf(figFilenamePattern, lowCutoff, highCutoff, order,
                                               min(elecNumbers),
                                               max(elecNumbers),
                                               xlim[1], xlim[2], normalize)
    dataMatrix <- NULL
    for(i in 1:length(elecNumbers)) {
        elecNumber <- elecNumbers[i]
        res <- getGroupAndElecNumber(elecNumber=elecNumber)
        bandpassedECoGFilename <- sprintf(bandpassedECoGFilenamePattern, 
                                           lowCutoff, highCutoff, order,
                                           res$groupNumber, res$elecNumber)
        show(sprintf("Processing electrode %d", elecNumber))
        bandpassedECoG <- get(load(bandpassedECoGFilename))
        filteredECoGData <- bandpassedECoG$filteredECoGData
        ecogSampleRate <- bandpassedECoG$ecogSampleRate
        if(is.null(dataMatrix)) {
            times <- (1:length(filteredECoGData))/ecogSampleRate
            samplesToShow <- which(xlim[1]<=times & times <=xlim[2])
            timesToShow <- times[samplesToShow]
            dataMatrix <- matrix(NA, nrow=length(elecNumbers),
                                     ncol=length(timesToShow))
            colnames(dataMatrix) <- sprintf("%.02f", timesToShow)
            rownames(dataMatrix) <- sprintf("%d", elecNumbers)
            infoInit <- getInfoCVSsInitiations(transcriptionFilename=
                                                 transcriptionFilename,
                                                transcriptionSampleRate=
                                                 transcriptionSampleRate,
                                                ecogSampleRate=ecogSampleRate)
            infoCVTrans <- getInfoCVSsCVTransitions(transcriptionFilename=
                                                      transcriptionFilename,
                                                     transcriptionSampleRate=
                                                      transcriptionSampleRate,
                                                     ecogSampleRate=
                                                      ecogSampleRate)
            infoTerm <- getInfoCVSsCVTerminations(transcriptionFilename=
                                                    transcriptionFilename,
                                                   transcriptionSampleRate=
                                                    transcriptionSampleRate,
                                                   ecogSampleRate=
                                                    ecogSampleRate)
            initSamplesToShow <- which(xlim[1]<=infoInit$times & 
                                                infoInit$times<=xlim[2])
            initTimesToShow <- infoInit$times[initSamplesToShow]
            cvTransSamplesToShow <- which(xlim[1]<=infoCVTrans$times & 
                                                   infoCVTrans$times<=xlim[2])
            cvTransTimesToShow <- infoCVTrans$times[cvTransSamplesToShow]
            termSamplesToShow <- which(xlim[1]<=infoTerm$times & 
                                                   infoTerm$times<=xlim[2])
            termTimesToShow <- infoTerm$times[termSamplesToShow]
        }
        dataMatrix[i,] <- filteredECoGData[samplesToShow]
    }
    if(normalize) {
        nDataMatrix <- t(scale(t(dataMatrix)))
    } else {
        nDataMatrix <- dataMatrix*1e6 # voltages in muV
    }
    meltedDataMatrix <- melt(nDataMatrix, varnames=c("electrode", "time"))
    colnames(meltedDataMatrix) <- c(colnames(meltedDataMatrix)[1:2], "voltage")

    eventsFactor <- factor(c(rep(x="init", times=length(initTimesToShow)), rep(x="cvTrans", times=length(cvTransTimesToShow)), rep(x="term", times=length(termTimesToShow))), levels=c("init", "cvTrans", "term"))
    eventsDF <- data.frame(time=c(initTimesToShow, cvTransTimesToShow, termTimesToShow), event=eventsFactor)
    p <- ggplot()
    p <- p + geom_line(data=meltedDataMatrix, mapping=aes(x=time, y=voltage, colour=factor(electrode), group=factor(electrode)), size=3)
    if(nrow(eventsDF)>0) {
        p <- p + geom_vline(data=eventsDF, mapping=aes(xintercept=time, linetype=event))
    }
    p <- p + scale_colour_discrete(name="electrode")
    p <- p + scale_linetype_manual(name="event", 
                                    values=c(init="solid", cvTrans="dashed", term="dotted"),
                                    labels=c(init="CVS initiation", cvTrans="CV transition", term="CVS termintation"))
    p <- p + xlab(xlab)
    if(normalize) {
        p <- p + ylab(normalizedYlab)
    } else {
        p <- p + ylab(unNormalizedYlab)
    }
    p <- p + guides(col = guide_legend(reverse=TRUE))
    # p <- p + guides(line = guide_legend(keywidth = 10, keyheight = 10))
    # p <- p + theme(legend.key.size = unit(keySize, "cm")) 
    ggsave(filename=figFilename, width=width, height=height, units=units)

    browser()
}

processAll()

rm(processAll)
