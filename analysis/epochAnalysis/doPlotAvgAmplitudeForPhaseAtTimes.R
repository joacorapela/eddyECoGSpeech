
source("doLoadSources.R")

processAll <- function() {
    freqPhase <- 1/1.62
    freqAmp <- 100.0
    lowCutoff <- 0.4
    highCutoff <- 0.8
    # elecNumbers <- 6:12
    # elecNumbers <- 7:16
    elecNumbers <- 7:10
    groupNumbers <- rep(3, times=length(elecNumbers))
    transcriptionSampleRate <- 1e7
    transcriptionFilename <-
     "../data/matlabData/EC2_B105/EC2_B105_transcription_final.lab"
    htFilenamePattern <- 
     "results/EC2_B105/htFilteredFrom%.02fTo%.02fWav%d%d.RData"
    pacFilenamePattern <- 
     "results/EC2_B105/amplitudesBinnedByPhaseWav%d%d_freqPhase%.2f_freqAmp%.2f.RData"
    figFilenamePattern <- 
     "figures/EC2_B105/avgAmplitudeForPhaseAtTimesBPFrom%.02fBPTo%.02fEFrom%dETo%dTFrom%.02fTTo%.02f.eps"
    xlim <- c(390.0, 400.0) # time in sec
    xlab <- "Time (sec)"
    ylab <- bquote(paste("Mean Amplitude at ", .(freqAmp), " Hz (", mu, "V)"))
    keySize <- 1.0
    width = 12
    height = 4
    units = "in"

    absoluteElecNumbers <- (groupNumbers-1)*64+elecNumbers
    figFilename <- sprintf(figFilenamePattern, lowCutoff, highCutoff,
                                               min(absoluteElecNumbers),
                                               max(absoluteElecNumbers),
                                               xlim[1], xlim[2])
    dataMatrix <- NULL
    for(i in 1:length(elecNumbers)) {
        groupNumber = groupNumbers[i]
        elecNumber = elecNumbers[i]
        htFilename <- sprintf(htFilenamePattern, lowCutoff, highCutoff,
                                                 groupNumber, elecNumber)
        pacFilename <- sprintf(pacFilenamePattern, groupNumber, elecNumber,
                                                   freqPhase, freqAmp)
        show(sprintf("Processing electrode %d", absoluteElecNumbers[i]))
        res <- get(load(htFilename))
        phases <- Arg(res$ht)
        ecogSampleRate <- res$ecogSampleRate
        res <- get(load(pacFilename))
        phaseBinsBreaks <- res$phaseBinsBreaks
        amplitudesBinnedByPhase <- res$allAmplitudesBinnedByPhase
        if(is.null(dataMatrix)) {
            times <- ((1:length(phases))-1)/ecogSampleRate
            samplesToShow <- which(xlim[1]<=times & times <=xlim[2])
            timesToShow <- times[samplesToShow]
            dataMatrix <- matrix(NA, nrow=length(elecNumbers),
                                     ncol=length(timesToShow))
            colnames(dataMatrix) <- sprintf("%.02f", timesToShow)
            rownames(dataMatrix) <- sprintf("%d", absoluteElecNumbers)
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
        dataMatrix[i,] <- getPACAmplitudesForPhases(phases=
                                                      phases[samplesToShow],
                                                     phaseBinsBreaks=
                                                      phaseBinsBreaks,
                                                     amplitudesBinnedByPhase=
                                                      amplitudesBinnedByPhase)
    }
    nDataMatrix <- dataMatrix*1e6 # voltages in muV
    meltedDataMatrix <- melt(nDataMatrix, varnames=c("electrode", "time"))
    colnames(meltedDataMatrix) <- c(colnames(meltedDataMatrix)[1:2], "voltage")

    eventsFactor <- factor(c(rep(x="init", times=length(initTimesToShow)), rep(x="cvTrans", times=length(cvTransTimesToShow)), rep(x="term", times=length(termTimesToShow))), levels=c("init", "cvTrans", "term"))
    eventsDF <- data.frame(time=c(initTimesToShow, cvTransTimesToShow, termTimesToShow), event=eventsFactor)
    p <- ggplot()
    p <- p + geom_line(data=meltedDataMatrix, mapping=aes(x=time, y=voltage, colour=factor(electrode), group=factor(electrode)), size=2)
    if(nrow(eventsDF)>0) {
        p <- p + geom_vline(data=eventsDF, mapping=aes(xintercept=time, linetype=event))
    }
    p <- p + scale_colour_discrete(name="electrode")
    p <- p + scale_linetype_manual(name="event", 
                                    values=c(init="solid", cvTrans="dashed", term="dotted"),
                                    labels=c(init="CVS initiation", cvTrans="CV transition", term="CVS termintation"))
    p <- p + xlab(xlab)
    p <- p + ylab(ylab)
    p <- p + guides(col = guide_legend(reverse=TRUE))
    # p <- p + guides(line = guide_legend(keywidth = 10, keyheight = 10))
    # p <- p + theme(legend.key.size = unit(keySize, "cm")) 
    ggsave(filename=figFilename, width=width, height=height, units=units)
    # print(p)

    browser()
}

processAll()

rm(processAll)
