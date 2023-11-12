
source("doLoadSources.R")

getColname <- function(displacement) {
    verticalLabel <- switch(sprintf("%d", displacement[1]), 
                             "-1"="bottom",
                              "0"="middle",
                              "1"="top")
    horizontalLabel <- switch(sprintf("%d", displacement[2]), 
                               "-1"="left",
                                "0"="center",
                                "1"="right")
    aColname <- sprintf("%s-%s", verticalLabel, horizontalLabel)
    return(aColname)
}

getSignificantPhaseCoherenceForPairOfElectrodes <- 
 function(pcsWithSign, i1, j1, i2, j2, significance) {
    indices <- which(pcsWithSign[,"i1"]==i1 & pcsWithSign[,"j1"]==j1 & 
                      pcsWithSign[,"i2"]==i2 & pcsWithSign[,"j2"]==j2)
    pcs <- pcsWithSign[indices, "pc"]
    pValues <- pcsWithSign[indices, "pValue"]
    delays <- pcsWithSign[indices, "delayInSecs"]
    signPCs <- pcs
    signPCs[pValues>significance] <- NaN

    answer <- list(delays=delays, signPCs=signPCs)
    return(answer)
}

getSignificantPhaseCoherencesAcrossTimeForElectrode <- function(pcsWithSign, 
                                                                 neighborSize,
                                                                 elecNumber,
                                                                 significance,
                                                                 nrows, ncols) {
    electrodeIndexInArray <- 
     getElectrodeIndexInArrayGGPlot(elecNumber=elecNumber)
    signPCs <- c()
    theColnames <- c()
    theRownames <- c()

    i <- electrodeIndexInArray[1]
    j <- electrodeIndexInArray[2]
    for(offsetX in c(-neighborSize:-1, 1:neighborSize)) {
        for(offsetY in c(-neighborSize:-1, 1:neighborSize)) {
            i2 <- i+offsetY
            j2 <- j+offsetX
            if(0<i2 && i2<nrows && 0<j2 && j2<ncols) {
                res <- getSignificantPhaseCoherenceForPairOfElectrodes(
                        pcsWithSign=pcsWithSign, i1=i, j1=j, i2=i2, j2=j2, 
                        significance)
                signPCs <- cbind(signPCs, res$signPCs)
                theColnames <- c(theColnames, sprintf("%d-%d", offsetY, offsetX))
                theRownames <- sprintf("%f", res$delays)
            }
        }
    }

    colnames(signPCs) <- theColnames
    rownames(signPCs) <- theRownames
    return(signPCs)
}

getTransitionSamplesForCVSsInGroup <- function(cvss, transitionSamples, 
                                                     groupOfCVSs) {
    selectedSamples <- c()
    for(aCVS in groupOfCVSs) {
        indices <- which(cvss==aCVS)
        if(length(indices)>0) {
            selectedSamples <- c(selectedSamples, transitionSamples[indices])
        }
    }
    return(selectedSamples)
}

processAll <- function() {
    delayFrom <- -.2
    delayTo <- .5
    delayBy <- .01
    neighborSize <- 3
    elecNumbersToPlot <- seq(from=256, to=1, by=-1)
    sessionLabel <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    fromTime <- 0
    toTime <- 700
    significance <- .01
    ylim <- c(0.0, 1.0)
    width <- 30
    height <- 30
    nrows <- 16
    ncols <- 16
    # groupOfCVSsFilename <- "../../results/EC2_B105/phonetics/cvsWithIVowel.txt"
    pcsWithSignFilenamePattern <- "results/%s/pcsWithSignFilteredFrom%.02fTo%.02fOrder%02dFromTime%.02fTo%.02fDelayFrom%.02fTo%.02fBy%.03fNeighborSize%dCVSs%s.RData"
    figFilenamePattern <- "figures/%s/signPCsAcrossTimeFilteredFrom%.02fTo%.02fOrder%02dFromTime%.02fTo%.02fDelayFrom%.02fTo%.02fBy%.03fNeighborSize%dCVSs%sSign%.02f.png"

    # groupOfCVSs <- read.table(groupOfCVSsFilename)$V1
    # groupOfCVSs <- c("mee") # done
    # groupOfCVSs <- c("naa") # done
    # groupOfCVSs <- c("wee") # done
    # groupOfCVSs <- c("nee")
    # groupOfCVSs <- c("kee") # done
    # groupOfCVSs <- c("zee") # done
    # groupOfCVSs <- c("noo") # done
    # groupOfCVSs <- c("koo") # done
    groupOfCVSs <- c("poo") # done
    # groupOfCVSs <- c("laa") # done

    pcsWithSignFilename <- sprintf(pcsWithSignFilenamePattern, 
                                    sessionLabel,
                                    lowCutoff, highCutoff, order,
                                    fromTime, toTime, 
                                    delayFrom, delayTo, delayBy, neighborSize,
                                    paste(groupOfCVSs, collapse="_"))
    figFilename <- sprintf(figFilenamePattern, sessionLabel, lowCutoff, highCutoff, order, fromTime, toTime, delayFrom, delayTo, delayBy, neighborSize, paste(groupOfCVSs, collapse="_"), significance)
    pcsWithSign <- get(load(pcsWithSignFilename))
    plots <- c()
    for(elecNumber in elecNumbersToPlot) {
        show(sprintf("Processing electrode %d", elecNumber))
        eSignificantPhaseCoherencesAcrossTime <- 
         getSignificantPhaseCoherencesAcrossTimeForElectrode(
          pcsWithSign=pcsWithSign, neighborSize=neighborSize, 
          elecNumber=elecNumber, significance=significance, 
          nrows=nrows, ncols=ncols)
        eSignificantPhaseCoherencesAcrossTimeMelted <- 
         data.frame(melt(eSignificantPhaseCoherencesAcrossTime, 
                          varnames=c("delay", "displacement")))
        p <- ggplot(data=eSignificantPhaseCoherencesAcrossTimeMelted, 
                      mapping=aes(x=delay, y=value, color=displacement))
        p <- p + geom_line()
        p <- p + geom_point()
        p <- p + geom_vline(xintercept=0, col="gray")
        p <- p + xlab("Delay from CV transition (sec)")
        p <- p + ylab("Phase Coherence")
        p <- p + ggtitle(sprintf("%d", elecNumber))
        p <- p + guides(colour=guide_legend(title="Electrode"))
        p <- p + ylim(ylim)
        p <- p + theme(legend.position="none")
        plots <- c(plots, list(p))
    }
    layoutMatrix <- matrix(data=1:length(plots), ncol=sqrt(length(plots)))
    p <- arrangeGrob(grobs=plots, ncol=sqrt(length(plots)), layout_matrix=layoutMatrix)
    ggsave(filename=figFilename, plot=p, width=width, height=height, limitsize=FALSE)

    browser()
}

processAll()
