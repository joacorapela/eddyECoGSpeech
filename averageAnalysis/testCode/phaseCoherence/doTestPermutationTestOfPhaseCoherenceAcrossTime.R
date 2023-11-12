
source("doLoadSources.R")

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
    elecNumber <- 68
    delaysFromCVSTransition <- seq(from=-0.5, to=1.0, by=.01)
    neighborSize <- 3
    sessionLabel <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    fromTime <- 0
    toTime <- 700
    transcriptionSampleRate <- 1e7
    nResamples <- 2000
    significance <- .01
    ylim <- c(0.0, 1.0)
    transcriptionFilenamePattern <- "../../../data/transcriptionFiles/%s/%s_transcription_final_completed.lab"
    nlflvDatacubeFilenamePattern <- "../../results/%s/nlflvDatacubeFilteredFrom%.02fTo%.02fOrder%02dFromTime%.02fTo%.02f.RData"
    groupOfCVSsFilename <- "../../results/EC2_B105/phonetics/cvsWithIVowel.txt"

    # groupOfCVSs <- read.table(groupOfCVSsFilename)$V1
    groupOfCVSs <- c("mee")
    # groupOfCVSs <- c("dee")
    # groupOfCVSs <- c("yoo")
    nlflvDatacubeFilename <- sprintf(nlflvDatacubeFilenamePattern, 
                                   sessionLabel,
                                   lowCutoff, highCutoff, order,
                                   fromTime, toTime)
    nlflvDatacubeLoadRes <- get(load(nlflvDatacubeFilename))
    nlflvsArray <- nlflvDatacubeLoadRes$nlflvsArray
    nlflvsArray[is.na(nlflvsArray)] <- 0.0
    sampleRate <- nlflvDatacubeLoadRes$actualSampleRate
    transcriptionFilename <- sprintf(transcriptionFilenamePattern, 
                                      sessionLabel, sessionLabel)
    resTransitions <- getInfoCVSsCVTransitions(transcriptionFilename=
                                                 transcriptionFilename,
                                                transcriptionSampleRate=
                                                 transcriptionSampleRate,
                                                ecogSampleRate=sampleRate)
    cvsGroupTransitionSamples <- 
     getTransitionSamplesForCVSsInGroup(cvss=
                                         as.character(resTransitions$
                                                       cvSyllables),
                                        transitionSamples=
                                         resTransitions$samples, 
                                        groupOfCVSs=groupOfCVSs)
    electrodeIndexInArray <- 
     getElectrodeIndexInArrayGGPlot(elecNumber=elecNumber)
    significantPhaseCoherencesAcrossTime <- matrix(NA, nrow=length(delaysFromCVSTransition), ncol=(2*neighborSize+1)^2)
    for(i in 1:length(delaysFromCVSTransition)) {
        show(sprintf("Processing delay %.02f (%.02f)", 
                     delaysFromCVSTransition[i],
                     delaysFromCVSTransition[length(delaysFromCVSTransition)]))
        permRes <- performPermutationTestOfPhaseCoherenceAtElectrode(
                    nlflvsArray=nlflvsArray, 
                    electrodeRow=electrodeIndexInArray[1], 
                    electrodeCol=electrodeIndexInArray[2], 
                    neighborSize=neighborSize,
                    allTransitionSamples=resTransitions$samples, 
                    selectedTransitionSamples=cvsGroupTransitionSamples+
                                               delaysFromCVSTransition[i]*
                                               sampleRate, 
                    nResamples=nResamples)
        pValues <- rep(NA, length(permRes$t0))
        for(j in 1:length(permRes$t0)) {
            pValues[j] <- sum(permRes$t[,j]>permRes$t0[j])/nrow(permRes$t)
        }
        significantPhaseCoherences <- permRes$t0
        significantPhaseCoherences[pValues>significance] <- NaN
        significantPhaseCoherencesAcrossTime[i,] <- significantPhaseCoherences
    }
    rownames(significantPhaseCoherencesAcrossTime) <- sprintf("%f", delaysFromCVSTransition)
    colnames(significantPhaseCoherencesAcrossTime) <- permRes$labels
    significantPhaseCoherencesAcrossTimeMelted <- 
     data.frame(melt(significantPhaseCoherencesAcrossTime, 
                      varnames=c("time", "displacement")))
    p1 <- ggplot(data=significantPhaseCoherencesAcrossTimeMelted, 
                  mapping=aes(x=time, y=value, color=displacement))
    p1 <- p1 + geom_line()
    p1 <- p1 + geom_point()
    p1 <- p1 + geom_vline(xintercept=0, col="gray")
    p1 <- p1 + xlab("Delay from CV transition (sec)")
    p1 <- p1 + ylab("Phase Coherence")
    p1 <- p1 + ggtitle(sprintf("%d", elecNumber))
    p1 <- p1 + guides(colour=guide_legend(title="Electrode"))
    p1 <- p1 + ylim(ylim)
    print(p1)
    browser()
}

processAll()
