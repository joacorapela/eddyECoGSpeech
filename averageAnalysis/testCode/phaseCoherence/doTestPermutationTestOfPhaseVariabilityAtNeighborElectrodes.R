
source("doLoadSources.R")

getInitationSamplesForCVSsInGroup <- function(cvss, samples, groupOfCVSs) {
    selectedSamples <- c()
    for(aCVS in groupOfCVSs) {
        indices <- which(cvss==aCVS)
        if(length(indices)>0) {
            selectedSamples <- c(selectedSamples, samples[indices])
        }
    }
    return(selectedSamples)
}

processAll <- function() {
    elecNumber <- 57
    neighborElecNumber <- 56
    delayFromCVSInitiation <- 0.3
    neighborSize <- 1
    sessionLabel <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    fromTime <- 0
    toTime <- 700
    transcriptionSampleRate <- 1e7
    nResamples <- 2000
    conf <- .95
    significance <- .01
    transcriptionFilenamePattern <- "../data/transcriptionFiles/%s/%s_transcription_final_completed.lab"
    nlflvDatacubeFilenamePattern <- "results/%s/nlflvDatacubeFilteredFrom%.02fTo%.02fOrder%02dFromTime%.02fTo%.02f.RData"
    groupOfCVSsFilename <- "results/EC2_B105/phonetics/cvsWithIVowel.txt"

    # groupOfCVSs <- read.table(groupOfCVSsFilename)$V1
    groupOfCVSs <- c("dee")
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
    resInitiations <- getInfoCVSsInitiations(transcriptionFilename=
                                              transcriptionFilename,
                                             transcriptionSampleRate=
                                              transcriptionSampleRate,
                                             ecogSampleRate=sampleRate)
    cvsGroupInitiationSamples <- 
     getInitationSamplesForCVSsInGroup(cvss=
                                         as.character(resInitiations$
                                                       cvSyllables),
                                        samples=resInitiations$samples, 
                                        groupOfCVSs=groupOfCVSs)
    electrodeIndexInArray <- 
     getElectrodeIndexInArrayGGPlot(elecNumber=elecNumber)
    permRes <- performPermutationTestOfPhaseVariabilityAtElectrode(
                nlflvsArray=nlflvsArray, 
                electrodeRow=electrodeIndexInArray[1], 
                electrodeCol=electrodeIndexInArray[2], 
                neighborSize=neighborSize,
                allInitiationSamples=resInitiations$cvSyllables, 
                selectedInitiationSamples=cvsGroupInitiationSamples+
                                           delayFromCVSInitiation, 
                nResamples=nResamples, conf=conf)
    pValues <- rep(NA, length(permRes$t0))
    for(i in 1:length(permRes$t0)) {
        pValues[i] <- sum(permRes$t[,i]<permRes$t0[i])/nrow(permRes$t)
    }
    significantPhaseVariabilities <- permRes$t0
    significantPhaseVariabilities[pValues>significance] <- NaN

    significantPhaseVariabilitiesMatrix <- 
     matrix(significantPhaseVariabilities, ncol=2*neighborSize+1)
    significantPhaseVariabilitiesMatrixMelted <- 
     data.frame(melt(significantPhaseVariabilitiesMatrix, varnames=c("y", "x")))
    p1 <- ggplot(data=significantPhaseVariabilitiesMatrixMelted, 
                 mapping=aes(x=x, y=y, fill=value))
    p1 <- p1 + geom_tile()
    p1 <- p1 + scale_fill_gradient(name="Variability", low="yellow", high="red", limits=c(0,1))
    p1 <- p1 + ggtitle(sprintf("%d, min=%.02f, max=%.02f, median=%.02f", 
                               elecNumber, 
                               min(significantPhaseVariabilities, na.rm=TRUE),
                               max(significantPhaseVariabilities, na.rm=TRUE),
                               median(significantPhaseVariabilities, 
                                       na.rm=TRUE)))
    # p <- p + guides(fill=guide_legend(title="Variability"))
    for(i in 1:nrow(significantPhaseVariabilitiesMatrixMelted)) {
        if(!is.nan(significantPhaseVariabilitiesMatrixMelted[i,"value"])) {
            p1 <- p1 + annotate("text", label=sprintf("%f", significantPhaseVariabilitiesMatrixMelted[i,"value"]), x=significantPhaseVariabilitiesMatrixMelted[i,"x"], y=significantPhaseVariabilitiesMatrixMelted[i,"y"])
        }
    }
    # print(p1)

    neighborElectrodeIndexInArray <- 
     getElectrodeIndexInArrayGGPlot(elecNumber=neighborElecNumber)
    neighborPermRes <- performPermutationTestOfPhaseVariabilityAtElectrode(
                nlflvsArray=nlflvsArray, 
                electrodeRow=neighborElectrodeIndexInArray[1], 
                electrodeCol=neighborElectrodeIndexInArray[2], 
                neighborSize=neighborSize,
                allInitiationSamples=resInitiations$cvSyllables, 
                selectedInitiationSamples=cvsGroupInitiationSamples+
                                           delayFromCVSInitiation, 
                nResamples=nResamples, conf=conf)
    neighborPValues <- rep(NA, length(neighborPermRes$t0))
    for(i in 1:length(neighborPermRes$t0)) {
        neighborPValues[i] <- sum(neighborPermRes$t[,i]<neighborPermRes$t0[i])/nrow(neighborPermRes$t)
    }
    neighborSignificantPhaseVariabilities <- neighborPermRes$t0
    neighborSignificantPhaseVariabilities[neighborPValues>significance] <- NaN

    neighborSignificantPhaseVariabilitiesMatrix <- 
     matrix(neighborSignificantPhaseVariabilities, ncol=2*neighborSize+1)
    neighborSignificantPhaseVariabilitiesMatrixMelted <- 
     data.frame(melt(neighborSignificantPhaseVariabilitiesMatrix, varnames=c("y", "x")))
    p2 <- ggplot(data=neighborSignificantPhaseVariabilitiesMatrixMelted, 
                 mapping=aes(x=x, y=y, fill=value))
    p2 <- p2 + geom_tile()
    p2 <- p2 + scale_fill_gradient(name="Variability", low="yellow", high="red", limits=c(0,1))
    p2 <- p2 + ggtitle(sprintf("%d, min=%.02f, max=%.02f, median=%.02f", 
                               neighborElecNumber, 
                               min(neighborSignificantPhaseVariabilities, na.rm=TRUE),
                               max(neighborSignificantPhaseVariabilities, na.rm=TRUE),
                               median(neighborSignificantPhaseVariabilities, 
                                       na.rm=TRUE)))
    # p <- p + guides(fill=guide_legend(title="Variability"))
    for(i in 1:nrow(neighborSignificantPhaseVariabilitiesMatrixMelted)) {
        if(!is.nan(neighborSignificantPhaseVariabilitiesMatrixMelted[i,"value"])) {
            p2 <- p2 + annotate("text", label=sprintf("%f", neighborSignificantPhaseVariabilitiesMatrixMelted[i,"value"]), x=neighborSignificantPhaseVariabilitiesMatrixMelted[i,"x"], y=neighborSignificantPhaseVariabilitiesMatrixMelted[i,"y"])
        }
    }
    # print(p2)

    grid.arrange(p1, p2, ncol=1)

    browser()
}

processAll()
