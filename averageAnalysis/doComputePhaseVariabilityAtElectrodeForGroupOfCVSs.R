
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
    elecNumber <- 123
    delayFromCVSInitiation <- 0.5
    neighborSize <- 1
    sessionLabel <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    fromTime <- 0
    toTime <- 700
    transcriptionSampleRate <- 1e7
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
    electrodeIndexInArray <- 
     getElectrodeIndexInArrayGGPlot(elecNumber=elecNumber)
    cvsGroupInitiationSamples <- 
     getInitationSamplesForCVSsInGroup(cvss=
                                         as.character(resInitiations$
                                                       cvSyllables),
                                        samples=resInitiations$samples, 
                                        groupOfCVSs=groupOfCVSs)
    nlflvsArraySubset <- nlflvsArray[,,cvsGroupInitiationSamples+
                                 delayFromCVSInitiation*sampleRate]
    phaseVariabilities <- 
     computePhaseVariabilityAtElectrode(phasesDatacube=nlflvsArraySubset, 
                                         electrodeRow=electrodeIndexInArray[1],
                                         electrodeCol=electrodeIndexInArray[2],
                                         neighborSize=neighborSize)

    phaseVariabilitiesMelted <- data.frame(melt(phaseVariabilities, 
                                                 varnames=c("y", "x")))
    p1 <- ggplot(data=phaseVariabilitiesMelted, 
                 mapping=aes(x=x, y=y, fill=value))
    p1 <- p1 + geom_tile()
    p1 <- p1 + scale_fill_gradient(low="yellow", high="red", limits=c(0,1))
    p1 <- p1 + ggtitle(sprintf("%d, min=%.02f, max=%.02f, median=%.02f", 
                               elecNumber, 
                               min(phaseVariabilities),
                               max(phaseVariabilities),
                               median(phaseVariabilities)))
    # p <- p + guides(fill=guide_legend(title="Variability"))
    # print(p)

    randomInitiationIndices <- sample.int(n=length(resInitiations$samples), size=length(cvsGroupInitiationSamples))
    randomInitiationSamples <- resInitiations$samples[randomInitiationIndices]
    nlflvsArrayRandomSubset <- nlflvsArray[,,randomInitiationSamples+
                                        delayFromCVSInitiation*sampleRate]
    randomPhaseVariabilities <- 
     computePhaseVariabilityAtElectrode(phasesDatacube=
                                          nlflvsArrayRandomSubset,
                                         electrodeRow=electrodeIndexInArray[1],
                                         electrodeCol=electrodeIndexInArray[2],
                                         neighborSize=neighborSize)

    randomPhaseVariabilitiesMelted <- 
     data.frame(melt(randomPhaseVariabilities, varnames=c("y", "x")))
    p2 <- ggplot(data=randomPhaseVariabilitiesMelted, 
                 mapping=aes(x=x, y=y, fill=value))
    p2 <- p2 + geom_tile()
    p2 <- p2 + scale_fill_gradient(low="yellow", high="red", limits=c(0,1))
    p2 <- p2 + ggtitle(sprintf("random %d, min=%.02f, max=%.02f, median=%.02f",
                               elecNumber, 
                               min(randomPhaseVariabilities),
                               max(randomPhaseVariabilities),
                               median(randomPhaseVariabilities)))
    # print(p2)
    # p <- p + guides(fill=guide_legend(title="Variability"))
    # print(p)

    grid.arrange(p1, p2, ncol=1)

    browser()
}

processAll()
