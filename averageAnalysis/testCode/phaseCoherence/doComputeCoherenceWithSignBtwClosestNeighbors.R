
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
    delayFrom <- -.5
    delayTo <- 1.0
    delayBy <- .01
    delaysFromCVSTransition <- seq(from=delayFrom, to=delayTo, by=delayBy)
    sessionLabel <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    fromTime <- 0
    toTime <- 700
    transcriptionSampleRate <- 1e7
    nResamples <- 2000
    transcriptionFilenamePattern <- "../../../data/transcriptionFiles/%s/%s_transcription_final_completed.lab"
    nlflvDatacubeFilenamePattern <- "../../results/%s/nlflvDatacubeFilteredFrom%.02fTo%.02fOrder%02dFromTime%.02fTo%.02f.RData"
    groupOfCVSsFilename <- "../../results/EC2_B105/phonetics/cvsWithIVowel.txt"
    pcsWithSignFilenamePattern <- "../../results/%s/pcsWithSignFilteredFrom%.02fTo%.02fOrder%02dFromTime%.02fTo%.02fDelayFrom%.02fTo%.02fBy%.03fCVSs%s.RData"

    # groupOfCVSs <- read.table(groupOfCVSsFilename)$V1
    groupOfCVSs <- c("mee")
    # groupOfCVSs <- c("poo")
    # groupOfCVSs <- c("naa")
    # groupOfCVSs <- c("dee")
    # groupOfCVSs <- c("yoo")
    nlflvDatacubeFilename <- sprintf(nlflvDatacubeFilenamePattern, 
                                   sessionLabel,
                                   lowCutoff, highCutoff, order,
                                   fromTime, toTime)
    pcsWithSignFilename <- sprintf(pcsWithSignFilenamePattern, 
                                   sessionLabel,
                                   lowCutoff, highCutoff, order,
                                   fromTime, toTime, 
                                   delayFrom, delayTo, delayBy,
                                   paste(groupOfCVSs, collapse="_"))
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

    pcsWithSign <- computeCoherencesWithSignBtwClosestNeighbors(
                    phaseDatacube=nlflvsArray, 
                    delaysInSamples=delaysFromCVSTransition*sampleRate, 
                    allTransitionSamples=resTransitions$samples, 
                    selectedTransitionSamples=cvsGroupTransitionSamples,
                    nResamples=nResamples)
    delayInSamplesColIndex <- which(colnames(pcsWithSign)=="delayInSamples")
    pcsWithSignWithDelaysInSecs <- cbind(pcsWithSign[,delayInSamplesColIndex]/sampleRate, pcsWithSign[,-delayInSamplesColIndex])
    colnames(pcsWithSignWithDelaysInSecs) <- c("delayInSecs", colnames(pcsWithSign)[-delayInSamplesColIndex])
    # save(pcsWithSignWithDelaysInSecs, file=pcsWithSignFilename)
    significance <- .01
    delays <- pcsWithSignWithDelaysInSecs[,"delayInSecs"]
    pcs <- pcsWithSignWithDelaysInSecs[,"pc"]
    pValues <- pcsWithSignWithDelaysInSecs[,"pValue"]
    significantIndices <- which(pValues<significance)
    d <- data.frame(delay=delays[significantIndices], 
                     pc=pcs[significantIndices])
    p <- ggplot(data=d, mapping=aes(x=delay, y=pc)) + geom_line() + geom_point()
    p <- p + ylim(c(0,1))
    print(p)
    browser()
}

processAll()
