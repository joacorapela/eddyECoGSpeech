
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
    delayFrom <- -.2
    delayTo <- .5
    delayBy <- .01
    neighborSize <- 3
    sessionLabel <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    fromTime <- 0
    toTime <- 700
    transcriptionSampleRate <- 1e7
    nResamples <- 2000
    transcriptionFilenamePattern <- "../data/transcriptionFiles/%s/%s_transcription_final_completed.lab"
    nlflvDatacubeFilenamePattern <- "results/%s/nlflvDatacubeFilteredFrom%.02fTo%.02fOrder%02dFromTime%.02fTo%.02f.RData"
    groupOfCVSsFilename <- "results/EC2_B105/phonetics/cvsWithIVowel.txt"
    pcsWithSignFilenamePattern <- "results/%s/pcsWithSignFilteredFrom%.02fTo%.02fOrder%02dFromTime%.02fTo%.02fDelayFrom%.02fTo%.02fBy%.03fNeighborSize%dCVSs%s.RData"

    # groupOfCVSs <- read.table(groupOfCVSsFilename)$V1
    # groupOfCVSs <- c("mee") # done
    # groupOfCVSs <- c("wee") # done
    # groupOfCVSs <- c("nee") # done
    # groupOfCVSs <- c("kee") # done
    # groupOfCVSs <- c("zee") # done
    # groupOfCVSs <- c("lee")
    # groupOfCVSs <- c("ree")
    # groupOfCVSs <- c("see")

    # groupOfCVSs <- c("noo") # done
    # groupOfCVSs <- c("koo") # done
    # groupOfCVSs <- c("koo")
    # groupOfCVSs <- c("boo")
    groupOfCVSs <- c("poo")

    # groupOfCVSs <- c("naa") # done
    # groupOfCVSs <- c("laa") # done
    # groupOfCVSs <- c("faa")
    delaysFromCVSTransition <- seq(from=delayFrom, to=delayTo, by=delayBy)
    nlflvDatacubeFilename <- sprintf(nlflvDatacubeFilenamePattern, 
                                   sessionLabel,
                                   lowCutoff, highCutoff, order,
                                   fromTime, toTime)
    pcsWithSignFilename <- sprintf(pcsWithSignFilenamePattern, 
                                   sessionLabel,
                                   lowCutoff, highCutoff, order,
                                   fromTime, toTime, 
                                   delayFrom, delayTo, delayBy,
                                   neighborSize,
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
                    neighborSize=neighborSize,
                    delaysInSamples=delaysFromCVSTransition*sampleRate, 
                    allTransitionSamples=resTransitions$samples, 
                    selectedTransitionSamples=cvsGroupTransitionSamples,
                    nResamples=nResamples)
    delayInSamplesColIndex <- which(colnames(pcsWithSign)=="delayInSamples")
    pcsWithSignWithDelaysInSecs <- cbind(pcsWithSign[,delayInSamplesColIndex]/sampleRate, pcsWithSign[,-delayInSamplesColIndex])
    colnames(pcsWithSignWithDelaysInSecs) <- c("delayInSecs", colnames(pcsWithSign)[-delayInSamplesColIndex])
    save(pcsWithSignWithDelaysInSecs, file=pcsWithSignFilename)
}

processAll()
