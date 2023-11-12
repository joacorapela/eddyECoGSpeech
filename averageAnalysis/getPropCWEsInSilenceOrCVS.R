
getPropCWEsInSilenceOrCVS <- function(infoInit, infoTerm, cwes) {
    nCWEsInitSilence <- 0
    nCWEsInitCVS <- 0
    nCWEsTermSilence <- 0
    nCWEsTermCVS <- 0
    nCWEsInSilence <- 0
    nCWEsInCVS <- 0
    for(i in 1:nrow(cwes)) {
        # check if the current cwe starts in silence or in a CVS
        cweInitiationTime <- cwes[i, 1]
        priorTermIndices <- which(infoTerm$time<cweInitiationTime)
        priorInfoTerm <- infoTerm[priorTermIndices,]
        indexCVSTermPrecedingCWEInitTime <- nrow(priorInfoTerm) # index of the CVS termination preceding cweInitiationTime
        if(indexCVSTermPrecedingCWEInitTime==nrow(infoInit) || 
            infoInit$time[indexCVSTermPrecedingCWEInitTime+1]>cweInitiationTime) { # if next CVS initation occurs after cweInitationTime
            initSilence <- TRUE
            nCWEsInitSilence <- nCWEsInitSilence+1
        } else {
            initSilence <- FALSE
            nCWEsInitCVS <- nCWEsInitCVS+1
        }
        # check if the current cwe terminates in silence or in a CVS
        cweTerminationTime <- cwes[i, 2]
        priorTermIndices <- which(infoTerm$time<cweTerminationTime)
        priorInfoTerm <- infoTerm[priorTermIndices,]
        indexCVSTermPrecedingCWETermTime <- nrow(priorInfoTerm) # index of the CVS termination preceding cweTermiationTime
        if(indexCVSTermPrecedingCWETermTime==nrow(infoInit) || infoInit$time[indexCVSTermPrecedingCWETermTime+1]>cweTerminationTime) { # if next CVS initation occurs after cweTerminationTime
            termSilence <- TRUE
            nCWEsTermSilence <- nCWEsTermSilence+1
        } else {
            termSilence <- FALSE
            nCWEsTermCVS <- nCWEsTermCVS+1
        }
        if(initSilence && termSilence) {
            nCWEsInSilence <- nCWEsInSilence + 1
        } else {
            if(!initSilence && !termSilence) {
                nCWEsInCVS <- nCWEsInCVS + 1
            }
        }
    }
    propCWEsInSilence <- nCWEsInSilence/nrow(cwes)
    propCWEsInCVS <- nCWEsInCVS/nrow(cwes)
    return(list(propCWEsInSilence=propCWEsInSilence,
                 propCWEsInCVS=propCWEsInCVS))
}
