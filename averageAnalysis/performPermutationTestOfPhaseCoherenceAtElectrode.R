performPermutationTestOfPhaseCoherenceAtElectrode <- 
 function(nlflvsArray, electrodeRow, electrodeCol, neighborSize,
                       allTransitionSamples, selectedTransitionSamples, 
                       nResamples) {
    buildLabels <- function(colnames, rownames) {
        labels <- rep()
        for(j in 1:length(colnames)) {
            for(i in 1:length(rownames)) {
                labels <- c(labels, sprintf("%s-%s", rownames[i], colnames[j]))
            }
        }
        return(labels)
    }
    nlflvsArraySubset <- nlflvsArray[,,selectedTransitionSamples]
    phaseCoherences <- computePhaseCoherenceAtElectrode(
                           phasesDatacube=nlflvsArraySubset, 
                           electrodeRow=electrodeRow,
                           electrodeCol=electrodeCol,
                           neighborSize=neighborSize)
    t0 <- as.vector(phaseCoherences)
    t <- matrix(NA, ncol=length(t0), nrow=nResamples)
    for(i in 1:nResamples) {
        randomTransitionSamples <- 
         sample(x=allTransitionSamples, size=length(selectedTransitionSamples))
        nlflvsArrayRandomSubset <- nlflvsArray[,,randomTransitionSamples]
        randomPhaseCoherences <- 
         computePhaseCoherenceAtElectrode(phasesDatacube=
                                              nlflvsArrayRandomSubset,
                                             electrodeRow=electrodeRow,
                                             electrodeCol=electrodeCol,
                                             neighborSize=neighborSize)
        t[i,] <- randomPhaseCoherences
    }
    labels <- buildLabels(colnames=colnames(phaseCoherences), 
                           rownames=rownames(phaseCoherences))
    return(list(t0=t0, t=t, labels=labels))
}
