computeCoherencesWithSignBtwClosestNeighbors <- 
 function(phaseDatacube, neighborSize, delaysInSamples, 
                         allTransitionSamples, selectedTransitionSamples,
                         nResamples) {
    pcsWithSign <- c()
    for(i in 1:length(delaysInSamples)) {
        show(sprintf("Processing delay %d (%d)", i, length(delaysInSamples)))
        delayInSamples <- delaysInSamples[i]
        pcsWithSignAtDelay <- 
         computeCoherencesWithSignBtwClosestNeighborsAtDelay(
          phaseDatacube=phaseDatacube,
          neighborSize=neighborSize,
          allTransitionSamples=allTransitionSamples,
          selectedTransitionSamples=
          selectedTransitionSamples+delayInSamples,
          nResamples=nResamples)
        pcsWithSignAtDelay <- cbind(delayInSamples, pcsWithSignAtDelay)
        pcsWithSign <- rbind(pcsWithSign, pcsWithSignAtDelay)
    }
    newColnames <- colnames(pcsWithSign)
    newColnames[1] <- "delayInSamples"
    colnames(pcsWithSign) <- newColnames
    return(pcsWithSign)
}
