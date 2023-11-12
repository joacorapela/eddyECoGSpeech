computeITCFromPhasesWithSign <- function(phases, allTransitionSamples,
                                                 selectedTransitionSamples,
                                                 nResamples) {
    t0 <- computeITCFromPhases(phases=phases[selectedTransitionSamples])
    t <- c()
    for(i in 1:nResamples) {
        randomTransitionSamples <- 
         sample(x=allTransitionSamples, size=length(selectedTransitionSamples))
        t <- c(t, computeITCFromPhases(phases=phases[randomTransitionSamples]))
    }
    pValue <- sum(t>t0)/length(t)
    return(list(itc=t0, pValue=pValue))
}

