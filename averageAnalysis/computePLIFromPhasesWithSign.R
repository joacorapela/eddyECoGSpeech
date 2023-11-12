computePLIFromPhasesWithSign <- function(phases, phasesReference, 
                                                  allTransitionSamples,
                                                  selectedTransitionSamples,
                                                  nResamples) {
    answer <- computeITCFromPhasesWithSign(phases=phases-phasesReference,
                                            allTransitionSamples=
                                             allTransitionSamples,
                                            selectedTransitionSamples=
                                             selectedTransitionSamples,
                                            nResamples=nResamples)
    return(list(pli=answer$itc, pValue=answer$pValue))
}
