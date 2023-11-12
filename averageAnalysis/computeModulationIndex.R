# From Measuring Phase-Amplitude Coupling Between Neuronal Oscillations of
# Different Frequencies. Tort et al 2010

computeModulationIndex <- function(amplitudesBinnedByPhase) {
    computeNormalizedAmplitudes <- function(amplitudesBinnedByPhase) {
        normalizedAmplitudes <- rep(NA, times=length(amplitudesBinnedByPhase))
        for(i in 1:length(amplitudesBinnedByPhase)) {
            normalizedAmplitudes[i] <- mean(amplitudesBinnedByPhase[[i]])
        }
        normalizedAmplitudes <- normalizedAmplitudes/sum(normalizedAmplitudes)
        return(normalizedAmplitudes)
    }

    p <- computeNormalizedAmplitudes(amplitudesBinnedByPhase=
                                      amplitudesBinnedByPhase)
    h <- -sum(p*log(p))
    n <- length(amplitudesBinnedByPhase)
    d <- log(n)-h
    mi <- d/log(n)
    return(mi)
}
