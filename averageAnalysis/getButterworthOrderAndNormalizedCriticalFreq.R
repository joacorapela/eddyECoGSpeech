#
# fP, fS and normalizedCriticalFreq are normalized so that the Nyquist 
# frequency is 1
#
getButterworthOrderAndNormalizedCriticalFreq <- function(fP, fS, rP, rS, 
                                                             sampleRate) {
    res <- getButterworthOrderAndCriticalFreq(wP=pi*fP, wS=pi*fS, rP=rP, rS=rS)
    criticalFreq <- res$criticalAngle/(2*pi)
    return(list(order=res$order, 
                 normalizedCriticalFreq=criticalFreq/(sampleRate/2)))
}
