unwrap1DPhases <- function(phases, threshold=pi) {
    N <- length(phases)
    for(i in 2:N) {
        phaseDiff <- phases[i]-phases[i-1]
        if(abs(phaseDiff)>threshold) {
            if(phases[i]>0) {
                for(j in i:N) {
                    phases[j] <- phases[j]-2*pi
                }
            } else {
                for(j in i:N) {
                    phases[j] <- phases[j]+2*pi
                }
            }
        }
    }
    return(phases)
}

