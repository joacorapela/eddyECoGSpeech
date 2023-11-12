getPhaseDiffsWRTElec1AtTime <- function(timeIndex, hts, distancesFromRefElec) {
    phases <- rep(NA, times=ncol(hts))
    for(i in 1:length(phases)) {
        phases[i] <- Arg(hts[timeIndex,i])
    }
    phaseDiffs <- phases-phases[1]
    return(phaseDiffs)
}
