computePhaseCoherenceAtElectrode <- function(phasesDatacube, electrodeRow, 
                                                             electrodeCol,
                                                             neighborSize) {
    phaseCoherences <- matrix(data=NA, nrow=1+2*neighborSize, 
                                          ncol=1+2*neighborSize)
    centerIndex <- neighborSize+1
    neighborsIndices <- seq(from=-neighborSize, to=neighborSize)
    for(i in neighborsIndices) {
        for(j in neighborsIndices) {
            if(i==0 & j==0) {
                phaseCoherences[centerIndex, centerIndex] <- 
                 computeITCFromPhases(phases=phasesDatacube[electrodeRow,
                                                             electrodeCol,])
            } else {
                phaseCoherences[centerIndex+i, centerIndex+j] <- 
                 computePLIFromPhases(phases=
                                       phasesDatacube[electrodeRow+i,
                                                       electrodeCol+j,],
                                      phasesReference=
                                       phasesDatacube[electrodeRow,
                                                       electrodeCol,])
            }
        }
    }
    # colnames(phaseCoherences) <- c("left", "center", "right")
    # rownames(phaseCoherences) <- c("bottom", "middle", "top")
    colnames(phaseCoherences) <- sprintf("%d", neighborsIndices)
    rownames(phaseCoherences) <- sprintf("%d", neighborsIndices)
    return(phaseCoherences)
}
