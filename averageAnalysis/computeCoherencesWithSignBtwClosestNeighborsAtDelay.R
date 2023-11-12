computeCoherencesWithSignBtwClosestNeighborsAtDelay <- 
 function(phaseDatacube, neighborSize,
                         allTransitionSamples, selectedTransitionSamples,
                         nResamples) {
    nrows <- dim(phaseDatacube)[1]
    ncols <- dim(phaseDatacube)[2]
    i1s <- c()
    j1s <- c()
    i2s <- c()
    j2s <- c()
    pcs <- c()
    pValues <- c()
    for(i in 1:nrows) {
        show(sprintf("Processing i1=%02d", i))
        for(j in 1:ncols) {
            for(offsetX in c(-neighborSize:-1, 1:neighborSize)) {
                for(offsetY in c(-neighborSize:-1, 1:neighborSize)) {
                    i2 <- i+offsetY
                    j2 <- j+offsetX
                    if(0<i2 && i2<nrows && 0<j2 && j2<ncols) {
                        res <- computePLIFromPhasesWithSign(
                                phases=phaseDatacube[i2,j2,],
                                phasesReference=phaseDatacube[i,j,],
                                allTransitionSamples=allTransitionSamples,
                                selectedTransitionSamples=selectedTransitionSamples,
                                nResamples=nResamples)
                        i1s <- c(i1s, i)
                        j1s <- c(j1s, j)
                        i2s <- c(i2s, i2)
                        j2s <- c(j2s, j2)
                        pcs <- c(pcs, res$pli)
                        pValues <- c(pValues, res$pValue)
                    }
                }
            }
        }
    }
    answer <- cbind(i1s, j1s, i2s, j2s, pcs, pValues)
    colnames(answer) <- c("i1", "j1", "i2", "j2", "pc", "pValue")
    return(answer)
}
