computeCoherencesWithSignBtwClosestNeighborsAtDelay <- 
 function(phaseDatacube, allTransitionSamples, selectedTransitionSamples,
                         nResamples) {
    nrows <- dim(phaseDatacube)[1]
    ncols <- dim(phaseDatacube)[2]
    i1s <- c()
    j1s <- c()
    i2s <- c()
    j2s <- c()
    pcs <- c()
    pValues <- c()
i <- 8
j <- 15
i2 <- i
j2 <- j-1
res <- computePLIFromPhasesWithSign(phases=
                                      phaseDatacube[i2,j2,],
                                     phasesReference=
                                      phaseDatacube[i,j,],
                                     allTransitionSamples=
                                      allTransitionSamples,
                                     selectedTransitionSamples=
                                      selectedTransitionSamples,
                                     nResamples=nResamples)
i1s <- c(i1s, i)
j1s <- c(j1s, j)
i2s <- c(i2s, i2)
j2s <- c(j2s, j2)
pcs <- c(pcs, res$pli)
pValues <- c(pValues, res$pValue)
answer <- cbind(i1s, j1s, i2s, j2s, pcs, pValues)
colnames(answer) <- c("i1", "j1", "i2", "j2", "pc", "pValue")
return(answer)

browser()
    for(i in 1:nrows) {
        for(j in 1:ncols) {
            # show(sprintf("Processing (%d,%d)", i, j))
            res <- computeITCFromPhasesWithSign(phases=phaseDatacube[i,j,],
                                                 allTransitionSamples=
                                                  allTransitionSamples,
                                                 selectedTransitionSamples=
                                                  selectedTransitionSamples,
                                                 nResamples=nResamples)
            i1s <- c(i1s, i)
            j1s <- c(j1s, j)
            i2s <- c(i2s, i)
            j2s <- c(j2s, j)
            pcs <- c(pcs, res$itc)
            pValues <- c(pValues, res$pValue)

            if(i<nrows && j<ncols) {
                i2 <- i+1
                j2 <- j+1
                res <- computePLIFromPhasesWithSign(phases=
                                                      phaseDatacube[i2,j2,],
                                                     phasesReference=
                                                      phaseDatacube[i,j,],
                                                     allTransitionSamples=
                                                      allTransitionSamples,
                                                     selectedTransitionSamples=
                                                      selectedTransitionSamples,
                                                     nResamples=nResamples)
                i1s <- c(i1s, i)
                j1s <- c(j1s, j)
                i2s <- c(i2s, i2)
                j2s <- c(j2s, j2)
                pcs <- c(pcs, res$pli)
                pValues <- c(pValues, res$pValue)
            }

            if(i<nrows) {
                i2 <- i+1
                j2 <- j
                res <- computePLIFromPhasesWithSign(phases=
                                                      phaseDatacube[i2,j2,],
                                                     phasesReference=
                                                      phaseDatacube[i,j,],
                                                     allTransitionSamples=
                                                      allTransitionSamples,
                                                     selectedTransitionSamples=
                                                      selectedTransitionSamples,
                                                     nResamples=nResamples)
                i1s <- c(i1s, i)
                j1s <- c(j1s, j)
                i2s <- c(i2s, i2)
                j2s <- c(j2s, j2)
                pcs <- c(pcs, res$pli)
                pValues <- c(pValues, res$pValue)
            }

            if(i<nrows && j>1) {
                i2 <- i+1
                j2 <- j-1
                res <- computePLIFromPhasesWithSign(phases=
                                                      phaseDatacube[i2,j2,],
                                                     phasesReference=
                                                      phaseDatacube[i,j,],
                                                     allTransitionSamples=
                                                      allTransitionSamples,
                                                     selectedTransitionSamples=
                                                      selectedTransitionSamples,
                                                     nResamples=nResamples)
                i1s <- c(i1s, i)
                j1s <- c(j1s, j)
                i2s <- c(i2s, i2)
                j2s <- c(j2s, j2)
                pcs <- c(pcs, res$pli)
                pValues <- c(pValues, res$pValue)
            }

            if(j>1) {
                i2 <- i
                j2 <- j-1
                res <- computePLIFromPhasesWithSign(phases=
                                                      phaseDatacube[i2,j2,],
                                                     phasesReference=
                                                      phaseDatacube[i,j,],
                                                     allTransitionSamples=
                                                      allTransitionSamples,
                                                     selectedTransitionSamples=
                                                      selectedTransitionSamples,
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
    answer <- cbind(i1s, j1s, i2s, j2s, pcs, pValues)
    colnames(answer) <- c("i1", "j1", "i2", "j2", "pc", "pValue")
    return(answer)
}
