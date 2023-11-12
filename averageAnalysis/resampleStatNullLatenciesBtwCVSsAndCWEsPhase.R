
resampleStatNullLatenciesBtwCVSsAndCWEsPhase <- 
 function(stat, infoInit, infoTerm, cwes, nResamples, cwesPhase,
                sdNoiseFirstTime=.5) {
    latencies <- getLatenciesBtwCVSsAndCWEsTimes(infoInit=infoInit,
                                                  infoTerm=infoTerm,
                                                  cwes=cwes,
                                                  cwesPhase=cwesPhase)
    t0 <- stat(latencies)
    show(sprintf("t0=%f", t0))
    t <- rep(NA, times=nResamples)
    for(i in 1:nResamples) {
        if(i%%100==0) {
            show(sprintf("Resample %d (%d)", i, nResamples))
            show(sprintf("t=%f", t[i-1]))
        }
        res <- randomizeInfoInitAndTerm(infoInit=infoInit, 
                                         infoTerm=infoTerm, 
                                         sdNoiseFirstTime=sdNoiseFirstTime)
        latencies <- getLatenciesBtwCVSsAndCWEsTimes(infoInit=res$randomInfoInit,
                                                      infoTerm=res$randomInfoTerm,
                                                      cwes=cwes,
                                                      cwesPhase=cwesPhase)
        t[i] <- stat(latencies)
    }
    return(list(t0=t0, t=t))
}

getLatenciesBtwCVSsAndCWEsTimes <- function(infoInit, infoTerm, cwes, 
                                                      cwesPhase) {
    cvsStarts <- infoInit$time
    cvsEnds <- infoTerm$time
    latencies <- c()
    for(i in 1:nrow(cwes)) {
        cweStart <- cwes[i, 1]
        cweEnd <- cwes[i, 2]
        cvsIndices <- getCVSIndicesOverlappingCWE(cweStart=cweStart, cweEnd=cweEnd, cvsStarts=cvsStarts, cvsEnds=cvsEnds)
# res <- getCVSIndicesOverlappingCWE(cweStart=cweStart, cweEnd=cweEnd, cvsStarts=cvsStarts, cvsEnds=cvsEnds)
# cvsIndices <- res$cvsIndices
# cweCaseBands <- res$cweCaseBands
        if(length(cvsIndices)>0) {
            if(cwesPhase=="initiation") {
                for(cvsIndex in cvsIndices) {
                    latency <- cweStart-cvsStarts[cvsIndex]
                    latencies <- c(latencies, latency)
                }
            } else {
                if(cwesPhase=="termination") {
# latencies <- c(latencies, cweEnd-cvsEnds[cvsIndices[1]])
# if(4 %in% cweCaseBands) {
#     browser()
# }
                    for(cvsIndex in cvsIndices) {
                        latency <- cweEnd-cvsEnds[cvsIndex]
                        latencies <- c(latencies, latency)
                    }
                } else {
                    stop(sprintf("Unrecognized cwesPhase=%s", cwesPhase))
                }
            }
        }
    }
#     browser()
    return(latencies)
}
