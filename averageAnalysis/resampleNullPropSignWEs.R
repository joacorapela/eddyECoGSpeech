resampleNullPropSignTWs <- function(propFunc, waveEvents, significance,
                                              beforeLag, afterLag,
                                              sdNoiseFirstTime,
                                              infoInit, infoTerm, nResamples) {
    cvsProductionTimingInfo <- data.frame(startTime=infoInit$time,
                                           endTime=infoTerm$time)
    res <- propFunc(cvsProductionTimingInfo=cvsProductionTimingInfo,
                     waveEvents=waveEvents, beforeLag=beforeLag, 
                     afterLag=afterLag, significance=significance)
    t0 <- res$proportion
    show(sprintf("t0=%f", t0))
    t <- rep(NA, times=nResamples)
    for(i in 1:nResamples) {
        if(i%%100==0) {
            show(sprintf("Resample %d (%d)", i, nResamples))
            show(sprintf("t[%d]=%f", i-1, t[i-1]))
        }
        res <- randomizeInfoInitAndTerm(infoInit=infoInit, 
                                         infoTerm=infoTerm, 
                                         sdNoiseFirstTime=sdNoiseFirstTime)
        cvsProductionTimingInfo <- 
         data.frame(startTime=res$randomInfoInit$times,
                     endTime=res$randomInfoTerm$times)
        res <- propFunc(cvsProductionTimingInfo=cvsProductionTimingInfo,
                         waveEvents=waveEvents, beforeLag=beforeLag, 
                         afterLag=afterLag, significance=significance)
        t[i] <- res$proportion
    }
    return(list(t0=t0, t=t))
}
