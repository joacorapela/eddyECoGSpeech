
resamplePropCWEsInSilence <- 
 function(infoInit, infoTerm, cwes, nResamples,
                    sdNoiseFirstTime=.5) {
    res <- getPropCWEsInSilenceOrCVS(infoInit=infoInit, infoTerm=infoTerm, 
                                                        cwes=cwes)
    t0 <- res$propCWEsInSilence
    t <- rep(NA, times=nResamples)
    for(i in 1:nResamples) {
        if(i%%100==0) {
            show(sprintf("Resample %d (%d)", i, nResamples))
        }
        res <- randomizeInfoInitAndTerm(infoInit=infoInit, infoTerm=infoTerm, 
                                                       sdNoiseFirstTime=
                                                        sdNoiseFirstTime)
        res <- getPropCWEsInSilenceOrCVS(infoInit=res$randomInfoInit, 
                                          infoTerm=res$randomInfoTerm, 
                                          cwes=cwes)
        t[i] <- res$propCWEsInSilence
    }
    return(list(t0=t0, t=t))
}
