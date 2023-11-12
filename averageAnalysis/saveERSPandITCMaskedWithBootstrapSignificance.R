
saveERSPandITCMaskedWithBootstrapSignificance <- 
 function(ecogFilename, 
           resultsFilename,
           transcriptionFilename,
           transcriptionSampleRate,
           epochFromTime,
           epochToTime,
           fmin,
           nvoice,
           w0,
           minSeparation,
           decimateFactor,
           baselineLimits,
           nResamples,
           conf) {
    res <- getNonOverlappingEpochs(ecogFilename=ecogFilename, 
                                    transcriptionFilename=
                                     transcriptionFilename, 
                                    transcriptionSampleRate=
                                     transcriptionSampleRate,
                                    epochFromTime=epochFromTime,
                                    epochToTime=epochToTime,
                                    decimateFactor=decimateFactor,
                                    minSeparation=minSeparation)
    epochs <- res$epochs
    ecogSampleRate <- res$srate
    noctave <- ceiling(log2(w0*ecogSampleRate/(2*pi*fmin*(2-1/nvoice))))
    if(!is.null(epochs)) {
        erp <- rowMeans(epochs)
        epochTimes <- seq(from=epochFromTime, to=epochToTime, 
                                              by=1/ecogSampleRate)
        baselineIndices <- which(baselineLimits[1]<=epochTimes & 
                                  epochTimes<=baselineLimits[2])
        res <- 
         computeERSPandITCMaskedWithBootstrapSignificance(times=epochTimes, 
                                                           trials=epochs, 
                                                           noctave=noctave,
                                                           nvoice=nvoice, 
                                                           w0=w0, 
                                                           srate=ecogSampleRate,
                                                           baselineIndices=
                                                            baselineIndices,
                                                           nResamples=
                                                            nResamples,
                                                           conf=conf)
        results <- list(times=res$times, freqs=res$freqs,
                                         maskedERSP=res$maskedERSP,
                                         maskedITC=res$maskedITC)
        save(results, file=resultsFilename)
    }
}

