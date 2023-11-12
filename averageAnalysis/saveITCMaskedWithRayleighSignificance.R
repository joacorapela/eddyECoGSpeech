
saveITCMaskedWithRayleighSignificance <- function(timeFreqs, significance,
                                                             resultsFilename) {
    res <- computeITCFromTimeFreqs(timeFreqs=timeFreqs,
                                    significance=significance)

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

        res <- computeITCMaskedWithRayleighSignificance(times=epochTimes, 
                                                         trials=epochs, 
                                                         noctave=noctave,
                                                         nvoice=nvoice, 
                                                         w0=w0,
                                                         maskFromTime,
                                                         maskToTime,
                                                         srate=ecogSampleRate,
                                                         significance=
                                                          significance)
        results <- list(times=res$times, freqs=res$freqs,
                                         maskedITC=res$maskedITC)
        save(results, file=resultsFilename)
    }
}

