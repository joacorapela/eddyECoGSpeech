
saveClippedMorletWTs <- function(ecogFilename, 
                                  resultsFilename,
                                  transcriptionFilename,
                                  transcriptionSampleRate,
                                  epochFromTime,
                                  epochToTime,
                                  clipFromTime,
                                  clipToTime,
                                  fmin,
                                  nvoice,
                                  w0,
                                  minSeparation,
                                  decimateFactor) {
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
    epochingTimes <- res$epochingTimes
    ecogSampleRate <- res$srate
    noctave <- ceiling(log2(w0*ecogSampleRate/(2*pi*fmin*(2-1/nvoice))))
    if(!is.null(epochs)) {
        epochTimes <- seq(from=epochFromTime, to=epochToTime, 
                                              by=1/ecogSampleRate)
        res <- performClippedMorletWTs(times=epochTimes, 
                                            trials=epochs, 
                                            noctave=noctave,
                                            nvoice=nvoice, 
                                            w0=w0, 
                                            srate=ecogSampleRate,
                                            clipFromTime=clipFromTime,
                                            clipToTime=clipToTime)
        clippedMWTs <- res$clippedMWTs
        clippedTimes <- res$clippedTimes
        freqs <- getFreqs(noctave=noctave, nvoice=nvoice, w0=w0,
                                           srate=ecogSampleRate)
        results <- list(mwts=clippedMWTs, times=clippedTimes, freqs=freqs,
                                          epochingTimes=epochingTimes)
        save(results, file=resultsFilename)
    }
}

