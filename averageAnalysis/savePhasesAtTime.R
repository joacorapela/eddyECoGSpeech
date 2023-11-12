
savePhasesAtTime <- function(ecogFilename, 
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
                              time) {
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
        epochTimes <- seq(from=epochFromTime, to=epochToTime, 
                                              by=1/ecogSampleRate)
        results <- getPhasesAtTime(times=epochTimes, 
                                    trials=epochs, 
                                    noctave=noctave,
                                    nvoice=nvoice, 
                                    w0=w0, 
                                    srate=ecogSampleRate,
                                    time=time)
        save(results, file=resultsFilename)
    }
}

