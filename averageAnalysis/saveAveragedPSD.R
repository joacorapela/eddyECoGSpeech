
saveAveragedPSD <- function(ecogFilename, 
                             resultsFilename,
                             transcriptionFilename,
                             transcriptionSampleRate,
                             epochFromTime,
                             epochToTime,
                             desiredFrameRate,
                             minSeparation,
                             spans, detrend, log, plot=FALSE,
                             nResamples, conf) {
    res <- getECoGData(ecogFilename=ecogFilename)
    ecogData <- res$ecogData*10^6
    ecogSampleRate <- res$ecogSampleRate
    decimateFactor <- round(ecogSampleRate/desiredFrameRate)
    downsampledECoGData <- decimate(x=ecogData, q=decimateFactor)
    actualFrameRate <- ecogSampleRate/decimateFactor
    res <- getNonOverlappingEpochs(ecogData=downsampledECoGData, 
                                    ecogSampleRate=actualFrameRate,
                                    transcriptionFilename=
                                     transcriptionFilename, 
                                    transcriptionSampleRate=
                                     transcriptionSampleRate,
                                    epochFromTime=epochFromTime,
                                    epochToTime=epochToTime,
                                    minSeparation=minSeparation)
    epochs <- res$epochs
    ecogSampleRate <- res$srate
    if(!is.null(epochs)) {
        res <- calculateAveragedPSD(epochs=epochs, 
                                     ecogSampleRate=ecogSampleRate, 
                                     spans=spans, detrend=detrend, log=log, 
                                     nResamples=nResamples, conf=conf, 
                                     plot=plot)
        results <- list(freq=res$freq,
                         averagedPSD=res$averagedPSD, 
                         averagedPSDCIUpper=res$averagedPSDCIUpper,  
                         averagedPSDCILower=res$averagedPSDCILower)
        save(results, file=resultsFilename)
    }
}

calculateAveragedPSD <- function(epochs, ecogSampleRate, spans, detrend, log,
                                         plot=FALSE, nResamples, conf) {
    allFreqs <- c()
    allSpecs <- c()
    for(i in 1:ncol(epochs)) {
        # show(sprintf("Processing %d (%d)", i, ncol(epochs)))
        aTS <- ts(data=epochs[,i], frequency=ecogSampleRate)
        res <- spectrum(x=aTS, spans=spans, detrend=detrend, log=log, 
                               plot=plot, main=sprintf("%d", elecNumber))
        allFreqs <- rbind(allFreqs, res$freq)
        allSpecs <- rbind(allSpecs, res$spec)
    }
# plot(colMeans(allFreqs), colMeans(allSpecs), log="xy", type="l"); abline(v=0.62, col="red"); grid()
# browser()
    bootRes <- bootstrapColMeans(x=allSpecs, nResamples=nResamples)
    bootCIs <- getBootstrapCIs(bootRes=bootRes, conf=conf)
    averagedPSD <- bootRes$t0
    averagedPSDCILower <- bootCIs[,2]
    averagedPSDCIUpper <- bootCIs[,3]
    freqs <- colMeans(allFreqs)
    answer <- list(freq=freqs, averagedPSD=averagedPSD,
                               averagedPSDCILower=averagedPSDCILower,
                               averagedPSDCIUpper=averagedPSDCIUpper)
    return(answer)
}
