getCVSSamples <- function(cvs, cvss, cvssInitiationSamples, 
                               fromTimeAfterCVSInitiation, 
                               maxToTimeAfterCVSInitiation, sampleRate) {
    indicesCVSsMatchingCVS <- which(cvs==cvss)
    fromSampleAfterCVSInitiation <- fromTimeAfterCVSInitiation*sampleRate
    maxToSampleAfterCVSInitiation <- maxToTimeAfterCVSInitiation*sampleRate
    cvsSamplesByRepeat <- list()
    cvsSamplesAll <- c()
    for(i in 1:length(indicesCVSsMatchingCVS)) {
        cvsInitiationSample <- cvssInitiationSamples[indicesCVSsMatchingCVS[i]]
        nextCVSInitiationSample <- cvssInitiationSamples[indicesCVSsMatchingCVS[i]+1]
        fromSample <- cvsInitiationSample+fromSampleAfterCVSInitiation
        toSample <- min(cvsInitiationSample+maxToSampleAfterCVSInitiation, 
                         nextCVSInitiationSample)
        repeatSamples <- seq(from=fromSample, to=toSample)
        cvsSamplesByRepeat[[i]] <- repeatSamples
        cvsSamplesAll <- c(cvsSamplesAll, repeatSamples)
    }
    return(list(byRepeat=cvsSamplesByRepeat, all=cvsSamplesAll))
}
