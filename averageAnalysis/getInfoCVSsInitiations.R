
getInfoCVSsInitiations <- function(transcriptionFilename, 
                                    transcriptionSampleRate, 
                                    ecogSampleRate) {
    transcription <- read.table(transcriptionFilename, col.names=c("startTime", "endTime", "label"), stringsAsFactors=FALSE)
    transcriptionToECoGFactor <- ecogSampleRate/transcriptionSampleRate
    initiationsTimes <- c()
    initiationsSamples <- c()
    cvSyllables <- c()
    prevCVSNumber <- -1
    for(i in 1:nrow(transcription)) {
        labelLength <- nchar(transcription[i,]$label)
        curCVSNumber <- substr(transcription[i,]$label, labelLength, labelLength)
        cvSyllable <- substr(transcription[i,]$label, 1, labelLength-1)
        if(curCVSNumber==1 || (curCVSNumber==3 && prevCVSNumber!=1)) {
            initiationsTimes <- 
             c(initiationsTimes, transcription[i,]$endTime/transcriptionSampleRate)
            initiationsSamples <- 
             c(initiationsSamples, 
                transcription[i,]$endTime*transcriptionToECoGFactor)
            cvSyllables <- c(cvSyllables, cvSyllable)
        }
        prevCVSNumber <- curCVSNumber
    }
    return(data.frame(cvSyllables=cvSyllables, samples=initiationsSamples,
                                               times=initiationsTimes))
}

