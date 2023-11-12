
getInfoCVSsTerminations <- function(transcriptionFilename, 
                                     transcriptionSampleRate, 
                                     ecogSampleRate) {
    transcription <- read.table(transcriptionFilename, col.names=c("startTime", "endTime", "label"), stringsAsFactors=FALSE)
    transcriptionToECoGFactor <- ecogSampleRate/transcriptionSampleRate
    terminationsTimes <- c()
    terminationsSamples <- c()
    cvSyllables <- c()
    for(i in 1:nrow(transcription)) {
        labelLength <- nchar(transcription[i,]$label)
        cvsNumber <- substr(transcription[i,]$label, labelLength, labelLength)
        cvSyllable <- substr(transcription[i,]$label, 1, labelLength-1)
        if(cvsNumber==2) {
            terminationsTimes <- 
             c(terminationsTimes, 
                transcription[i,]$endTime/transcriptionSampleRate)
            terminationsSamples <- 
             c(terminationsSamples, 
                transcription[i,]$endTime*transcriptionToECoGFactor)
            cvSyllables <- c(cvSyllables, cvSyllable)
        }
    }
    answer <- data.frame(cvSyllables=cvSyllables, samples=terminationsSamples,
                                                  times=terminationsTimes)
    return(answer)
}

