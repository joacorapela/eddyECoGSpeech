
getInfoCVSsCVTransitions <- function(transcriptionFilename, 
                                      transcriptionSampleRate, 
                                      ecogSampleRate) {
    transcription <- read.table(transcriptionFilename, col.names=c("startTime", "endTime", "label"), stringsAsFactors=FALSE)
    transcriptionToECoGFactor <- ecogSampleRate/transcriptionSampleRate
    transitionsTimes <- c()
    transitionsSamples <- c()
    cvSyllables <- c()
    labelLength <- nchar(transcription[1,]$label)
    nextCVSNumber <- substr(transcription[1,]$label, labelLength, labelLength)
    for(i in 1:(nrow(transcription)-1)) {
        labelLength <- nchar(transcription[i,]$label)
        cvSyllable <- substr(transcription[i,]$label, 1, labelLength-1)
        currentCVSNumber <- nextCVSNumber
        nextCVSNumber <- substr(transcription[i+1,]$label, labelLength, 
                                                           labelLength)
        if((currentCVSNumber==3 && nextCVSNumber!=4) || currentCVSNumber==4) {
            transitionsTimes <- 
             c(transitionsTimes, 
                transcription[i,]$endTime/transcriptionSampleRate)
            transitionsSamples <- 
             c(transitionsSamples, 
                transcription[i,]$startTime*transcriptionToECoGFactor)
            cvSyllables <- c(cvSyllables, cvSyllable)
        }
    }
    i <- nrow(transcription)
    labelLength <- nchar(transcription[i,]$label)
    cvSyllable <- substr(transcription[i,]$label, 1, labelLength-1)
    currentCVSNumber <- nextCVSNumber
    if(currentCVSNumber==3) {
        transitionsTimes <- 
         c(transitionsTimes, transcription[i,]$endTime/transcriptionSampleRate)
        transitionsSamples <- 
         c(transitionsSamples, 
            transcription[i,]$startTime*transcriptionToECoGFactor)
        cvSyllables <- c(cvSyllables, cvSyllable)
    }
    answer <- data.frame(cvSyllables=cvSyllables, samples=transitionsSamples,
                                                  times=transitionsTimes)
    return(answer)
}

