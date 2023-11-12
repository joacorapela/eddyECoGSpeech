
source("/home/rapela/dev/research/programs/src/R/ees/syncTWsAndCVSs/randomizeInfoInitAndTerm.R")
source("/home/rapela/dev/research/programs/src/R/ees/infoCVSs/getInfoCVSsInitiations.R")
source("/home/rapela/dev/research/programs/src/R/ees/infoCVSs/getInfoCVSsTerminations.R")

writeTranscriptionFile <- function(filename, infoInit, infoTerm,
                                             transcriptionSampleRate) {
    con <- file(filename, open="wt")
    for(i in 1:nrow(infoInit)) {
        writeLines(sprintf("0000000000 %.0f %s1",
                           round(infoInit$time[i]*transcriptionSampleRate),
                           infoInit$cvSyllable[i]), con)
        writeLines(sprintf("0000000000 %.0f %s2",
                           round(infoTerm$time[i]*transcriptionSampleRate),
                           infoTerm$cvSyllable[i]), con)
    }
    close(con)
}

processAll <- function() {
    sessionName <- "EC2_B105"
    transcriptionSampleRate <- 1e7
    transcriptionToECoGFactor <- transcriptionSampleRate/transcriptionSampleRate
    transcriptionFilename <-
     "../../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final_completed.lab"
    randomizedTranscriptionFilename <-
     "../../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final_completed_randomized.lab"

    infoInit <- getInfoCVSsInitiations(transcriptionFilename=
                                         transcriptionFilename,
                                        transcriptionSampleRate=
                                         transcriptionSampleRate,
                                        ecogSampleRate=transcriptionSampleRate)
    infoTerm <- getInfoCVSsTerminations(transcriptionFilename=
                                          transcriptionFilename,
                                         transcriptionSampleRate=
                                          transcriptionSampleRate,
                                         ecogSampleRate=
                                          transcriptionSampleRate)
    res <- randomizeInfoInitAndTerm(infoInit=infoInit, infoTerm=infoTerm, sdNoiseFirstTime=5)
    writeTranscriptionFile(filename=randomizedTranscriptionFilename,
                            infoInit=res$randomInfoInit,
                            infoTerm=res$randomInfoTerm,
                            transcriptionSampleRate=transcriptionSampleRate)
}

processAll()

rm(processAll)
