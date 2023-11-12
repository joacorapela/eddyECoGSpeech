
source("doLoadSources.R")

processAll <- function() {
    sessionName <- "EC2_B105"
    desiredFrameRate <- 25
    transcriptionSampleRate <- 1e7
    transcriptionFilenamePattern <-
     "../data/transcriptionFiles/%s/%s_transcription_final_completed.lab"
    outFilenamePattern <- "results/%s/nCVSRepeats.txt"

    transcriptionFilename <- sprintf(transcriptionFilenamePattern, sessionName,
                                                                   sessionName)
    outFilename <- sprintf(outFilenamePattern, sessionName)
    infoInit <- getInfoCVSsInitiations(transcriptionFilename=
                                         transcriptionFilename,
                                        transcriptionSampleRate=
                                         transcriptionSampleRate,
                                        ecogSampleRate=desiredFrameRate)
    cvss <- infoInit$cvSyllables
    uniqueCVSs <- unique(cvss)
    nRepeats <- array(data=NA, dim=length(uniqueCVSs))
    for(i in 1:length(uniqueCVSs)) {
        nRepeats[i] <- length(which(uniqueCVSs[i]==cvss))
    }
    sortRes <- sort(nRepeats, index.return=TRUE)
    con <- file(outFilename, open="wt")
    for(i in 1:length(uniqueCVSs)) {
        writeLines(sprintf("%s: %d", 
                           uniqueCVSs[sortRes$ix[i]], nRepeats[sortRes$ix[i]]),
                    con=con)
    }
    close(con)
}

processAll()

rm(processAll)
