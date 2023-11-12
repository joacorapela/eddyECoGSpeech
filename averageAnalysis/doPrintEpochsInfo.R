
source("doLoadSources.R")

processAll <- function() {
    epochsInfoFilename <- "results/epochsInfo.txt"
    sessionLabels <- c("EC2_B1", "EC2_B15", "EC2_B8", "EC2_B9", "EC2_B89", 
                       "EC2_B76", "EC2_B105")
    ecogFilenamePattern <- "rData/%s/RawHTK/Wav%d%d.bin"
    transcriptionFilenamePattern <- "matlabData/%s/%s_transcription_final.lab"
    transcriptionSampleRate <- 1e7
    groupNumber <- 2
    elecNumber <- 1
    maxISS <- 10

    out <- file(epochsInfoFilename, "w")
    for(sessionLabel in sessionLabels) {
        transcriptionFilename <- sprintf(transcriptionFilenamePattern,
                                          sessionLabel, sessionLabel)
        ecogFilename <- sprintf(ecogFilenamePattern, sessionLabel, groupNumber, 
                                                     elecNumber)
        ecogSampleRate <- getECoGSampleRate(ecogFilename=ecogFilename)
        res <- getEpochsSamples(transcriptionFilename=transcriptionFilename,
                                 transcriptionSampleRate=
                                  transcriptionSampleRate,
                                 ecogSampleRate=ecogSampleRate)
        epochsSamples <- res$samples

        cvTransitionsTimes <- epochsSamples/ecogSampleRate
        cvSeparations <- cvTransitionsTimes[2:length(cvTransitionsTimes)]-
                          cvTransitionsTimes[1:(length(cvTransitionsTimes)-1)]
        cvSeparations <- cvSeparations[cvSeparations<maxISS]
        writeLines(sprintf("%s: numberOfCVSyllables=%d, medianCVSeparation=%.2f, sdCVSeparation=%.2f", 
                           ecogFilename, length(epochsSamples), 
                           median(cvSeparations), sd(cvSeparations)),
                    con=out)
    }
    close(out)
}

processAll()

rm(processAll)
