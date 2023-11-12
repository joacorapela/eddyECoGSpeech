
source("doLoadSources.R")

processAll <- function() {
    # sessionLabels <- c("EC2_B1", "EC2_B15", "EC2_B8", "EC2_B9", "EC2_B89", "EC2_B76", "EC2_B105")
    sessionLabels <- c("EC2_B105")
    ecogFilenamePattern <- "../data/rData/%s/RawHTK/Wav%d%d.bin"
    transcriptionFilenamePattern <- "../data/transcriptionFiles/%s/%s_transcription_final_completed.lab"
    figFilenamePattern <- "figures/%s/histSyllablesSeparations.eps"
    transcriptionSampleRate <- 1e7
    groupNumber <- 2
    elecNumber <- 1
    maxISS <- 10
    # breaks <- 100
    breaks <- "Sturges"
    # xlim <- c(0, 6)
    xlim <- range(breaks)
    width <- 6
    height <- 6
    histMain <- NA
    histXLab <- "Inter-Syllable Separation (sec)" 
    ablineCol <- "red"

    for(sessionLabel in sessionLabels) {
        transcriptionFilename <- sprintf(transcriptionFilenamePattern,
                                          sessionLabel, sessionLabel)
        figFilename <- sprintf(figFilenamePattern, sessionLabel)
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
        trellis.device("postscript", color=TRUE, width=width, height=height,
                       onefile=FALSE, file=figFilename)
#         hist(cvSeparations, main=histMain, xlab=histXLab,
#                             xlim=xlim, breaks=breaks)
        medianCVSeparation <- median(cvSeparations)
        if(is.na(histMain)) {
            histMain <- sprintf("Median = %.02f", medianCVSeparation)
        }
        hist(cvSeparations, main=histMain, xlab=histXLab)
        abline(v=medianCVSeparation, col=ablineCol)
        dev.off()
    }
}

processAll()

rm(processAll)
