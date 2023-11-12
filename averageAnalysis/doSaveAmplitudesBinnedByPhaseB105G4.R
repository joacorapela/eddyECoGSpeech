
source("doLoadSources.R")

processAll <- function() {
    # groupNumbers <- 3
    # elecNumbers <- 36
    # freqForAmplitudes <- 100.0

    groupNumbers <- 4
    elecNumbers <- 1:64
    freqForAmplitudes <- 100.0

    freqForPhases <- 1/1.62
    nBins <- 18
    phaseBinsBreaks <- seq(from=-pi, to=pi, length.out=nBins+1)
    # the breaks should not include pi or -pi 
    phaseBinsBreaks <- phaseBinsBreaks[2:nBins] 
    #
    timeFreqsFilenamePattern <- "results/EC2_B105/mwtWav%d%d.RData"
    resultsFilenamePattern <- "results/EC2_B105/amplitudesBinnedByPhaseWav%d%d_freqPhase%.2f_freqAmp%.2f.RData"

    for(groupNumber in groupNumbers) {
        for(elecNumber in elecNumbers) {
            show(sprintf("Processing Wav%d%d", groupNumber, elecNumber))
            timeFreqsFilename <- sprintf(timeFreqsFilenamePattern, groupNumber, 
                                                                   elecNumber)
            if(file.exists(timeFreqsFilename)) {
                res <- get(load(timeFreqsFilename))
                timeFreqs <- res$mwt
                times <- res$times
                freqs <- res$freqs
                resultsFilename <- sprintf(resultsFilenamePattern, 
                                            groupNumber, 
                                            elecNumber,
                                            freqForPhases,
                                            freqForAmplitudes)
                res <- binTrialsAmplitudesByPhaseFromTimeFreqs(
                        timeFreqs=timeFreqs,
                        freqs=freqs,                 
                        freqForPhases=freqForPhases,
                        freqForAmplitudes=freqForAmplitudes,
                        phaseBinsBreaks=phaseBinsBreaks)
                save(res, file=resultsFilename)
            }
        }
    }
}

processAll()

rm(processAll)
