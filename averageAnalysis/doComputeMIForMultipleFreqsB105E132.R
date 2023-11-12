
source("doLoadSources.R")

processAll <- function() {
    groupNumbers <- 3
    elecNumbers <- 4
    # groupNumbers <- 1
    # elecNumbers <- 1:64

    freqsForPhases <- c(seq(from=0.1, to=2.9, by=0.1), seq(from=3.0, to=20.0, by=1.0))
    freqsForAmplitudes <- seq(from=5, to=200, by=1.0)

    nBins <- 18
    timeFreqsFilenamePattern <- "results/EC2_B105/mwtWav%d%d.RData"
    resultsFilenamePattern <- "results/EC2_B105/misForMultipleFreqsWav%d%d.RData"

    for(groupNumber in groupNumbers) {
        for(elecNumber in elecNumbers) {
            show(sprintf("Processing electrode %d", 
                         (groupNumber-1)*64+elecNumber))
            timeFreqsFilename <- sprintf(timeFreqsFilenamePattern, groupNumber, 
                                                                   elecNumber)
            if(file.exists(timeFreqsFilename)) {
                res <- get(load(timeFreqsFilename))
                timeFreqs <- res$mwt
                times <- res$times
                freqs <- res$freqs
                resultsFilename <- sprintf(resultsFilenamePattern, 
                                            groupNumber, 
                                            elecNumber)
                res <- computeMIForMultipleFreqs(timeFreqs=timeFreqs, 
                                                  times=times, freqs=freqs,
                                                  freqsForPhases=freqsForPhases,
                                                  freqsForAmplitudes=
                                                  freqsForAmplitudes,
                                                 nBins=nBins)
                save(res, file=resultsFilename)
            }
        }
    }
}

processAll()

rm(processAll)
