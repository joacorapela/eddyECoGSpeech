
source("doLoadSources.R")

processAll <- function() {
    # groupNumbers <- 3
    # elecNumbers <- 2
    groupNumbers <- 3
    elecNumbers <- 33:64

    significance <- .5
    timeFreqsFilenamePattern <- "results/EC2_B89/mwtWav%d%d.RData"
    resultsFilenamePattern <- "results/EC2_B89/itcSigWav%d%d.RData"

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
                resultsFilename <- sprintf(resultsFilenamePattern, groupNumber, 
                                                                   elecNumber)
                res <- computeITCFromTimeFreqsMaskedWithRayleighSignificance(
                        timeFreqs=timeFreqs, significance=significance)
                results <- c(res, list(times=times, freqs=freqs))
                save(results, file=resultsFilename)
            }
        }
    }
}

processAll()

rm(processAll)
