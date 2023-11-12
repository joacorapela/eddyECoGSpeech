
source("doLoadSources.R")

processAll <- function() {
    # groupNumbers <- 4
    # elecNumbers <- 2
    groupNumbers <- 4
    elecNumbers <- 1:32

    baselineLimits <- c(593, 634)/1000
    nResamples <- 200
    conf <- .95
    timeFreqsFilenamePattern <- "results/EC2_B89/mwtWav%d%d.RData"
    resultsFilenamePattern <- "results/EC2_B89/erspSigWav%d%d.RData"

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
                res <- computeERSPFromTimeFreqsMaskedWithBootstrapSignificance(
                        timeFreqs=timeFreqs, 
                        times=times,
                        baselineLimits=baselineLimits,
                        nResamples=nResamples,
                        conf=conf)
                results <- c(res, list(times=times, freqs=freqs))
                save(results, file=resultsFilename)
            }
        }
    }
}

processAll()

rm(processAll)
