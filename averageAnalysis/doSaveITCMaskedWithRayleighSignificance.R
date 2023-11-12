
source("doLoadSources.R")

processAll <- function() {
    sessionLabel <- "EC2_B1"
    elecNumbers <- c(129:134, 137:144)

    significance <- .5
    timeFreqsFilenamePattern <- "results/%s/mwtWav%d%d.RData"
    resultsFilenamePattern <- "results/%s/itcSigWav%d%d.RData"

    for(elecNumber in elecNumbers) {
        show(sprintf("Processing electrode %d", elecNumber))
        resGroupElecNumbers <- getGroupAndElecNumber(elecNumber=elecNumber)
        timeFreqsFilename <- sprintf(timeFreqsFilenamePattern, 
                                      sessionLabel,
                                      resGroupElecNumbers$groupNumber, 
                                      resGroupElecNumbers$elecNumber)
        if(file.exists(timeFreqsFilename)) {
            res <- get(load(timeFreqsFilename))
            timeFreqs <- res$mwt
            times <- res$times
            freqs <- res$freqs
            resultsFilename <- sprintf(resultsFilenamePattern, 
                                        sessionLabel,
                                     resGroupElecNumbers$groupNumber,
                                      resGroupElecNumbers$elecNumber)
            res <- computeITCFromTimeFreqsMaskedWithRayleighSignificance(
                    timeFreqs=timeFreqs, significance=significance)
            results <- c(res, list(times=times, freqs=freqs))
            save(results, file=resultsFilename)
        } else {
            show(sprintf("File %s does not exist", timeFreqsFilename))
        }
    }
}

processAll()

rm(processAll)
