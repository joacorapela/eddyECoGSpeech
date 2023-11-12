getHTsAndTimes <- function(elecNumbers, htFilenamePattern) {
    hts <- c()
    times <- NA
    for(i in 1:length(elecNumbers)) {
        res <- getGroupAndElecNumber(elecNumber=elecNumbers[i])
        groupNumber <- res$groupNumber
        elecNumber <- res$elecNumber
        htFilename <- sprintf(htFilenamePattern, groupNumber, elecNumber)
        if(file.exists(htFilename)) {
            res <- get(load(htFilename))
            hts <- cbind(hts, res$ht)
            if(is.na(times[1])) {
                times <- ((1:length(res$ht))-1)/res$ecogSampleRate
            }
        } else {
            stop(sprintf("Analitic signal file not found: %s", htFilename))
        }
    }
    answer <- list(hts=hts, times=times)
}
