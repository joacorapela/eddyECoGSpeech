

source("doLoadSources.R")

processAll <- function() {
    sessionLabel <- "EC2_B105"
    elecNumber <- 136
    lowCutoff <- 80.0
    highCutoff <- 110.0
    desiredFrameRates <- 25:600
    htFilenamePattern <- "results/%s/htFilteredFrom%.02fTo%.02fWav%d%d.RData"

    res <- getGroupAndElecNumber(elecNumber=elecNumber)
    htFilename <- sprintf(htFilenamePattern, sessionLabel,
                                             lowCutoff, highCutoff,
                                             res$groupNumber, 
                                             res$elecNumber)
    if(file.exists(htFilename)) {
        loadRes <- get(load(htFilename))
        decimateFactors <- loadRes$ecogSampleRate/desiredFrameRates
        df <- data.frame(desiredFrameRates=desiredFrameRates,
                          decimateFactorsReminders=decimateFactors%%1)
        sortCriteria <-
         apply(X=cbind(df$decimateFactorsReminders, 
                        1-df$decimateFactorsReminders), 
                MARGIN=1, FUN=min)
        sortedDF <- df[order(sortCriteria),]
        show(sortedDF)
    }
}

processAll()

rm(processAll)
