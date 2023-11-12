buildHTDatacube <- function(elecNumbers, fromTime, toTime, htFilenamePattern, 
                                         sessionLabel, lowCutoff, highCutoff, 
                                         order, zScore, desiredSampleRate, 
                                         nrow, ncol) {
    firstElectrode <- TRUE

    for(elecNumber in elecNumbers) {
        show(sprintf("Processing electrode %d", elecNumber))
        res <- getGroupAndElecNumber(elecNumber=elecNumber)
        htFilename <- sprintf(htFilenamePattern, 
                               sessionLabel,
                               lowCutoff, highCutoff, order, zScore,
                               res$groupNumber, res$elecNumber)
        if(file.exists(htFilename)) {
            htLoadRes <- get(load(htFilename))
            if(length(htLoadRes$ht)>0) {
                if(firstElectrode) {
                    times <- ((1:length(htLoadRes$ht))-1)/
                              htLoadRes$ecogSampleRate
                    samplesToSave <- which(fromTime<=times & times<=toTime)
                    timesToSave <- times[samplesToSave]
                    htsArray <- array(0, dim=c(nrow, ncol, 
                                                     length(samplesToSave)))
                    firstElectrode <- FALSE
                }
                electrodeIndexInArray <- 
                 getElectrodeIndexInArrayGGPlot(elecNumber=elecNumber)
                htsArray[electrodeIndexInArray[1], electrodeIndexInArray[2],] <-
                 htLoadRes$ht[samplesToSave]
            }
        } else {
            warning(sprintf("File %s does not exist", htFilename))
        }
    }
    return(list(htsArray=htsArray, timesToSave=timesToSave, 
                                   sampleRate=htLoadRes$ecogSampleRate))
}
