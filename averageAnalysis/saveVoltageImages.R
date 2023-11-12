saveVoltageImages <- function(bandpassedFilenamePattern,
                               figDirnamePattern,
                               figFilenamePattern,
                               elecNumbers,
                               lowCutoff, highCutoff,
                               fromTime, toTime,
                               desiredFrameRate,
                               titlePattern,
                               nrow, ncol,
                               width, height) {
    firstElectrode <- TRUE

    for(elecNumber in elecNumbers) {
        show(sprintf("Processing electrode %d", elecNumber))
        res <- getGroupAndElecNumber(elecNumber=elecNumber)
        bandpassedFilename <- sprintf(bandpassedFilenamePattern, 
                                       lowCutoff, highCutoff, 
                                       res$groupNumber, res$elecNumber)
        electrodeIndexInArray <- getElectrodeIndexInArray(elecNumber=elecNumber)
        if(file.exists(bandpassedFilename)) {
            loadRes <- get(load(bandpassedFilename))
            if(length(loadRes$filteredECoG)>0) {
                if(firstElectrode) {
                    decimateFactor <- round(loadRes$ecogSampleRate/desiredFrameRate)
                    electrodeVs <- decimate(x=loadRes$filteredECoG, 
                                             q=decimateFactor)
                    actualFrameRate <- loadRes$ecogSampleRate/decimateFactor
                    times <- ((1:length(electrodeVs))-1)/actualFrameRate
                    samplesToSave <- which(fromTime<=times & times<=toTime)
                    timesToSave <- times[samplesToSave]
                    voltagesArray <- array(NaN, 
                                            dim=c(nrow, ncol,
                                                   length(samplesToSave)))
                    firstElectrode <- FALSE
                } else {
                    electrodeVs <- decimate(x=loadRes$filteredECoG, 
                                            q=decimateFactor)
                }
                voltagesArray[electrodeIndexInArray[1], 
                               electrodeIndexInArray[2],] <- 
                 electrodeVs[samplesToSave]
            }
        }
    }
    figDirname <- sprintf(figDirnamePattern, actualFrameRate, fromTime, toTime)
    if(!file.exists(figDirname)) {
        dir.create(figDirname)
    }
    for(i in 1:dim(voltagesArray)[3]) {
        figFilename <- sprintf(figFilenamePattern, lowCutoff, highCutoff, i)
        figAbsFilename <- sprintf("%s/%s", figDirname, figFilename)
        timeInSecs <- timesToSave[i]-timesToSave[1]
        minutes <- timeInSecs%/%60
        seconds <- as.integer(timeInSecs%%60)
        title <- sprintf(titlePattern, minutes, seconds)
        trellis.device("png", file=figAbsFilename)
        image(voltagesArray[,,i], xaxt="n", yaxt="n", main=title)
        dev.off()
    }
}
