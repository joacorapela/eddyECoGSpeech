getCVSBPVoltagesArray <- function(cvs, sessionLabel, 
                                       transcriptionSampleRate,
                                       transcriptionFilename,
                                       bandpassedFilenamePattern, 
                                       elecNumbers, 
                                       lowCutoff, highCutoff, order,
                                       fromTimeAfterCVSInitiation, 
                                       maxToTimeAfterCVSInitiation, 
                                       pauseTimeBTWCVSs,
                                       pauseValueBTWCVSs,
                                       desiredFrameRate,
                                       nrow, ncol) {

    firstElectrode <- TRUE

    for(elecNumber in elecNumbers) {
        show(sprintf("Processing electrode %d", elecNumber))
        res <- getGroupAndElecNumber(elecNumber=elecNumber)
        bandpassedFilename <- sprintf(bandpassedFilenamePattern, 
                                       sessionLabel,
                                       lowCutoff, highCutoff, order,
                                       res$groupNumber, res$elecNumber)
        electrodeIndexInArray <-
         getElectrodeIndexInArrayGGPlot(elecNumber=elecNumber)
        if(file.exists(bandpassedFilename)) {
            loadRes <- get(load(bandpassedFilename))
            if(length(loadRes$filteredECoG)>0) {
                if(firstElectrode) {
                    downsampleFactor <-
                     round(loadRes$ecogSampleRate/desiredFrameRate)
                    actualFrameRate <- loadRes$ecogSampleRate/downsampleFactor
                    voltages <- decimate(x=loadRes$filteredECoG, 
                                          q=downsampleFactor)
                    infoInit <- 
                     getInfoCVSsInitiations(transcriptionFilename=
                                              transcriptionFilename,
                                             transcriptionSampleRate=
                                              transcriptionSampleRate,
                                             ecogSampleRate=actualFrameRate)
                    cvsSamplesRes <- getCVSSamples(cvs=cvs, 
                                                    cvss=infoInit$cvSyllables,
                                                    cvssInitiationSamples=
                                                     infoInit$samples,
                                                    fromTimeAfterCVSInitiation=
                                                     fromTimeAfterCVSInitiation,
                                                    maxToTimeAfterCVSInitiation=
                                                     maxToTimeAfterCVSInitiation,
                                                    sampleRate=actualFrameRate)
                    pauseNFramesBTWCVSs <- pauseTimeBTWCVSs*actualFrameRate
                    pauseSamples <- rep(pauseValueBTWCVSs, 
                                         times=pauseNFramesBTWCVSs)
                    nRepeatsCVS <- length(cvsSamplesRes$byRepeat)
                    timesToSave <- c()
                    for(i in 1:nRepeatsCVS) {
                        timesToSave <- c(timesToSave, 
                                          cvsSamplesRes$byRepeat[[i]]/
                                           actualFrameRate, 
                                          rep(0, times=pauseNFramesBTWCVSs))
                    }
                    totalNCVSSamples <- length(cvsSamplesRes$all)
                    totalNFrames <- totalNCVSSamples+
                                     nRepeatsCVS*length(pauseSamples)
                    voltagesArray <- array(NaN, dim=c(nrow, ncol, totalNFrames))
                    firstElectrode <- FALSE
                } else {
                    voltages <- decimate(x=loadRes$filteredECoG,
                                          q=downsampleFactor)
                }
                meanVoltagesForElectrode <- mean(voltages[cvsSamplesRes$all])
                sdVoltagesForElectrode <- sd(voltages[cvsSamplesRes$all])
                voltagesForElectrode <- c()
                for(i in 1:nRepeatsCVS) {
                    voltagesForOnePresentation <- voltages[cvsSamplesRes$byRepeat[[i]]]
                    zVoltagesForOnePresentation <- (voltagesForOnePresentation-
                                                     meanVoltagesForElectrode)/
                                                    sdVoltagesForElectrode
                    voltagesForElectrode <- c(voltagesForElectrode, 
                                               c(zVoltagesForOnePresentation, 
                                                  pauseSamples))
                }
                voltagesArray[electrodeIndexInArray[1], 
                             electrodeIndexInArray[2],] <- voltagesForElectrode
            }
        } else {
            warning(sprintf("File %s does not exist", bandpassedFilename))
        }
    }
    return(list(voltagesArray=voltagesArray, actualFrameRate=actualFrameRate, 
                                             timesToSave=timesToSave))
}
