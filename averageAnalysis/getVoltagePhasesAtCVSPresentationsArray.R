getVoltagePhasesAtCVSPresentationsArray <- function(sessionLabel, 
                                                     bandpassedFilenamePattern, 
                                                     elecNumbers, 
                                                     lowCutoff, highCutoff, 
                                                     order,
                                                     fromTime, toTime, 
                                                     transcriptionFilename,
                                                     transcriptionSampleRate,
                                                     peaksLowpassCutoff,
                                                     peaksLowpassTransitionWidth,
                                                     peaksLowpassRipple,
                                                     plotLowpass,
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
                    resInitiations <- 
                     getInfoCVSsInitiations(transcriptionFilename=
                                              transcriptionFilename,
                                             transcriptionSampleRate=
                                              transcriptionSampleRate,
                                             ecogSampleRate=
                                              loadRes$ecogSampleRate)
                    phasesArray <- array(NaN, 
                                            dim=c(nrow, ncol,
                                                   length(resInitiations$times)))
                    firstElectrode <- FALSE
                }
                voltages <- loadRes$filteredECoG
                times <- ((1:length(voltages))-1)/loadRes$ecogSampleRate
                peaksInfo <- getClosestPeakInfoForCVSs(x=voltages, 
                                                        times=times,
                                                        cvsStartTimes=
                                                         resInitiations$times, 
                                                        lowpassCutoffHz=peaksLowpassCutoff,
                                                        lowpassTransitionWidthHz=
                                                         peaksLowpassTransitionWidth,
                                                        lowpassRipple=peaksLowpassRipple,
                                                        sampleRate=loadRes$ecogSampleRate,
                                                        plotLowpass=plotLowpass)
                phasesArray[electrodeIndexInArray[1], electrodeIndexInArray[2],] <-
                 peaksInfo$cvsPhases
            }
        } else {
            warning(sprintf("File %s does not exist", bandpassedFilename))
        }
    }
    return(list(phasesArray=phasesArray, timesToSave=resInitiations$times))
}
