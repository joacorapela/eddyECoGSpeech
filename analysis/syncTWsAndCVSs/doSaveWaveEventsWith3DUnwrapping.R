
source("doLoadSources.R")
require(boot)

processAll <- function() {
    sessionName <- "EC2_B105"
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    zScore <- TRUE
    twTemporalFreq <- 0.62
    # elecNumbers <- c(95, 110, 109, 108, 107, 123)
    # elecNumbers <- seq(from=141, to=136, by=-1)
    # elecNumbers <- c(70, 71, 72, 88, 104, 120)
    elecNumbers <- c(54, 70, 71, 72, 88, 104, 120, 121, 
                     138, 139, 140, 141, 158, 174,
                     154, 155, 156, 157, 173,
                                    172)
    distancesFromRefElec <- (0:(length(elecNumbers)-1))*4*1e-3
    saveFromTime <- 0
    saveToTime <- 700
    saveDT <- .1
    unwrapThreshold <- pi
    # nResamples <- 1000
    metaDataPhasesDatacubeFilenamePattern <- "results/%s/metaDataPhasesDatacubeFilteredFrom%.02fTo%.02fOrder%02dZScored%dFromTime%.02fTo%.02f.%s"
    phasesDatacubeFilenamePattern <- "results/%s/unwrapped3DPhasesDatacubeFilteredFrom%.02fTo%.02fOrder%02dZScored%dFromTime%.02fTo%.02f.%s"
    waveEventsFilenamePattern <- "results/%s/waveEventsWith3DPhaseUnwrappingFilteredFrom%.02fTo%.02fOrder%02dSaveFromTime%.02fToTime%.02fDT%.02fFromElec%03dToElec%03d.%s"

    metaDataPhasesDatacubeFilename <- 
     sprintf(metaDataPhasesDatacubeFilenamePattern, sessionName, lowCutoff, 
                                                    highCutoff, order, zScore,
                                                    saveFromTime, saveToTime, 
                                                    "txt")
    con <- file(description=metaDataPhasesDatacubeFilename, open="r")
    width <- as.integer(readLines(con=con, n=1))
    height <- as.integer(readLines(con=con, n=1))
    nFrames <- as.integer(readLines(con=con, n=1))
    Fs <- as.double(readLines(con=con, n=1))
    close(con)
    phasesDatacubeFilename <- sprintf(phasesDatacubeFilenamePattern, 
                                       sessionName,
                                       lowCutoff, highCutoff, order, zScore,
                                       saveFromTime, saveToTime, "bin")
    con <- file(description=phasesDatacubeFilename, open="rb")
    buffer <- readBin(con=con, what=single(), n=width*height*nFrames, size=4)
    close(con)
    phasesDatacube <- array(buffer, dim=c(height, width, nFrames))

    electrodesPhases <- getElectrodesPhasesFromDatacube(elecNumbers=elecNumbers,
                                                         phasesDatacube=
                                                          phasesDatacube)
    times <- ((1:nrow(electrodesPhases))-1)/Fs
    saveFromSample <- which.min(abs(times-saveFromTime))
    saveToSample <- which.min(abs(times-saveToTime))
    saveBySamples <- as.integer(saveDT*Fs)
    saveSamples <- seq(from=saveFromSample, to=saveToSample, by=saveBySamples)
    timesToSave <- times[saveSamples]

    phaseDiffs <- matrix(NA, nrow=length(timesToSave),
                             ncol=ncol(electrodesPhases))
    for(i in 1:length(timesToSave)) {
        if(i%%100==0) {
            show(sprintf("Processed %d (%d)", i, length(timesToSave)))
        }
        timeToSave <- timesToSave[i]
        sampleToSave <- which.min(abs(times-timeToSave))
        phaseDiffs[i,] <- electrodesPhases[sampleToSave,]-electrodesPhases[sampleToSave,1]
    }
    stats <- getWaveEventsStats(distancesFromRefElec=distancesFromRefElec, 
                                 phaseDiffs=phaseDiffs, twTemporalFreq=twTemporalFreq)
    wes <- cbind(data.frame(times=timesToSave), stats)
    waveEventsRDataFilename <- sprintf(waveEventsFilenamePattern, sessionName, lowCutoff, highCutoff, order, saveFromTime, saveToTime, saveDT, elecNumbers[1], elecNumbers[length(elecNumbers)], "RData")
    save(wes, file=waveEventsRDataFilename)
    waveEventsCSVFilename <- sprintf(waveEventsFilenamePattern, sessionName, lowCutoff, highCutoff, order, saveFromTime, saveToTime, saveDT, elecNumbers[1], elecNumbers[length(elecNumbers)], "csv")
    # write.csv(x=wes, file=waveEventsCSVFilename, quote=FALSE, row.names=FALSE, col.names=TRUE)
    write.csv(x=wes, file=waveEventsCSVFilename, quote=FALSE, row.names=FALSE)

    browser()
}

processAll()
