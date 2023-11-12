saveAmplitudesImages <- function(sessionLabel,
                             htFilenamePattern,
                             figDirnamePattern,
                             figFilenamePattern,
                             elecNumbers,
                             lowCutoff, highCutoff,
                             fromTime, toTime,
                             desiredFrameRate,
                             titlePattern, 
                             nrow, ncol,
                             width, height) {
    res <- getAmplitudesArray(sessionLabel=sessionLabel, 
                           htFilenamePattern=htFilenamePattern, 
                           elecNumbers=elecNumbers, 
                           lowCutoff=lowCutoff, highCutoff=highCutoff, 
                           fromTime=fromTime, toTime=toTime, 
                           desiredFrameRate=desiredFrameRate,
                           nrow=nrow, ncol=ncol)
    saveImagesFromArray(anArray=res$amplitudesArray,
                         sessionLabe=sessionLabel,
                         actualFrameRate=res$actualFrameRate,
                         timesToSave=res$timesToSave,
                         lowCutoff=lowCutoff, highCutoff=highCutoff, 
                         fromTime=fromTime, toTime=toTime,
                         figDirnamePattern=figDirnamePattern, 
                         figFilenamePattern=figFilenamePattern,
                         scaleName=expression(cos(phi)),
                         titlePattern=titlePattern)

    firstElectrode <- TRUE

    for(elecNumber in elecNumbers) {
        show(sprintf("Processing electrode %d", elecNumber))
        res <- getGroupAndElecNumber(elecNumber=elecNumber)
        htFilename <- sprintf(htFilenamePattern, sessionLabel,
                                                 lowCutoff, highCutoff,
                                                 res$groupNumber, 
                                                 res$elecNumber)
        electrodeIndexInArray <-
         getElectrodeIndexInArray(elecNumber=elecNumber)
        if(file.exists(htFilename)) {
            loadRes <- get(load(htFilename))
            if(length(loadRes$ht)>0) {
                if(firstElectrode) {
                    decimateFactor <-
                     round(loadRes$ecogSampleRate/desiredFrameRate)
                    actualFrameRate <- loadRes$ecogSampleRate/decimateFactor
                    amplitudes <- decimate(x=Mod(loadRes$ht), q=decimateFactor)
                    times <- ((1:length(amplitudes))-1)/actualFrameRate
                    samplesToSave <- which(fromTime<=times & times<=toTime)
                    timesToSave <- times[samplesToSave]
                    amplitudesArray <- array(NaN, 
                                            dim=c(nrow, ncol,
                                                   length(samplesToSave)))
                    firstElectrode <- FALSE
                } else {
                    amplitudes <- decimate(x=Arg(loadRes$ht), q=decimateFactor)
                }
                amplitudesArray[electrodeIndexInArray[1], 
                             electrodeIndexInArray[2],] <-
                 amplitudes[samplesToSave]
            }
        }
    }
    figDirname <- sprintf(figDirnamePattern, sessionLabel,
                                             actualFrameRate, fromTime, toTime)
    if(!file.exists(figDirname)) {
        dir.create(figDirname)
    }
    for(i in 1:dim(amplitudesArray)[3]) {
        if(i%%100==0) {
            show(sprintf("Saved %d frames (%d)", i, dim(amplitudesArray)[3]))
        }
        figFilename <- sprintf(figFilenamePattern, lowCutoff, highCutoff, i)
        figAbsFilename <- sprintf("%s/%s", figDirname, figFilename)
        timeInSecs <- timesToSave[i]-timesToSave[1]
        minutes <- timeInSecs%/%60
        seconds <- as.integer(timeInSecs%%60)
        title <- sprintf(titlePattern, minutes, seconds)

        trellis.device("png", file=figAbsFilename)

        # I need to rotate 90 degrees clockwise amplitudesArray due to the
        # peculiar way image displays an array. See help(image):
        #
        # Notice that ‘image’ interprets the ‘z’ matrix as a table of
        # ‘f(x[i], y[j])’ values, so that the x axis corresponds to row
        # number and the y axis to column number, with column 1 at the
        # bottom, i.e. a 90 degree counter-clockwise rotation of the
        # conventional printed layout of a matrix.

        matrixToDisplay <- 
         rotateMatrixBy90DegreesClockwise(amplitudesArray[,,i])
        image(matrixToDisplay, xaxt="n", yaxt="n", main=title)
        dev.off()
    }
}

