
source("doLoadSources.R")

processAll <- function() {
    processOne <- function(transcriptionSampleRate,
                            epochFromTime,
                            epochToTime,
                            decimateFactor,
                            minSeparation,
                            nResamples,
                            confCI, typeCI, typeListNameForCI,
                            xlim, ylim, xlab, ylab,
                            width=6, height=6,
                            ecogFilename,
                            transcriptionFilename,
                            figFilename) {
                           
        res <- getNonOverlappingEpochs(ecogFilename=ecogFilename, 
                                        transcriptionFilename=
                                         transcriptionFilename, 
                                        transcriptionSampleRate=
                                         transcriptionSampleRate,
                                        epochFromTime=epochFromTime,
                                        epochToTime=epochToTime,
                                        decimateFactor=decimateFactor,
                                        minSeparation=minSeparation)
        epochs <- res$epochs
        ecogSampleRate <- res$srate

        if(!is.null(epochs)) {
            epochTimes <- seq(from=epochFromTime, to=epochToTime, 
                                                  by=1/ecogSampleRate)
            timeIndicesToDisplay <- which(xlim[1]<=epochTimes & 
                                                   epochTimes<=xlim[2])
            epochTimesToDisplay <- epochTimes[timeIndicesToDisplay]
            epochsToDisplay <- epochs[timeIndicesToDisplay,]
            bootRes <- bootstrapMeans(x=t(epochsToDisplay), 
                                       nResamples=nResamples)
            bootCI <- getBootstrapCIs(bootRes=bootRes, conf=confCI, 
                                                       type=c(typeCI),
                                                       typeListName=
                                                        typeListNameForCI)
            trellis.device("pdf", color=TRUE, width=width, height=height,
                           onefile=FALSE, file=figFilename)
    #                        onefile=FALSE, horizontal=FALSE, file=figFilename)
            plotERPWithCI(times=epochTimesToDisplay, erp=bootCI[,1], 
                                                     ci=bootCI[,2:3], 
                                                     xlim=xlim, 
                                                     xlab=xlab, ylab=ylab)
            dev.off()
        }
    }


    # groupNumbers <- 4
    # elecNumbers <- 55
    groupNumbers <- 1:4
    elecNumbers <- 1:64
    transcriptionSampleRate <- 1e7
    epochFromTime=-0.5
    epochToTime=0.6
    xlim <- c(-.5, .6)
    # epochFromTime=xlim[1]-4.0
    # epochToTime=xlim[2]+4.0
    # noctave <- 14
    # ylim <- c(4, 100)
    # ylim <- c(4, Inf)
    ylim <- NULL
    minSeparation <- 0
    decimateFactor <- 4
    nResamples <- 2000
    confCI <- .95
    typeCI <- "perc"
    typeListNameForCI <- "percent"
    xlab = "Time (sec)"
    ylab = "Voltage (V)"
    width <- 6
    height <- 6
    transcriptionFilename <- "matlabData/EC2_B89/EC2_B89_transcription_final.lab"
    ecogFilenamePattern <- "rData/EC2_B89/RawHTK/Wav%d%d.bin"
    figFilenamePattern <- "figures/EC2_B89/erpWithCI%d.pdf"

    for(groupNumber in groupNumbers) {
        for(elecNumber in elecNumbers) {
            show(sprintf("Processing Wav%d%d", groupNumber, elecNumber))
            ecogFilename <- sprintf(ecogFilenamePattern, groupNumber, 
                                                         elecNumber)
            absoluteElecNumber <- getAbsoluteElectrodeNumber(groupNumber=groupNumber,
                                                              elecNumber=elecNumber)
            figFilename <- sprintf(figFilenamePattern, absoluteElecNumber)
            if(file.exists(ecogFilename)) {
                processOne(transcriptionSampleRate=transcriptionSampleRate,
                            epochFromTime=epochFromTime,
                            epochToTime=epochToTime,
                            decimateFactor=decimateFactor,
                            minSeparation=minSeparation,
                            nResamples=nResamples,
                            confCI=confCI, typeCI=typeCI, typeListNameForCI,
                            xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab,
                            width=width, height=height,
                            ecogFilename=ecogFilename, 
                            transcriptionFilename=transcriptionFilename,
                            figFilename=figFilename)
            }
        }
    }
}

processAll()

rm(processAll)
