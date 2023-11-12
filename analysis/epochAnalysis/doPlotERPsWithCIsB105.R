
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
                            width=6, height=6, units="in",
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
#             trellis.device("pdf", color=TRUE, width=width, height=height,
#                            onefile=FALSE, file=figFilename)
            plotERPWithCI(times=epochTimesToDisplay, erp=bootCI[,1], 
                                                     ci=bootCI[,2:3], 
                                                     xlim=xlim, 
                                                     xlab=xlab, ylab=ylab)
            ggsave(filename=figFilename, width=width, height=height, 
                                         units=units)
#             dev.off()
        }
    }


    elecNumbers <- 1:256
    transcriptionSampleRate <- 1e7
    epochFromTime=-0.5
    epochToTime=0.6
    xlim <- c(-.5, .6)
    ylim <- NULL
    minSeparation <- 0
    decimateFactor <- 4
    nResamples <- 2000
    confCI <- .95
    typeCI <- "perc"
    typeListNameForCI <- "percent"
    xlab = "Time (sec)"
    ylab=expression(paste("Voltage (", mu, "V)"))
    width <- 6
    height <- 6
    transcriptionFilename <- "../data/transcriptionFiles/EC2_B105/EC2_B105_transcription_final_completed.lab"
    ecogFilenamePattern <- "../data/rData/EC2_B105/RawHTK/Wav%d%d.bin"
    figFilenamePattern <- "figures/EC2_B105/erpWithCI%d.png"

    for(elecNumber in elecNumbers) {
        show(sprintf("Processing electrode %d", elecNumber))
        res <- getGroupAndElecNumber(elecNumber=elecNumber)
        ecogFilename <- sprintf(ecogFilenamePattern, res$groupNumber, 
                                                     res$elecNumber)
        figFilename <- sprintf(figFilenamePattern, elecNumber)
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

processAll()

rm(processAll)
