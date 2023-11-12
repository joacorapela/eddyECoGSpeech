
source("doLoadSources.R")

processAll <- function() {
    # ecogFilename <- "rData/EC2_B1/RawHTK/Wav410.bin"

    # transcriptionFilename <- "matlabData/EC2_B1/EC2_B1_transcription_final.lab"
    # ecogFilename <- "rData/EC2_B1/RawHTK/Wav12.bin"
    # ecogFilename <- "rData/EC2_B1/RawHTK/Wav123.bin"
    # ecogFilename <- "rData/EC2_B1/RawHTK/Wav151.bin"
    # ecogFilename <- "rData/EC2_B1/RawHTK/Wav255.bin"
    # ecogFilename <- "rData/EC2_B1/RawHTK/Wav260.bin"
    # ecogFilename <- "rData/EC2_B1/RawHTK/Wav240.bin"
    # ecogFilename <- "rData/EC2_B1/RawHTK/Wav210.bin"
    # ecogFilename <- "rData/EC2_B1/RawHTK/Wav214.bin"
    # ecogFilename <- "rData/EC2_B1/RawHTK/Wav220.bin"
    # ecogFilename <- "rData/EC2_B1/RawHTK/Wav221.bin"
    # ecogFilename <- "rData/EC2_B1/RawHTK/Wav310.bin"
    # ecogFilename <- "rData/EC2_B1/RawHTK/Wav325.bin"
    # ecogFilename <- "rData/EC2_B1/RawHTK/Wav341.bin"
    # ecogFilename <- "rData/EC2_B1/RawHTK/Wav347.bin"
    # ecogFilename <- "rData/EC2_B1/RawHTK/Wav362.bin"

    # transcriptionFilename <- "matlabData/EC2_B15/EC2_B15_transcription_final.lab"
    # ecogFilename <- "rData/EC2_B15/RawHTK/Wav224.bin"
    # ecogFilename <- "rData/EC2_B15/RawHTK/Wav326.bin"
    # ecogFilename <- "rData/EC2_B15/RawHTK/Wav336.bin"

    # transcriptionFilename <- "matlabData/EC2_B76/EC2_B76_transcription_final.lab"
    # ecogFilename <- "rData/EC2_B76/RawHTK/Wav220.bin"
    # ecogFilename <- "rData/EC2_B76/RawHTK/Wav31.bin"
    # ecogFilename <- "rData/EC2_B76/RawHTK/Wav341.bin"

    transcriptionFilename <- "matlabData/EC2_B105/EC2_B105_transcription_final.lab"
    entrainmentFreq <- 1/1.62
    # ecogFilename <- "rData/EC2_B105/RawHTK/Wav13.bin"
    # ecogFilename <- "rData/EC2_B105/RawHTK/Wav145.bin"
    # ecogFilename <- "rData/EC2_B105/RawHTK/Wav147.bin"
    # ecogFilename <- "rData/EC2_B105/RawHTK/Wav161.bin"
    # ecogFilename <- "rData/EC2_B105/RawHTK/Wav162.bin"
    # ecogFilename <- "rData/EC2_B105/RawHTK/Wav163.bin"
    # ecogFilename <- "rData/EC2_B105/RawHTK/Wav22.bin"
    # ecogFilename <- "rData/EC2_B105/RawHTK/Wav24.bin"
    # ecogFilename <- "rData/EC2_B105/RawHTK/Wav217.bin"
    # ecogFilename <- "rData/EC2_B105/RawHTK/Wav218.bin"
    # ecogFilename <- "rData/EC2_B105/RawHTK/Wav325.bin"
    # ecogFilename <- "rData/EC2_B105/RawHTK/Wav326.bin"
    ecogFilename <- "rData/EC2_B105/RawHTK/Wav335.bin"
    # ecogFilename <- "rData/EC2_B105/RawHTK/Wav336.bin"
    # ecogFilename <- "rData/EC2_B105/RawHTK/Wav337.bin"
    # ecogFilename <- "rData/EC2_B105/RawHTK/Wav410.bin"
    # ecogFilename <- "rData/EC2_B105/RawHTK/Wav447.bin"

    # transcriptionFilename <- "matlabData/EC2_B89/EC2_B89_transcription_final.lab"
    # ecogFilename <- "rData/EC2_B89/RawHTK/Wav22.bin"
    # ecogFilename <- "rData/EC2_B89/RawHTK/Wav326.bin"

    transcriptionSampleRate <- 1e7
    xlim <- c(-.5, .6)
    # epochFromTime=-0.5
    # epochToTime=0.6
    epochFromTime=xlim[1]-10.0
    epochToTime=xlim[2]+10.0
    # noctave <- 14
    fmin <- 0.1
    nvoice <- 10
    w0 <- 2*pi
    baseline <- c(593, 634)/1000
    significance <- .05
    ylimERSP <- NULL
    ylimITC <- c(0, 3)
    # zlimITC <- c(0, 1.0)
    zlimITC <- NULL
    # minSeparation <- 1.10
    minSeparation <- 0
    decimateFactor <- 4
    logScaleERSP <- TRUE
    logScaleITC <- FALSE
    contour <- FALSE
    nResamplesERP <- 2000
    typeERPCI <- "perc"
    typeListNameForERPCI <- "percent"
    confERPCI <- .95
    ylabERP = "Voltage (V)"
    xlab = "Time (sec)"
    ylab = "Frequency (Hz)"


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

    epochTimes <- seq(from=epochFromTime, to=epochToTime, by=1/ecogSampleRate)

    # plotERPWithCI
#     timeIndicesToDisplay <- which(xlim[1]<=epochTimes & epochTimes<=xlim[2])
#     epochTimesToDisplay <- epochTimes[timeIndicesToDisplay]
#     epochsToDisplay <- epochs[timeIndicesToDisplay,]
#     bootRes <- bootstrapMeans(x=t(epochsToDisplay), nResamples=nResamplesERP)
#     bootCI <- getBootstrapCIs(bootRes=bootRes, conf=confERPCI, 
#                                                type=c(typeERPCI),
#                                                typeListName=
#                                                 typeListNameForERPCI)
#     X11()
#     plotERPWithCI(times=epochTimesToDisplay, erp=bootCI[,1], ci=bootCI[,2:3], 
#                                              xlim=xlim, xlab=xlab, ylab=ylabERP)
    #

    # plot ITC with significance
#     res <- computeITCMaskedWithRayleighSignificance(times=epochTimes, 
#                                                      trials=epochs, 
#                                                      noctave=noctave,
#                                                      nvoice=nvoice, 
#                                                      w0=w0, 
#                                                      srate=ecogSampleRate,
#                                                      significance=significance)
#     X11()
#     plotTimeFreqRes(timeFreqRes=res$maskedITC, times=res$times, freqs=res$freqs, xlim=xlim, ylim=ylim, zlim=zlimITC, contour=contour, xlab=xlab, ylab=ylab)
    #

    baselineIndices <- which(baseline[1]<=epochTimes & epochTimes<=baseline[2])
    noctave <- ceiling(log2(w0*ecogSampleRate/(2*pi*fmin*(2-1/nvoice))))
    res <- computeERSPandITCNoSignificance(times=epochTimes, 
                                            trials=epochs, 
                                            noctave=noctave,
                                            nvoice=nvoice, 
                                            w0=w0, 
                                            baselineIndices=baselineIndices,
                                            srate=ecogSampleRate)
#     if(is.null(ylim)) {
#         ylim <- range(res$freqs)
#     } else {
#         if(is.infinite(ylim[1])) {
#             ylim[1] <- min(res$freqs)
#         }
#         if(is.infinite(ylim[2])) {
#             ylim[2] <- max(res$freqs)
#         }
#     }
    X11()
    plotTimeFreqRes(timeFreqRes=res$itc, times=res$times, freqs=res$freqs,
                                         hline=entrainmentFreq,
                                         xlim=xlim, ylim=ylimITC, zlim=zlimITC,
                                         logScale=logScaleITC, 
                                         xlab=xlab, ylab=ylab, contour=contour)
    X11()
    plotTimeFreqRes(timeFreqRes=res$ersp, times=res$times, freqs=res$freqs,
                                          xlim=xlim, ylim=ylimERSP,
                                          logScale=logScaleERSP, xlab=xlab,
                                          ylab=ylab, contour=contour)

    browser()
}

processAll()

rm(processAll)
