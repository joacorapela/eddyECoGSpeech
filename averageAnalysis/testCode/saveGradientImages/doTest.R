source("doLoadSources.R")

processAll <- function() {
    T <- 1 # sec
    Fs <- 1000.00 # Hz
    freq <- 13.5 # Hz
    imageSize <- 16 # pixels
    decaySigma <- 5 # pixels
    wavelength <- 20 # pixels/cycle
    pixelSpacing <- 1 # a.u.

    sessionLabel <- "EC2_B105"
    lowCutoff <- 11.0
    highCutoff <- 15.0
    order <- 2
    sampleRate <- Fs
    fromTimeHT <- 0.00
    durationHT <- 1.00
    fromTimeToPlot <- 0.00
    durationToPlot <- T
    titlePattern <- "%d:%02d"
    arrowLength <- 0.35
    arrowAngle <- 20
    vectorCol <- "blue"
    nrow <- 16
    ncol <- 16
    height <- 6
    figDirnamePattern <- "videos/%s/gradientsElec%03d-%03dFR%.02fFromTime%.02fTo%.02fHtFilteredFrom%.02fTo%0.2fOrder%02d"
    figFilenamePattern <- "gradientsIndex%06d.png"

    # elecNumbersToPlot <- c(135, 151, 167, 183, 199, 215, 231, 247,
    #                  134, 150, 166, 182, 198, 214, 230, 246, 
    #                  119, 103,  87,  71,  55,  39,  23,   7, 
    #                  118, 102,  86,  70,  54,  38,  22,   6)
    elecNumbersToPlot <- 1:256

    # generate data
    xf <- generateDecayTargetWave(imageSize=imageSize, dt=1/Fs, T=T, 
                                                       frequency=freq,
                                                       lambda=wavelength, 
                                                       sigma=decaySigma)
    # z-score data
    zxf <- zscoreDatacube(x=xf)
    # form analytic signal
    xph <- computeAnalyticSignalForDatacube(x=zxf)
    # calculate instanteous frequency
    resIF <- computeInstantaneousFrequencyForDatacube(xph=xph, Fs=Fs)
    signIF <- resIF$signIF
    # calculate phase gradient
    resGrad <- phaseGradientComplexMultiplication(xph=xph,
                                                   pixelSpacing=pixelSpacing, 
                                                   signIF=signIF)
    times <- seq(from=1/Fs, to=T, by=1/Fs)
    resGradWithTimes <- c(resGrad, list(times=times))
    toTimeToPlot <- fromTimeToPlot + durationToPlot
    saveGradientImages(sessionLabel=sessionLabel, 
                        resGradWithTimes=resGradWithTimes, 
                        figDirnamePattern=figDirnamePattern,
                        figFilenamePattern=figFilenamePattern,
                        elecNumbers=elecNumbersToPlot,
                        fromTime=fromTimeToPlot, toTime=toTimeToPlot,
                        lowCutoff=lowCutoff, highCutoff=highCutoff, order=order,
                        sampleRate=sampleRate,
                        titlePattern=titlePattern, 
                        arrowLength=arrowLength, arrowAngle=arrowAngle,
                        vectorCol=vectorCol,
                        nrow=nrow, ncol=ncol)

    browser()
}

processAll()

rm(processAll)
