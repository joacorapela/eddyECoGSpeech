
source("doLoadSources.R")

processAll <- function() {
    # lowCutoff <- 0.4
    # highCutoff <- 0.8
    # order <- 2
    lowCutoff <- 1.0
    highCutoff <- 1.4
    order <- 3
    sessionLabel <- "EC2_B105"
    # elecNumbers <- 1:256
    # elecNumbers <- c(37:39, 50:54, 67:70, 85:86, 101:103)
    # elecNumbers <- c(135, 151, 167, 183, 199, 215, 231, 247,
    #                  134, 150, 166, 182, 198, 214, 230, 246, 
    #                  119, 103,  87,  71,  55,  39,  23,   7, 
    #                  118, 102,  86,  70,  54,  38,  22,   6)
    # elecNumbers <- c(105, 122, 139, 156, 173,
    #                  121, 137, 138, 154, 155, 171, 172, 189)
    # elecNumbers <- c(48+16*(0:3),47+16*(0:3), 46+16*(0:3), 45+16*(0:3), 44+16*(0:3), 43+16*(0:3), 42+16*(0:3), 41+16*(0:3))
    # elecNumbers <- c(135:143, 154:159)
    # elecNumbers <- c(88:96, 103:112)
    # elecNumbers <- c(164:175, 180:191, 196:207, 212:223, 228:239, 244:255)
    # elecNumbers <- c(145:176)
    # elecNumbers <- c(129:176)
    # elecNumbers <- c(7:14, 23:30, 39:46, 55:62, 71:78, 87:94, 103:110, 119:126, 135:142, 151:158, 167:174, 183:190, 199:206, 215:222, 231:238, 247:254)
    # elecNumbers <- c(3, 20, 37, 54, 71,
    #                  4, 21, 38, 55,
    #                  19,36, 53, 70)
    # elecNumbers <- 162:256
    # elecNumbers <- c(135:141)
    # elecNumbers <- c(54, 70, 71, 72, 88, 104, 120, 121, 
    #                  138, 139, 140, 141, 158, 174,
    #                  154, 155, 156, 157, 173,
    #                                 172)
    # elecNumbers <- c(     112,
    #                       111,
    #                       110,
    #                       109,           061,
    #                            092,      060,
    #                            091,      059,
    #                            090,
    #                            089,
    #                  120, 104, 088, 072, 056,
    #                  119, 103, 087, 071, 055)
    elecNumbers <- c(     112,
                          111,
                          110,
                          109,     
                               092,
                               091,
                               090,
                               089,
                               088,
                               087)
    fromTime <- 240
    duration <- 60
    # blurSigma <- .75
    blurSigma <- 0
    titlePattern <- "%d:%02d"
    nrow <- 16
    ncol <- 16
    desiredFrameRate <- 25
    bandpassedFilenamePattern <- "results/%s/bandpassedFilteredFrom%.02fTo%.02fOrder%02dWav%d%d.RData"
    figDirnamePattern <- "videos/%s/voltagesElec%03d-%03dFR%.02fFrom%03dTo%03dHtFilteredFrom%.02fTo%0.2fOrder%02dBlurSigma%.02f"
    figFilenamePattern <- "voltagesIndex%06d.png"

    toTime <- fromTime + duration
    saveBPVoltageImagesGGPlot2(sessionLabel=sessionLabel,
                     bandpassedFilenamePattern=bandpassedFilenamePattern, 
                     figDirnamePattern=figDirnamePattern, 
                     figFilenamePattern=figFilenamePattern,
                     elecNumbers=elecNumbers,
                     lowCutoff=lowCutoff, highCutoff=highCutoff, order=order,
                     fromTime=fromTime, toTime=toTime,
                     desiredFrameRate=desiredFrameRate,
                     blurSigma=blurSigma,
                     titlePattern=titlePattern, 
                     nrow=nrow, ncol=ncol)
}

processAll()

rm(processAll)
