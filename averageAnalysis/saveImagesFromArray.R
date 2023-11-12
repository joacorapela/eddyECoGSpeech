saveImagesFromArray <- function(anArray, sessionLabel, actualFrameRate, 
                                         timesToSave,
                                         lowCutoff, highCutoff,
                                         fromTime, toTime, 
                                         figDirnamePattern, 
                                         figFilenamePattern,
                                         scaleName,
                                         titlePattern) {
    figDirname <- sprintf(figDirnamePattern, sessionLabel,
                                             actualFrameRate, 
                                             fromTime, toTime, 
                                             lowCutoff, highCutoff)
    if(!file.exists(figDirname)) {
        dir.create(figDirname)
    }
    for(i in 1:dim(anArray)[3]) {
        if(i %% 100==0) {
            show(sprintf("Saved %d (%d) images", i, dim(anArray)[3]))
        }
        figFilename <- sprintf(figFilenamePattern, i)
        figAbsFilename <- sprintf("%s/%s", figDirname, figFilename)
        timeInSecs <- timesToSave[i]-timesToSave[1]
        minutes <- timeInSecs%/%60
        seconds <- as.integer(timeInSecs%%60)
        title <- sprintf(titlePattern, minutes, seconds)
        trellis.device("png", file=figAbsFilename)

        # I need to rotate 90 degrees clockwise anArray due to the
        # peculiar way image displays an array. See help(image):
        #
        # Notice that ‘image’ interprets the ‘z’ matrix as a table of
        # ‘f(x[i], y[j])’ values, so that the x axis corresponds to row
        # number and the y axis to column number, with column 1 at the
        # bottom, i.e. a 90 degree counter-clockwise rotation of the
        # conventional printed layout of a matrix.

#         matrixToDisplay <- 
#          cos(rotateMatrixBy90DegreesClockwise(anArray[,,i]))
#         image(matrixToDisplay, xaxt="n", yaxt="n", main=title)

        y <- 1:dim(anArray)[1]
        x <- 1:dim(anArray)[2]
        df <- expand.grid(y=y, x=x)
        df$z <- as.vector(anArray[,,i])
        p <- ggplot()
        p <- p + geom_tile(data=df, mapping=aes(x=x, y=y, fill=z))
#         p <- p + scale_fill_continuous(limits=c(-1, 1))
        p <- p + scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0, limits=c(-1, 1), name=scaleName)
        p <- p + ggtitle(title)
        print(p)
        dev.off()

    }
}

