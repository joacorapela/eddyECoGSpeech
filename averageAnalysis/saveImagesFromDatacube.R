saveImagesFromDatacube <- function(datacube, timesToSave, zlim=NA, scaleName,
                                             titlePattern,
                                             plotTimeInMinSec=TRUE,
                                             figDirname, 
                                             figFilenamePattern) {
    if(!file.exists(figDirname)) {
        dir.create(figDirname)
    }
    for(i in 1:dim(datacube)[3]) {
        if(i %% 100==0) {
            show(sprintf("Saved %d (%d) images", i, dim(datacube)[3]))
        }
        figFilename <- sprintf(figFilenamePattern, i)
        figAbsFilename <- sprintf("%s/%s", figDirname, figFilename)
        if(plotTimeInMinSec) {
            timeInSecs <- timesToSave[i]
            minutes <- timeInSecs%/%60
            seconds <- round(timeInSecs%%60)
            title <- sprintf(titlePattern, minutes, seconds)
        } else {
            title <- sprintf(titlePattern, timesToSave[i])
        }

        # I need to rotate 90 degrees clockwise datacube due to the
        # peculiar way image displays an array. See help(image):
        #
        # Notice that ‘image’ interprets the ‘z’ matrix as a table of
        # ‘f(x[i], y[j])’ values, so that the x axis corresponds to row
        # number and the y axis to column number, with column 1 at the
        # bottom, i.e. a 90 degree counter-clockwise rotation of the
        # conventional printed layout of a matrix.

#         matrixToDisplay <- 
#          cos(rotateMatrixBy90DegreesClockwise(datacube[,,i]))
#         image(matrixToDisplay, xaxt="n", yaxt="n", main=title)

        y <- 1:dim(datacube)[1]
        x <- 1:dim(datacube)[2]
        df <- expand.grid(y=y, x=x)
        slice <- datacube[,,i]
        df$z <- as.vector(slice)
        p <- ggplot()
        p <- p + geom_tile(data=df, mapping=aes(x=x, y=y, fill=z))
        if(!is.na(zlim[1])) {
            p <- p + scale_fill_gradient2(low="blue", high="red", mid="white", limits=zlim, midpoint=(zlim[1]+zlim[2])/2, name=scaleName)
        } else {
            p <- p + scale_fill_gradient2(low="blue", high="red", mid="white", name=scaleName)
        }
        p <- p + ggtitle(title)
        trellis.device("png", file=figAbsFilename)
        print(p)
        dev.off()
    }
}

