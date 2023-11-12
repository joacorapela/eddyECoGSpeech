
source("doLoadSources.R")

processAll <- function() {
    getElectrodeIndexInArray <- function(groupNumber, elecNumber) {
        absoluteElecNumber <- getAbsoluteElectrodeNumber(groupNumber=groupNumber, 
                                                          elecNumber=elecNumber)
        rowIndex <- 16-(absoluteElecNumber-1)%/%16
        colIndex <- (absoluteElecNumber-1)%%16+1
        indexInArray <- c(rowIndex, colIndex)
        return(indexInArray)
    }

    # bandpassedFilenamePattern <- "results/EC2_B105/bandpassedFilteredFrom%.02fTo%.02fWav%d%d.RData"
    bandpassedFilenamePattern <- "results/simulated/twFreq0.62WaveLength8.00FromTime%.02fToTime%.02fSR762.94Wav%d%d.RData"
    figFilenamePattern <- "figures/simulated/animation/twFreq0.62Length8.00SR762.94From%.02fTo%.02fFrame%08d.eps"
    groupNumbers <- 1:4
    elecNumbers <- 1:64
    lowCutoff <- 0.4
    highCutoff <- 0.8
    fromTime <- 0
    toTime <- 1
    nrow <- 16
    ncol <- 16
    firstElectrode <- TRUE
    width <- 6
    height <- 6
    refershRate <- 24

    for(groupNumber in groupNumbers) {
        for(elecNumber in elecNumbers) {
            bandpassedFilename <- sprintf(bandpassedFilenamePattern, 
                                           fromTime, toTime, 
                                           groupNumber, elecNumber)
            electrodeIndexInArray <-
             getElectrodeIndexInArray(groupNumber=groupNumber,
                                       elecNumber=elecNumber)
            if(file.exists(bandpassedFilename)) {
                loadRes <- get(load(bandpassedFilename))
                if(firstElectrode) {
                    decimateFactor <- 
                     round(loadRes$ecogSampleRate/refershRate)
                    ecogSampleRate <- loadRes$ecogSampleRate/decimateFactor
                    times <- (1:length(loadRes$filteredECoG))/ecogSampleRate
                    samplesForImages <- which(fromTime<=times & times<=toTime)
                    voltagesArray <- array(NaN, 
                                            dim=c(nrow, ncol,
                                                   length(samplesForImages)))
                    firstElectrode <- FALSE
                }
                if(length(loadRes$filteredECoG)>0) {
                    electrodeVs <- decimate(x=loadRes$filteredECoG,
                                             q=decimateFactor)
                    voltagesArray[electrodeIndexInArray[1], 
                                   electrodeIndexInArray[2],] <- 
                     electrodeVs[samplesForImages]
# if(groupNumber==4 && elecNumber==64) {
#     voltagesArray[electrodeIndexInArray[1], 
#                electrodeIndexInArray[2],] <- NaN
# }
                }
            }
        }
    }
    for(i in 1:dim(voltagesArray)[3]) {
        # df <- data.frame(x=1:dim(voltagesArray)[1], y=1:dim(voltagesArray)[2], z=I(voltagesArray[,,i]))
        # df <- data.frame(x=1:dim(voltagesArray)[1], y=1:dim(voltagesArray)[2], z=I(matrix(runif(dim(voltagesArray)[1]*dim(voltagesArray)[2]), nrow=dim(voltagesArray)[1])))
        # df <- expand.grid(x=1:dim(voltagesArray)[1],
        #                    y=1:dim(voltagesArray)[2])
        # df$z <- matrix(runif(dim(voltagesArray)[1]*dim(voltagesArray)[2]), nrow=dim(voltagesArray)[1])
        # p <- ggplot(df, aes(x=x, y=y))
        # p <- p + geom_tile(aes(fill=z))
        figFilename <- sprintf(figFilenamePattern, lowCutoff, highCutoff, i)
        trellis.device("postscript", color=TRUE, width=width, height=height, onefile=FALSE, horizontal=FALSE, file=figFilename)
        image(voltagesArray[,,i], xaxt="n", yaxt="n")
        # print(p)
        dev.off()
        # browser()
    }
}

processAll()

rm(processAll)

