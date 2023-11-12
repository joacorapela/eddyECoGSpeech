
source("doLoadSources.R")
require(plotly)

processAll <- function() {
    lowCutoff <- 0.4
    highCutoff <- 0.8
    order <- 2
    # lowCutoff <- 1.0
    # highCutoff <- 1.4
    # order <- 3
    zScore <- TRUE
    sessionLabel <- "EC2_B105"
    fromTime <- 00
    duration <- 700
    xlab <- "Electrode Position X"
    ylab <- "Electrode Position Y"
    legendLabel <- "Phase\n(radians)"
    width <- 16
    height <- 16
    # binImageFilename <- "results/EC2_B105/phasesImageFilteredFrom0.40To0.80Order02ZScored0TimeFrom350.01To350.05.bin"
    binImageFilename <- "results/EC2_B105/unwrappedPhasesImageFilteredFrom0.40To0.80Order02ZScored0TimeFrom350.01To350.05.bin"
    # binImageFilename <- "results/EC2_B105/phasesImageFilteredFrom0.40To0.80Order02ZScored0TimeFrom250.01To250.05.bin"
    # binImageFilename <- "results/EC2_B105/unwrappedPhasesImageFilteredFrom0.40To0.80Order02ZScored0TimeFrom250.01To250.05.bin"
    # binImageFilename <- "results/EC2_B105/phasesImageFilteredFrom0.40To0.80Order02ZScored0TimeFrom299.99To300.03.bin"
    # binImageFilename <- "results/EC2_B105/unwrappedPhasesImageFilteredFrom0.40To0.80Order02ZScored0TimeFrom299.99To300.03.bin"

    con <- file(description=binImageFilename, open="rb")
    buffer <- readBin(con=con, what=single(), n=width*height, size=4)
    close(con)
    phasesImage <- array(buffer, dim=c(height, width))

    p <- plot_ly(z = ~phasesImage) %>% add_surface()
    print(p)

    browser()
}

processAll()

rm(processAll)
