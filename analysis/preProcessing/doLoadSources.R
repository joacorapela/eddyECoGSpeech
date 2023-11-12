
dirname <- "~/dev/research/programs/src/R/ees"
source(sprintf("%s/getGroupAndElecNumber.R", dirname))
source(sprintf("%s/getElectrodeIndexInArrayGGPlot.R", dirname))
rm(dirname)

dirname <- "~/dev/research/programs/src/R/ees/datacube"
source(sprintf("%s/buildHTDatacube.R", dirname))
rm(dirname)

dirname <- "~/dev/research/programs/src/R/ees/misc"
source(sprintf("%s/getNeighborsOfPixel.R", dirname))
rm(dirname)

