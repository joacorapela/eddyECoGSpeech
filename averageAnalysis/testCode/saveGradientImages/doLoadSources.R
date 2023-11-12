setwd("../../")
source("doLoadSources.R")
setwd("testCode/saveGradientImages")

dirname <- "/home/rapela/dev/research/programs/src/R/travelingWaves"
source(sprintf("%s/computeAnalyticSignalForDatacube.R", dirname))
source(sprintf("%s/computeInstantaneousFrequencyForDatacube.R", dirname))
source(sprintf("%s/phaseGradientComplexMultiplication.R", dirname))
rm(dirname)

dirname <- "/home/rapela/dev/research/programs/src/R/travelingWaves/simulations"
source(sprintf("%s/generateDecayTargetWave.R", dirname))
rm(dirname)

dirname <- "/home/rapela/dev/research/programs/src/R/travelingWaves/utils"
source(sprintf("%s/zscoreDatacube.R", dirname))
rm(dirname)

