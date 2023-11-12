
dirname = "~/dev/research/programs/src/R/stats"
source(sprintf("%s/getOutliersIndices.R", dirname))
rm(dirname)

dirname = "~/dev/research/programs/src/R/stats/permute"
source(sprintf("%s/permuteSkippedPearsonCorCoef.R", dirname))
rm(dirname)

dirname = "~/dev/research/programs/src/R/ees/infoCVSs"
source(sprintf("%s/getInfoCVSsInitiations.R", dirname))
source(sprintf("%s/getInfoCVSsCVTransitions.R", dirname))
source(sprintf("%s/getInfoCVSsTerminations.R", dirname))
rm(dirname)

dirname = "~/dev/research/programs/src/R/ees/syncTWsAndCVSs"
source(sprintf("%s/computePropEventsAroundStartOrEndOfCVS.R", dirname))
source(sprintf("%s/computePropEventsInSilence.R", dirname))
source(sprintf("%s/resampleNullPropEventsAlignedToCVSs.R", dirname))
source(sprintf("%s/resamplePropCWEsInSilence.R", dirname))
source(sprintf("%s/getPropCWEsInSilenceOrCVS.R", dirname))
source(sprintf("%s/randomizeInfoInitAndTerm.R", dirname))
source(sprintf("%s/unwrap1DPhases.R", dirname))
source(sprintf("%s/getWaveEventsStats.R", dirname))
source(sprintf("%s/getCWEs.R", dirname))
source(sprintf("%s/getPlotPhaseDiffsVsDistances.R", dirname))
source(sprintf("%s/computePropWEsBtwBeforeAndAfter.R", dirname))
source(sprintf("%s/computePropWEsBtwAfterAndBefore.R", dirname))
source(sprintf("%s/resampleNullPropWEs.R", dirname))
source(sprintf("%s/cweOverlapsCVSs.R", dirname))
source(sprintf("%s/resampleStatNullLatenciesBtwCVSsAndCWEsPhase.R", dirname))
source(sprintf("%s/getCVSIndicesOverlappingCWE.R", dirname))
rm(dirname)

dirname = "~/dev/research/programs/src/R/ees/datacube"
source(sprintf("%s/getElectrodesPhasesFromDatacube.R", dirname))
rm(dirname)

dirname = "~/dev/research/programs/src/R/ees"
source(sprintf("%s/getElectrodeIndexInArrayGGPlot.R", dirname))
source(sprintf("%s/getGroupAndElecNumber.R", dirname))
rm(dirname)

dirname <- "/home/rapela/dev/research/programs/src/R/contributed"
source(sprintf("%s/Rallfun-v34.txt", dirname))
rm(dirname)

