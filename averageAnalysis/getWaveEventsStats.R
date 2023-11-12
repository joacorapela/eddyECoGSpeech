# getWaveEventsStats <- function(distancesFromRefElec, phaseDiffs) {
getWaveEventsStats <- function(distancesFromRefElec, phaseDiffs, twTemporalFreq, nResamples=2000) {
    rs <- rep(x=NA, times=nrow(phaseDiffs))
    pValues <- rep(x=NA, times=nrow(phaseDiffs))
    slopes <- rep(x=NA, times=nrow(phaseDiffs))
    speeds <- rep(x=NA, times=nrow(phaseDiffs))
    for(i in 1:nrow(phaseDiffs)) {
        if(i%%100==0) {
            show(sprintf("Processed %d (%d)", i, nrow(phaseDiffs)))
        }
        pbcorRes <- pbcor(x=distancesFromRefElec, y=phaseDiffs[i,])
        r <- pbcorRes$cor
        pValue <- pbcorRes$p.value
        lmRes <- lm(phaseDiffs[i,]~distancesFromRefElec)
        # permRes <- permuteSkippedPearsonCorCoef(x=distancesFromRefElec, y=phaseDiffs[i,], nResamples=nResamples)
        # r <- permRes$t0
        # pValue <- sum(abs(permRes$t)>abs(permRes$t0))/length(permRes$t)
        # outliersIndices <- permRes$outliersIndices
        # if(length(outliersIndices)>0) {
            # lmRes <- lm(phaseDiffs[i,-outliersIndices]~distancesFromRefElec[-outliersIndices])
        # } else {
            # lmRes <- lm(phaseDiffs[i,]~distancesFromRefElec)
        # }
        slope <- lmRes$coefficients[2]
        speed <- 2*pi*twTemporalFreq/slope
# if(pValue<.05) {
#     show(sprintf("speed=%f", speed))
# }
        rs[i] <- r
        pValues[i] <- pValue
        slopes[i] <- slope
        speeds[i] <- speed
    }
    answer <- data.frame(rs=rs, pValues=pValues, slopes=slopes, speeds=speeds)
    return(answer)
}
