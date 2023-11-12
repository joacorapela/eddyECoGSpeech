getPeakTimesWithAndWithoutMatchingCVSs <- function(peaksInfo, cvsStartTimes) {
    allPeakIndices <- 1:length(peaksInfo$peakTimes)
    closestPeakIndices <- peaksInfo$closestPeakIndices
    matchRes <- allPeakIndices%in%closestPeakIndices
    peaksIndicesWCVSs <- which(matchRes)
    peaksIndicesWOCVSs <- which(!matchRes)
    peaksTimesWCVSs <- peaksInfo$peakTimes[peaksIndicesWCVSs]
    peaksTimesWOCVSs <- peaksInfo$peakTimes[peaksIndicesWOCVSs]
    minCVSStartTime <- cvsStartTimes[1]
    maxCVSStartTime <- cvsStartTimes[length(cvsStartTimes)]
    inRangeIndicesWCVSs <- which(minCVSStartTime<=peaksTimesWCVSs&
                                  peaksTimesWCVSs<=maxCVSStartTime)
    inRangeIndicesWOCVSs <- which(minCVSStartTime<=peaksTimesWOCVSs &
                                  peaksTimesWOCVSs<=maxCVSStartTime)
    inRangePeaksWCVSsTimes <- peaksTimesWCVSs[inRangeIndicesWCVSs]
    inRangePeaksWOCVSsTimes <- peaksTimesWOCVSs[inRangeIndicesWOCVSs]
    return(list(peakTimesWithMatchingCVSs=inRangePeaksWCVSsTimes,
                 peakTimesWithoutMatchingCVSs=inRangePeaksWOCVSsTimes))
}
