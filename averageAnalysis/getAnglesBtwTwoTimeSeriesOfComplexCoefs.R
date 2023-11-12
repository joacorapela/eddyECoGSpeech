getAnglesBtwTwoTimeSeriesOfComplexCoefs <- function(complexCoefs1, 
                                                     complexCoefs2) {
    angles <- rep(NA, times=length(complexCoefs1))
    for(i in 1:length(angles)) {
        angles[i] <- 
         computeSignedAngleBtwTwoVectors(vec1=c(Re(complexCoefs1[i]), 
                                                 Im(complexCoefs1[i])),
                                          vec2=c(Re(complexCoefs2[i]),
                                                  Im(complexCoefs2[i])))
    }
    return(angles)
}
