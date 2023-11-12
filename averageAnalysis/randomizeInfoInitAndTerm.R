randomizeInfoInitAndTerm <- function(infoInit, infoTerm, sdNoiseFirstTime=.5) {
    interCVSDelays <- infoInit$times[2:length(infoInit$times)]-
                      infoTerm$times[1:(length(infoTerm$times)-1)]
    statsInterCVSDelay <- list(mean=mean(interCVSDelays),
                                sd=sd(interCVSDelays))
    cvsDurs <- infoTerm$times[1:length(infoTerm$times)]-
                infoInit$times[1:length(infoInit$times)]
    randomInitTimes <- rep(NA, times=length(infoInit$times))
    randomTermTimes <- rep(NA, times=length(infoTerm$times))
    randomInitTimes[1] <- infoInit$times[1]+
                           rnorm(1, mean=0, sd=sdNoiseFirstTime)
    randomTermTimes[1] <- randomInitTimes[1]+cvsDurs[1]
    for(i in 2:nrow(infoInit)) {
        randomInitTimes[i] <- randomTermTimes[i-1]+
                               rnorm(1, mean=statsInterCVSDelay$mean,
                                     sd=statsInterCVSDelay$sd)
        randomTermTimes[i] <- randomInitTimes[i]+cvsDurs[i]
    }
    randomInfoInit <- data.frame(cvSyllables=infoInit$cvSyllables,
                                  times=randomInitTimes)
    randomInfoTerm <- data.frame(cvSyllables=infoTerm$cvSyllables,
                                  times=randomTermTimes)
    return(list(randomInfoInit=randomInfoInit, randomInfoTerm=randomInfoTerm))
}
