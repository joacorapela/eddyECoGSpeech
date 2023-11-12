
buildEpochs <- function(data, epochsSamples, fromTime, toTime, sampleRate) {
    fromSample <- fromTime*sampleRate
    toSample <- toTime*sampleRate
    # In some cases the initial samples do not contain enough preceding data
    # to form a full epoch and in other cases the data contains less samples 
    # than was is indicated in the log file
    validEpochsSamples <- epochsSamples[which(1<epochsSamples+fromSample & 
                                              epochsSamples+toSample<
                                               length(data))]
    if(length(validEpochsSamples)>0) {
        epochs <- matrix(NA, ncol=length(validEpochsSamples),
                             nrow=toSample-fromSample+1)
        for(i in 1:ncol(epochs)) {
            epochs[,i] <- data[validEpochsSamples[i]+fromSample:toSample]
        }
        return(epochs)
    } else {
        return(NULL)
    }
}

