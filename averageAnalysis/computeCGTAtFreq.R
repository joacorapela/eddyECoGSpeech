computeCGTAtFreq <- function(data, freq, srate, nCycles) {
    scale <- nCycles*srate/(2*pi*freq)                       
    cgtRes <- cgt(input=trials[,i], nvoice=1, freqstep=2*freq/srate, scale=scale, plot=FALSE)

    for(i in 1:ncol(trials)) {                               
        if(i%%100==0) {                                      
            show(sprintf("Processed %d trials", i))
        }
        amplitudeERPImage <- cbind(amplitudeERPImage, Mod(cgtRes))
        phaseERPImage <- cbind(phaseERPImage, Arg(cgtRes))
    }               
    return(list(amplitudeERPImage=amplitudeERPImage,
                                  phaseERPImage=phaseERPImage))             
}                                                          

}
