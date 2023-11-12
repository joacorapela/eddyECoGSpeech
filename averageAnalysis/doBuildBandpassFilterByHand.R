
processAll <- function() {
    omegaTs1 <- 0.2
    omegaTp1 <- 0.4
    omegaTp2 <- 0.8
    omegaTs2 <- 1.0
    deltaTP  <- 1.06
    deltaTS1 <- 0.035
    deltaTS2 <- 0.035

    res <- designBandpassButterworthFilter(omegaTs1=omegaTs1,
                                            omegaTp1=omegaTp1,
                                            omegaTp2=omegaTp2,
                                            omegaTs2=omegaTs2,
                                            deltaTP=deltaTP,
                                            deltaTS1=deltaTS1,
                                            deltaTS2=deltaTS2)
    browser()

}

designBandpassButterworthFilter <- function(omegaTs1, 
                                             omegaTp1, 
                                             omegaTp2,
                                             omegaTs2,
                                             deltaTP,
                                             deltaTS1,
                                             deltaTS2) {
    getLowpassParameters <- function() {
        lowToBandpassWTransformation <- function(omega, omega1, omegah) {
            return((omega^2-w1*omegah)/(omega*(omegah-omega1)))
        }
        lowToBandpassSTransformation <- function(s, omega1, omegah) {
            return((s^2+omega1*omegah)/(s*(omegah-omega1)))
        }

        deltaP <- deltaTP # (10.72)
        deltaS <- min(deltaTS1, deltaTS2) # (10.72)
        omega1 <- omegaTp1
        omegah <- omegaTp2
        omegaP <- 1.0
        omegaS1 <- lowToBandpassWTransformation(omega=omegaTS1, omega1=omegaTP1,
                                                                omega2=omegaTP2)
        omegaS2 <- lowToBandpassWTransformation(omega=omegaTS2, omega1=omegaTP1,
                                                                omega2=omegaTP2)
        omegaS <- min(abs(omegaS1), abs(omegaS2))
        return(list(omegaP=omegaP, omegaS=omegaS, deltaP=deltaP, deltaS=deltaS))
    }
    ap <- -20*log10(1-deltaP)
    as <- -20*log10(deltaS)
    d <- ((10^(0.1*ap)-1)/(10^(0.1*as)-1))^.5
    k <- omegaP/omegaS

    N <- ceiling(log(1/d)/log(1/k))
    omega0LowerBound <- omegaP*((1-deltaP)^-2-1)^(-1/(2*N))
    omega0UpperBound <- omegaS*(deltaS^-2-1)^(-1/(2*N))
    omega0 <- (omega0LowerBound+omega0UpperBound)/2

    ks <- 0:(N-1)
    sks <- complex(real=-w0*sin((2*ks+1)*pi/(2*N)),
                    imaginary=w0*cos((2*ks+1)*pi/(2*N)))
}

processAll()

rm(processAll)
