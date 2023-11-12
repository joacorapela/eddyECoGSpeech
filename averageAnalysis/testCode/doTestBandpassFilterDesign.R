
processAll <- function() {
    srate <- 763
    # wp <- c(0.3, 7.0)
    # ws <- c(0.1, 15.0)
    wp <- c(100.0, 200.0)
    ws <- c(50.0, 250.0)
    # rp <- 0.5
    # rs <- 29
    rp <- 1.0
    rs <- 15.0

    btord <- buttord(Wp=wp/(srate/2), Ws=ws/(srate/2), Rp=rp, Rs=rs)
    # bt <- butter(btord)
    bt <- butter(n=btord$n, W=btord$W, type="pass")
    hf <- freqz(bt, Fs=srate)
    plot(x=hf$f, y=20*log10(abs(hf$h)), type="l",
                 xlab = "Frequency (Hz)", 
                 ylab = "Attenuation (dB)",
                 ylim=c(-10, 0))
    browser()
}

processAll()

rm(processAll)
